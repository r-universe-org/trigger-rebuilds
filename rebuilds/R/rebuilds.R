#' Rebuild packages
#'
#' Automatically trigger workflows once every n days.
#'
#' @export
#' @rdname rebuilds
#' @param repository name of the github repository
#' @param workflow name of the workflow to trigger
#' @param days trigger rebuild every n days
trigger_rebuilds <- function(repository = 'r-universe/jeroen', workflow = 'build.yml', days = 30){
  url <- sprintf('https://github.com/%s', repository)
  stats <- package_stats(monorepo = url)
  age <- unclass(Sys.Date() - as.Date(stats$modified))
  select <- (age %/% days > 0) & (age %% days == 0)
  rebuilds <- stats[select,]
  print(rebuilds)
  for(pkg in rebuilds$file){
    rebuild_one(repository = repository, pkg = pkg, workflow = workflow)
  }
  print("All done!")
  invisible()
}

#' @export
#' @rdname rebuilds
#' @param universe name of the universe, use NULL for all universes
rebuild_vignettes <- function(universe = 'jeroen'){
  subdomain <- paste(sprintf('%s.', universe), collapse = '')
  endpoint <- sprintf('https://%sr-universe.dev/stats/vignettes?limit=100000', subdomain)
  df <- jsonlite::stream_in(url(endpoint), verbose = FALSE)
  df <- unique(df[c('universe', 'package')])
  row.names(df) <- NULL
  for(i in seq_len(nrow(df))){
    rebuild_one(paste0('r-universe/', df$universe[i]), df$package[i])
  }
  df
}

#' @export
#' @param before date before which to rebuild
#' @rdname rebuilds
rebuild_oldies <- function(universe, before = '2021-03-14'){
  subdomain <- paste(sprintf('%s.', universe), collapse = '')
  endpoint <- sprintf('https://%sr-universe.dev/stats/checks?limit=100000', subdomain)
  checks <- jsonlite::stream_in(url(endpoint), verbose = FALSE)
  dates <- vapply(checks$runs, function(runs){
    as.integer(runs$builder$date[1])
  }, integer(1))
  dates <- structure(dates, class = class(Sys.time()))
  df <- checks[dates < as.POSIXct(before),]
  for(i in seq_len(nrow(df))){
    rebuild_one(paste0('r-universe/', df$user[i]), df$package[i])
  }
  df
}

#' @export
#' @rdname rebuilds
rebuild_missing_binaries <- function(universe = 'ropensci'){
  endpoint <- sprintf('https://%s.r-universe.dev', universe)
  packages <- jsonlite::stream_in(url(paste0(endpoint, '/src/contrib')), verbose = FALSE)
  macos <- jsonlite::stream_in(url(paste0(endpoint, '/bin/macosx/contrib/4.1')), verbose = FALSE)
  windows <- jsonlite::stream_in(url(paste0(endpoint, '/bin/windows/contrib/4.1')), verbose = FALSE)
  missing_mac <- which(!paste(packages$Package, packages$Version) %in% paste(macos$Package, macos$Version))
  missing_win <- which(!paste(packages$Package, packages$Version) %in% paste(windows$Package, windows$Version))
  missing <- unique(c(missing_mac, missing_win))
  sapply(packages$Package[missing], function(pkg){
    rebuild_one(paste0('r-universe/', universe), pkg)
  })
}

#' @export
#' @rdname rebuilds
rebuild_missing_sources <- function(universe = 'ropensci'){
  available <- jsonlite::fromJSON(sprintf('https://%s.r-universe.dev/packages', universe))
  all <- jsonlite::fromJSON(sprintf('https://raw.githubusercontent.com/r-universe/%s/master/.metadata.json', universe))
  packages <- all$package
  missing <- packages[!(packages %in% available)]
  sapply(missing, function(pkg){
    rebuild_one(paste0('r-universe/', universe), pkg)
  })
}

#' @export
#' @rdname rebuilds
rebuild_all_missing_binaries <- function(){
  orgstats <- jsonlite::stream_in(url('https://r-universe.dev/stats/organizations'), verbose = FALSE)
  lapply(rev(orgstats$organization), function(orgname){
    rebuild_missing_binaries(orgname)
  })
}

#' @export
#' @rdname rebuilds
cancel_queued_builds <- function(universe = 'ropensci'){
  # limit here looks at .limit most recent runs, and then filters by status. So it needs to be high.
  runs <- gh::gh(sprintf('/repos/r-universe/%s/actions/runs', universe), status = 'queued', .limit = 1000)
  lapply(runs$workflow_runs, function(run){
    cat("Cancelling build", run$id, "in", universe, "\n")
    url <- sprintf('/repos/r-universe/%s/actions/runs/%d/cancel', universe, run$id)
    gh::gh(url, .method = 'POST')
  })
}

#' @export
#' @rdname rebuilds
cancel_all_queued_builds <- function(){
  universes <- gh::gh('/orgs/r-universe/repos', .limit = Inf)
  lapply(universes, function(universe){
    cat("Looking for queued builds in:", universe$name, "\n")
    cancel_queued_builds(universe$name)
  })
}

#' @export
#' @rdname rebuilds
#' @param pkg name of package to delete
delete_one <- function(universe, pkg){
  userpwd <- Sys.getenv("CRANLIKEPWD", NA)
  if(is.na(userpwd)) stop("No CRANLIKEPWD set, cannot deploy")
  message("Deleting: ", pkg)
  h <- curl::new_handle(customrequest = 'DELETE', userpwd = userpwd)
  url <- sprintf("https://%s.r-universe.dev/packages/%s", universe, pkg)
  res <- curl::curl_fetch_memory(url, handle = h)
  out <- jsonlite::fromJSON(rawToChar(res$content))
  stopifnot(out$Package == pkg)
}

delete_old_builds <- function(before = '2021-03-01'){
  checks <- jsonlite::stream_in(url('https://r-universe.dev/stats/checks?limit=9999999'), verbose = FALSE)
  checks$builddate <- structure(sapply(checks$runs, function(df){df$builder$date[1]}), class = class(Sys.time()))
  oldies <- checks[checks$builddate < before & checks$user != 'hrbrmstr_gitlab.com',]
  for(i in seq_along(oldies$user)){
    cat(sprintf("Deleting %s from %s\n", oldies$package[i], oldies$user[i]))
    delete_one(oldies$user[i], oldies$package[i])
    rebuild_one(paste0('r-universe/', oldies$user[i]), oldies$package[i])
  }
}

#' @import gert
package_stats <- function(monorepo){
  repo <- git_clone(monorepo, tempfile())
  modules <- git_ls(repo = repo)
  pkgs <- modules[!grepl("^\\.", modules$path),]
  git_stat_files(pkgs$path, repo = repo)
}

#' @importFrom gh gh
rebuild_one <- function(repository, pkg, workflow = 'build.yml'){
  cat(sprintf("Triggering rebuild of %s for %s in %s\n", workflow, pkg, repository))
  url <- sprintf('/repos/%s/actions/workflows/%s/dispatches', repository, workflow)
  gh(url, .method = 'POST', ref = 'master', inputs = list(package = pkg))
}

#' @export
#' @rdname rebuilds
rebuild_all_remotes <- function(){
  universes <- gh('/orgs/r-universe/repos', .limit = Inf)
  lapply(universes, function(x){
    cat("Checking universe:", x$name, "\n")
    rebuild_universe_remotes_only(x$name)
  })
}

#' @export
#' @rdname rebuilds
rebuild_universe_remotes_only <- function(universe){
  pkgs <- list_remote_packages(universe)
  lapply(pkgs, function(pkg){
    rebuild_one(paste0('r-universe/', universe), pkg = pkg)
  })
}

list_remote_packages <- function(user){
  tmp <- tempfile('.config')
  on.exit(unlink(tmp))
  url <- sprintf('https://raw.githubusercontent.com/r-universe/%s/master/.gitmodules', user)
  curl::curl_download(url, tmp)
  txt <- system(sprintf('git config --file %s --list', tmp), intern = T)
  lines <- grep('registered=false', txt, fixed = TRUE, value = TRUE)
  vapply(strsplit(lines, '.', fixed = TRUE), function(x){x[2]}, character(1))
}
