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
