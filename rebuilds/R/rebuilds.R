#' Rebuild packages
#'
#' Automatically trigger workflows once every n days.
#'
#' @export
#' @param repository name of the github repository
#' @param workflow name of the workflow to trigger
#' @param days trigger rebuild every n days
trigger_rebuilds <- function(repository = 'r-universe/r-lib', workflow = 'build.yml', days = 30){
  url <- sprintf('https://github.com/%s', repository)
  stats <- package_stats(monorepo = url)
  age <- unclass(Sys.Date() - as.Date(stats$modified))
  rebuilds <- stats[age %% days == 0,]
  print(rebuilds)
  for(pkg in rebuilds$file){
    cat(sprintf("Triggering rebuild of %s for %s\n", workflow, pkg))
    url <- sprintf('/repos/%s/actions/workflows/%s/dispatches', repository, workflow)
    gh::gh(url, .method = 'POST', ref = 'master', inputs = list(package = pkg))
  }
  print("All done!")
  invisible()
}

#' @import gert
package_stats <- function(monorepo){
  repo <- git_clone(monorepo, tempfile())
  modules <- git_ls(repo = repo)
  pkgs <- modules[!grepl("^\\.", modules$path),]
  git_stat_files(pkgs$path, repo = repo)
}
