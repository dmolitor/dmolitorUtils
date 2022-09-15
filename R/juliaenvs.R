#' Set up R project to activate project-local Julia environments.
#'
#' Similar to the RStudio settings that allow the user to automatically
#' activate project-local Python environments, this function sets up the current
#' project to activate local Julia environments.
#'
#' @return TRUE invisibly, if function executes successfully. This funcion is
#'   called entirely for its side effects.
#' @export
#' @examples
#' \dontrun{
#'   # This function must be called within a directory that is an RStudio Project
#'   activate_julia_env()
#' }
activate_julia_env <- function() {
  in_r_project <- any(grepl("*.Rproj", list.files(all.files = TRUE)))
  if (!in_r_project) {
    stop(
      "It appears that '", getwd(), "' is not a Project",
      call. = FALSE
    )
  }
  rprofile_exists <- any(rp <- grepl(".Rprofile", list.files(all.files = TRUE)))
  julia_env <- {
    project <- grepl(
      "*Project.toml",
      files <- list.files(all.files = TRUE, recursive = TRUE)
    )
    manifest <- grepl("*Manifest.toml", files)
    if (
      (!any(project) || !any(manifest)) ||
      (dirname(files[project]) != dirname(files[manifest]))
    ) {
      stop(
        "It appears that '", getwd(), "' contains no valid Julia project",
        call. = FALSE
      )
    }
    dirname(paste0(getwd(), "/", files[project]))
  }
  if (rprofile_exists) {
    rprofile <- readLines(".Rprofile")
    rprofile <- c(
      rprofile,
      paste0("Sys.setenv(JULIA_PROJECT='", julia_env, "')")
    )
    writeLines(rprofile, ".Rprofile")
  } else {
    rprofile <- paste0("Sys.setenv(JULIA_PROJECT='", julia_env, "')")
    writeLines(rprofile, ".Rprofile")
  }
  message("Please restart your R session for your changes to take place")
  invisible(TRUE)
}
