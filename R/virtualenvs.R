#' Open Python REPL in virtual environment
#'
#' \code{repl_python_virtualenv} Activates a Python virtual environment and
#' launches a Python REPL within that environment.
#'
#' @param envdir The virtual environment directory.
#'
#' @return `NULL`. Called for its side effects.
#'
#' @examples
#' \dontrun{
#'   repl_python_virtualenv("~/.venv/foo-bar/")
#' }
#'
#' @export
repl_python_virtualenv <- function(envdir) {
  if (!check_win()) stop("Designed to be used on Windows only", call. = FALSE)
  venv_status <- py_venv_exists(envdir)
  if (!venv_status[[1]]) {
    stop(sprintf("Could not find %s", venv_status[[2]]), call. = FALSE)
  }
  Sys.setenv("RETICULATE_PYTHON" = venv_status[[2]])
  on.exit(Sys.unsetenv("RETICULATE_PYTHON"))
  repl_python()
  invisible(NULL)
}

#' Create Python virtual environment
#'
#' \code{virtualenv_create_win} creates a new Python virtual environment and
#' installs updated versions of pip, setuptools, and wheel in addition to any
#' other user-provided packages that are available on
#' \href{https://pypi.org/}{PyPI}.
#'
#' @param envdir Directory to create virtual environment within.
#' @param envname Name of new virtual environment.
#' @param packages Additional python packages to install. When `NULL`, pip,
#'   setuptools, and wheel are all installed by default.
#'
#' @return `NULL`. Called for side effects.
#'
#' @examples
#' \dontrun{
#'   virtualenv_create_win("~/.venv/", "foo-bar", c("numpy", "pandas"))
#' }
#'
#' @export
virtualenv_create_win <- function(envdir, envname, packages = NULL) {
  if (!check_win()) stop("Designed to be used on Windows only", call. = FALSE)
  stopifnot(all(vapply(packages, is.character, logical(1))))
  b <- venv_create_body(envdir = envdir, envname = envname, packages = packages)
  writeLines(
    b,
    con = paste0(tempdir(), "\\makeNewVenv.ps1")
  )
  system2(
    "powershell",
    args = paste0(tempdir(), "\\makeNewVenv.ps1"),
    invisible = FALSE
  )
  invisible(NULL)
}

#' Translate filepaths to Windows style
#'
#' Filepaths in the Windows OS typically use `\` as a separator. This function
#' strips out all forward slashes and replaces them with back slashes.
#'
#' @param path Directory path.
#' @param check.exists A logical (not `NA`); check if directory exists.
#'
#' @examples
#' target_dir <- gsub("\\\\", "/", tempdir())
#'
#' # Convert directory path
#' win_dir(target_dir)
#' # Convert and check for directory existence
#' win_dir(target_dir, TRUE)
#'
#' @return Directory path in Windows format.
#'
#' @export
win_dir <- function(path, check.exists = FALSE) {
  if (check.exists) {
    stopifnot(dir.exists(path))
  } else {
    stopifnot(!is.null(path) && is.character(path))
  }
  gsub("/", "\\\\", path.expand(path))
}

# Helpers -----------------------------------------------------------------

check_win <- function() {
  identical(.Platform$OS.type, "windows")
}

py_venv_exists <- function(path) {
  is_there <- file.exists(paste0(path, "/Scripts/python.exe"))
  a_file <- win_dir(paste0(path, "/Scripts/python.exe"))
  list(is_there, a_file)
}

venv_create_body <- function(envdir, envname, packages = NULL) {
  stopifnot(is.null(packages) || is.vector(packages), is.character(envname))
  envdir <- win_dir(envdir, check.exists = TRUE)
  packages <- append(c("pip", "setuptools", "wheel"), packages)
  paste0(
    c(
      "# Create virtual env",
      sprintf("cd %s", envdir),
      sprintf("python -m venv %s", envname),
      "# Set execution policy (if necessary) and activate environment",
      sprintf("try { .\\%s\\Scripts\\Activate.ps1 }", envname),
      "catch {",
      "  Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser",
      sprintf("  .\\%s\\Scripts\\Activate.ps1", envname),
      "}",
      "# Install pip, setuptools, and wheel",
      paste("python -m pip install --upgrade --no-cache-dir", packages),
      "deactivate"
    ),
    collapse = "\n"
  )
}
