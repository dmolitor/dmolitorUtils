#' RMarkdown Skeleton
#'
#' This function creates an RMarkdown skeleton with pretty defaults
#'
#' @param name A string - the name of the RMarkdown document.
#' @param dir The target directory.
#' @param title The title of the RMarkdown document. Optional argument.
#' @param author The author of the RMarkdown document. Optional argument.
#' @param abstract A logical value - whether to include an Abstract section.
#'
#' @return Returns `NULL` invisibly. It is called for its side effects
#'
#' @export
rmd_skeleton <- function(name = "Untitled",
                         dir = here::here(),
                         title = NULL,
                         author = "Daniel Molitor",
                         abstract = FALSE) {
  path <- paste0(dir, "/", name, ".Rmd")
  if (abstract) {
    ab <- paste0(
      c("abstract: |",
        "  "),
      collapse = "\n"
    )
  } else {
    ab <- NULL
  }
  rmd_out <- paste0(
    c(
      "---",
      paste0("title: \"", title, "\""),
      paste0("author: \"",  author, "\""),
      "date: \"`r Sys.Date()`\"",
      "output:",
      "  html_document:",
      "    df_print: paged",
      "    highlight: haddock",
      "    theme: journal",
      "    toc: yes",
      "    toc_depth: 3",
      "    toc_float: yes",
      ab,
      "---",
      "",
      "```{r setup, include=FALSE}",
      "knitr::opts_chunk$set(eval = TRUE,",
      "                      echo = TRUE,",
      "                      warning = FALSE,",
      "                      message = FALSE)",
      "set.seed(123) # for replicating the results",
      "```",
      "",
      "# Requisite Packages",
      "```{r Requisite Packages}",
      "",
      "```",
      ""
    ),
    collapse = "\n"
  )
  writeLines(rmd_out, path)
  if (interactive()) {
    utils::file.edit(path)
  }
  invisible(NULL)
}
