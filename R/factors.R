#' Create Factor with NA as Reference
#'
#' Function that sets NA as factor reference level (if any NAs exist).
#'
#' @param var Vector to convert to factor
#' @param ... Additional arguments to pass to [factor()].
#'
#' @examples
#' no_na <- na_ref(1:10)
#' with_na <- na_ref(c(NA, 1:10))
#'
#' levels(no_na)
#' levels(with_na)
#'
#' @return `var` as a factor with NA as a reference level (if any exist).
#'
#' @export
na_ref <- function(var, ...) {
  if (is.factor(var)) {
    warning("Input is already a factor - returning it unchanged.", call. = FALSE)
    return(var)
  }
  var <- factor(var,
                exclude = NULL,
                levels = c(
                  NA,
                  unique(var)[!is.na(unique(var))]
                ),
                ...
  )
  var <- droplevels(var)
  return(var)
}

#' Create Sorted Factor
#'
#' Set factor with sorted ascending unique values as levels. Also allows the
#' user to explicitly set the reference level.
#'
#' @param var Vector to convert to factor.
#' @param base.level Reference level as a character string. Optional argument.
#' @param ... Additional arguments to pass to [factor()]
#'
#' @return A factor with levels in ascending order and a potential user-defined
#'   reference level.
#'
#' @examples
#' no_ref <- sort_factor(c("e", "f", "z", "d", "a"))
#' with_ref <- sort_factor(c("e", "f", "z", "d", "a"), base.level = "z")
#'
#' levels(no_ref)
#' levels(with_ref)
#'
#' @export
sort_factor <- function(var, base.level = NULL, ...) {
  if (is.factor(var)) {
    warning("Input is already a factor - returning it unchanged.", call. = FALSE)
    return(var)
  }
  if (!is.null(base.level)) {
    var <- droplevels(
      factor(var,
             levels = unique(
               c(
                 base.level,
                 sort(unique(var), na.last = TRUE)
               )
             ),
             ...
      )
    )
    return(var)
  } else {
    var <- droplevels(
      factor(var,
             levels = sort(unique(var), na.last = TRUE),
             ...
      )
    )
    return(var)
  }
}
