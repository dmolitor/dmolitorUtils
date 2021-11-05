#' Create Factor with NA as Reference
#'
#' Function that sets NA as factor reference level (if any NAs exist).
#'
#' @param var Vector to convert to factor
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
na_ref <- function(var) {
  var <- factor(var,
                exclude = NULL,
                levels = c(
                  NA,
                  unique(var)[!is.na(unique(var))]
                )
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
#'
#' @return A factor with levels in ascending order and a potential user-defined
#'   reference level.
#'
#' @export
sort_factor <- function(var, base.level = NULL) {
  if (!is.null(base.level)) {
    var <- droplevels(
      factor(var,
             levels = c(
               base.level,
               sort(unique(var))[which(sort(unique(var)) != base.level)]
             )
      )
    )
    return(var)
  } else {
    var <- droplevels(
      factor(var,
             levels = sort(unique(var))
      )
    )
    return(var)
  }
}
