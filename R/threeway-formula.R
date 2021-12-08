formula_lhs <- function(form) {
  form <- tryCatch(
    as.formula(form),
    error = function(e) stop("Invalid formula", call. = FALSE)
  )
  if (length(form) < 3) stop("Formula is missing LHS", call. = FALSE)
  trimws(form[[2]])
}

formula_rhs <- function(form, data = NULL) {
  if (!(is.data.frame(data) || is.null(data))) {
    stop("Argument `data` must be a `data.frame`", call. = FALSE)
  }
  form <- tryCatch(
    as.formula(form),
    error = function(e) stop("Invalid formula", call. = FALSE)
  )
  dat <- data[1, ]
  if (length(form) < 3) rhs <- form[[2]] else rhs <- form[[3]]
  rhs_form <- as.formula(paste0("~", as.character(enquote(rhs))[[2]]))
  if (!is.null(data)) {
    out <- all.vars(terms(rhs_form, data = dat, simplify = TRUE)[[2]])
  } else {
    out <- all.vars(terms(rhs_form, simplify = TRUE)[[2]])
  }
  out
}

#' Convert a threeway formula into distinct sub-formulas
#'
#' A threeway formula is one with the form of `a ~ b ~ c` that implies the
#' following distinct sub-formulas: `a ~ b` and `a ~ c`. This function splits
#' such a threeway formula into its distinct sub-formulas.
#'
#' @param form A `formula` or string that is coercible to one.
#' @param data An optional `data.frame` from which to extract formula terms.
#'
#' @examples
#' threeway_formula(z ~ y ~ w + w:x)
#' threeway_formula("z ~ y ~ w + w:x")
#' threeway_formula(mpg ~ carb ~ . - wt + gear:carb, data = mtcars)
#'
#' @return A list of length 2, where the components are the sub-formulas of the
#'   provided threeway formula.
#'
#' @export
threeway_formula <- function(form, data = NULL) {
  stopifnot(length(as.formula(form)) == 3)
  form <- tryCatch(
    as.formula(form),
    error = function(e) stop("Invalid formula", call. = FALSE)
  )
  lhs_form <- tryCatch(
    as.formula(form[[2]]),
    error = function(e) stop("Invalid LHS formula", call. = FALSE)
  )
  y1 <- formula_lhs(lhs_form)
  y2 <- formula_rhs(lhs_form, data = data)
  if (length(y2) != 1) stop("The formula is formatted incorrectly", call. = FALSE)
  rhs <- as.character(enquote(form[[3]]))[[2]]
  list(
    as.formula(paste0(y1, "~", rhs), env = globalenv()),
    as.formula(paste0(y2, "~", rhs), env = globalenv())
  )
}
