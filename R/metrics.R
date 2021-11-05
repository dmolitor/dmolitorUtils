#' Calculate Model Metrics
#'
#' Calculates a variety of standard model metrics, given the truth and
#' predicted values as vectors.
#'
#' @param truth A vector - the true outcome values.
#' @param predicted A vector - the predicted outcome values.
#' @param categorical A logical - Is the true outcome a categorical response
#'   variable? This function only handles the binary case where `truth` is a
#'   factor with levels 0 and 1.
#'
#' @return A tibble with calculated model metrics
#'
#' @export
model_metrics <- function(truth, predicted, categorical = FALSE) {
  if (categorical) {
    truth <- as.numeric(as.character(truth))
    if (!all(truth %in% c(0, 1))) stop("Only handles binary case", call. = FALSE)
    dat <- dplyr::tibble(
      "truth" = factor(truth, levels = c(0, 1)),
      "0" = 1 - predicted,
      "1" = predicted,
      "predicted" = factor(
        dplyr::case_when(
          `1` > 0.5 ~ 1,
          TRUE ~ 0
        ),
        levels = c(0, 1)
      )
    )
    metrics <- yardstick::metrics(dat, truth, predicted, `0`)
  } else {
    dat <- dplyr::tibble(
      "truth" = truth,
      "predicted" = predicted
    )
    metrics <- yardstick::metrics(dat, truth = truth, estimate = predicted)
  }
  metrics
}
