library(dmolitorUtils)

test_that("model_metrics works on continuous outcomes", {
  # OLS for continuous outcome
  set.seed(123)
  mtcarslm <- lm(mpg ~ ., mtcars)
  m <- model_metrics(mtcars$mpg, mtcarslm$fitted.values)

  expect_s3_class(m, class = c("tbl_df", "tbl", "data.frame"), exact = TRUE)
  expect_equal(names(m), c(".metric", ".estimator", ".estimate"))
  expect_equal(unname(lapply(m, class)), list("character", "character", "numeric"))
  expect_equal(m$`.metric`, c("rmse", "rsq", "mae"))
})


test_that("model_metrics works on binary outcomes", {
  # GLM for binary outcome
  set.seed(123)
  iris$isSetosa <- factor(as.integer(iris$Species == "setosa"))
  irisglm <- glm(isSetosa ~ Sepal.Length, iris, family = "binomial")
  g <- model_metrics(iris$isSetosa, irisglm$fitted.values, TRUE)

  expect_s3_class(g, class = c("tbl_df", "tbl", "data.frame"), exact = TRUE)
  expect_equal(names(g), c(".metric", ".estimator", ".estimate"))
  expect_equal(unname(lapply(g, class)), list("character", "character", "numeric"))
  expect_equal(g$`.metric`, c("accuracy", "kap", "mn_log_loss", "roc_auc"))
})
