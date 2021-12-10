# Function to extract formula as string
form_string <- function(x) {
  as.character(enquote(x))[[2]]
}

test_that("threeway_formula extracts sub-formulas correctly", {
  expect_equal(lapply(threeway_formula(a ~ b ~ c + z:x:q + b*all), form_string),
               list(form_string(a ~ c + z:x:q + b * all - b), form_string(b ~ c + z:x:q + b*all - a)))
  expect_equal(lapply(threeway_formula("a ~ b ~ c + z:x:q + b*all"), form_string),
               list(form_string(a ~ c + z:x:q + b * all - b), form_string(b ~ c + z:x:q + b*all - a)))
  expect_equal(lapply(threeway_formula(mpg ~ wt ~ . - carb, mtcars), form_string),
               list(form_string(mpg ~ . - carb - wt), form_string(wt ~ . - carb - mpg)))
})
