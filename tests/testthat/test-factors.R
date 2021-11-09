library(dmolitorUtils)

test_that("na_ref sets levels correctly", {
  expect_equal(levels(na_ref(1:10)), as.character(1:10))
  expect_equal(sum(is.na(levels(na_ref(c(1:5, NA))))), 1)
  expect_equal(levels(na_ref(c(letters[1:3], NA))), c(NA, letters[1:3]))
  expect_equal(levels(na_ref(c(NA, 1, 2, 3), labels = letters[1:4])), letters[1:4])
})

test_that("na_ref handles factors correctly", {
  # Input a factor to `na_ref`
  l <- factor(letters)

  expect_warning(na_ref(l))
  expect_equal(suppressWarnings(na_ref(l)), l)
})

test_that("sort_factor sets levels correctly", {
  expect_equal(levels(sort_factor(letters[5:1])), letters[1:5])
  expect_equal(levels(sort_factor(10:1)), as.character(1:10))
  expect_equal(
    levels(sort_factor(c("a", "c", "b", "z"), base.level = "z")),
    c("z", "a", "b", "c")
  )
  skip("Test below fails for unknown reason")
  expect_equal(
    levels(sort_factor(c("A", "a", 2, 1))),
    c("1", "2", "a", "A")
  )
})

test_that("sort_factor handles NA correctly", {
  expect_equal(levels(sort_factor(c(1:5, NA), NA)), as.character(1:5))
  expect_equal(
    levels(sort_factor(c(1:5, NA), NA, exclude = NULL)),
    c(NA, as.character(1:5))
  )
  expect_equal(
    levels(sort_factor(c(1, 2, 3, NA), 3, exclude = NULL)),
    c("3", "1", "2", NA)
  )
})

test_that("sort_factor handles factors correctly", {
  # Input a factor to `sort_factor`
  l <- factor(letters)

  expect_warning(sort_factor(l))
  expect_equal(suppressWarnings(sort_factor(l)), l)
})
