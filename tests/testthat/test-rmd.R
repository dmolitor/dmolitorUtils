library(dmolitorUtils)

test_that("rmd_skeleton successfully creates a .Rmd skeleton", {
  # Create empty .Rmd skeleton in temp directory
  target_dir <- tempdir()
  rmd_path <- rmd_skeleton(name = "test",
                           dir = target_dir,
                           title = "Test Document")

  expect_true(file.exists(paste0(target_dir, "/test.Rmd")))
  expect_true(file.remove(paste0(target_dir, "/test.Rmd")))
  expect_equal(paste0(target_dir, "/test.Rmd"), rmd_path)
})
