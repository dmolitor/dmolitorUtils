test_that("win_dir correctly translates file paths", {
  # These are Windows-specific functions
  skip_on_os(os = c("mac", "linux", "solaris"))
  # Change dir path to R-style
  target_dir <- gsub("\\\\", "/", tempdir())
  target_dir_bad <- paste0(target_dir, "/bad")

  expect_equal(win_dir(target_dir), tempdir())
  expect_equal(win_dir(target_dir, TRUE), tempdir())
  expect_equal(win_dir(target_dir_bad), paste0(tempdir(), "\\bad"))
  expect_error(win_dir(target_dir_bad, TRUE))
  expect_equal(win_dir(tempdir(), TRUE), tempdir())
})
