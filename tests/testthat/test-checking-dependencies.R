# Testing and installing dependencies.
context("Testing that the package has all it needs to work.")

test_that("The dependencies are properly installed in the system", {
  
  has.lubridate <- ifelse("lubridate" %in% rownames(installed.packages()), TRUE, FALSE)
  has.RCurl <- ifelse("RCurl" %in% rownames(installed.packages()), TRUE, FALSE)
  has.jsonlite <- ifelse("jsonlite" %in% rownames(installed.packages()), TRUE, FALSE)
  
  expect_that(has.lubridate, is_true())
  expect_that(has.RCurl, is_true())
  expect_that(has.jsonlite, is_true())

})