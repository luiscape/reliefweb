# Testing rw.query
context("Testing that rw.query is working properly.")

test_that("The elements of the query are being returned correctly.", {
  
  aleppo <- rw.query(entity = 'report', limit = 10, text.query = "Aleppo", 
                     add.fields = c('id', 'title', 'date.created'))
  
  df <- is.data.frame(aleppo)
  row.numbers <- ifelse(nrow(aleppo) == 10, TRUE, FALSE)
  number.of.fields <- ifelse(length(aleppo) == 3, TRUE, FALSE)
  
  expect_that(df, is_true())
  expect_that(row.numbers, is_true())
  expect_that(number.of.fields, is_true())

})