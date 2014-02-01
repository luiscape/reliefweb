## Function to convert Epoch time (in milliseconds) to dates.
#
# A data.frame has to be provided and the header has to be "time". 

rw.time <- function(df = NULL) {
  df$created <- gsub("000", replacement = "", x = df$created)
  df$created <- as.Date(as.POSIXct(as.numeric(df$created), origin = "1970-01-01"))
  return(df)
}

clean.dates <- function (df = NULL) { 
  require(lubridate) 
  
  x <- ymd(df$x) < ymd('2014-01-30')
  x <- data.frame(x)

  return(x)
}

test <- cbind(test,x)


test <- subset(test, test$x == TRUE)
test$x <- NULL
