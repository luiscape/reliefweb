### Function for cleaning the dates of RW stream. 


clean.dates <- function (df = "NA") { 
  if (df != "NA" && is.data.frame(df) == TRUE) {
    require(lubridate) 
    x <- data.frame(ymd(df$date.created) < ymd('2014-01-30'))
    x <- cbind(df, x)
    x <- subset(df, df$x == 'TRUE') 
  } 
  return(x) 
}
