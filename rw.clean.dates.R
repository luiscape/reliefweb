### Function for cleaning the dates of RW stream. 


rw.clean.dates <- function (df = "NA") { 
  require(lubridate) 
    x <- ymd(df$date.created) < ymd('2014-01-30')
    df <- cbind(df, x)
    x <- subset(df, df$x == 'TRUE') 
  return(x) 
}
