## Function to convert Epoch time (in milliseconds) to dates.
#
# The function has an issue: it handles miliseconds by stripping "000" out of the 
# Epoch time. That isn't the best method. Find a better method to handle miliseconds.
# 
# A data.frame has to be provided and the header has to be "time". 


## The `gsub` needs to be corrected. Now it catches anything with "000" anywhere in the string.
## I need to catch the "000" in the end of the string.

rw.time <- function(df = "NA") {
  if (df != "NA" && is.data.frame(df) == TRUE) {
    df$date.created <- gsub("000", replacement = "", x = df$date.created) 
    df$date.created <- as.Date(as.POSIXct(as.numeric(df$date.created), origin = "1970-01-01"))
  return(df) } else print("Error: the RW data.frame isn't recognized.") 
}