## Function to count the total of report available in RW. 
rw.count <- function() {
  require(rjson) # for reading the resulting JSON file.
  library(RCurl) # for queryig URLs. 
  
  sys.time <- as.data.frame(Sys.time())
  colnames(sys.time)[1] <- "sys.time"
  line <- data.frame()
  
  url <- c("http://api.rwlabs.org/v0/report/count")
  
  count <- tryCatch( # Error handler.
    data.frame(fromJSON(getURLContent(url))), 
    error = function(e) e
  )
  if(inherits(count, "error")) { count <- c(NA) }
  count <- count$data.count
  
  final <- cbind(sys.time, count)
  
  return(final)
}
