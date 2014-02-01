## Function to count the total of report available in RW. 
#  Luis Capelo | @luiscape | capelo@un.org

rw.count <- function() {
  require(rjson) # for reading the resulting JSON file.
  require(RCurl) # for queryig URLs. Try to use `jsonlite` instead.
  
  sys.time <- as.data.frame(Sys.time()) # Getting the current time.
  colnames(sys.time)[1] <- "sys.time"
  
    count <- tryCatch( # Error handler.
      data.frame(fromJSON(getURLContent("http://api.rwlabs.org/v0/report/count"))), 
      error = function(e) e
    )
    if (inherits(count, "error")) { count <- c(NA) }
  
  count <- count$data.count # Cleaning useless information.
  
  final <- cbind(sys.time, count)
  
  return(final)
}