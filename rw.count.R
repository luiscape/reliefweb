## Function to count the total of report available in RW. 
#  Author: Luis Capelo | @luiscape | capelo@un.org

rw.count <- function(type = c("report", "job", "training", "country", "disaster")) {
  require(jsonlite) # for reading the resulting JSON file. Try to use `jsonlite` instead.
  require(RCurl) # for queryig URLs. 
  sys.time <- as.data.frame(Sys.time()) # Getting the current time.
  colnames(sys.time)[1] <- "sys.time"
    url <- paste("http://api.rwlabs.org/v0/", type, "/count", sep = "")
    count <- data.frame(fromJSON(getURLContent(url)))
    count <- count$data.count # Cleaning useless information.
    x <- cbind(sys.time, count)
  return(x)
}