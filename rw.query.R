## This code has been written with th objective of exploring if the data coming from ReliefWeb could be used
# as an indicator of activity in the Humanitarian Sector. 
#  
#  Luis Capelo | capelo@un.org | @luiscape
# 

## Code Structure 
# The documentation for the API is available here: http://apidoc.rwlabs.org/
# The type can be: report, job, training, country, disaster. 


library(lubridate)
library(countrycode)
library(jsonlite) # This is the one that has been performing the best, although with several problems.
library(RCurl)



rw.query <- function(type = c("report", "job", "training", "country", "disaster"),
                     limit = c(1:1000), 
                     primary.country = "Syria",
                     field1 = "NA", 
                     field2 = "NA", 
                     field3 = "NA") { 
  
  require(jsonlite) # for reading the resulting JSON file.
  require(RCurl) # for making HTTP requests.
  
  # Building the query structure using HTTP POST.
  base.url <- paste("http://api.rwlabs.org/v0/", type, "/list", "?limit=", limit, "&", sep="") # For now always `list`.
  
  query <- data.frame() 
  
  if (field1 != "NA") { 
    field1 <- paste(base.url,"&fields[include][0]=", field1, sep = "") 
    url <- paste(base.url, field1, sep = "")
  } 
  if (field2 != "NA") { 
    field2 <- paste(base.url,"&fields[include][1]=", field2, sep = "")
    url <- paste(url, field2, sep = "")
  }
  if (field3 != "NA") {
    field3 <- paste(base.url,"&fields[include][2]=", field3, sep = "")
    url <- paste(url, field3, sep = "")
  }
  
  url <- paste(url, "&query[value]=primary_country:", primary.country, sep = "")
  
  
  ### Fetching the data.
  query <- data.frame(fromJSON(getURLContent(url)))
          
  # Error handler.
#   query <- tryCatch( 
#     data.frame(fromJSON(getURLContent(url))), 
#     error = function(e) e )
#   if(inherits(query, "error")) { query <- c(NA) }
      
  return(query)

  ## Cleaning the data.
  title <- query$data.list.fields[1]
  query <- cbind(query,title)
  
  date.created <- as.list(query$data.list.fields[2])
  test <- cbind(query, date.created)
  
  rw.url <- as.list(query$data.list.fields[3])
  query <- cbind(query, rw.url)
  
  query$data.list.fields <- NULL

  return(query)

}

