## This code has been written with th objective of exploring if the data coming from ReliefWeb could be used
# as an indicator of activity in the Humanitarian Sector. 
#  
#  Luis Capelo | capelo@un.org | @luiscape

## Code Structure 
# The documentation for the API is available here: http://apidoc.rwlabs.org/
# The type can be: report, job, training, country, disaster. 

rw.query <- function(type = c("report", "job", "training", "country", "disaster"),
                     limit = c(1:1000), 
                     primary.country = "Syria",
                     field1 = "NA", 
                     field2 = "NA", 
                     field3 = "NA",
                     clean.dates = c(TRUE, FALSE)) { 
  
  require(jsonlite) # for reading the resulting JSON file.
  require(RCurl) # for making HTTP requests.
  
  # RW functions. 
#   require(rw.time) # for converting the Epoch times.
#   require(rw.clean.dates) # for cleaning the weird dates.
  
  # Building the query structure using HTTP POST.
  base.url <- paste("http://api.rwlabs.org/v0/", type, "/list", "?limit=", limit, "&", sep="") # For now always `list`.
  
  query <- data.frame() 
  
  if (field1 != "NA") { 
    field1 <- paste(base.url,"&fields[include][0]=", field1, sep = "") 
    url <- field1
  } 
  if (field2 != "NA") { 
    field2 <- paste(base.url,"&fields[include][1]=", field2, sep = "")
    url <- field2
  }
  if (field3 != "NA") {
    field3 <- paste(base.url,"&fields[include][2]=", field3, sep = "")
    url <- field3
  }
  
  url <- paste(url, 
               "&query[value]=primary_country:", 
               primary.country,
               "&sort[0]=date.created:desc", 
               sep = "")
  
  print(url)
  ### Fetching the data.
  query <- data.frame(fromJSON(getURLContent(url)))
  return(query)

  
  ## Cleaning the data.
  title <- query$data.list.fields[1]
  query <- cbind(query,title)
  
  date.created <- as.list(query$data.list.fields[2])
  query <- cbind(query, date.created)
  query <- rw.time(query)
  
  rw.url <- as.list(query$data.list.fields[3])
  query <- cbind(query, rw.url)
  
  query$data.list.fields <- NULL
  
  # Creating a metadata data.frame.
  meta.data <- query[1, 1:7]
  
  # Cleaning useless columns.
  x <- query[8]
  y <- query[10:12]
  
  query <- cbind(x,y)

  return(query)
  
  # Cleaning the dates. 
  if (clean.dates == TRUE) { query <- clean.dates(query) } 
    else return(query)

}