## This code has been written with th objective of exploring if 
# the data coming from ReliefWeb could be used
# as an indicator of activity in the Humanitarian Sector. 
#  
#  Author: Luis Capelo | capelo@un.org | @luiscape

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

  # Building the query structure using HTTP POST.
  base.url <- paste("http://api.rwlabs.org/v0/", 
                    type, 
                    "/list", 
                    "?limit=", 
                    limit, 
                    "&", sep="") # For now always `list`.
  
  if (field1 != "NA") { url <- paste(base.url,"&fields[include][0]=", field1, sep = "") }
      
  if (field2 != "NA") { url <- paste(url,"&fields[include][1]=", field2, sep = "") }
  
  if (field3 != "NA") { url <- paste(url,"&fields[include][2]=", field3, sep = "") }
  
  
  # add for loop here. 
  for (i in 1:ceiling(5100/limit)) {
    url <- paste(url, "&filter[field]=date.created&filter[value][to]=", to, sep = "")
  }
  
  if (field3 != "NA") { url <- paste(url,"&fields[include][2]=", field3, sep = "") } ## add the to: filter here.

  url <- paste(url, 
               "&query[value]=primary_country:", 
               primary.country,
               "&sort[0]=date.created:desc", 
               sep = "")
  
  print("This is the URL being fetched:")
  print(url)
  
  ### Fetching the data.
  query <- data.frame(fromJSON(getURLContent(url)))
  
  ## Cleaning the data.
  rw.time <- function(df = "NA") {
      df$date.created <- df$date.created/1000
      df$date.created <- as.Date(as.POSIXct(as.numeric(df$date.created), origin = "1970-01-01"))
      return(df) 
  }
  
  title <- query$data.list.fields[1]
  query <- cbind(query,title)
  colnames(query)[11] <- "title"
  
  date.created <- as.list(query$data.list.fields[2])
  query <- cbind(query, date.created)
  colnames(query)[12] <- "date.created"
  
  query <- rw.time(df = query)
  
  
  rw.url <- as.list(query$data.list.fields[3])
  query <- cbind(query, rw.url)
  colnames(query)[13] <- "rw.url"
  
  query$data.list.fields <- NULL

  # Creating a metadata data.frame.
  meta.data <- query[1, 1:7]
  write.csv(meta.data, file = "data/metadata.csv", row.names = FALSE)
  
  # Get the first epoch.
  a <- nrow(query)
  to <- query[a, 3]
  
  # Cleaning the dates. 
  rw.clean.dates <- function (df = "NA") { 
    require(lubridate) 
      x <- ymd(df$date.created) < ymd('2014-01-30')
      df <- cbind(df, x)
      x <- subset(df, df$x == 'TRUE') 
    return(x) 
  }
  
  query <- rw.clean.dates(df = query)
  
  # Cleaning useless columns.
    x <- query[8]
    y <- query[10:12]
  query <- cbind(x,y)

  
  # Storing the resulting data in a CSV file.
  write.csv(query, file = paste("data/", primary.country, ".csv", sep = ""))
  
  return(query)
}



