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
  require(lubridate) # for working with dates. 
  
  # Building the query structure using HTTP POST.
  base.url <- paste("http://api.rwlabs.org/v0/", 
                    type, 
                    "/list", 
                    "?limit=", 
                    limit, 
                    "&", sep="") # this one is for the data.
  
  count.url <- paste("http://api.rwlabs.org/v0/", 
                     type, 
                     "/count", 
                     "?limit=", 
                     limit, 
                     "&", sep="") # this one is for the count.
  
  if (field1 != "NA") { query.url <- paste("&fields[include][0]=", field1, sep = "") }
      
  if (field2 != "NA") { query.url <- paste(query.url,"&fields[include][1]=", field2, sep = "") }
  
  if (field3 != "NA") { query.url <- paste(query.url,"&fields[include][2]=", field3, sep = "") }

  url <- paste(base.url, query.url, 
               "&query[value]=primary_country:", 
               primary.country,
               "&sort[0]=date.created:desc", 
               sep = "")
  
  c.url <- paste(count.url, 
                 "&query[value]=primary_country:", 
                 primary.country,
                 "&sort[0]=date.created:desc", 
                 sep = "")
  
  # Creating the count function here. Later it will be used to calculate iterations.
  rw.count <- function() {
    sys.time <- as.data.frame(Sys.time()) # Getting the current time.
    colnames(sys.time)[1] <- "sys.time"
    count <- data.frame(fromJSON(getURLContent(c.url)))
    count <- count$data.count # Cleaning useless information.
    x <- cbind(sys.time, count)
    return(x)
  }
  
  count <- rw.count()
  count <- count$count
  
  ### Fetching the data.
  print("This is the URL being fetched:")
  print(url)
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
  
  rw.url <- as.list(query$data.list.fields[3])
  query <- cbind(query, rw.url)
  colnames(query)[13] <- "rw.url"
  
  query$data.list.fields <- NULL

  # Creating a metadata data.frame.
#   meta.data <- query[1, 1:7]
#   write.csv(meta.data, file = "data/metadata.csv", row.names = FALSE)  
  
  to <- query$date.created[nrow(query)]
  
  # Creating iterations to go around the limits issue.
  rw.it <- function(df = "NA") {
    final <- df
    for (i in 2:ceiling(count/limit)) {
      print(paste("This is iteration number: ", print(i), " out of ", ceiling(count/limit), " And we have fetched ", nrow(final), " out of ", print(count), " rows.", sep = ""))
      
        to <- final$date.created[nrow(final)]
        
        it.url <- paste(url, "&filter[field]=date.created&filter[value][to]=", format(to, scientific = FALSE), sep = "")
      
        x <- data.frame(fromJSON(getURLContent(it.url)))
        
        title <- x$data.list.fields[1]
        x <- cbind(x,title)
        colnames(x)[11] <- "title"
        
        date.created <- as.list(x$data.list.fields[2])
        x <- cbind(x, date.created)
        colnames(x)[12] <- "date.created"
        
        rw.url <- as.list(x$data.list.fields[3])
        x <- cbind(x, rw.url)
        colnames(x)[13] <- "rw.url"
        
        x$data.list.fields <- NULL
        
        final <- rbind(final, x)
    }
    return(final)
  } 

  query <- rw.it(df = query)

  query <- rw.time(df = query) # to transform dates from Epoch to year-month-day.
  
  # Cleaning the dates. 
  rw.clean.dates <- function (df = "NA") { 
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