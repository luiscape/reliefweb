#### ReliefWeb Package ####
# This script yet-to-be-package has been written with th objective of exploring
# data from the ReliefWeb service for research purposes.
#
# This work relies on ReliefWeb's API (which is still in aplha).
# The documentation for the API is available at http://apidoc.rwlabs.org/
#
#  Version:  0.1
#
#  Author: Luis Capelo | capelo@un.org | @luiscape


rw.query <- function(type = c("report", "job"), # These are the only two options available.
                     limit = c(1:1000, "all"), # 'all' means retreive all metadata from a certain query.
                     country = "Syria",
                     field1 = "NA",
                     field2 = "NA",
                     field3 = "NA",
                     field4 = "NA",
                     field5 = "NA",
                     debug = FALSE) {

  require(jsonlite) # for reading the resulting JSON file.
  require(RCurl) # for making HTTP requests.
  require(lubridate) # for working with dates.

  # If a limit isn't established, then it is emplied that we'll get all the data.
  if (limit == 'NULL') { limit <- c(1000) }
  if (limit == "all") { limit <- c(1000) }

  # This URL is the base-URL for acquiring data.
  base.url <- paste("http://api.rwlabs.org/v0/",
                    type,
                    "/list",
                    "?limit=",
                    limit,
                    sep = "")

  # This URL is for acquiring the 'count' metadata.
  count.url <- paste("http://api.rwlabs.org/v0/",
                     type,
                     "/count",
                     "?limit=",
                     limit,
                     sep = "")

  if (field1 != "NA") { query.url <- paste("&fields[include][0]=", field1, sep = "") }
  if (field2 != "NA") { query.url <- paste(query.url,"&fields[include][1]=", field2, sep = "") }
  if (field3 != "NA") { query.url <- paste(query.url,"&fields[include][2]=", field3, sep = "") }
  if (field4 != "NA") { query.url <- paste(query.url,"&fields[include][3]=", field4, sep = "") }
  if (field5 != "NA") { query.url <- paste(query.url,"&fields[include][4]=", field5, sep = "") }

  url <- paste(base.url, query.url,
               "&query[value]=country:",
               country,
               "&sort[0]=date.created:desc",
               sep = "")

  c.url <- paste(count.url,
                 "&query[value]=country:",
                 country,
                 "&sort[0]=date.created:desc",
                 sep = "")

  # Here we count how many results a query generates. Later the count number will be used to calculate iterations.
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

  ### Fetching the data. ###
  if (debug == TRUE) {
    x <- paste("The number of fields being fetched is: ", url, sep = "")
    print(x)
  }

  query <- data.frame(fromJSON(getURLContent(url)))

  # Function to convert the nested lists into rows in the data.frame.
  rw.fields <- function(df = "NA") {

    # Counting the number of fields. Will work better with regex.
    if (field1 != "NA") { a <- 1 }
    if (field2 != "NA") { a <- 2 }
    if (field3 != "NA") { a <- 3 }
    if (field4 != "NA") { a <- 4 }
    if (field5 != "NA") { a <- 5 }

    if (debug == TRUE) {
      x <- paste("This is how many fields we have: ", a, sep = "")
      print(x)
    }

    for (i in 1:a) {
        if (i == 1) { field <- field1 }
        if (i == 2) { field <- field2 }
        if (i == 3) { field <- field3 }
        if (i == 4) { field <- field4 }
        if (i == 5) { field <- field5 }

        if (debug == TRUE ) {
         if (i == 1) { print('And they are:') }
         print(field)
        }
        
        # I am having a problem here where the dates are not being parsed correctly. 
        x <- data.frame(as.list(df$data.list.fields[i]))
        df <- cbind(df, x)
#         colnames(df)[ncol(df)] <- field # adding names is confusing due to the odd order fields show up. 
    }
    df$data.list.fields <- NULL
    return(df)
  }

  query <- rw.fields(df = query)


  # Creating a metadata data.frame.
  if (debug != TRUE) {
    meta.data <- query[1, 1:7]
    write.csv(meta.data, file = "data/metadata.csv", row.names = FALSE)
  }

  if (debug == TRUE) {
    print("The 'colnames' are:")
    print(colnames(query))
    y <- paste("The number of rows 'query' are: ", nrow(query), sep = "")
    print(y)

    print("this is the final date row.")
    print(query$date.created[nrow(query)])
  }

  # Creating iterations to go around the limits issue.
  rw.it <- function(df = "NA") {
      to <- df$created[nrow(df)]
      final <- df
      
      # create progress bar
      total <- ceiling(count/limit)
      pb <- txtProgressBar(min = 0, max = total, style = 3)

      for (i in 2:total) {
        Sys.sleep(0.1)
        # update progress bar
        setTxtProgressBar(pb, i)

          to <- final$created[nrow(final)]

          it.url <- paste(url, "&filter[field]=date.created&filter[value][to]=", format(to, scientific = FALSE), sep = "")

        if (debug == TRUE) {
          x <- paste("This is the iteration URL: ", it.url, sep = "")
          print(x)
      
          y <- paste("And this is the iteration date: ", to, sep = "")
          print(y)
        }

          x <- data.frame(fromJSON(getURLContent(it.url)))

          x <- rw.fields(df = x)

          final <- rbind(final, x)
      }
      close(pb)
    return(final)
  }

query <- rw.it(df = query)
#   if (limit == "all") { query <- rw.it(df = query) } 

  ## Cleaning the data.
  rw.time <- function(df = "NA") {
    df$created <- df$created/1000 # to eliminate the miliseconds.
    df$created <- as.Date(as.POSIXct(as.numeric(df$created), origin = "1970-01-01"))
    return(df)
  }

  query <- rw.time(df = query) # to transform dates from Epoch to year-month-day.

  # Cleaning the dates.
  rw.clean.dates <- function (df = "NA") {
      x <- ymd(df$created) < ymd('2014-01-30')
      df <- cbind(df, x)
      x <- subset(df, df$x == 'TRUE')
    return(x)
  }

  query <- rw.clean.dates(df = query)

  # Cleaning useless columns.
    x <- query[8]
    y <- query[10:ncol(query) - 1]
  query <- cbind(x,y)

  # Cleaning duplicate entries.
  rw.clean.duplicates <- function(df = "NA") {
    query$duplicated <- duplicated(query)
    x <- subset(query, query$duplicated == FALSE)
    x$duplicated <- NULL
    return(x)
  }

  query <- rw.clean.duplicates(query)

  query$country <- country

# Storing the resulting data in a CSV file.
  write.csv(query, file = paste("data/", country, "-", type, ".csv", sep = ""))

  return(query)
}