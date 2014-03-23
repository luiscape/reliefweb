#### ReliefWeb Package ####
# This script yet-to-be-package has been written with th objective of exploring
# data from the ReliefWeb service for research purposes.
#
# This work relies on ReliefWeb's API (which is still in aplha).
# The documentation for the API is available at http://apidoc.rwlabs.org/
#
#  Version:  0.1.1
#
#  Author: Luis Capelo | capelo@un.org | @luiscape


#### Development Notes #### 
# - Change the 'country' param for 'query'. And add the 'query-value' param.
# - Change the 'fields' for the actual API params. 


rw.query <- function(type = NULL,  # These are the only two options available: "report" and "job".
                     limit = NULL,  # Can be a number from 1 to 1000 or "all".
                     country = NULL,  # Queries a particular country (or a list). 'all' queries all countries. 
                     field1 = NULL,
                     field2 = NULL,
                     field3 = NULL,
                     field4 = NULL,
                     field5 = NULL,
                     from = NULL,  # Parameter not implemented.
                     to = NULL,  # Parameter not implemented.
                     debug = FALSE,  # Prints debug and system warnings.
                     csv = FALSE) {  # Stores the resulting data.frame in a standard CSV file together with a metadata file.

  require(jsonlite)  # for reading the resulting JSON file.
  require(RCurl)  # for making HTTP requests.
  require(lubridate)  # for working with dates.

  country.url <- c("&query[value]=country:")

  # Adding the limit numbers and the country parameter.
  if (is.null(limit) == TRUE) { stop("Please provide the 'limit' parameter.") }

  # If 'all' countries are required it clears the 'country' query from the URL.
  if (country == "all") { country <- NULL
                          country.url <- NULL }


  # Base-URL for acquiring data.
  base.url <- paste("http://api.rwlabs.org/v0/",
                    type,
                    "/list",
                    "?limit=",
                    ifelse(limit == "all", 1000, limit), # Handles the `all` case for the `limit` parameter. 
                    sep = "")

  # URL for acquiring the 'count' metadata.
  count.url <- paste("http://api.rwlabs.org/v0/",
                     type,
                     "/count",
                     "?limit=", 
                     ifelse(limit == "all", 1000, limit), # Handles the `all` case for the `limit` parameter. 
                     sep = "")

  # Conditional statements for building the query URL. 
  if (is.null(field1) == FALSE) { query.url <- paste("&fields[include][0]=", field1, sep = "") }
  if (is.null(field2) == FALSE) { query.url <- paste(query.url,"&fields[include][1]=", field2, sep = "") }
  if (is.null(field3) == FALSE) { query.url <- paste(query.url,"&fields[include][2]=", field3, sep = "") }
  if (is.null(field4) == FALSE) { query.url <- paste(query.url,"&fields[include][3]=", field4, sep = "") }
  if (is.null(field5) == FALSE) { query.url <- paste(query.url,"&fields[include][4]=", field5, sep = "") }

  # Query URL.
  url <- paste(base.url, query.url,
               country.url,
               country,
               "&sort[0]=date.created:desc",
               sep = "")

  # Count URL.
  c.url <- paste(count.url,
                 country.url,
                 country,
                 "&sort[0]=date.created:desc",
                 sep = "")

  # Here we count how many results a query generates. Later the count number will be used to calculate iterations.
  rw.count <- function() {
    sys.time <- as.data.frame(Sys.time())  # Getting the current time.
    colnames(sys.time)[1] <- "sys.time"
    count <- data.frame(fromJSON(getURLContent(c.url)))
    count <- count$data.count  # Cleaning useless information.
    x <- cbind(sys.time, count)
    return(x)
  }

  # Running the function and keeping the data we want.
  count <- rw.count()
  count <- count$count


  #### Fetching the data. ####
  if (debug == TRUE) {
    x <- paste("The URL being queried is: ", url, sep = "")
    print(x)
  }

  # Using `jsonlite` and `RCurl` to fetch JSON from the URL.
  query <- data.frame(fromJSON(getURLContent(url)))

  # Function to convert the nested lists into rows in the data.frame.
  rw.fields <- function(df = "NA") {

    # Counting the number of fields.
    # (Note: should work better with regex.)
    if (is.null(field1) == FALSE) { n.field <- 1 }
    if (is.null(field2) == FALSE) { n.field <- 2 }
    if (is.null(field3) == FALSE) { n.field <- 3 }
    if (is.null(field4) == FALSE) { n.field <- 4 }
    if (is.null(field5) == FALSE) { n.field <- 5 }

    for (i in 1:n.field) {
        if (i == 1) { field <- field1 }
        if (i == 2) { field <- field2 }
        if (i == 3) { field <- field3 }
        if (i == 4) { field <- field4 }
        if (i == 5) { field <- field5 }

        ### Problem ###
        # There is an issue here with the kind of fields the user queries.
        # Nested fields break the function.
        # The solution below doesn't address the issue and only works with non-nested items.

        x <- data.frame(as.list(df$data.list.fields[i]))
        df <- cbind(df, x)
    }
    df$data.list.fields <- NULL
    return(df)
  }

  query <- rw.fields(df = query)

  # Creating a metadata data.frame.
  if (csv == TRUE) {
    meta.data <- query[1, 1:7]
    write.csv(meta.data, file = paste("data/", ifelse(country == "all", "all", country), "-", type, "-metadata.csv", sep = ""), row.names = FALSE)
  }

  # UI element.
  print(paste("Fetching ~", ifelse(limit == "all", count, limit), " records.", sep = ""))

  # Creating iterations to go around the 1000-limitation.
  rw.it <- function(df = "NA") {
    limit <- ifelse(limit == "all", 1000, limit)
      to <- df$created[nrow(df)]
      final <- df

      # Create progress bar.
      total <- ceiling(count/limit)
      pb <- txtProgressBar(min = 0, max = total, style = 3)

      for (i in 2:total) {

        # Update progress bar.
        setTxtProgressBar(pb, i)

          to <- final$created[nrow(final)]

          it.url <- paste(url, "&filter[field]=date.created&filter[value][to]=", format(to, scientific = FALSE), sep = "")

        if (debug == TRUE) {
          print(paste("This is the it.url", it.url, sep = ""))
          print(paste("From iteration number ", i, sep = ""))
        }

          x <- data.frame(fromJSON(getURLContent(it.url)))

          x <- rw.fields(df = x)

          final <- rbind(final, x)

        }
      close(pb)
    return(final)
  }

#    query <- rw.it(df = query)

  # Only run iterator if we are fetching "all" entries.
  # Note: improve to be more > 1000.
  if (limit == "all") { query <- rw.it(df = query) }


  #### Cleaning the resulting data. ####

  # Transform dates from Epoch to year-month-day.
  rw.time <- function(df = "NA") {
    df$created <- df$created/1000 # To eliminate the miliseconds.
    df$created <- as.Date(as.POSIXct(as.numeric(df$created), origin = "1970-01-01"))
    return(df)
  }

  query <- rw.time(df = query)

  # Can't remember what this function is for.
  # Cleaning the dates.
  rw.clean.dates <- function (df = "NA") {
      x <- ymd(df$created) < ymd('2014-01-30')
      df <- cbind(df, x)
      x <- subset(df, df$x == 'TRUE')
    return(x)
  }

#   query <- rw.clean.dates(df = query)

#   print(n.field)  # Will have to re-create the check fields function here.

  # Cleaning useless columns.
#   query <- cbind(query$data.list.id,query$field1,)


  # Cleaning duplicate entries.
  rw.clean.duplicates <- function(df = "NA") {
    query$duplicated <- duplicated(query)
    x <- subset(query, query$duplicated == FALSE)
    x$duplicated <- NULL
    return(x)
  }

  query <- rw.clean.duplicates(query)

  ### Problem here adding a column with the all countries label. ###
  country <- ifelse(is.null(country) == TRUE, "All", country)
  query$country <- country

  # Keeping only the columns of interest.
  x <- query[8] # data.list.id
  y <- query[10:ncol(query)] # the fetched fields.
  query <- cbind(x, y)

  # Storing the resulting data in a CSV file.
  if (csv == TRUE) {
    write.csv(query, file = paste("data/", ifelse(country == "all", "all", country), "-", type, ".csv", sep = ""))
  }

  print("Done.")
  return(query)
}