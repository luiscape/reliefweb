#### ReliefWeb Package ####
# This script yet-to-be-package has been written with th objective of exploring
# data from the ReliefWeb service for research purposes.
#
# This work relies on ReliefWeb's API (which is still in aplha).
# The documentation for the API is available at http://apidoc.rwlabs.org/
#
#  Version:  0.1.3
#
#  Author: Luis Capelo | capelo@un.org | @luiscape

#' @param entity = report, job, training, disaster or country. In this version only 'report' is available.
#' @param query.field = single field to be queried. Run rw.query.fields() for a list of fields that can be queried.
#' @param query.field.value = the value of the field.
#' @param add.fields = what fields should be returned in the query. date.created comes in all queries.
#' @param limit = it can be any number between 1 and 1000 or 'all'. If all, the result will be all of the query. The default is 10.
#' @param country = to specify a query about a country.
#' @param from = from a certain date. The date has to be in the YYYY-MM-DD format. NOT IMPLEMENTED YET. 
#' @param to = to a certain date.The date has to be in the YYYY-MM-DD format. NOT IMPLEMENTED YET.
#' @param debug = for entering the debugging mode. This prints a number of statements making easier to debug.
#' @param csv = if TRUE will store two CVS viles: one with the resulting data.frame from the query and a metadata file.
#' @param fields = what fields should appear.


rw.query <- function(entity = NULL,  # Any of the entities available: report, job, training, disaster or country.  (For now only 'report' is available.)
                     limit = NULL,  # Can be a number from 1 to 1000 or "all". The default is 10.
                     text.query = NULL,  # Open query field.
                     query.field = NULL,  # Query using the standard fields. Only one field can be queried at a time.
                     query.field.value = NULL,  # Provide the value of the query.
                     add.fields = NULL,  # What fields should the query return. date.created is a default field due to sorting.
                     from = NULL,  # Parameter not implemented.
                     to = NULL,  # Parameter not implemented.
                     debug = FALSE,  # Prints debug and system warnings.
                     csv = FALSE) {  # Stores the resulting data.frame in a standard CSV file together with a metadata file.

  # Loading dependencies.
  require(jsonlite)  # For reading the resulting JSON file.
  require(RCurl)  # For making HTTP requests.
  require(lubridate)  # For working with dates.

  
  #### Validation tests. ####
  # The validation tests before are useful for helping the user make a 
  # right query and understand why his query isn't working. 
  
  source('rw.searcheable.fields.R')  # For checking the query field against a list of the searcheable fields.
  rw.searcheable.fields(l = query.field, debug = debug)  # Running the validator above.
  if (is.null(query.field) == TRUE && is.null(text.query) == TRUE) { stop("You have to either provide a `text.query' input or a `query.field` + `query.field.value` input.") }
  if (is.null(query.field) == FALSE && is.null(query.field.value) == TRUE) { stop("Please provide a value with a query field.") }
  if (length(query.field) > 1) { stop('Please provide only one query field. Run rw.query.fields() if you are in doubt.') }
  if (is.null(limit) == FALSE && limit < 0) { stop('Please provide an integer between 1 and 1000.') }
  if (is.null(limit) == FALSE && limit > 1000) { stop('Please provide an integer between 1 and 1000.') }  # Increase the upper limit of the function.
  query.field <- tolower(query.field)  # In case user inputs in high case.


  
  #### Building the URL snippets and checking the validity of each parameter. ####
  # Here we are building the query.url, param-by-param.
  
  if (is.null(entity) == TRUE) { stop('Please provide an entity. At this point only the `report` entity is fully implemented.') }
  if (is.null(entity) == FALSE) { entity.url <- entity }
  if (is.null(limit) == TRUE) { limit.url <- paste("?limit=", 10, sep = "")
                                warning("The default limit for this querier is 10. If you need more please provide a number using the 'limit' parameter.") }
  if (is.null(limit) == FALSE) { limit.url <- paste("?limit=", limit, sep = "") }  
  if (is.null(text.query) == TRUE) { text.query.url <- NULL }
  if (is.null(text.query) == FALSE) {
      text.query.url <- paste("query[value]=", text.query, sep = "")
      warning('In this version searching the open text field will override whatever other field you have\nincluded the `query` paramenter. In further versions the open text field will allow you to\nfurther refine your search.')
  }
  if (is.null(query.field) == FALSE) { query.field.url <- paste("query[value]=", query.field, ":", query.field.value, sep = "") }
  
  # Function for building the right query when more than one field is provided.
  many.fields <- function(qf = NULL) { 
    qf[length(qf) + 1] <- 'date.created'  # date.created is a default field due to sorting.
    all.fields.url.list <- list()
    for (i in 0:(length(qf) - 1)) { 
      field.url <- paste("fields[include][",i,"]=", qf[i + 1], sep = "")
      all.fields.url.list[i + 1] <- paste("&", field.url, sep = "")
    }
  all.fields.url <- paste(all.fields.url.list, collapse = "")
  return(all.fields.url)
  }
  
  if (is.null(add.fields) == FALSE) { add.fields.url <- many.fields(qf = add.fields) }
#   if (is.null(from) == TRUE) {}  ## Implement in future versions.
#   if (is.null(to) == TRUE) {}  ## Implement in future versions.

  
  ## Building URL for aquiring data. ##
  api.url <- "http://api.rwlabs.org/v0/"
  query.url <- paste(api.url,  # it was base.url
                    entity.url,
                    limit.url,
                    query.field.url,
                    add.fields.url,
                    "&sort[0]=date.created:desc",
                    sep = "")


#   # 
#   url <- paste(base.url, query.url,
#                country.url,
#                country,
#                
#                sep = "")





  #### Fetching the data. ####
  if (debug == TRUE) {
    x <- paste("The URL being queried is: ", query.url, sep = "")
    warning(x)
  }

  # Getting the count number for iterations later.
  count <- data.frame(fromJSON(getURLContent(query.url)))
  c.url <- count$data.total[1]

  # Using `jsonlite` and `RCurl` to fetch JSON from the URL.
  query <- data.frame(fromJSON(getURLContent(query.url)))

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
    write.csv(meta.data, file = paste("data/", ifelse(country == "all", "all", country), "-", entity, "-metadata.csv", sep = ""), row.names = FALSE)
  }

  # UI element.
  print(paste("Fetching ~", ifelse(limit == "all", count, limit), " records.", sep = ""))

  # Creating iterations to go around the 1000-results limitation.
  rw.it <- function(df = NULL) {
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

#   # Can't remember what this function is for.
#   # Cleaning the dates.
#   rw.clean.dates <- function (df = "NA") {
#       x <- ymd(df$created) < ymd('2014-01-30')
#       df <- cbind(df, x)
#       x <- subset(df, df$x == 'TRUE')
#     return(x)
#   }

#   query <- rw.clean.dates(df = query)

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
    write.csv(query, file = paste(ifelse(country == "all", "all", country), "-", entity, ".csv", sep = ""))
  }

  print("Done.")
  return(query)
}