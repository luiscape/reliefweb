#' ReliefWeb Querier
#' 
#' This function allows users to perform custom queries to the ReliefWeb database. 
#' The results are a \code{data.frame} ready for analysis. The function works effectivelly 
#' as a binding for the ReliefWeb API (http://api.rwlabs.org). Documentation for the API
#' can be found at http://apidocs.rwlabs.org. Detailed documentation about the package
#' can be found at http://luiscapelo.info/reliefweb
#' 
#' 
#' @param entity The entity can be the Reliefweb entities: "report", "job", "training", "disaster" or "country". In this version only the "support" entity has been tested.
#' @param text.query Open text field. Any text can be inputed in this field. If this field is provided nor "query.field" neither "query.field.value" should be provided.
#' @param query.field One of the single fields that can be queried in an entity. Run \code{rw.query.fields()} for a list of fields that can be queried.
#' @param query.field.value Specifies the value of the queried field. The correct values depend 
#' on each field. For instance, the field 'primary_countr.iso3' requires a 3-letter ISO3 code such. 
#' Syria, in that case, would be 'syr'. 
#' @param add.fields Determine which fields should be returned in the query. The field "date.created" is default and comes in all queries.
#' @param limit The number of results that the query should return. It can be any number between 
#' 1 and 1000 or 'all'. If "all" is provided the result will return all the records that match 
#' that query in the ReliefWeb database. The default is 10.
#' @param from Limits the query from a certain date. The date has to be in the YYYY-MM-DD format. [Not implemented yet. Will be implemented in future versions.]
#' @param to Limits the query up to a certain date. The date has to be in the YYYY-MM-DD format. [Not implemented yet. Will be implemented in future versions.]
#' @param debug Parameter creater for debugging purposes.
#' @param csv If TRUE will store two CVS files: one with the resulting data.frame from the query and a metadata file.
#' 
#' @export
#' 
#' @examples 
#' # Here we are querying all the latest reposts about Syria using the field 'country'.
#' # syria <- rw.query(entity = 'report', query.field = 'country', query.field.value = 'Syria', 
#' # add.fields = c('id', 'title', 'date.created', 'primary_country.iso3')
#' # Here we are using the open text parameter 'text.query' to query for 'Aleppo'.
#' # aleppo <- rw.query(entity = 'report', text.query = 'Aleppo', add.fields = c('id', 'date.created',
#' # 'title'))
#'  
#' # The result from both queries is a data.frame.

rw.query <- function(entity = NULL,
                     limit = NULL,
                     text.query = NULL,
                     query.field = NULL,
                     query.field.value = NULL,
                     add.fields = NULL,
                     from = NULL,
                     to = NULL,
                     debug = FALSE,
                     csv = FALSE) {
  
  #### Validation tests. ####
  # The validation tests before are useful for helping the user make a 
  # right query and understand why his query isn't working. 
  
  # Install depencency packages if not installed 
  if ("lubridate" %in% rownames(installed.packages()) == FALSE) install.packages("lubridate")
  if ("RCurl" %in% rownames(installed.packages()) == FALSE) install.packages("RCurl")
  if ("jsonlite" %in% rownames(installed.packages()) == FALSE) install.packages("jsonlite")
  
  if (is.null(query.field) == TRUE && is.null(text.query) == TRUE) { stop("You have to either provide a `text.query' input or a `query.field` + `query.field.value` input.") }
  if (is.null(query.field) == FALSE && is.null(query.field.value) == TRUE) { stop("Please provide a value with a query field.") }
  if (length(query.field) > 1) { stop('Please provide only one query field. Run rw.query.fields() if you are in doubt.') }
  if (is.null(limit) == FALSE && limit < 0 && tolower(limit) != "all") { stop('Please provide an integer between 1 and 1000 or all.') }
  if (is.null(limit) == FALSE && limit > 1000 && tolower(limit) != "all") { stop('Please provide an integer between 1 and 1000 or all.') }  # Increase the upper limit of the function.
  if (is.null(limit) == FALSE) { limit <- tolower(limit) }
  all <- "all"

  
  #### Building the URL snippets and checking the validity of each parameter. ####
  # Here we are building the query.url, param-by-param.
  if (is.null(entity) == TRUE) { stop('Please provide an entity. \nAt this point only the `report` \nentity is fully implemented.') }
  
  if (is.null(entity) == FALSE) { entity.url <- paste(entity, "/list", sep = "") }
  if (is.null(limit) == TRUE) { limit.url <- paste("?limit=", 10, "&", sep = "")
                                warning("The default limit for this querier is 10. \nIf you need more please provide a number           using \nthe 'limit' parameter.") }
  
  if (is.null(limit) == FALSE) { limit.url <- paste("?limit=", 
                                                    ifelse(limit == "all", 1000, limit),
                                                    "&", sep = "") }
  if (is.null(text.query) == TRUE) { text.query.url <- NULL }
  if (is.null(text.query) == FALSE) {
      text.query.url <- paste("query[value]=", 
                              text.query, 
                              sep = "")
      warning('In this version searching the open text field \nwill override whatever other field you have\nincluded the `query` paramenter. In further \nversions the open text field will allow you to\nfurther refine your search.')
  }
  if (is.null(query.field) == FALSE) { query.field.url <- paste("query[value]=", 
                                                                query.field, 
                                                                ":", 
                                                                query.field.value, 
                                                                sep = "") }
  if (is.null(query.field) == TRUE) { query.field.url <- NULL }
  
  # Function for building the right query when more than one field is provided.
  many.fields <- function(qf = NULL) { 
    
    ifelse(all(is.na(match(qf, 'date.created')) == TRUE), qf[length(qf) + 1] <- 'date.created', '')
    
    # date.created is a default field due to sorting.
    
    all.fields.url.list <- list()
    for (i in 0:(length(qf) - 1)) { 
      field.url <- paste("fields[include][",i,"]=", qf[i + 1], sep = "")
      all.fields.url.list[i + 1] <- paste("&", field.url, sep = "")
    }
  all.fields.url <- paste(all.fields.url.list, collapse = "")
  return(all.fields.url)
  }
  
  if (is.null(add.fields) == FALSE) { add.fields.url <- many.fields(qf = add.fields) }
  
  ## From and to paramenters. ##
  # if (is.null(from) == TRUE) {}  ## Implement in future versions.
  # if (is.null(to) == TRUE) {}  ## Implement in future versions.
  
  ## Building URL for aquiring data. ##
  api.url <- "http://api.rwlabs.org/v0/"
  query.url <- paste(api.url,
                    entity.url,
                    limit.url,
                    text.query.url,
                    query.field.url,
                    add.fields.url,
                    "&sort[0]=date.created:desc",
                    sep = "")

  #### Fetching the data. ####
  if (debug == TRUE) {
    x <- paste("The URL being queried is: ", query.url, sep = "")
    warning(x)
  }

  # Getting the count number for iterations later.
  count <- data.frame(fromJSON(getURLContent(query.url)))
  count <- count$data.total[1]

  #### Enhancement #### 
  # Here I am querying the url and getting a base data.frame. 
  # It would probably be better to implement this process within the iteration bellow.
  # Using `jsonlite` and `RCurl` to fetch JSON from the URL.
  query <- data.frame(fromJSON(getURLContent(query.url)))

  # Function to convert the nested lists into rows in the data.frame.
  rw.fields <- function(df = "NA") {
  
    for (i in 1:length(add.fields)) {
      
      ## Multi Fields ##
      # Multi fields not implemented. Will be implemented in future version.
      # if (length(df$data.list.fields[i]) > 1) { x <- paste(df$data.list.fields[i], collapse = ",") }  
      
      if (length(df$data.list.fields[i]) <= 1) { x <- data.frame(as.list(df$data.list.fields[i])) }
      df <- cbind(df, x)
    }
    df$data.list.fields <- NULL
    return(df)
  }

  query <- rw.fields(df = query)

  # Creating a metadata data.frame.
  if (csv == TRUE) {
    meta.data <- query[1, 1:7]
    write.csv(meta.data, file = paste("data/", 
                                      paste(add.fields, 
                                            collapse = "-", 
                                            "-", 
                                            entity, 
                                            "-metadata.csv", sep = ""), row.names = FALSE))}

  

  # UI element.
  print(paste("Fetching ~", ifelse(is.null(limit) == TRUE, 10, ifelse(identical(limit,all) == TRUE, count, limit)), " records.", sep = ""))
  
  # Creating iterations to go around the 1000-results limitation.
  rw.it <- function(df = NULL) {
    
      to <- df$created[nrow(df)]
      final <- df

      # Create progress bar.
      limit <- ifelse(limit == "all", 1000, limit)
      total <- ceiling(count/limit)
      pb <- txtProgressBar(min = 0, max = total, style = 3)

      for (i in 2:total) {
        # Update progress bar.
        setTxtProgressBar(pb, i)

          to <- format(final$created[nrow(final)], scientific = FALSE)

          it.url <- paste(query.url, "&filter[field]=date.created&filter[value][to]=", to, sep = "")

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


  # Only run iterator if we are fetching "all" entries.
  # Note: improve to be more > 1000, but not 'all'.
  if (identical(limit,all) == TRUE) { query <- rw.it(df = query) }

  #### Cleaning the resulting data. ####
  # Transform dates from Epoch to year-month-day.
  rw.time <- function(df = "NA") {
    df$created <- df$created/1000 # To eliminate the miliseconds.
    df$created <- as.Date(as.POSIXct(as.numeric(df$created), origin = "1970-01-01"))
    return(df)
  }

  query <- rw.time(df = query)
  
  if (debug == TRUE) {
    before.duplicates <- nrow(query)
  }

  # Cleaning duplicate entries.
  rw.clean.duplicates <- function(df = "NA") {
    query$duplicated <- duplicated(query)
    x <- subset(query, query$duplicated == FALSE)
    x$duplicated <- NULL
    return(x)
  }

  query <- rw.clean.duplicates(query)
  
  
  if (debug == TRUE) { 
    # UI element     
    after.duplicates <- nrow(query)
    duplicates <- before.duplicates - after.duplicates
    print(paste("There were ", duplicates, " duplicats in the query."))
  }

  # Keeping only the columns of interest.
  query <- query[,10:ncol(query)]

#   # Storing the resulting data in a CSV file. 
  if (csv == TRUE) {
    write.csv(query, file = "ReliefWeb-query", "-", entity, ".csv", sep = "")
  }

  print("Done.")
  return(query)
}