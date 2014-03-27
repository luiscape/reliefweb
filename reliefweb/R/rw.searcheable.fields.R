#### Searcheable Fields. #### 

#' Searcheable Fields
#' 
#' This function provides the list of saercheable fields in the Reliefweb API.
#' The function only take the query terms as a parameter.

rw.searcheable.fields <- function(l = NULL, debug = FALSE) { 
  
  # Loading the list of searcheable fields. 
  searcheable.fields <- read.csv('data/searcheable-fields.csv')
  
  # Comparing the provided query against the list and testing for wrong queries.
  if (any(is.na(match(l, searcheable.fields[,1], nomatch = NA))) == TRUE) { 
    warning("One of your terms isn't searcheable. Run rw.query.fields() if you want to know what fields are querieable.")
  }
  if (debug == TRUE) { warning('All fields seem to be searcheable.')}
}