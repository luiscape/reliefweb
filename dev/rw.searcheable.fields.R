#### Searcheable Fields. #### 

#' Searcheable Fields
#' 
#' This function provides the list of saercheable fields in the Reliefweb API.
#' The function only take the query terms as a parameter.

rw.searcheable.fields <- function(l = query) { 
  
  # Loading the list of searcheable fields. 
  searcheable.fields <- read.csv('data/searcheable-fields.csv')
  
  # Comparing the provided query against the list and testing for wrong queries.
  if (any(is.na(match(l, searcheable.fields[,1], nomatch = NA))) == TRUE) { 
    stop("One of your terms isn't searcheable. Please check the searcheable terms here (URL-XXXX).") 
  }
  
}