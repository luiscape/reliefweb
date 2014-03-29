#' Reliefweb | Query Fields 
#' 
#' This function allows for the understanding of the query fields. It gives a list of the fields, together with 
#' their description and basic metadata about them. 
#' 
#' @param field = provide a partial string of the field of interest. The results will contain all relevant fields.

rw.query.fields <- function(field = NULL) {
  
  #### Development Notes #### 
  # The user has to be able to provide a particular field and get a 
  # description of what that field is. It can work basically as a codebook.
  # Ideally fuzzy search could work here as well. 
  
  if (is.null(field) == TRUE) {
    x <- read.csv('data/searcheable-fields.csv')
    print("This are the searcheable fields available. Use the 'query' parameter to query them.")
    print(x[,1])
  }  
}