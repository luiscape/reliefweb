#' The ReliefWeb Package
#'
#' The ReliefWeb Package allows you to query Reliefweb's API, 
#' download its output and prepare it for analysis. 
#' 
#' It allows you to create custom queries
#' to the Reliefweb API and download the resulting data into a data.frame,
#' making its subsequent analysis easier. It cleans the data (especially the
#' dates fields) and performs light transformations (i.e. all entries are transformed into 
#' per day and not per milisecond as it is in the ReliefWeb database).
#' 
#' TMaking a custom query: the main element of this package is the \code{\link{rw.query}} function.
#' With \code{rw.query} you can determine what your query will be, the number
#' of results you want back, and what fields from the database should be returned.
#' The result will be a data.frame ready for analysis. 
#'
#' @import lubridate RCurl jsonlite
#' @name reliefweb
#' @docType package