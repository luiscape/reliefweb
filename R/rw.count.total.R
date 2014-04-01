#### A count of the results number. ####

#' ReliefWeb count total
#' 
#' Function to count the total of results an entity has on ReliefWeb.
#' 
#' Note: At this point the function seems to be returning unreliable values.
#' This issue seems to be caused by the \code{count} method in the API rather 
#' than a problem with the package. Future versions will address this issue.
#' For now use with caution.
#' 
#' @param entity = Any of the entities available: "report", "job", "training", "disaster" or "country". 
#' (It seems that only the 'report' entity is working properly.)
#' 
#' @export
#' 
#' @examples
#' # Asking for the total number of reports currently available.
#' # rw.count.total(entity = 'report')
#' 
#' # The result with be a data.frame with the current number of reports in the database.

rw.count.total <- function(entity = NULL) {
  if (is.null(entity) == TRUE) { stop("Please provide an entity: 'report', 'job', 'training', 'disaster' or 'country'.") }
  
  count.url <- paste("http://api.rwlabs.org/v0/",
                   entity,
                   "/count",
                   sep = "")
  count <- data.frame(fromJSON(getURLContent(count.url)))
  for (i in 1:3) { count[1] <- NULL }
  return(count)
} 