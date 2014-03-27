#### A count of the results number. ####

#' ReliefWeb Count total
#' 
#' Function to count the total of results an entity has on ReliefWeb.
#' Note: Apparently this function isn't working well. The count for whatever else than 'reports' seems to be extremely wrong.
#' 
#' @param entity = Any of the entities available: report, job, training, disaster or country. (It seems that only the 'report' entity is working properly.)

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