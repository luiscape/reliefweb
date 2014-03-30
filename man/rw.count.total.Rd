\name{rw.count.total}
\alias{rw.count.total}
\title{ReliefWeb count total}
\usage{
rw.count.total(entity = NULL)
}
\arguments{
  \item{entity}{= Any of the entities available: "report",
  "job", "training", "disaster" or "country". (It seems
  that only the 'report' entity is working properly.)}
}
\description{
Function to count the total of results an entity has on
ReliefWeb.
}
\details{
Note: At this point the function seems to be returning
unreliable values. This issue seems to be caused by the
\code{count} method in the API rather than a problem with
the package. Future versions will address this issue. For
now use with caution.
}
\examples{
# Asking for the total number of reports currently available.
# rw.count.total(entity = 'report')

# The result with be a data.frame with the current number of reports in the database.
}

