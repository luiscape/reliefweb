\name{rw.query}
\alias{rw.query}
\title{ReliefWeb Querier}
\usage{
rw.query(entity = NULL, limit = NULL, text.query = NULL,
  query.field = NULL, query.field.value = NULL, add.fields = NULL,
  from = NULL, to = NULL, debug = FALSE, csv = FALSE)
}
\arguments{
  \item{entity}{The entity can be the Reliefweb entities:
  "report", "job", "training", "disaster" or "country". In
  this version only the "support" entity has been tested.}

  \item{text.query}{Open text field. Any text can be
  inputed in this field. If this field is provided nor
  "query.field" neither "query.field.value" should be
  provided.}

  \item{query.field}{One of the single fields that can be
  queried in an entity. Run \code{rw.query.fields()} for a
  list of fields that can be queried.}

  \item{query.field.value}{Specifies the value of the
  queried field. The correct values depend on each field.
  For instance, the field 'primary_countr.iso3' requires a
  3-letter ISO3 code such. Syria, in that case, would be
  'syr'.}

  \item{add.fields}{Determine which fields should be
  returned in the query. The field "date.created" is
  default and comes in all queries.}

  \item{limit}{The number of results that the query should
  return. It can be any number between 1 and 1000 or 'all'.
  If "all" is provided the result will return all the
  records that match that query in the ReliefWeb database.
  The default is 10.}

  \item{from}{Limits the query from a certain date. The
  date has to be in the YYYY-MM-DD format. [Not implemented
  yet. Will be implemented in future versions.]}

  \item{to}{Limits the query up to a certain date. The date
  has to be in the YYYY-MM-DD format. [Not implemented yet.
  Will be implemented in future versions.]}

  \item{debug}{Parameter creater for debugging purposes.}

  \item{csv}{If TRUE will store two CVS files: one with the
  resulting data.frame from the query and a metadata file.}
}
\description{
This function allows users to perform custom queries to the
ReliefWeb database. The results are a \code{data.frame}
ready for analysis. The function works effectivelly as a
binding for the ReliefWeb API (http://api.rwlabs.org).
Documentation for the API can be found at
http://apidocs.rwlabs.org. Detailed documentation about the
package can be found at http://luiscapelo.info/reliefweb
}
\examples{
# Here we are querying all the latest reposts about Syria using the field 'country'.
# syria <- rw.query(entity = 'report', query.field = 'country', query.field.value = 'Syria',
# add.fields = c('id', 'title', 'date.created', 'primary_country.iso3')
# Here we are using the open text parameter 'text.query' to query for 'Aleppo'.
# aleppo <- rw.query(entity = 'report', text.query = 'Aleppo', add.fields = c('id', 'date.created',
# 'title'))

# The result from both queries is a data.frame.
}

