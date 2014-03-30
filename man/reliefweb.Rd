\docType{package}
\name{ReliefWeb}
\alias{reliefweb}
\alias{reliefweb-package}
\title{The ReliefWeb Querier: querying, parsing, and acquiring  data from ReliefWeb.int}
\description{
The ReliefWeb Package allows you to query Reliefweb's API, 
download its output and prepare it for analysis. It allows you to create custom queries
to the Reliefweb API and download the resulting data into a data.frame,
making its subsequent analysis easier. It cleans the data (especially the
dates fields) and performs light transformations (i.e. all entries are transformed into 
per day and not per milisecond as it is in the ReliefWeb database).
}