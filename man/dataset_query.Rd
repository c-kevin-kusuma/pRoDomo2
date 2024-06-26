% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset_query.R
\name{dataset_query}
\alias{dataset_query}
\title{Query a DataSet}
\usage{
dataset_query(client_id, secret, dataset_id, sql_query = "select * from table")
}
\arguments{
\item{client_id}{A client_id that can be created on the \url{developer.domo.com} page.}

\item{secret}{A secret that can be created on the \url{developer.domo.com} page.}

\item{dataset_id}{The unique ID of the dataset.}

\item{sql_query}{SQL query such as \code{select * from table}}
}
\description{
Queries the data in an existing Domo DataSet.
\href{https://developer.domo.com/portal/52fd0777839f4-query-a-data-set}{DOMO Documentation}.
}
\examples{
dataset_query(client_id = client_id,
  secret = secret,
  dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4',
  sql_query = 'select * from')
}
