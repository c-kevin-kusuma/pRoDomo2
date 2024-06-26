% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pRoGroup.R
\name{pRoGroup}
\alias{pRoGroup}
\title{Group Management}
\usage{
pRoGroup(client_id, secret, data_table)
}
\arguments{
\item{client_id}{A client_id that can be created on the \url{developer.domo.com} page.}

\item{secret}{A secret that can be created on the \url{developer.domo.com} page.}

\item{data_table}{A dataframe/tibble that contains two required fields:
\enumerate{
\item group_id
\item user_id}}
}
\description{
Maintain the memberships of groups based on a certain table.
}
\examples{
pRoGroup(client_id = client_id, secret = secret, data_table = data_table)
}
