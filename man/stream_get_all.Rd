% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stream_get_all.R
\name{stream_get_all}
\alias{stream_get_all}
\title{List Streams}
\usage{
stream_get_all(client_id, secret, limit = 500, offset = 0)
}
\arguments{
\item{client_id}{A client_id that can be created on the \url{developer.domo.com} page.}

\item{secret}{A secret that can be created on the \url{developer.domo.com} page.}

\item{limit}{The amount of Stream to return in the list. The default is 50 and the maximum is 500.}

\item{offset}{The offset of the Stream ID to begin list of users within the response.}
}
\description{
Get a list of all Streams for which the user has view permissions.
\href{https://developer.domo.com/portal/242dd31d0746d-create-a-stream-execution}{DOMO Documentation}.
}
\examples{
stream_get_all(client_id = client_id,
  secret = secret,
  limit = 500,
  offset = 500)
}
