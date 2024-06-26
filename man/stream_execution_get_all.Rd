% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stream_execution_get_all.R
\name{stream_execution_get_all}
\alias{stream_execution_get_all}
\title{List Stream executions}
\usage{
stream_execution_get_all(client_id, secret, stream_id, limit = 500, offset = 0)
}
\arguments{
\item{client_id}{A client_id that can be created on the \url{developer.domo.com} page.}

\item{secret}{A secret that can be created on the \url{developer.domo.com} page.}

\item{stream_id}{The unique ID of the stream.}

\item{limit}{The amount of Stream to return in the list. The default is 50 and the maximum is 500.}

\item{offset}{The offset of the Stream ID to begin list of users within the response.}
}
\description{
Returns all Stream Execution objects that meet argument criteria from original request.
\href{https://developer.domo.com/portal/4ebc56e4e9771-list-stream-executions}{DOMO Documentation}.
}
\examples{
stream_execution_get_all(client_id = client_id,
  secret = secret,
  stream_id = 1,
  limit = 500,
  offset = 0)
}
