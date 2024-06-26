% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stream_get.R
\name{stream_get}
\alias{stream_get}
\title{Retrieve a Stream}
\usage{
stream_get(client_id, secret, stream_id)
}
\arguments{
\item{client_id}{A client_id that can be created on the \url{developer.domo.com} page.}

\item{secret}{A secret that can be created on the \url{developer.domo.com} page.}

\item{stream_id}{The unique ID of the stream.}
}
\description{
Retrieves the details of an existing stream.
\href{https://developer.domo.com/portal/3104e81ceff46-retrieve-a-stream}{DOMO Documentation}.
}
\examples{
stream_get(client_id = client_id,
  secret = secret,
  stream_id = 1)
}
