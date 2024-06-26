% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/page_collection_get.R
\name{page_collection_get}
\alias{page_collection_get}
\title{Retrieve a page collection}
\usage{
page_collection_get(client_id, secret, page_id)
}
\arguments{
\item{client_id}{A client_id that can be created on the \url{developer.domo.com} page.}

\item{secret}{A secret that can be created on the \url{developer.domo.com} page.}

\item{page_id}{The ID of the page.}
}
\description{
Retrieve a page collection.
\href{https://developer.domo.com/portal/03c865271e614-retrieve-a-page-collection}{DOMO Documentation}.
}
\examples{
page_collection_get(client_id = client_id,
  secret = secret,
  page_id = 1805162873)
}
