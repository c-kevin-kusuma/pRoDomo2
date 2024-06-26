% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/page_collection_delete.R
\name{page_collection_delete}
\alias{page_collection_delete}
\title{Delete a page collection}
\usage{
page_collection_delete(client_id, secret, page_id, page_collection_id)
}
\arguments{
\item{client_id}{A client_id that can be created on the \url{developer.domo.com} page.}

\item{secret}{A secret that can be created on the \url{developer.domo.com} page.}

\item{page_id}{The ID of the page.}

\item{page_collection_id}{The ID of the page collection.}
}
\description{
\strong{This is destructive and cannot be reversed.} Permanently deletes a page collection from your Domo instance.
\href{https://developer.domo.com/portal/42402e303b375-delete-a-page-collection}{DOMO Documentation}.
}
\examples{
page_collection_delete(client_id = client_id,
  secret = secret,
  page_id = 1805162873,
  page_collection_id = 522185994)
}
