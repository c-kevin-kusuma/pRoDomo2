% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pRoPdpGet.R
\name{pRoPdpGet}
\alias{pRoPdpGet}
\title{List Personalized Data Permission (PDP) policies}
\usage{
pRoPdpGet(client_id, secret, dataset_id, data_frame = FALSE)
}
\arguments{
\item{client_id}{A client_id that can be created on the \url{developer.domo.com} page.}

\item{secret}{A secret that can be created on the \url{developer.domo.com} page.}

\item{dataset_id}{The unique ID of the dataset.}

\item{data_frame}{The default is "FALSE".}
}
\description{
List the Personalized Data Permission (PDP) policies for a specified DataSet.
\href{https://developer.domo.com/portal/4cab29aca0fd3-list-personalized-data-permission-pdp-policies}{DOMO Documentation}.
}
\examples{
pRoPdpGet(client_id = client_id,
  secret = secret,
  dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4')
}
