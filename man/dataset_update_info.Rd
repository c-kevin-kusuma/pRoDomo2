% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset_update_info.R
\name{dataset_update_info}
\alias{dataset_update_info}
\title{Update a DataSet}
\usage{
dataset_update_info(client_id, secret, dataset_id, body)
}
\arguments{
\item{client_id}{A client_id that can be created on the \url{developer.domo.com} page.}

\item{secret}{A secret that can be created on the \url{developer.domo.com} page.}

\item{dataset_id}{The unique ID of the dataset.}

\item{body}{A \code{list()} of name, description, pdpEnabled, and schema.}
}
\description{
Updates the specified DataSet’s metadata by providing values to parameters passed.
\href{https://developer.domo.com/portal/b96e1dbabff23-update-a-data-set}{DOMO Documentation}.
}
\examples{
dataset_update_info(client_id = client_id,
  secret = secret,
  dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4',
  body = list(name = 'The Name', pdpEnabled = TRUE))
}
