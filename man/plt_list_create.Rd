% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plt_list_create.R
\name{plt_list_create}
\alias{plt_list_create}
\title{Create a list}
\usage{
plt_list_create(client_id, secret, project_id, body)
}
\arguments{
\item{client_id}{A client_id that can be created on the \url{developer.domo.com} page.}

\item{secret}{A secret that can be created on the \url{developer.domo.com} page.}

\item{project_id}{The unique ID of the project.}

\item{body}{The parameters of the list. Please refer to the documentation for more details.}
}
\description{
Creates a new list within the given project id.
\href{https://developer.domo.com/portal/c857e329cd0a1-create-a-list}{DOMO Documentation}.
}
\examples{
plt_list_create(client_id = client_id,
  secret = secret,
  project_id = 123,
  body = list(name = 'Custom List', type = 'TODO', index = 1))
}
