% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plt_attachment_add.R
\name{plt_attachment_add}
\alias{plt_attachment_add}
\title{Add attachment}
\usage{
plt_attachment_add(client_id, secret, project_id, list_id, task_id, body)
}
\arguments{
\item{client_id}{A client_id that can be created on the \url{developer.domo.com} page.}

\item{secret}{A secret that can be created on the \url{developer.domo.com} page.}

\item{project_id}{The unique ID of the project.}

\item{list_id}{The unique ID of the list.}

\item{task_id}{The unique ID of the task.}

\item{body}{The content of the attachment. Please refer to the documentation for details.}
}
\description{
Add a multipart form file to a task item as an attachment.
\href{https://developer.domo.com/portal/ea34b968c5164-add-attachment}{DOMO Documentation}.
}
\examples{
plt_attachment_add(client_id = client_id,
  secret = secret,
  project_id = 123,
  list_id = 5,
  task_id = 12,
  body = body)
}
