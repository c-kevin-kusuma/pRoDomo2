% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_update.R
\name{group_update}
\alias{group_update}
\title{Update a group}
\usage{
group_update(client_id, secret, group_id, name, active = TRUE, default = FALSE)
}
\arguments{
\item{client_id}{A client_id that can be created on the \url{developer.domo.com} page.}

\item{secret}{A secret that can be created on the \url{developer.domo.com} page.}

\item{group_id}{The unique ID of the group.}

\item{name}{The name of the group.}

\item{active}{To either set the group active or inactive. \strong{\emph{Optional}}}

\item{default}{Whether to set the group to its default or not. \strong{\emph{Optional}}}
}
\description{
Updates the specified group by providing values to parameters passed. Any parameter left out of the request will cause the specific group’s attribute to remain unchanged.
\href{https://developer.domo.com/portal/270fc7c8d702b-update-a-group}{DOMO Documentation}.
}
\examples{
group_update(client_id = client_id,
  secret = secret,
  group_id = '123456789',
  name = 'Group Name')
}
