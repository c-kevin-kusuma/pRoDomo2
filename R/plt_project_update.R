#' Retrieve individual project
#'
#' Updates attributes of an existing project in your Domo instance. The following properties are read-only and cannot be updated with this request: id, members, createdBy, & createdDate.
#' \href{https://developer.domo.com/portal/bc780d297a0d6-update-a-project}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can created on the \url{developer.domo.com} page.
#' @param project_id The unique ID of the project.
#' @param body The parameters needed to create the project.
#' @examples
#' body <- list(name = 'Project Name', public = TRUE)
#' plt_project_update(client_id = client_id,
#'   secret = secret,
#'   project_id = 123,
#'   body = body)
#' @export


plt_project_update <- function(client_id, secret, project_id, body) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::PUT(url = paste0('https://api.domo.com/v1/projects/',project_id),
              body = body,
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/json"),
              httr::accept("application/json"),
              encode = 'json'))

  return(data)
}

