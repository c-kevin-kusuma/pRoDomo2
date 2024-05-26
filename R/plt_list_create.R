#' Create a list
#'
#' Creates a new list within the given project id.
#' \href{https://developer.domo.com/portal/c857e329cd0a1-create-a-list}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param project_id The unique ID of the project.
#' @param body The parameters of the list. Please refer to the documentation for more details.
#' @examples plt_list_create(client_id = client_id,
#'   secret = secret,
#'   project_id = 123,
#'   body = list(name = 'Custom List', type = 'TODO', index = 1))
#' @export


plt_list_create <- function(client_id, secret, project_id, body) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::POST(url = paste0('https://api.domo.com/v1/projects/',project_id,'/lists'),
               body = body,
               config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
               httr::content_type("application/json"),
               httr::accept("application/json"),
               encode = 'json'))

  cat(data$id)
  return(data)
}

