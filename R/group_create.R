#' Create a group
#'
#' Creates a new group in your Domo instance.
#' \href{https://developer.domo.com/portal/d88967f3c2bd5-create-a-group}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param name The name of the group.
#' @examples group_create(client_id = client_id,
#'   secret = secret,
#'   name = 'Group Name')
#' @export


group_create <- function(client_id, secret, name) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::POST(url = 'https://api.domo.com/v1/groups/',
              body = list(name = name),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/json"),
              httr::accept("application/json"),
              encode = 'json'))


  return(data)
}

