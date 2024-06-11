#' Retrieve a group
#'
#' Retrieves the details of an existing group.
#' \href{https://developer.domo.com/portal/1f17fb4f14577-retrieve-a-group}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param group_id The unique ID of the group.
#' @examples group_get(client_id = client_id,
#'   secret = secret,
#'   group_id = '123456789')
#' @export


group_get <- function(client_id, secret, group_id) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::GET(url = paste0('https://api.domo.com/v1/groups/', group_id),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' ')))))

  return(data)
}
