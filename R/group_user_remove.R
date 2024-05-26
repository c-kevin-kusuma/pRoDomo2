#' Remove a user from a group
#'
#' Remove a user from a group in your Domo instance.
#' \href{https://developer.domo.com/portal/608c69894df01-remove-a-user-from-a-group}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param group_id The unique Id of the group to add the user to.
#' @param user_id The unique Id of the user to be added to the group.
#' @examples group_user_remove(client_id = client_id,
#'   secret = secret,
#'   group_id = '768473',
#'   user_id = '123456789')
#' @export


group_user_remove <- function(client_id, secret, group_id, user_id) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::DELETE(url = paste0('https://api.domo.com/v1/groups/', group_id, '/users/', user_id),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/octet-stream")))


  return(data)
}

