#' Delete A User
#'
#' \strong{This is destructive and cannot be reversed.} This function allows you to delete a certain user based on ID. Domo won't allow deletion of users if they own Datasets or Dataflows. You'll need to transfer ownership of these assets before deletion. All other assets previously owned by the deleted user will be orphaned.
#' \href{https://developer.domo.com/portal/b86a1b801b1f3-delete-a-user}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param user_id The unique ID of the user.
#' @examples user_delete(client_id = client_id,
#'   secret = secret,
#'   user_id = '123456789')
#' @export


user_delete <- function(client_id, secret, user_id) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::DELETE(url = paste0('https://api.domo.com/v1/users/',user_id),
                 config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
                 httr::content_type("application/octet-stream")))

  return(data)
}
