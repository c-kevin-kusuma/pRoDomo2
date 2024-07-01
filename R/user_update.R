#' Update A User
#'
#' Updates the specified user by providing values to parameters passed. Any parameter left out of the request will cause the specific user’s attribute to remain unchanged. (Known limitations) Currently all user fields are required.
#' \href{https://developer.domo.com/portal/6d07be3b66fad-update-a-user}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param user_id The unique ID of the user.
#' @param body A list() of parameters that will be used to update the user, email is a required input aside from the following optional inputs: alternateEmail, employeeNumber, locale, location, name, phone, role, roleId, timezone, and title
#' @examples user_update(client_id = client_id,
#'   secret = secret,
#'   user_id = '123456789',
#'   body = list(email = "email", role = "Admin"))
#' @export


user_update <- function(client_id, secret, user_id, body) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  user <- httr::content(
    httr::PUT(url = paste0('https://api.domo.com/v1/users/',user_id),
              body = body,
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/json"),
              encode = 'json'))

  return(user)
}
