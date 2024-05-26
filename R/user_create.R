#' Create A User
#'
#' Creates a new user in your Domo instance.
#' \href{https://developer.domo.com/portal/6fc40f66c26e2-create-a-user}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param body A list() of parameters that will be used to create the user, email, name, and role are required inputs aside from the following optional inputs: sendInvite, alternateEmail, employeeNumber, locale, location, phone, roleId, timezone, and title
#' @examples user_create(client_id = client_id,
#'   secret = secret,
#'   body = list(name = 'John Doe', email = "email", role = "Admin", sendInvite = TRUE))
#' @export


user_create <- function(client_id, secret, body) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  user <- httr::content(
    httr::POST(url = 'https://api.domo.com/v1/users/',
               config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
               encode = 'json',
               body = body))

  return(user)
}
