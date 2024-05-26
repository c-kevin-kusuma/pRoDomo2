#' List users
#'
#' Get a list of all users in your Domo instance.
#' \href{https://developer.domo.com/portal/7f5648bdd673e-list-users}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @examples user_get_all(client_id = client_id, secret = secret)
#' @export


user_get_all <- function(client_id, secret) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  limit =500
  offset = 0

  i=0
  a <- list()
  nr=1
  while (nr>0) {

    user <- dplyr::bind_rows(
      httr::content(
      httr::GET(url = paste0('https://api.domo.com/v1/users?','limit=', limit, '&offset=', offset),
                config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))))))

    nr <- nrow(user)
    if(nr>0) {a[[i+1]] <- user}
    offset <- offset+limit
  }

  user <- dplyr::bind_rows(a)
  return(user)
}
