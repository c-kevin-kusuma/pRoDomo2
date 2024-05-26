#' List pages
#'
#' Get a list of all pages in your Domo instance.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param limit The amount of pages to return in the list. The default is 50 and the maximum is 500.
#' @param offset The offset of the page ID to begin list of pages within the response.
#' @examples page_get_all(client_id = client_id, secret = secret, limit = 500, offset = 0)
#' @export


page_get_all <- function(client_id, secret, limit, offset) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
      httr::GET(url = 'https://api.domo.com/v1/pages',
                config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
                query = list(limit = limit, offset = offset),
                httr::content_type("application/octet-stream"),
                httr::accept("application/json")))

  return(data)
}

