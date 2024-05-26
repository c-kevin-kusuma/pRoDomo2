#' Delete a page
#'
#' \strong{This is destructive and cannot be reversed.} Permanently deletes a page from your Domo instance.
#' \href{https://developer.domo.com/portal/d7c166334953f-delete-a-page}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param page_id The ID of the page.
#' @examples page_delete(client_id = client_id,
#'   secret = secret,
#'   page_id = 123455667)
#' @export


page_delete <- function(client_id, secret, page_id) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::DELETE(url = paste0('https://api.domo.com/v1/pages/', page_id),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/octet-stream")))

  return(data)
}

