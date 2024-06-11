#' Update a page collection
#'
#' Update a page collection.
#' \href{https://developer.domo.com/portal/4dc767f205764-update-a-page-collection}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param page_id The ID of the page.
#' @param page_collection_id The ID of the page collection.
#' @param body The parameters of the page collection.
#' @examples
#' cardIds <- list(); cardIds[[1]] <- 96460956
#' body <- list(title = 'Collection1', description = 'Just a description', cardIds = cardIds)
#' page_collection_update(client_id = client_id,
#'   secret = secret,
#'   page_id = 1805162873,
#'   page_collection_id = 522185994,
#'   body = body)
#' @export


page_collection_update <- function(client_id, secret, page_id, page_collection_id, body) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::PUT(url = paste0('https://api.domo.com/v1/pages/', page_id, '/collections/', page_collection_id),
               body = body,
               config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
               httr::content_type("application/json"),
               encode = 'json'))

  return(data)
}

