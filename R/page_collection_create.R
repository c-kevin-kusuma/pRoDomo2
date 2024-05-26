#' Create a page collection
#'
#' Create a page collection.
#' \href{https://developer.domo.com/portal/f95f2f9dc8c0f-create-a-page-collection}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param page_id The ID of the page.
#' @param body An R \code{list()} that contains the specified definitions of the new collection such as:
#' \itemize{
#' \item title: Page collection's name displayed above the set of cards.
#' \item description: Additional text within the page collection. \strong{\emph{Optional}}.
#' \item cardIds: IDs provided will add or remove cards that are not a part of a page collection.}
#' @examples
#' cardIds <- list(); cardIds[[1]] <- 96460956
#' body <- list(title = 'Collection1', description = 'Just a description', cardIds = cardIds)
#' page_collection_create(client_id = client_id,
#'   secret = secret,
#'   page_id = 1805162873,
#'   body = body)
#' @export


page_collection_create <- function(client_id, secret, page_id, body) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::POST(url = paste0('https://api.domo.com/v1/pages/', page_id, '/collections'),
              body = body,
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/json"),
              encode = "json"))

  cat(data$id)
  return(data)
}

