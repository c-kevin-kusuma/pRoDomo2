#' Create a page
#'
#' Creates a new page in your Domo instance.
#' \href{https://developer.domo.com/portal/0fce9616dfe5c-create-a-page}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param body An R \code{list()} that contains the specified definitions of the new page such as:
#' \itemize{
#'   \item \strong{name:} The name of the page.
#'   \item \strong{parentId:} If provided, the page will be created as a subpage to the page provided.
#'   \item \strong{locked:} will restrict other users the ability to make edits to page or its content - the default value is false.
#'   \item \strong{cardIds:} IDs provided will add or remove cards that are not a part of a page collection.
#'   \item \strong{visibility:} Determines the access given to both individual users or groups within Domo by providing these 2 parameters:
#'   \itemize{
#'     \item \strong{userIds:} The IDs of users that will be given access to view the page.
#'     \item \strong{roupIds:} The IDs of groups that will be given access to view the page.}}
#' @examples
#' page_create(client_id = client_id,
#'   secret = secret,
#'   body = list(name = 'Page #1'))
#' @export


page_create <- function(client_id, secret, body) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
      httr::POST(url = 'https://api.domo.com/v1/pages',
                body = body,
                config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
                httr::content_type("application/json"),
                httr::accept("application/json"),
                encode = 'json'))

  cat('Page ID: ', data$id)

  return(data)
}

