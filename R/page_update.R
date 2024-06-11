#' Update a page
#'
#' Updates the specified page by providing values to parameters passed. Any parameter left out of the request will cause the specific page’s attribute to remain unchanged.
#' Also, collections cannot be added or removed via this endpoint, only reordered. Giving access to a user or group will also cause that user or group to have access to the parent page (if the page is a subpage). Moving a page by updating the parentId will also cause everyone with access to the page to have access to the new parent page.
#' \href{https://developer.domo.com/portal/93a60be5d5965-update-a-page}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param page_id The ID of the page.
#' @param body An R \code{list()} that contains the specified definitions of the new page such as:
#' \itemize{
#'   \item \strong{name:} The name of the page.
#'   \item \strong{parentId:} If provided, the page will be created as a subpage to the page provided.
#'   \item \strong{locked:} will restrict other users the ability to make edits to page or its content - the default value is false.
#'   \item \strong{cardIds:} IDs provided will add or remove cards that are not a part of a page collection.
#'   \item \strong{visibility:} Determines the access given to both individual users or groups within Domo by providing these 2 parameters:
#'   \itemize{
#'     \item \strong{userIds:} The IDs of users that will be given access to view the page.
#'     \item \strong{groupIds:} The IDs of groups that will be given access to view the page.}}
#' @examples page_update(client_id = client_id,
#'   secret = secret,
#'   user_id = '123456789',
#'   body = list(name = 'New Name', locked = 'true'))
#' @export


page_update <- function(client_id, secret, page_id, body) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::PUT(url = paste0('https://api.domo.com/v1/pages/', page_id),
              body = body,
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/json"),
              encode = 'json'))

  return(data)
}

