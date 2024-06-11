#' Update a group
#'
#' Updates the specified group by providing values to parameters passed. Any parameter left out of the request will cause the specific group’s attribute to remain unchanged.
#' \href{https://developer.domo.com/portal/270fc7c8d702b-update-a-group}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param group_id The unique ID of the group.
#' @param name The name of the group.
#' @param active To either set the group active or inactive. \strong{\emph{Optional}}
#' @param default Whether to set the group to its default or not. \strong{\emph{Optional}}
#' @examples group_update(client_id = client_id,
#'   secret = secret,
#'   group_id = '123456789',
#'   name = 'Group Name')
#' @export


group_update <- function(client_id, secret, group_id, name, active = TRUE, default = FALSE) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
      httr::PUT(url = paste0('https://api.domo.com/v1/groups/', group_id),
                body = list(name = name, active = active, default = default),
                config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
                httr::content_type("application/json"),
                encode = 'json'))


  return(data)
}

