#' List Stream executions
#'
#' Returns all Stream Execution objects that meet argument criteria from original request.
#' \href{https://developer.domo.com/portal/4ebc56e4e9771-list-stream-executions}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param stream_id The unique ID of the stream.
#' @param limit The amount of Stream to return in the list. The default is 50 and the maximum is 500.
#' @param offset The offset of the Stream ID to begin list of users within the response.
#' @examples stream_execution_get_all(client_id = client_id,
#'   secret = secret,
#'   stream_id = 1,
#'   limit = 500,
#'   offset = 0)
#' @export


stream_execution_get_all <- function(client_id, secret, stream_id, limit = 500, offset = 0) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::GET(url = paste0("https://api.domo.com/v1/streams/", stream_id,'/executions'),
              body = list(limit = limit, offset = offset),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              content_type("application/octet-stream"),
              accept("application/json")))

  return(data)
}
