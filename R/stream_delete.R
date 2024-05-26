#' Delete a Stream
#'
#' \strong{This is destructive and cannot be reversed.} Deletes a Stream from your Domo instance. This does not a delete the associated DataSet.
#' \href{https://developer.domo.com/portal/6875057565875-delete-a-stream}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param stream_id The unique ID of the stream.
#' @examples stream_delete(client_id = client_id,
#'   secret = secret,
#'   stream_id = 1)
#' @export


stream_delete <- function(client_id, secret, stream_id) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::DELETE(url = paste0("https://api.domo.com/v1/streams/", stream_id),
                config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
                httr::content_type("application/octet-stream")))

  return(data)
}
