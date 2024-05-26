#' Abort a Stream execution
#'
#' If needed during an execution, aborts an entire Stream execution. To abort the current stream execution within a Stream, simply identify the Stream’s ID within request
#' \href{https://developer.domo.com/portal/ab38f0e6a67ac-abort-a-stream-execution}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param stream_id The unique ID of the stream.
#' @param execution_id The unique ID of the execution.
#' @examples stream_execution_abort(client_id = client_id,
#'   secret = secret,
#'   stream_id = 1,
#'   execution_id = 1)
#' @export


stream_execution_abort <- function(client_id, secret, stream_id, execution_id) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::PUT(url = paste0("https://api.domo.com/v1/streams/", stream_id,'/executions/',execution_id,'/abort'),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/json")))

  return(data)
}
