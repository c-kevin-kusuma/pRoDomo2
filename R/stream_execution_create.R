#' Create a Stream execution
#'
#' When you’re ready to upload data to your DataSet via a Stream, you first tell Domo that you’re ready to start sending data by creating an Execution.
#' Creating an Execution on a Stream will abort all other Executions on that Stream. Each Stream can only have one active Execution at a time.
#' \href{https://developer.domo.com/portal/242dd31d0746d-create-a-stream-execution}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can created on the \url{developer.domo.com} page.
#' @param stream_id The unique ID of the stream.
#' @examples stream_execution_create(client_id = client_id,
#'   secret = secret,
#'   stream_id = 1)
#' @export


stream_execution_create <- function(client_id, secret, stream_id) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::POST(url = paste0("https://api.domo.com/v1/streams/", stream_id,'/executions'),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/json"),
              httr::accept("application/json")))

  return(data)
}
