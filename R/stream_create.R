#' Create a Stream
#'
#' When creating a Stream, specify the DataSet properties (name and description) and as a convenience, the create Stream API will create a DataSet for you.
#' In addition, you can only have one Stream open at a time. If you need to add additional data, we recommended adding more parts to the currently open Stream or executing a commit of the open stream before creating a new stream.
#' (Known limitation) The StreamAPI currently only allows you to import data to a DataSet created via the Stream API. For example, it is currently not supported to import data to a DataSet created by a Domo Connector.
#' \href{https://developer.domo.com/portal/7f71f4aa90726-create-a-stream}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param body The content of the dataset. Please refer to the documentation for details.
#' @examples
#' columns <- list(); columns[[1]] <- list(name = 'Column 1', type = 'STRING')
#' schema <- list(columns = columns)
#' body <- list(dataSet = list(name = 'The Name', description = 'Some description', schema = schema), updateMethod = "REPLACE")
#' stream_create(client_id = client_id,
#'   secret = secret,
#'   body = body)
#' @export


stream_create <- function(client_id, secret, body) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::POST(url = 'https://api.domo.com/v1/streams',
               body = body,
               config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
               httr::content_type("application/json"),
               httr::accept("application/json"),
               encode = 'json'))

  return(data)
}
