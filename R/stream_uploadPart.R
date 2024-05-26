#' Upload a data part
#'
#' Creates a data part within the Stream execution to upload chunks of rows to the DataSet. The calling client should keep track of parts and order them accordingly in an increasing sequence. If a part upload fails, retry the upload as all parts must be present before committing the stream execution.
#' Parts can be uploaded simultaneously in separate threads assuming that each part has a distinct part ID and is ordered correctly. To reduce upload time, compress each data as a gzip file (application/gzip).
#' \href{https://developer.domo.com/portal/f186ddf30fca9-upload-a-data-part}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the developer.domo.com page.
#' @param secret A secret that can created on the developer.domo.com page.
#' @param execution_id The ID of the Stream execution within the Stream.
#' @param part_id The ID of the data part being used to upload a subset of data within the Stream execution.
#' @param stream_id The ID of the Stream of data being imported into a DataSet.
#' @param body The content of the data. Please refer to the documentation for details.
#' @examples stream_uploadPart(client_id = client_id,
#'   secret = secret,
#'   execution_id = 1,
#'   part_id = 1,
#'   body = body)
#' @export


stream_uploadPart <- function(client_id, secret, execution_id, part_id, stream_id, body) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::PUT(url = paste0('https://api.domo.com/v1/streams/', stream_id,'/executions/', execution_id,'/part/', part_id),
               body = body,
               config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
               httr::content_type("text/csv"),
               httr::accept("application/json"),
               encode = 'gzip'))

  return(data)
}
