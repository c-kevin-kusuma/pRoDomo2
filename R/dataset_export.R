#' Export data from a DOMO DataSet
#'
#' Export data from a DataSet in your Domo instance. Data types will be exported as they are currently stored in the dataset. In addition, the only supported export type is CSV.
#' \href{https://developer.domo.com/portal/83ef8a4d71450-export-data-from-data-set}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param dataset_id The unique ID of the dataset.
#' @examples dataset_export(client_id = client_id,
#'   secret = secret,
#'   dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4')
#' @export


dataset_export <- function(client_id, secret, dataset_id) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))


  data <- httr::content(
    httr::GET(url = paste0('https://api.domo.com/v1/datasets/', dataset_id, '/data'),
              query = list(includeHeader = "true", fileName = "data.csv"),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/octet-stream"),
              httr::accept("text/csv")))

  return(data)
}
