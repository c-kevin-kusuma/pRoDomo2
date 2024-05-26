#' Import data into DataSet
#'
#' Import data into a DataSet in your Domo instance. This request will replace the data currently in the DataSet.
#' \href{https://developer.domo.com/portal/4ba1c85b30ae1-import-data-into-data-set}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can created on the \url{developer.domo.com} page.
#' @param dataset_id The unique ID of the dataset.
#' @param data_table A dataframe/tibble. The function will convert the data_table into a csv before uploading.
#' @examples dataset_import(client_id = client_id,
#'   secret = secret,
#'   dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4',
#'   data_table = data_table)
#' @export


dataset_import <- function(client_id, secret, dataset_id, data_table) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))


  data <- httr::content(
    httr::PUT(url = paste0('https://api.domo.com/v1/datasets/', dataset_id, '/data'),
               body = apply(data_table, 1, paste, collapse = ','),
               config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
               httr::content_type("text/csv"),
               httr::accept("application/json"),
               encode = 'raw'))

  return(data)
}
