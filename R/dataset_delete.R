#' Delete a DataSet
#'
#' \strong{This is destructive and cannot be reversed.} Permanently deletes a DataSet from your Domo instance. This can be done for all DataSets, not just those created through the API.
#' \href{https://developer.domo.com/portal/8e96d3ae1061d-delete-a-data-set}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param dataset_id The unique ID of the dataset.
#' @examples dataset_delete(client_id = client_id,
#'   secret = secret,
#'   dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4')
#' @export


dataset_delete <- function(client_id, secret, dataset_id) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::DELETE(url = paste0('https://api.domo.com/v1/datasets/', dataset_id),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' ')))))

  return(data)
}
