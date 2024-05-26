#' Update a DataSet
#'
#' Updates the specified DataSet’s metadata by providing values to parameters passed.
#' \href{https://developer.domo.com/portal/b96e1dbabff23-update-a-data-set}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param dataset_id The unique ID of the dataset.
#' @param body A \code{list()} of name, description, pdpEnabled, and schema.
#' @examples dataset_update_info(client_id = client_id,
#'   secret = secret,
#'   dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4',
#'   body = list(name = 'The Name', pdpEnabled = TRUE))
#' @export


dataset_update_info <- function(client_id, secret, dataset_id, body) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::PUT(url = paste0('https://api.domo.com/v1/datasets/', dataset_id),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              encode = 'json',
              body = body))

  return(data)
}
