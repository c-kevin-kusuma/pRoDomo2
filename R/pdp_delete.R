#' Delete a Personalized Data Permission (PDP) Policy
#'
#' \strong{This is destructive and cannot be reversed.} Permanently deletes a PDP policy on a DataSet in your Domo instance.
#' \href{https://developer.domo.com/portal/1d5e64ae6ce1a-delete-a-personlized-data-permission-pdp-policy}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param dataset_id The unique ID of the dataset.
#' @param pdp_id The unique ID of the policy
#' @examples pdp_delete(client_id = client_id,
#'   secret = secret,
#'   dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4',
#'   pdp_id = 123)
#' @export


pdp_delete <- function(client_id, secret, dataset_id, pdp_id) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))


  data <- httr::content(
    httr::DELETE(url = paste0('https://api.domo.com/v1/datasets/', dataset_id, '/policies/', pdp_id),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/octet-stream")))


  return(data)
}


