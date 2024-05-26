#' Update a Personalized Data Permission (PDP) policy
#'
#' Update the specific PDP policy for a DataSet by providing values to parameters passed.
#' \href{https://developer.domo.com/portal/dcdf6cd230d61-update-a-personalized-data-permission-pdp-policy}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can created on the \url{developer.domo.com} page.
#' @param dataset_id The unique ID of the dataset.
#' @param pdp_id The unique ID of the policy.
#' @param body Parameters to update the policy.
#' @examples pdp_update(client_id = client_id,
#'   secret = secret,
#'   dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4',
#'   pdp_id = 123,
#'   body = body)
#' @export


pdp_update <- function(client_id, secret, dataset_id, pdp_id, body) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))


  data <- httr::content(
    httr::PUT(url = paste0('https://api.domo.com/v1/datasets/', dataset_id, '/policies/', pdp_id),
              body = body,
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/json"),
              httr::accept("application/json"),
              encode = 'json'))


  return(data)
}

