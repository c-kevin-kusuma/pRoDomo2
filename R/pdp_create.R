#' Create a Personalized Data Permission (PDP) Policy
#'
#' Create a PDP policy for user and or group access to data within a DataSet. Users and groups must exist before creating PDP policy. The number of characters for the list of values for a single PDP policy, including a delimiter character for each value, must be less than 255 characters.
#' \href{https://developer.domo.com/portal/57eb007324b49-create-a-personalized-data-permission-pdp-policy}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param dataset_id The unique ID of the dataset.
#' @param body Parameters to update the policy.
#' @examples
#' pdp_create(client_id = client_id,
#'   secret = secret,
#'   dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4',
#'   body = body)
#' @export


pdp_create <- function(client_id, secret, dataset_id, body) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))


  data <- httr::content(
    httr::POST(url = paste0('https://api.domo.com/v1/datasets/', dataset_id, '/policies/'),
              body = body,
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/json"),
              httr::accept("application/json"),
              encode = 'json'))

  return(data)
}

