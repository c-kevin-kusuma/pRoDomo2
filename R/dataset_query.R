#' Query a DataSet
#'
#' Queries the data in an existing Domo DataSet.
#' \href{https://developer.domo.com/portal/52fd0777839f4-query-a-data-set}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param dataset_id The unique ID of the dataset.
#' @param sql_query SQL query such as \code{select * from table}
#' @examples dataset_query(client_id = client_id,
#'   secret = secret,
#'   dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4',
#'   sql_query = 'select * from')
#' @export


dataset_query <- function(client_id, secret, dataset_id, sql_query = 'select * from table') {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))


  #Body
  content <- httr::content(
    httr::POST(url = paste0('https://api.domo.com/v1/datasets/query/execute/', dataset_id),
               body = paste0('{"sql": ','"',sql_query,'"}'),
               config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
               httr::content_type("application/json"),
               httr:: accept("application/json"),
               encode = 'json'))

  data <- rlist::list.stack(content$rows)
  colnames(data) <- content$columns

  return(data)
}
