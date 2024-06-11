#' Search Streams
#'
#' Returns all Stream objects that meet argument criteria from original request.
#' \href{https://developer.domo.com/portal/41db49e09d006-search-streams}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param qualifiers The search qualifiers to search by available qualifiers: dataSource.id or dataSource.owner.id
#' @param fields Return desired fields: {all} or {id, dataset, updateMethod, createdAt, or modifiedAt}
#' @examples stream_search(client_id = client_id,
#'   secret = secret,
#'   dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4',
#'   fields = 'all')
#' @export


stream_search <- function(client_id, secret, dataset_id, fields = 'all') {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::GET(url = 'https://api.domo.com/v1/streams/search',
              query = list(q=paste0('dataSource.id:',dataset_id),fields=fields),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/octet-stream"),
              httr::accept("application/json")))

  return(data)
}

