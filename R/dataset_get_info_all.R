#' List DataSets
#'
#' Get a list of all DataSets in your Domo instance.
#' \href{https://developer.domo.com/portal/72ae9b3e80374-list-data-sets}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param nameLike If included, will limit the list of DataSets to those with names that contain the string passed in. nameLike is case insensitive. \strong{\emph{Optional}}
#' @param sort The DataSet field to sort by. Fields prefixed with a negative sign reverses the sort (i.e. '-name' does a reverse sort by the name of the DataSets). \strong{\emph{Optional}}
#' @examples dataset_get_info_all(client_id = client_id,
#' secret = secret
#' nameLike = 'First',
#' sort = 'name')
#' @export


dataset_get_info_all <- function(client_id, secret, nameLike = NULL, sort = NULL) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  limit = 50
  offset = 0

  i=0
  data <- list()
  nr=1
  while(nr > 0){
    d <- dplyr::bind_rows(httr::content(
      httr::GET(url = 'https://api.domo.com/v1/datasets',
                config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
                query = list(limit = limit, offset = offset, nameLike = nameLike, sort = sort))))

    nr <- nrow(d)
    i <- i+1
    if('status' %in% colnames(d)){break}
    if(nr>0) {data[[i]] <- dplyr::slice_tail(d, n=1, by = id)}

    offset <- offset+limit
  }

  data <- dplyr::bind_rows(data)

  return(data)
}
