#' List groups
#'
#' Get a list of all groups in your Domo instance.
#' \href{https://developer.domo.com/portal/d7364df8a5f06-list-groups}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can created on the \url{developer.domo.com} page.
#' @examples group_get_all(client_id = client_id, secret = secret)
#' @export


group_get_all <- function(client_id, secret) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  limit = 500
  offset = 0

  i=0
  data <- list()
  nr=1
  while(nr > 0){
    d <- dplyr::bind_rows(httr::content(
      httr::GET(url = 'https://api.domo.com/v1/groups/',
                config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
                query = list(limit = limit, offset = offset))))

    nr <- nrow(d)
    i <- i+1
    offset <- offset+limit
    data[[i]] <- d
  }

  data <- dplyr::bind_rows(data)

  return(data)
}
