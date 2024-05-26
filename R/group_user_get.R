#' List users in a group
#'
#' List the users in a group in your Domo instance.
#' \href{https://developer.domo.com/portal/1ce9b0022073a-list-users-in-a-group}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can created on the \url{developer.domo.com} page.
#' @param group_id The unique ID of the group.
#' @examples group_user_get(client_id = client_id,
#'   secret = secret,
#'   group_id = '123456789')
#' @export


group_user_get <- function(client_id, secret, group_id) {

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
    d <- httr::content(
      httr::GET(url = paste0('https://api.domo.com/v1/groups/', group_id, '/users'),
                config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
                query = list(limit = limit, offset = offset),
                httr::content_type("application/octet-stream")))

    nr <- length(d)
    i <- i+1
    offset <- offset+limit
    data[[i]] <- dplyr::tibble(group_id = group_id, user_id = d)
  }

  data <- dplyr::bind_rows(data)

  return(data)
}

