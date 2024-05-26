#' List Personalized Data Permission (PDP) policies
#'
#' List the Personalized Data Permission (PDP) policies for a specified DataSet.
#' \href{https://developer.domo.com/portal/4cab29aca0fd3-list-personalized-data-permission-pdp-policies}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param dataset_id The unique ID of the dataset.
#' @param data_frame The default is "FALSE".
#' @examples pRoPdpGet(client_id = client_id,
#'   secret = secret,
#'   dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4')
#' @export


pRoPdpGet <- function(client_id, secret, dataset_id, data_frame = FALSE) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))


  data <- httr::content(
    httr::GET(url = paste0('https://api.domo.com/v1/datasets/', dataset_id, '/policies'),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/octet-stream"),
              httr::accept("application/json")))

  if(data_frame == FALSE){return(data)}

  # Function
  extractPdp <- function(x) {
    if(length(x)==0) {break}
    for (i in 1:length(x)) {
      if(length(x[[i]]$filters) == 0){filters <- dplyr::tibble(column = '', values = '')} else{filters <- x[[i]]$filters %>% rlist::list.stack() %>% dplyr::select(column, values) %>% dplyr::mutate(values = as.character(values)) %>% dplyr::arrange(values)}
      if(length(x[[i]]$users) == 0){users <- dplyr::tibble(users = '')} else{users <- dplyr::tibble(users = x[[i]]$users) %>% dplyr::mutate(users = as.character(users)) %>% dplyr::arrange(users)} # Extract Users
      if(length(x[[i]]$groups) == 0){groups <- dplyr::tibble(groups = '')} else{groups <- dplyr::tibble(groups = x[[i]]$groups) %>% dplyr::mutate(groups = as.character(groups)) %>% dplyr::arrange(groups)} # Extract Groups
      x[[i]] <- dplyr::tibble(`Policy ID` = x[[i]]$id, `Policy Type` = x[[i]]$type, `Policy Name` = x[[i]]$name) %>% merge(filters) %>% merge(users) %>% merge(groups) %>% dplyr::rename(`Policy Column` = column, `Policy Value` = values, `User ID` = users, `Group ID` = groups)}

    y <- dplyr::bind_rows(x)
  }

  return(extractPdp(data))
}

