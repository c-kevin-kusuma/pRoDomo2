#' Retrieve All Pages
#'
#' Get a list of all pages in your Domo instance.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param data_frame If "TRUE", the output will be a data frame.
#' @examples pRoPageGet(client_id = client_id, secret = secret)
#' @export


pRoPageGet <- function(client_id, secret, data_frame = TRUE) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  limit = 500; offset = 0; i=0; data <- list(); nr=1
  while(nr > 0){
    d <- httr::content(
      httr::GET(url = 'https://api.domo.com/v1/pages',
                config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
                query = list(limit = limit, offset = offset),
                httr::content_type("application/octet-stream"),
                httr::accept("application/json")))

    nr <- length(d)
    if(nr == 0){break}
    i <- i+1
    offset <- offset+limit
    data[[i]] <- d
  }


  x <- 0; data_1 <- list()
  for(i in 1:length(data)){
    y <- data[[i]]
    for (j in 1:length(y)) {data_1[[x+j]] <- y[[j]]}
    x <- x + length(y)
  }

  if(data_frame){
  parent <- list(); child <- list(); grandchild <- list(); x=0; y=0; z=0; t=0
  for (i in 1:length(data_1)) {
    # Parent
    a <- data_1[[i]]
    parent[[i]]  <- dplyr::tibble(page_id = a$id, page_name = a$name, number_children = length(a$children), parent_id = NA, type = 'Parent')

    # Child
    b <- a$children
    if(length(b) > 0){
      for(y in 1:length(b)){
        c <- b[[y]]
        child[[x+y]] <- dplyr::tibble(page_id = c$id, page_name = c$name, number_children = length(c$children), parent_id = a$id, type = 'Child')

        # Grand Child
        d <- c$children
        if(length(d) > 0){
          for (t in 1:length(d)) {
            k <- d[[t]]
            grandchild[[z+t]] <- dplyr::tibble(page_id = k$id, page_name = k$name, number_children = length(k$children), parent_id = c$id, type = 'Grand Child')
          }
        }
        z <- z+t
        }
      }
    x <- x+y
    }

  return(dplyr::bind_rows(parent, child, grandchild))
  }


  return(data_1)
}

