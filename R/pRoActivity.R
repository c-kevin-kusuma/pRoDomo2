#' Retrieve Activity Log Entries (Simplified)
#'
#' This function allows you to retrieves all activity log entries, all time/date values are in UTC.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param start (Optional) Only 'numeric' or 'Date' are acceptable as input. The start time(milliseconds) of when you want to receive log events, defaulted to the last 24 hours.
#' @param end (Optional) Only 'numeric' or 'Date' are acceptable as input. The end time(milliseconds) of when you want to receive log events, defaulted to now.
#' @param user_id (Optional) Specify the ID of the user to retrieve a certain individual's activities.
#' @examples pRoActivity(client_id = client_id,
#'   secret = secret,
#'   start = as.Date('2024-01-01'),
#'   end = as.Date('2024-01-01'),
#'   user_id = 27)
#' @export



pRoActivity <- function(client_id, secret, start = NULL, end = NULL, user_id = NULL) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  # Check End parameter
  stopifnot(class(end) == 'numeric' | class(end)=='Date' | is.null(end))
  if(class(end) == 'Date' & !is.null(end)) {end <- (as.integer(as.POSIXct(end))+86400)*1000}
  if(is.null(end)) {end <- as.integer(as.POSIXct(Sys.time()))*1000}


  # Check Start parameter
  stopifnot(class(start) == 'numeric' | class(start)=='Date' | is.null(start))
  if(class(start) == 'Date' & !is.null(start)) {start <- as.integer(as.POSIXct(start))*1000}
  if(is.null(start)) {start <- end - 86400000}

  limit =1000
  offset = 0

  i=0
  a <- list()
  nr=1
  while(nr > 0){
    param <- paste0('https://api.domo.com/v1/audit?',
                    ifelse(!is.na(start), paste0('start=',start)),
                    ifelse(!is.na(end), paste0('&end=',end)),
                    ifelse(!is.na(limit), paste0('&limit=',limit)),
                    ifelse(!is.na(offset), paste0('&offset=',offset)),
                    ifelse(!is.na(user_id), paste0('&user=',user_id)))

    d <- dplyr::bind_rows(
      httr::content(
        httr::GET(url = param, config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))))))

    nr <- nrow(d)
    i <- i+1
    if(nr>0) {a[[i]] <- d}
    offset <- offset+limit
  }

  a <- dplyr::bind_rows(a)

  return(a)
}

