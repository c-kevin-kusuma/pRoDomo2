#' Retrieve Activity Log Entries (Simplified)
#'
#' This function retrieves all activity log entries. All time/date values are in UTC.
#'
#' @details `start` and `end` may be numeric millisecond timestamps, `Date`, or POSIX date-time values. When `end` is provided as a `Date`, the full day is included by converting it to the next UTC midnight.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param start (Optional) The start time as a millisecond timestamp, `Date`, or POSIX date-time. Defaults to the last 24 hours.
#' @param end (Optional) The end time as a millisecond timestamp, `Date`, or POSIX date-time. Defaults to now.
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
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" must be installed to use this function.", call. = FALSE)}

  normalize_activity_time <- function(x, name) {
    if (is.null(x)) {return(NULL)}
    if (length(x) != 1 || is.na(x)) {stop(paste0("Invalid activity log parameter: ", name), call. = FALSE)}
    if (inherits(x, "Date")) {
      if (name == "end") {x <- x + 1}
      return(floor(as.numeric(as.POSIXct(x, tz = "UTC")) * 1000))
    }
    if (inherits(x, "POSIXt")) {return(floor(as.numeric(x) * 1000))}
    if (is.numeric(x)) {return(floor(x))}
    stop(paste0("Invalid activity log parameter: ", name), call. = FALSE)
  }

  end <- normalize_activity_time(end, "end")
  if (is.null(end)) {end <- floor(as.numeric(as.POSIXct(Sys.time())) * 1000)}

  start <- normalize_activity_time(start, "start")
  if (is.null(start)) {start <- end - 86400000}

  if (!is.null(user_id) && (length(user_id) != 1 || is.na(user_id))) {stop("Invalid activity log parameter: user_id", call. = FALSE)}

  # Access
  access_response <- httr::GET(url = 'https://api.domo.com/oauth/token',
                               config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
                               query = list(grant_type='client_credentials'))
  httr::stop_for_status(access_response, task = 'get Domo access token')
  access <- httr::content(access_response)
  if (is.null(access$access_token)) {stop('Domo access token was not returned.', call. = FALSE)}

  limit = 1000
  offset = 0

  i = 0
  a <- list()
  nr = 1
  while(nr > 0){
    query <- list(start = start, end = end, limit = limit, offset = offset, user = user_id)
    query <- query[!vapply(query, is.null, logical(1))]

    activity_response <- httr::GET(url = 'https://api.domo.com/v1/audit',
                                   query = query,
                                   config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))))
    httr::stop_for_status(activity_response, task = 'retrieve activity log entries')

    d <- dplyr::bind_rows(httr::content(activity_response))

    nr <- nrow(d)
    i <- i + 1
    if(nr>0) {a[[i]] <- d}
    offset <- offset + limit
  }

  a <- dplyr::bind_rows(a)

  return(a)
}

