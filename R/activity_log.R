#' Retrieve Activity Log Entries
#'
#' This function retrieves activity log entries and supports custom limit and offset values. All time/date values are in UTC.
#' \href{https://developer.domo.com/portal/i19jain6fvwjj-activity-log-api}{DOMO Documentation}.
#'
#' @details Optional query parameters may be left as `NULL`. If provided, query parameters must be single, non-`NA` values.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param start The start time(milliseconds) of when you want to receive log events. \strong{\emph{Optional}}
#' @param end The end time(milliseconds) of when you want to receive log events. \strong{\emph{Optional}}
#' @param limit The maximum number of events you want to retrieve(default is 50, maximum of 1000). \strong{\emph{Optional}}
#' @param offset The offset location of events you retrieve(default is 0). \strong{\emph{Optional}}
#' @param user_id Specify the ID of the user to retrieve a certain individual's activities. \strong{\emph{Optional}}
#' @examples activity_log(client_id = client_id,
#'   secret = secret,
#'   start = 1709337594000,
#'   end = 1709937594000,
#'   limit = 50,
#'   offset = 0)
#' @export



activity_log <- function(client_id, secret, start = NULL, end = NULL, limit = NULL, offset = NULL, user_id = NULL) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  query <- list(start = start, end = end, limit = limit, offset = offset, user = user_id)
  query <- query[!vapply(query, is.null, logical(1))]
  invalid_query <- vapply(query, function(x) length(x) != 1 || is.na(x), logical(1))
  if (any(invalid_query)) {stop(paste0("Invalid activity log parameter(s): ", paste(names(query)[invalid_query], collapse = ", ")), call. = FALSE)}

  # Access
  access_response <- httr::GET(url = 'https://api.domo.com/oauth/token',
                               config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
                               query = list(grant_type='client_credentials'))
  httr::stop_for_status(access_response, task = 'get Domo access token')
  access <- httr::content(access_response)
  if (is.null(access$access_token)) {stop('Domo access token was not returned.', call. = FALSE)}

  activity_response <- httr::GET(url = 'https://api.domo.com/v1/audit',
                                 query = query,
                                 config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))))
  httr::stop_for_status(activity_response, task = 'retrieve activity log entries')

  activity_log <- httr::content(activity_response)

  return(activity_log)
}



