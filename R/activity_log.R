#' Retrieve Activity Log Entries
#'
#' This function allows you to Retrieves activity log entries and set custom limit and offset, all time/date values are in UTC.
#' \href{https://developer.domo.com/portal/i19jain6fvwjj-activity-log-api}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param start The start time(milliseconds) of when you want to receive log events.
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

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  param <- paste0('https://api.domo.com/v1/audit?',
                  ifelse(!is.na(start), paste0('start=',start)),
                  ifelse(!is.na(end), paste0('&end=',end)),
                  ifelse(!is.na(limit), paste0('&limit=',limit)),
                  ifelse(!is.na(offset), paste0('&offset=',offset)),
                  ifelse(!is.na(user_id), paste0('&user=',user_id)))

  activity_log <- httr::content(
    httr::GET(url = param,
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' ')))))

  return(activity_log)
}



