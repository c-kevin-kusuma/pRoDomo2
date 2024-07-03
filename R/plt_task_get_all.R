#' Retrieve all project tasks
#'
#' Retrieves all tasks from a given project id.
#' \href{https://developer.domo.com/portal/cf1cad1dc8fe5-retrieve-all-project-tasks}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param project_id The unique ID of the project.
#' @examples plt_task_get_all(client_id = client_id,
#'   secret = secret,
#'   project_id = 123)
#' @export


plt_task_get_all <- function(client_id, secret, project_id) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::GET(url = paste0('https://api.domo.com/v1/projects/',project_id,'/tasks?),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/octet-stream"),
              httr::accept("application/json")))

  return(data)
}

