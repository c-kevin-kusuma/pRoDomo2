#' Update a Task
#'
#' Update the details of a task given an existing project id, list id, and task id.
#' \href{https://developer.domo.com/portal/a9c700565ffba-update-a-task}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param project_id The unique ID of the project.
#' @param list_id The unique ID of the list.
#' @param task_id The unique ID of the task.
#' @param body The content of the task. Please refer to the documentation for details.
#' @examples plt_task_update(client_id = client_id,
#'   secret = secret,
#'   project_id = 123,
#'   list_id = 1,
#'   task_id = 4,
#'   body = body)
#' @export


plt_task_update <- function(client_id, secret, project_id, list_id, task_id, body) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rjson", quietly = TRUE)) {stop("Package \"rjson\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))


  data <- httr::content(
    httr::PUT(url = paste0('https://api.domo.com/v1/projects/',project_id,'/lists/',list_id,'/tasks/',task_id),
              body = rjson::toJSON(body),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/json"),
              httr::accept("application/json"),
              encode = 'json'))

  return(data)
}

