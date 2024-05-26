#' Create a task
#'
#' Add a task to a project list.
#' \href{https://developer.domo.com/portal/e5d77a7e640ac-create-a-task}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param project_id The unique ID of the project.
#' @param list_id The unique ID of the list.
#' @param body The content of the task. Please refer to the documentation for details.
#' @examples
#' contributors <- list(); contributors[[1]] <- 790204705
#' body <- list(projectId = 7, projectListId = 22, taskName = 'New Task 1', createdDate = paste0(Sys.Date(),'T00:00:00Z'),
#'              updatedDate = paste0(Sys.Date(),'T00:00:00Z'), priority = 1, createdBy = 790204705, ownedBy = 790204705,
#'              contributors = contributors, attachmentCount = 0, tags = list(), archived = FALSE)
#' plt_task_create(client_id = client_id,
#'   secret = secret,
#'   project_id = 7,
#'   list_id = 22,
#'   body = body)
#' @export


plt_task_create <- function(client_id, secret, project_id, list_id, body) {

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
    httr::POST(url = paste0('https://api.domo.com/v1/projects/',project_id,'/lists/',list_id,'/tasks'),
              body = rjson::toJSON(body),
              config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
              httr::content_type("application/json"),
              httr::accept("application/json"),
              encode = 'json'))

  return(data)
}
