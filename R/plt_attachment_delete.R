#' Delete a project
#'
#' \strong{This is destructive and cannot be reversed.} Permanently deletes a project from your Domo instance.
#' \href{https://developer.domo.com/portal/abc4b606acabc-delete-an-attachment}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can created on the \url{developer.domo.com} page.
#' @param project_id The unique ID of the project.
#' @param list_id The unique ID of the list.
#' @param task_id The unique ID of the task.
#' @param attachment_id The unique ID of the attachment.
#' @examples
#' plt_attachment_delete(client_id = client_id,
#'   secret = secret,
#'   project_id = 123,
#'   list_id = 5,
#'   task_id = 12,
#'   attachment_id = 1)
#' @export


plt_attachment_delete <- function(client_id, secret, project_id, task_id, attachment_id) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::DELETE(url = paste0('https://api.domo.com/v1/project/',project_id,'/tasks/',task_id,'/attachments/',attachment_id),
                 config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
                 httr::content_type("application/octet-stream")))

  return(data)
}

