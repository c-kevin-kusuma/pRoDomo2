#' Create a project
#'
#' Create a new project in your Domo instance.
#' \href{https://developer.domo.com/portal/abaea94e18a2f-create-a-project}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param body The parameters needed to create the project.
#' @examples
#' members = list(); members[[1]] <- 12345
#' body <- list(name = 'Project Name', members = members, public = TRUE)
#' plt_project_create(client_id = client_id,
#'   secret = secret,
#'   body = body)
#' @export


plt_project_create <- function(client_id, secret, body) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  data <- httr::content(
    httr::POST(url = 'https://api.domo.com/v1/projects',
               body = body,
               config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
               httr::content_type("application/json"),
               httr::accept("application/json"),
               encode = 'json'))

  return(data)
}

