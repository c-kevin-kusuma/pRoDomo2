#' Create An Empty DataSet
#'
#' Creates a new empty DataSet in your Domo instance. Once the empty DataSet has been created, data can then be imported into the DataSet. Use \code{pRoDsCreate()} function for the best option.
#' \href{https://developer.domo.com/portal/0d0517e6b6830-create-a-data-set}{DOMO Documentation}.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param name Name of the DataSet to create.
#' @param description Description of DataSet to create.
#' @param schema The current schema associated with this DataSet. A tibble/dataframe that has two columns: "name" & "type". The "name" field is the list of column names and "type" is the type of each column such as:
#'  \itemize{
#'    \item STRING
#'    \item DECIMAL
#'    \item LONG
#'    \item DOUBLE
#'    \item DATE
#'    \item DATETIME}
#' @examples dataset_create(client_id = client_id,
#'   secret = secret,
#'   name = 'A New Dataset',
#'   description = 'Description of the new dataset',
#'   schema = data.frame(name =c('col1', 'col2'), type = c('STRING', 'DOUBLE')))
#' @export


dataset_create <- function(client_id, secret, name, description, schema) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))


  #Body
  columns <- list()
  for (i in 1:nrow(schema)) {
    columns[[i]] <- list(
      type = schema$type[i],
      name = schema$name[i])}

  body <- list(
    name = name,
    description = description,
    schema = list(columns = columns))

  data <- httr::content(
    httr::POST(url = 'https://api.domo.com/v1/datasets/',
               config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
               encode = 'json',
               body = body))

  print(data$id)
  return(data)
}
