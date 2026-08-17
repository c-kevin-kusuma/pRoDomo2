#' Update AppDB Document
#'
#' Updates an existing AppDB document.
#'
#' @param collection_id The AppDB collection ID.
#' @param appdb_document_id The document ID to update.
#' @param developer_token A valid Domo Developer Token.
#' @param instance Domo instance URL. For example:
#'   \code{"https://company.domo.com"}.
#' @param data A document payload, typically generated using
#'   \code{appdb_row_to_document()}.
#'
#' @return The API response object returned by
#'   \code{httr2::req_perform()}.
#'
#' @examples
#' update_data <- appdb_row_to_document(
#'   data = clients,
#'   row_number = 1
#' )
#'
#' response <- appdb_doc_update(
#'   collection_id = "12345678-1234-1234-1234-123456789012",
#'   appdb_document_id = "12345678-1234-1234-1234-123456789012",
#'   developer_token = developer_token,
#'   instance = "https://company.domo.com",
#'   data = update_data
#' )
#'
#' @export

appdb_doc_update <- function(
  collection_id,
  appdb_document_id,
  developer_token,
  instance,
  data
) {

  # Check Required Packages
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      'Package "httr2" must be installed to use this function.',
      call. = FALSE
    )
  }

  instance <- sub("/+$", "", instance)
  update_url <- paste0(
    instance,
    "/api/datastores/v1/collections/",
    collection_id,
    "/documents/",
    appdb_document_id
  )

  request <- httr2::request(update_url)
  request <- httr2::req_method(request, "PUT")
  request <- httr2::req_headers(
    request,
    `X-DOMO-Developer-Token` = developer_token,
    Accept = "application/json"
  )
  request <- httr2::req_body_json(
    request,
    data,
    auto_unbox = TRUE
  )

  response <- httr2::req_perform(request)

  return(response)

}
