#' Update AppDB Document
#'
#' Updates an existing AppDB document.
#'
#' @param url The AppDB collection URL.
#' @param appdb_document_id The document ID to update.
#' @param developer_token A valid Domo Developer Token.
#' @param data A document payload, typically generated using
#'   \code{appdb_row_to_document()}.
#'
#' @examples
#' update_data <- appdb_row_to_document(
#'   data = clients,
#'   row_number = 1
#' )
#'
#' response <- appdb_doc_update(
#'   url = collection_url,
#'   appdb_document_id = "12345678-1234-1234-1234-123456789012",
#'   developer_token = developer_token,
#'   data = update_data
#' )
#'
#' @export

appdb_doc_update <- function(
  url,
  appdb_document_id,
  developer_token,
  data
) {

  # Check Required Packages
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      'Package "httr2" must be installed to use this function.',
      call. = FALSE
    )
  }

  update_url <- paste0(
    url,
    "/",
    appdb_document_id
  )

  response <- httr2::request(update_url) %>%
    httr2::req_method("PUT") %>%
    httr2::req_headers(
      `X-DOMO-Developer-Token` = developer_token
    ) %>%
    httr2::req_body_json(
      data,
      auto_unbox = TRUE
    ) %>%
    httr2::req_perform()

  return(response)

}
