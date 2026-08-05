#' Delete AppDB Document
#'
#' Deletes an existing AppDB document.
#'
#' @param url The AppDB collection URL.
#' @param appdb_document_id The document ID to delete.
#' @param developer_token A valid Domo Developer Token.
#'
#' @examples
#' response <- appdb_doc_delete(
#'   url = collection_url,
#'   appdb_document_id = "12345678-1234-1234-1234-123456789012",
#'   developer_token = developer_token
#' )
#'
#' @export

appdb_doc_delete <- function(
  url,
  appdb_document_id,
  developer_token
) {

  # Check Required Packages
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      'Package "httr2" must be installed to use this function.',
      call. = FALSE
    )
  }

  delete_url <- paste0(
    url,
    "/",
    appdb_document_id
  )

  response <- httr2::request(delete_url) %>%
    httr2::req_method("DELETE") %>%
    httr2::req_headers(
      `X-DOMO-Developer-Token` = developer_token,
      Accept = "application/json"
    ) %>%
    httr2::req_perform()

  return(response)

}
