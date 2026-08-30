#' Delete AppDB Document
#'
#' Deletes an existing AppDB document.
#'
#' @param collection_id The AppDB collection ID.
#' @param appdb_document_id The document ID to delete.
#' @param developer_token A valid Domo Developer Token.
#' @param instance Domo instance URL. For example:
#'   \code{"https://company.domo.com"}. A bare hostname is also
#'   accepted; \code{https://} is added automatically when no scheme
#'   is present.
#'
#' @return The API response object returned by
#'   \code{httr2::req_perform()}.
#'
#' @examples
#' response <- appdb_doc_delete(
#'   collection_id = "12345678-1234-1234-1234-123456789012",
#'   appdb_document_id = "12345678-1234-1234-1234-123456789012",
#'   developer_token = developer_token,
#'   instance = "https://company.domo.com"
#' )
#'
#' @export

appdb_doc_delete <- function(
  collection_id,
  appdb_document_id,
  developer_token,
  instance
) {

  # Check Required Packages
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      'Package "httr2" must be installed to use this function.',
      call. = FALSE
    )
  }

  instance <- .normalize_domo_instance(instance)
  delete_url <- paste0(
    instance,
    "/api/datastores/v1/collections/",
    collection_id,
    "/documents/",
    appdb_document_id
  )

  request <- httr2::request(delete_url)
  request <- httr2::req_method(request, "DELETE")
  request <- httr2::req_headers(
    request,
    `X-DOMO-Developer-Token` = developer_token,
    Accept = "application/json"
  )
  # DELETE is idempotent (deleting an already-deleted document is a no-op
  # from the caller's perspective), so retrying on a transient failure is
  # safe here.
  request <- httr2::req_retry(request, max_tries = 3)
  request <- .appdb_req_error(request)

  response <- httr2::req_perform(request)

  return(response)

}
