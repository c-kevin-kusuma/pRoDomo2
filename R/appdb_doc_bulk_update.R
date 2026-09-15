#' Bulk Upsert AppDB Documents
#'
#' Creates and/or updates multiple AppDB documents in a single API
#' call. Each item in \code{data} that includes an \code{id} field
#' updates that existing document; each item without an \code{id}
#' creates a new document.
#'
#' @param collection_id The AppDB collection ID.
#' @param developer_token A valid Domo Developer Token.
#' @param instance Domo instance URL. For example:
#'   \code{"https://company.domo.com"}. A bare hostname is also
#'   accepted; \code{https://} is added automatically when no scheme
#'   is present.
#' @param data A list of document payloads. To update an existing
#'   document, add an \code{id} field to the payload returned by
#'   \code{appdb_row_to_document()}, e.g.
#'   \code{doc <- appdb_row_to_document(df, i); doc$id <- appdb_document_id}.
#'   Payloads without an \code{id} field are created as new documents.
#'
#' @return The API response object returned by
#'   \code{httr2::req_perform()}. On success, the response body is
#'   \code{list(Updated = <n>, Created = <n>)}. Domo mislabels this
#'   response as \code{Content-Type: text/plain} even though the body
#'   is JSON, so parse it with
#'   \code{httr2::resp_body_json(response, check_type = FALSE)} rather
#'   than the default \code{resp_body_json(response)}, which errors on
#'   the content-type mismatch.
#'
#' @examples
#' update_data <- list(
#'   list(
#'     id = "12345678-1234-1234-1234-123456789012",
#'     content = list(status = "Applied")
#'   ),
#'   appdb_row_to_document(data = clients, row_number = 2)
#' )
#'
#' response <- appdb_doc_bulk_update(
#'   collection_id = "12345678-1234-1234-1234-123456789012",
#'   developer_token = developer_token,
#'   instance = "https://company.domo.com",
#'   data = update_data
#' )
#'
#' @export

appdb_doc_bulk_update <- function(
  collection_id,
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

  instance <- .normalize_domo_instance(instance)
  url <- paste0(
    instance,
    "/api/datastores/v1/collections/",
    collection_id,
    "/documents/bulk"
  )

  request <- httr2::request(url)
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
  # Deliberately no automatic retry here: items without an `id` are
  # created rather than updated, so a blind retry risks creating
  # duplicate documents for that portion of the batch if an earlier
  # attempt actually succeeded server-side but the response was lost.
  request <- .appdb_req_error(request)

  response <- httr2::req_perform(request)

  return(response)

}
