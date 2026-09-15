#' Bulk Create AppDB Documents
#'
#' Creates multiple AppDB documents in a single API call.
#'
#' @param collection_id The AppDB collection ID.
#' @param developer_token A valid Domo Developer Token.
#' @param instance Domo instance URL. For example:
#'   \code{"https://company.domo.com"}. A bare hostname is also
#'   accepted; \code{https://} is added automatically when no scheme
#'   is present.
#' @param data A list of document payloads, each typically generated
#'   using \code{appdb_row_to_document()}. For example,
#'   \code{purrr::map(seq_len(nrow(df)), ~appdb_row_to_document(df, .x))}.
#'
#' @return The API response object returned by
#'   \code{httr2::req_perform()}. On success, the response body is
#'   \code{list(Created = <n>)}; it does not include the generated
#'   document IDs.
#'
#' @examples
#' new_data <- purrr::map(
#'   seq_len(nrow(clients)),
#'   ~ appdb_row_to_document(data = clients, row_number = .x)
#' )
#'
#' response <- appdb_doc_bulk_create(
#'   collection_id = "12345678-1234-1234-1234-123456789012",
#'   developer_token = developer_token,
#'   instance = "https://company.domo.com",
#'   data = new_data
#' )
#'
#' @export

appdb_doc_bulk_create <- function(
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
  request <- httr2::req_method(request, "POST")
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
  # Deliberately no automatic retry here: POST/create is not idempotent,
  # and blind retries risk creating duplicate documents if an earlier
  # attempt actually succeeded server-side but the response was lost.
  request <- .appdb_req_error(request)

  response <- httr2::req_perform(request)

  return(response)

}
