#' Query AppDB Documents
#'
#' Retrieves documents from a Domo AppDB collection that match a
#' MongoDB-syntax filter, evaluated server-side, and returns them as
#' a flattened data frame. Use this instead of \code{appdb_doc_get()}
#' when a collection is large and only a subset of its documents is
#' needed, so filtering happens on Domo's side instead of after
#' pulling every document into R.
#'
#' @param collection_id The AppDB collection ID.
#' @param query A list representing the MongoDB-syntax filter to
#'   apply, referencing document fields as \code{content.<field>}.
#'   For example, \code{list(content.status = "Pending")}, or an
#'   \code{"$or"}/\code{"$regex"}/\code{"$ne"} expression.
#' @param developer_token A valid Domo Developer Token.
#' @param instance Domo instance URL. For example:
#'   \code{"https://company.domo.com"}. A bare hostname is also
#'   accepted; \code{https://} is added automatically when no scheme
#'   is present.
#' @param batch_limit Number of records to retrieve per API call.
#'   Defaults to \code{10000}, which is Domo's documented maximum
#'   page size for this endpoint. Values above \code{10000} are
#'   capped to \code{10000} with a warning.
#'
#' @details
#' As with \code{appdb_doc_get()}, multi-select fields are collapsed
#' to one scalar per record (\code{NA} when empty, \code{"; "}-joined
#' when multi-valued) before being bound into a single data frame.
#'
#' @examples
#' appdb_doc_query(
#'   collection_id = "12345678-1234-1234-1234-123456789012",
#'   query = list(`content.status` = "Pending"),
#'   developer_token = developer_token,
#'   instance = "https://company.domo.com"
#' )
#'
#' @export

appdb_doc_query <- function(
  collection_id,
  query,
  developer_token,
  instance,
  batch_limit = 10000
) {

  # Check Required Packages
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package \"httr2\" must be installed to use this function.",
         call. = FALSE)
  }

  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package \"purrr\" must be installed to use this function.",
         call. = FALSE)
  }

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" must be installed to use this function.",
         call. = FALSE)
  }

  if (batch_limit > 10000) {
    warning(
      "batch_limit capped at 10000 (Domo's documented maximum page size ",
      "for this endpoint).",
      call. = FALSE
    )
    batch_limit <- 10000
  }

  offset <- 0
  all_documents <- list()

  instance <- .normalize_domo_instance(instance)
  url <- paste0(
    instance,
    "/api/datastores/v1/collections/",
    collection_id,
    "/documents/query"
  )

  repeat {

    message("Fetching records starting at offset: ", offset)

    request <- httr2::request(url)
    request <- httr2::req_method(request, "POST")
    request <- httr2::req_url_query(
      request,
      limit = batch_limit,
      offset = offset
    )
    request <- httr2::req_headers(
      request,
      `X-DOMO-Developer-Token` = developer_token,
      Accept = "application/json"
    )
    request <- httr2::req_body_json(
      request,
      query,
      auto_unbox = TRUE
    )
    # Unlike appdb_doc_create()'s POST, this one is read-only (it queries
    # existing documents rather than creating anything), so retrying on
    # a transient failure is safe here.
    request <- httr2::req_retry(request, max_tries = 3)
    request <- .appdb_req_error(request)

    response <- httr2::req_perform(request)

    batch <- httr2::resp_body_json(response)

    if (length(batch) == 0) {
      break
    }

    all_documents <- c(all_documents, batch)

    if (length(batch) < batch_limit) {
      break
    }

    offset <- offset + batch_limit
  }

  if (length(all_documents) == 0) {
    return(dplyr::tibble())
  }

  data <- dplyr::bind_rows(
    purrr::map(
      all_documents,
      function(x) {

        record <- .appdb_normalize_record(x$content)
        record$appdb_document_id <- x$id

        record
      }
    )
  )

  return(data)
}
