#' Get AppDB Documents
#'
#' Retrieves all documents from a Domo AppDB collection and returns
#' them as a flattened data frame.
#'
#' @param collection_id The AppDB collection ID.
#' @param developer_token A valid Domo Developer Token.
#' @param instance Domo instance URL. For example:
#'   \code{"https://company.domo.com"}. A bare hostname (for example
#'   \code{"company.domo.com"}) is also accepted; \code{https://} is
#'   added automatically when no scheme is present.
#' @param batch_limit Number of records to retrieve per API call.
#'   Defaults to \code{10000}, which is Domo's documented maximum
#'   page size for this endpoint. Values above \code{10000} are
#'   capped to \code{10000} with a warning.
#'
#' @examples
#' appdb_doc_get(
#'   collection_id = "12345678-1234-1234-1234-123456789012",
#'   developer_token = developer_token,
#'   instance = "https://company.domo.com"
#' )
#'
#' @export

appdb_doc_get <- function(
  collection_id,
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
    "/documents"
  )

  repeat {

    message("Fetching records starting at offset: ", offset)

    request <- httr2::request(url)
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

        record <- x$content
        record$appdb_document_id <- x$id

        record
      }
    )
  )

  return(data)
}
