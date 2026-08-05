#' Create AppDB Document
#'
#' Creates a new AppDB document using the provided payload.
#'
#' @param url The AppDB collection URL.
#' @param developer_token A valid Domo Developer Token.
#' @param data A document payload, typically generated using
#'   \code{appdb_row_to_document()}.
#'
#' @examples
#' appdb_doc_create(
#'   url = collection_url,
#'   developer_token = developer_token,
#'   data = new_data
#' )
#'
#' @export

appdb_doc_create <- function(
  url,
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

  response <- httr2::request(url) %>%
    httr2::req_method("POST") %>%
    httr2::req_headers(
      `X-DOMO-Developer-Token` = developer_token,
      Accept = "application/json"
    ) %>%
    httr2::req_body_json(
      data,
      auto_unbox = TRUE
    ) %>%
    httr2::req_perform()

  return(response)

}
