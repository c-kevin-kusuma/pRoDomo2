#' List AppDB Datastores
#'
#' Retrieves all AppDB datastores owned by the calling developer
#' token's user.
#'
#' @param developer_token A valid Domo Developer Token.
#' @param instance Domo instance URL. For example:
#'   \code{"https://company.domo.com"}. A bare hostname is also
#'   accepted; \code{https://} is added automatically when no scheme
#'   is present.
#'
#' @return A tibble with one row per datastore.
#'
#' @examples
#' appdb_datastore_get_all(
#'   developer_token = developer_token,
#'   instance = "https://company.domo.com"
#' )
#'
#' @export

appdb_datastore_get_all <- function(
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

  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package \"purrr\" must be installed to use this function.",
         call. = FALSE)
  }

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" must be installed to use this function.",
         call. = FALSE)
  }

  instance <- .normalize_domo_instance(instance)
  url <- paste0(instance, "/api/datastores/v1")

  request <- httr2::request(url)
  request <- httr2::req_headers(
    request,
    `X-DOMO-Developer-Token` = developer_token,
    Accept = "application/json"
  )
  request <- httr2::req_retry(request, max_tries = 3)
  request <- .appdb_req_error(request)

  response <- httr2::req_perform(request)

  datastores <- httr2::resp_body_json(response)

  if (length(datastores) == 0) {
    return(dplyr::tibble())
  }

  data <- dplyr::bind_rows(
    purrr::map(
      datastores,
      function(x) {
        list(
          id = .appdb_null_to_na(x$id),
          name = .appdb_null_to_na(x$name),
          customer = .appdb_null_to_na(x$customer),
          owner = .appdb_null_to_na(x$owner),
          createdOn = .appdb_null_to_na(x$createdOn),
          updatedOn = .appdb_null_to_na(x$updatedOn)
        )
      }
    )
  )

  return(data)

}
