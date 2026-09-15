#' List AppDB Collections
#'
#' Retrieves all AppDB collections in the instance's datastore and
#' returns them as a data frame.
#'
#' @param developer_token A valid Domo Developer Token.
#' @param instance Domo instance URL. For example:
#'   \code{"https://company.domo.com"}. A bare hostname is also
#'   accepted; \code{https://} is added automatically when no scheme
#'   is present.
#'
#' @details
#' Only the collection-level fields (\code{id}, \code{name},
#' \code{datastoreId}, \code{syncEnabled}, \code{createdOn},
#' \code{updatedOn}) are returned; a collection's \code{schema}
#' (its column definitions) is nested and varies in shape from
#' collection to collection, so it's left out of this flattened
#' table. Use \code{appdb_collection_update()}'s response, or a
#' direct API call, to inspect a single collection's schema.
#'
#' @return A tibble with one row per collection.
#'
#' @examples
#' appdb_collection_get_all(
#'   developer_token = developer_token,
#'   instance = "https://company.domo.com"
#' )
#'
#' @export

appdb_collection_get_all <- function(
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
  url <- paste0(instance, "/api/datastores/v1/collections")

  request <- httr2::request(url)
  request <- httr2::req_headers(
    request,
    `X-DOMO-Developer-Token` = developer_token,
    Accept = "application/json"
  )
  request <- httr2::req_retry(request, max_tries = 3)
  request <- .appdb_req_error(request)

  response <- httr2::req_perform(request)

  collections <- httr2::resp_body_json(response)

  if (length(collections) == 0) {
    return(dplyr::tibble())
  }

  data <- dplyr::bind_rows(
    purrr::map(
      collections,
      function(x) {
        list(
          id = .appdb_null_to_na(x$id),
          name = .appdb_null_to_na(x$name),
          datastoreId = .appdb_null_to_na(x$datastoreId),
          syncEnabled = isTRUE(x$syncEnabled),
          createdOn = .appdb_null_to_na(x$createdOn),
          updatedOn = .appdb_null_to_na(x$updatedOn)
        )
      }
    )
  )

  return(data)

}
