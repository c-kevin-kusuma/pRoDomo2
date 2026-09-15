#' Create AppDB Datastore
#'
#' Creates a new AppDB datastore, owned by the calling developer
#' token's user. A datastore is the top-level container that AppDB
#' collections live in; a brand-new collection not tied to an
#' existing DDX app needs a \code{datastore_id} from here (or from
#' \code{appdb_datastore_get_all()}) before it can be created.
#'
#' @param name Name of the datastore to create.
#' @param developer_token A valid Domo Developer Token.
#' @param instance Domo instance URL. For example:
#'   \code{"https://company.domo.com"}. A bare hostname is also
#'   accepted; \code{https://} is added automatically when no scheme
#'   is present.
#'
#' @return The API response object returned by
#'   \code{httr2::req_perform()}. The created datastore's ID is
#'   available at \code{httr2::resp_body_json(response)$id}.
#'
#' @examples
#' response <- appdb_datastore_create(
#'   name = "A New Datastore",
#'   developer_token = developer_token,
#'   instance = "https://company.domo.com"
#' )
#'
#' @export

appdb_datastore_create <- function(
  name,
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
  url <- paste0(instance, "/api/datastores/v1")

  request <- httr2::request(url)
  request <- httr2::req_method(request, "POST")
  request <- httr2::req_headers(
    request,
    `X-DOMO-Developer-Token` = developer_token,
    Accept = "application/json"
  )
  request <- httr2::req_body_json(
    request,
    list(name = name),
    auto_unbox = TRUE
  )
  # Deliberately no automatic retry here: POST/create is not idempotent,
  # and blind retries risk creating duplicate datastores if an earlier
  # attempt actually succeeded server-side but the response was lost.
  request <- .appdb_req_error(request)

  response <- httr2::req_perform(request)

  return(response)

}
