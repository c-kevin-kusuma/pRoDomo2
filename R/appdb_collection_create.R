#' Create AppDB Collection
#'
#' Creates a new AppDB collection.
#'
#' @param name Name of the collection to create.
#' @param developer_token A valid Domo Developer Token.
#' @param instance Domo instance URL. For example:
#'   \code{"https://company.domo.com"}. A bare hostname is also
#'   accepted; \code{https://} is added automatically when no scheme
#'   is present.
#' @param schema The schema for this collection, only required when
#'   \code{sync_enabled = TRUE}. A tibble/dataframe with two columns,
#'   "name" and "type", where "type" is one of:
#'   \itemize{
#'     \item STRING
#'     \item DECIMAL
#'     \item LONG
#'     \item DOUBLE
#'     \item DATE
#'     \item DATETIME}
#' @param sync_enabled Whether this collection should sync back to a
#'   Domo DataSet. Defaults to \code{FALSE}. When \code{TRUE},
#'   \code{schema} is required.
#'
#' @return The API response object returned by
#'   \code{httr2::req_perform()}. The created collection's ID is
#'   available at \code{httr2::resp_body_json(response)$id}.
#'
#' @examples
#' response <- appdb_collection_create(
#'   name = "A New Collection",
#'   developer_token = developer_token,
#'   instance = "https://company.domo.com",
#'   schema = data.frame(name = c("col1", "col2"), type = c("STRING", "DOUBLE")),
#'   sync_enabled = TRUE
#' )
#'
#' @export

appdb_collection_create <- function(
  name,
  developer_token,
  instance,
  schema = NULL,
  sync_enabled = FALSE
) {

  # Check Required Packages
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      'Package "httr2" must be installed to use this function.',
      call. = FALSE
    )
  }

  if (sync_enabled && is.null(schema)) {
    stop("`schema` is required when `sync_enabled = TRUE`.", call. = FALSE)
  }

  body <- list(
    name = name,
    syncEnabled = sync_enabled
  )

  if (!is.null(schema)) {
    columns <- list()
    for (i in seq_len(nrow(schema))) {
      columns[[i]] <- list(
        name = schema$name[i],
        type = schema$type[i]
      )
    }
    body$schema <- list(columns = columns)
  }

  instance <- .normalize_domo_instance(instance)
  url <- paste0(instance, "/api/datastores/v1/collections")

  request <- httr2::request(url)
  request <- httr2::req_method(request, "POST")
  request <- httr2::req_headers(
    request,
    `X-DOMO-Developer-Token` = developer_token,
    Accept = "application/json"
  )
  request <- httr2::req_body_json(
    request,
    body,
    auto_unbox = TRUE
  )
  # Deliberately no automatic retry here: POST/create is not idempotent,
  # and blind retries risk creating duplicate collections if an earlier
  # attempt actually succeeded server-side but the response was lost.
  request <- .appdb_req_error(request)

  response <- httr2::req_perform(request)

  return(response)

}
