#' Get a Dataset's Tags
#'
#' Reads the tags on a dataset from \code{/api/data/v3/datasources/\{id\}}.
#'
#' @param dataset_id The dataset's id (a UUID).
#' @param developer_token A valid Domo Developer Token. Dataset tags are not
#'   exposed through the OAuth client id / secret API used by the other dataset
#'   functions.
#' @param instance Domo instance URL. For example:
#'   \code{"https://company.domo.com"}. A bare hostname is also accepted;
#'   \code{https://} is added automatically when no scheme is present.
#'
#' @return A character vector of tags; \code{character(0)} when the dataset has
#'   none.
#'
#' @examples
#' \dontrun{
#' dataset_tags_get("7fdc119c-df91-4906-9b85-72fb998c8a16",
#'                  Sys.getenv("DOMO_TOKEN"), "https://company.domo.com")
#' }
#'
#' @export

dataset_tags_get <- function(dataset_id, developer_token, instance) {
  id <- .dataset_check_id(dataset_id)
  .dataflow_check_token(developer_token)
  response <- .dataset_tags_perform(
    paste0(.normalize_domo_instance(instance), "/api/data/v3/datasources/", id),
    developer_token
  )
  .dataset_parse_tags(httr2::resp_body_json(response)$tags)
}

#' Add Tags to a Dataset
#'
#' Adds one or more tags to a dataset, keeping the tags it already has, through
#' the same endpoint the Domo UI uses
#' (\code{/api/data/ui/v3/datasources/\{id\}/tags}). Adding a tag the dataset
#' already has changes nothing, so it is safe to call twice.
#'
#' @inheritParams dataset_tags_get
#' @param tags A character vector of tags to add.
#'
#' @return The dataset's tags after the change (a character vector), invisibly.
#'   An error is raised if Domo accepts the request but a tag is still missing
#'   afterwards.
#'
#' @details
#' This is an undocumented UI endpoint rather than part of Domo's public API, so
#' it could change without notice. The result is read back for that reason.
#'
#' @examples
#' \dontrun{
#' dataset_tags_add("7fdc119c-df91-4906-9b85-72fb998c8a16", "Custom Connector",
#'                  Sys.getenv("DOMO_TOKEN"), "https://company.domo.com")
#' }
#'
#' @export

dataset_tags_add <- function(dataset_id, tags, developer_token, instance) {
  id <- .dataset_check_id(dataset_id)
  .dataflow_check_token(developer_token)
  if (!is.character(tags) || length(tags) == 0 || anyNA(tags) || any(!nzchar(trimws(tags)))) {
    stop("`tags` must be a character vector of non-empty strings.", call. = FALSE)
  }
  tags <- unique(trimws(tags))

  existing <- dataset_tags_get(id, developer_token, instance)
  if (all(tags %in% existing)) return(invisible(existing))

  .dataset_tags_perform(
    paste0(.normalize_domo_instance(instance), "/api/data/ui/v3/datasources/", id, "/tags"),
    developer_token,
    body = as.list(union(existing, tags))
  )

  after <- dataset_tags_get(id, developer_token, instance)
  missing <- setdiff(tags, after)
  if (length(missing) > 0) {
    stop("Domo accepted the request but ", length(missing),
         " tag(s) are still missing from the dataset.", call. = FALSE)
  }
  invisible(after)
}

# Internal helpers. Not exported (leading ".").

# A dataset id is a UUID.
.dataset_check_id <- function(x, arg = "dataset_id") {
  ok <- is.character(x) && length(x) == 1 && !is.na(x) &&
    grepl("^[0-9a-fA-F]{8}-([0-9a-fA-F]{4}-){3}[0-9a-fA-F]{12}$", x)
  if (!ok) stop("`", arg, "` must be a single dataset id (a UUID).", call. = FALSE)
  tolower(x)
}

# The datasource endpoint returns tags as a JSON string such as
# '["Custom Connector"]'; accept a list or a plain vector as well.
.dataset_parse_tags <- function(tags) {
  if (is.null(tags) || length(tags) == 0) return(character(0))
  if (is.list(tags)) return(as.character(unlist(tags)))
  tags <- as.character(tags)
  if (length(tags) == 1 && grepl("^\\s*\\[", tags)) {
    return(as.character(jsonlite::fromJSON(tags)))
  }
  tags[nzchar(tags)]
}

# One request: a GET, or a POST of a JSON array when `body` is given. Only Domo's
# own error message is attached to a failure, so the token cannot leak.
.dataset_tags_perform <- function(url, developer_token, body = NULL) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package \"httr2\" must be installed to use this function.", call. = FALSE)
  }
  request <- httr2::request(url)
  request <- httr2::req_headers(request, `X-DOMO-Developer-Token` = developer_token,
                                Accept = "application/json")
  if (!is.null(body)) request <- httr2::req_body_json(request, body, auto_unbox = TRUE)
  request <- httr2::req_retry(
    request,
    max_tries = 3,
    is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 503)
  )
  request <- .dataflow_req_error(request)
  httr2::req_perform(request)
}
