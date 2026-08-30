# Internal helpers shared by the appdb_* functions.
#
# Not exported: NAMESPACE uses exportPattern("^[[:alpha:]]+"), which only
# matches names starting with a letter, so the leading "." on these names
# keeps them internal to the package.

.normalize_domo_instance <- function(instance) {
  instance <- sub("/+$", "", instance)
  if (!grepl("^https?://", instance, ignore.case = TRUE)) {
    instance <- paste0("https://", instance)
  }
  instance
}

# Attach Domo's own error message (when present) to httr2 HTTP-error
# conditions, so callers see e.g. "Collection not found" instead of a
# generic "HTTP 404" message.
.appdb_req_error <- function(request) {
  httr2::req_error(
    request,
    body = function(resp) {
      parsed <- tryCatch(
        httr2::resp_body_json(resp),
        error = function(e) NULL
      )
      if (!is.null(parsed) && !is.null(parsed$message)) {
        return(parsed$message)
      }
      NULL
    }
  )
}
