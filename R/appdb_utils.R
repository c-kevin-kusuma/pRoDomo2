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

# Normalize a single AppDB document's `content` list to one scalar per
# field before it's handed to dplyr::bind_rows().
#
# AppDB multi-select fields (e.g. stakeholders, requester,
# department_supported, vendor_supported) come back from the API as a
# JSON array on documents where a value was picked, but as a single
# scalar or as an absent/NULL field on documents where it wasn't. Left
# as-is, purrr::map() + dplyr::bind_rows() chokes on the resulting
# per-record mix of vector lengths/types for the same column (e.g.
# "Can't recycle `stakeholders` (size 4) to match `vendor_supported`
# (size 0)" or "Can't combine ..1$stakeholders <list> and
# ..4$stakeholders <character>"). Collapsing every field to a single
# scalar (NA when empty, "; "-joined when multi-valued) makes every
# record the same shape so bind_rows() can combine them regardless of
# which fields happen to be array-valued in a given collection.
.appdb_normalize_record <- function(record) {
  purrr::map(record, function(v) {
    if (length(v) == 0) {
      NA_character_
    } else if (length(v) > 1) {
      paste(unlist(v), collapse = "; ")
    } else {
      v
    }
  })
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
