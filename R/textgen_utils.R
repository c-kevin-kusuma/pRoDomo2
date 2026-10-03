# Internal helpers for the textgen_* functions. Not exported (leading ".").

# Attach Domo's own message and error code to httr2 HTTP-error
# conditions. Only fields from the response are used -- never the request
# body -- so the prompt can't leak into an error message.
.textgen_req_error <- function(request) {
  httr2::req_error(
    request,
    body = function(resp) {
      parsed <- tryCatch(
        httr2::resp_body_json(resp),
        error = function(e) NULL
      )
      if (is.null(parsed)) return(NULL)
      detail <- if (!is.null(parsed$localizedMessage)) {
        parsed$localizedMessage
      } else {
        parsed$message
      }
      if (is.null(detail)) return(NULL)
      if (!is.null(parsed$errorCode)) {
        detail <- paste0(detail, " (", parsed$errorCode, ")")
      }
      detail
    }
  )
}
