# Internal helpers for the codeengine_* functions. Not exported (leading ".").

.codeengine_check_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(x))) {
    stop("`", arg, "` must be a single non-empty string.", call. = FALSE)
  }
}

# Domo's `errorInformation` is null on success; on failure it is an object
# that usually carries a message. Return one string, or NA.
.codeengine_error_text <- function(info) {
  if (is.null(info) || length(info) == 0) return(NA_character_)
  if (is.character(info)) return(info[1])
  msg <- info$message
  if (is.null(msg)) msg <- info$error
  if (is.null(msg)) msg <- info$localizedMessage
  if (is.null(msg)) return("(no error message returned)")
  as.character(msg)[1]
}
