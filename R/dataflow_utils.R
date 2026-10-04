# Internal helpers shared by the dataflow_* functions. Not exported (leading
# ".").
#
# All dataflow functions talk to /api/dataprocessing/v1/dataflows, which
# accepts a Developer Token (X-DOMO-Developer-Token) but refuses the OAuth
# client_id/secret pair that the dataset functions use.

# A dataflow id is a whole number, passed as a number or a numeric string.
.dataflow_check_id <- function(x, arg = "dataflow_id") {
  ok <- length(x) == 1 && !is.na(x) && !is.list(x) &&
    grepl("^[0-9]+$", format(x, scientific = FALSE, trim = TRUE))
  if (!ok) {
    stop("`", arg, "` must be a single whole number.", call. = FALSE)
  }
  format(x, scientific = FALSE, trim = TRUE)
}

.dataflow_check_token <- function(developer_token) {
  if (!is.character(developer_token) || length(developer_token) != 1 ||
      is.na(developer_token) || !nzchar(developer_token)) {
    stop("`developer_token` must be a single non-empty string.",
         call. = FALSE)
  }
}

.dataflow_url <- function(instance, dataflow_id, path = "") {
  paste0(
    .normalize_domo_instance(instance),
    "/api/dataprocessing/v1/dataflows/", dataflow_id, path
  )
}

# Attach Domo's own message to httr2 HTTP-error conditions. Only response
# fields are used, never the request, so the token can't leak into an error.
.dataflow_req_error <- function(request) {
  httr2::req_error(
    request,
    body = function(resp) {
      parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
      if (is.null(parsed)) return(NULL)
      detail <- if (!is.null(parsed$message)) parsed$message else parsed$error
      if (is.null(detail)) return(NULL)
      as.character(detail)[1]
    }
  )
}

# Build, perform and parse one JSON request. `method` is "GET" or "POST";
# POST sends an empty JSON object, which is what starts a run.
.dataflow_call <- function(url, developer_token, method = "GET") {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package \"httr2\" must be installed to use this function.",
         call. = FALSE)
  }
  request <- httr2::request(url)
  request <- httr2::req_headers(
    request,
    `X-DOMO-Developer-Token` = developer_token,
    Accept = "application/json"
  )
  if (identical(method, "POST")) {
    request <- httr2::req_body_json(request, structure(list(), names = character(0)))
  }
  # Retry only transient failures (429/503); a 4xx like a bad token or an
  # unknown dataflow should fail immediately.
  request <- httr2::req_retry(
    request,
    max_tries = 3,
    is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 503)
  )
  request <- .dataflow_req_error(request)

  response <- httr2::req_perform(request)

  # A malformed token can come back as HTTP 200 with an empty / non-JSON
  # body rather than a 401.
  if (!isTRUE(grepl("json", httr2::resp_content_type(response))) ||
      !httr2::resp_has_body(response)) {
    stop(
      "Domo returned an unexpected non-JSON response (HTTP ",
      httr2::resp_status(response),
      "). Check that `developer_token` is a valid Developer Token and ",
      "`instance` is correct.",
      call. = FALSE
    )
  }
  httr2::resp_body_json(response)
}

# Domo reports times as epoch milliseconds.
.dataflow_time <- function(ms) {
  if (is.null(ms)) return(as.POSIXct(NA, tz = "UTC"))
  as.POSIXct(as.numeric(ms) / 1000, origin = "1970-01-01", tz = "UTC")
}

.dataflow_num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
.dataflow_chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)

# One execution (as returned by the API) as a one-row tibble.
.dataflow_execution_row <- function(e) {
  tibble::tibble(
    execution_id = .dataflow_chr(e$id),
    state = .dataflow_chr(e$state),
    failed = isTRUE(e$failed),
    activation_type = .dataflow_chr(e$activationType),
    begin_time = .dataflow_time(e$beginTime),
    end_time = .dataflow_time(e$endTime),
    rows_read = .dataflow_num(e$totalRowsRead),
    rows_written = .dataflow_num(e$totalRowsWritten)
  )
}

# States after which an execution will not change again.
.dataflow_final_states <- c("SUCCESS", "FAILED", "KILLED", "CANCELLED",
                            "CANCELED", "ABORTED")
