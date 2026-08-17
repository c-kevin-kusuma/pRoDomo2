#' Send an email via Domo CodeEngine
#'
#' Triggers a Domo CodeEngine function to send a single HTML email to a Domo
#' user. The email is sent from Domo's notification email context, not directly
#' from your R session. Returns a named list — never throws — so it is safe to
#' use in a loop.
#'
#' @param user_id Numeric Domo user ID of the recipient (e.g. \code{"123456789"}).
#' @param subject Email subject line.
#' @param html_body Full HTML string for the email body.
#' @param group_id Optional Domo group ID to include as a recipient.
#' @param csv_file Optional data frame, CSV text, or CSV file path to upload as
#'   a FILE input.
#' @param csv_file_name Optional filename to use for the uploaded CSV file.
#' @param domo_instance Your Domo instance name (e.g. \code{"your-instance"} for
#'   \code{your-instance.domo.com}).
#' @param package_id CodeEngine package ID (UUID string).
#' @param package_version Package version string (e.g. \code{"1.0.0"}).
#' @param function_name Name of the CodeEngine function (e.g. \code{"sendCustomEmail"}).
#' @param token Domo bearer or developer token.
#' @param auth_mode One of \code{"auto"} (default), \code{"bearer"}, or
#'   \code{"developer"}. \code{"auto"} tries bearer first, falls back to
#'   developer on a 401.
#' @return A named list with \code{sent} (logical), \code{user_id},
#'   \code{http_status_code}, \code{auth_mode}, and \code{error_message}.
#' @examples
#' \dontrun{
#' result <- codeEngine_send_email(
#'   user_id         = "123456789",
#'   subject         = "Subject of the Email",
#'   html_body       = "<p>Hello, your file is ready.</p>",
#'   domo_instance   = "your-instance",
#'   package_id      = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
#'   package_version = "1.0.0",
#'   function_name   = "sendCustomEmail",
#'   token           = domo_access_token,
#'   group_id        = "987654321",
#'   csv_file        = mtcars,
#'   csv_file_name   = "your_file.csv"
#' )
#' if (!result$sent) message("Failed: ", result$error_message)
#' }
#' @export

codeEngine_send_email <- function(
  user_id,
  subject,
  html_body,
  domo_instance,
  package_id,
  package_version,
  function_name,
  token,
  group_id = NULL,
  csv_file = NULL,
  csv_file_name = "attachment.csv",
  auth_mode = c("auto", "bearer", "developer")
) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package \"httr2\" must be installed to use this function.", call. = FALSE)
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Package \"curl\" must be installed to use this function.", call. = FALSE)
  }

  auth_mode <- match.arg(auth_mode)
  user_id   <- trimws(as.character(user_id))
  group_id  <- if (is.null(group_id)) "" else trimws(as.character(group_id))
  csv_file_name <- basename(trimws(as.character(csv_file_name)))
  if (!nzchar(csv_file_name)) csv_file_name <- "attachment.csv"

  tryCatch({
    # --- input validation ---
    if (!nzchar(user_id))                   stop("Missing user_id.")
    if (!grepl("^[0-9]+$", user_id))        stop("user_id must be numeric.")
    if (!nzchar(as.character(subject)))     stop("Missing subject.")
    if (!nzchar(as.character(html_body)))   stop("Missing html_body.")
    if (nzchar(group_id) && !grepl("^[0-9]+$", group_id)) {
      stop("group_id must be numeric.")
    }
    # --- normalise token ---
    tok <- trimws(as.character(token))
    tok <- gsub("^['\"]|['\"]$", "", tok)
    tok <- trimws(sub("^Bearer\\s+", "", tok, ignore.case = TRUE))
    if (!nzchar(tok)) stop("Missing token.")

    # --- build URL ---
    url <- paste0(
      "https://", trimws(domo_instance), ".domo.com/api/codeengine/v2/packages/",
      trimws(package_id), "/versions/", trimws(package_version),
      "/functions/", trimws(function_name)
    )

    # --- build request body ---
    temp_dir <- NULL
    csv_path <- NULL
    if (!is.null(csv_file)) {
      temp_dir <- tempfile("codeEngine_send_email_")
      dir.create(temp_dir)
      csv_path <- file.path(temp_dir, csv_file_name)
      on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

      if (is.data.frame(csv_file)) {
        utils::write.csv(csv_file, file = csv_path, row.names = FALSE, na = "")
      } else if (is.character(csv_file) && length(csv_file) == 1) {
        if (file.exists(csv_file)) {
          if (!file.copy(csv_file, csv_path, overwrite = TRUE)) {
            stop("Unable to copy csv_file to a temporary upload file.")
          }
        } else {
          writeLines(csv_file, csv_path, useBytes = TRUE)
        }
      } else {
        stop("csv_file must be a data frame, CSV text, or CSV file path.")
      }
    }

    if (!is.null(csv_path)) {
      parts <- list(
        userId = user_id,
        subject = as.character(subject),
        htmlBody = as.character(html_body),
        csvFile = curl::form_file(csv_path, type = "text/csv")
      )
      if (nzchar(group_id)) parts$groupId <- group_id

      req <- do.call(httr2::req_body_multipart, c(list(httr2::request(url)), parts)) |>
        httr2::req_error(is_error = \(r) FALSE)
    } else {
      input_variables <- list(
        userId   = user_id,
        subject  = as.character(subject),
        htmlBody = as.character(html_body)
      )
      if (nzchar(group_id)) input_variables$groupId <- group_id

      req <- httr2::request(url) |>
        httr2::req_body_json(list(inputVariables = input_variables)) |>
        httr2::req_error(is_error = \(r) FALSE)
    }

    # --- attempt auth, auto falls back on 401 ---
    try_auth <- function(mode) {
      if (mode == "bearer") {
        httr2::req_perform(httr2::req_auth_bearer_token(req, tok))
      } else {
        httr2::req_perform(httr2::req_headers(req, "x-domo-developer-token" = tok))
      }
    }

    if (auth_mode == "auto") {
      resp <- try_auth("bearer")
      if (httr2::resp_status(resp) == 401) {
        resp      <- try_auth("developer")
        auth_mode <- "developer"
      } else {
        auth_mode <- "bearer"
      }
    } else {
      resp <- try_auth(auth_mode)
    }

    # --- check for HTTP errors ---
    status <- httr2::resp_status(resp)
    if (status >= 400) {
      body <- tryCatch(httr2::resp_body_string(resp), error = \(e) "")
      stop("HTTP ", status, " ", httr2::resp_status_desc(resp), ": ",
           if (nzchar(trimws(body))) trimws(body) else "(empty body)")
    }

    list(sent = TRUE, user_id = user_id, http_status_code = status,
         auth_mode = auth_mode, error_message = "")

  }, error = function(e) {
    msg    <- conditionMessage(e)
    status <- { m <- regmatches(msg, regexec("HTTP ([0-9]{3})", msg))[[1]]; if (length(m) >= 2) as.integer(m[2]) else NA_integer_ }
    mode   <- { m <- regmatches(msg, regexec("Auth mode: ([a-z]+)", msg))[[1]]; if (length(m) >= 2) m[2] else auth_mode }
    list(sent = FALSE, user_id = user_id, http_status_code = status,
         auth_mode = mode, error_message = msg)
  })
}
