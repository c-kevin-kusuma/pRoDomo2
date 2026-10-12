#' Run a Code Engine Function
#'
#' Runs one function of a Code Engine package version by posting to
#' \code{/api/codeengine/v2/packages/\{package_id\}/versions/\{version\}/functions/\{function_name\}}
#' and waiting for the result. The function runs as the owner of the Developer
#' Token.
#'
#' @param package_id The Code Engine package id (a UUID string).
#' @param version The package version to run, for example \code{"1.0.4"}. A
#'   working (unreleased) version can be run too, but apps and workflows only
#'   see released versions.
#' @param function_name The name of the function to run.
#' @param input_variables A named list of the function's inputs, for example
#'   \code{list(name = "value", flag = "yes")}. The names must match the
#'   input names declared on the function. Defaults to no inputs.
#' @param developer_token A Domo Developer Token (\code{DOMO_TOKEN}).
#' @param instance Domo instance, with or without the \code{https://} prefix.
#' @param timeout Seconds to wait for the response. Default 300.
#' @param check If \code{TRUE} (the default) an execution that does not end in
#'   \code{"SUCCESS"} raises an error with Domo's message. Set it to
#'   \code{FALSE} to get the failed execution back instead.
#'
#' @return A list with \code{execution_id}, \code{status} (for example
#'   \code{"SUCCESS"}), \code{result} (whatever the function returned, as a
#'   list) and \code{error} (Domo's error message, or \code{NA}).
#'
#' @details
#' This runs real code with side effects, whatever the function does. The
#' \code{result} is returned as-is; if the function can return client or
#' employee data, do not print it. Declared input names are case sensitive and
#' an unknown input name is silently ignored by Domo.
#'
#' @examples
#' \dontrun{
#' run <- codeengine_run(
#'   package_id = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
#'   version = "1.0.0",
#'   function_name = "myFunction",
#'   input_variables = list(name = "value", flag = "yes"),
#'   developer_token = Sys.getenv("DOMO_TOKEN"),
#'   instance = "company.domo.com"
#' )
#' run$status
#' run$result
#' }
#'
#' @export

codeengine_run <- function(package_id, version, function_name,
                           input_variables = list(), developer_token,
                           instance, timeout = 300, check = TRUE) {
  .codeengine_check_string(package_id, "package_id")
  .codeengine_check_string(version, "version")
  .codeengine_check_string(function_name, "function_name")
  .codeengine_check_string(developer_token, "developer_token")
  if (!is.list(input_variables) ||
      (length(input_variables) > 0 &&
       (is.null(names(input_variables)) || any(!nzchar(names(input_variables)))))) {
    stop("`input_variables` must be a named list.", call. = FALSE)
  }
  if (!is.numeric(timeout) || length(timeout) != 1 || is.na(timeout) || timeout <= 0) {
    stop("`timeout` must be a single positive number of seconds.", call. = FALSE)
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package \"httr2\" must be installed to use this function.", call. = FALSE)
  }

  url <- paste0(
    .normalize_domo_instance(instance),
    "/api/codeengine/v2/packages/", utils::URLencode(package_id, reserved = TRUE),
    "/versions/", utils::URLencode(version, reserved = TRUE),
    "/functions/", utils::URLencode(function_name, reserved = TRUE)
  )

  request <- httr2::request(url)
  request <- httr2::req_headers(
    request,
    `X-DOMO-Developer-Token` = developer_token,
    Accept = "application/json"
  )
  inputs <- if (length(input_variables)) input_variables else structure(list(), names = character(0))
  request <- httr2::req_body_json(request, list(inputVariables = inputs), auto_unbox = TRUE)
  request <- httr2::req_timeout(request, timeout)
  # Not retried: a retry could run the function twice. Only response fields
  # go into the error message, never the request, so the token can't leak.
  request <- httr2::req_error(
    request,
    body = function(resp) {
      parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
      if (is.null(parsed)) return(NULL)
      detail <- if (!is.null(parsed$message)) parsed$message else parsed$error
      if (is.null(detail)) return(NULL)
      as.character(detail)[1]
    }
  )

  response <- httr2::req_perform(request)

  # A malformed token can come back as HTTP 200 with an empty / non-JSON body.
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
  parsed <- httr2::resp_body_json(response)

  status <- if (is.null(parsed$status)) NA_character_ else as.character(parsed$status)
  out <- list(
    execution_id = if (is.null(parsed$executionId)) NA_character_ else as.character(parsed$executionId),
    status = status,
    result = parsed$result,
    error = .codeengine_error_text(parsed$errorInformation)
  )
  if (isTRUE(check) && !identical(status, "SUCCESS")) {
    stop(
      "Code Engine execution ended with status ", status,
      if (!is.na(out$error)) paste0(": ", out$error) else "", ".",
      call. = FALSE
    )
  }
  out
}
