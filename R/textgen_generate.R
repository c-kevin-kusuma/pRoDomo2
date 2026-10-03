#' Generate Text with Domo AI
#'
#' Sends a single prompt to Domo's AI Service Layer text-generation
#' endpoint (\code{/api/ai/v1/text/generation}) and returns the generated
#' text along with the model used and token usage.
#'
#' @param prompt A single, non-empty string to send to the model.
#' @param developer_token A valid Domo Developer Token.
#' @param instance Domo instance URL. For example:
#'   \code{"https://company.domo.com"}. A bare hostname is also accepted;
#'   \code{https://} is added automatically when no scheme is present.
#' @param model Optional model id. \code{NULL} (the default) uses the AI
#'   Service Layer's default model.
#' @param parameters Optional named list of model parameters, passed
#'   through as-is (for example \code{list(temperature = 0)}). Whether a
#'   given parameter changes the output is not guaranteed.
#'
#' @return A list with three elements: \code{text} (the generated text),
#'   \code{model} (the model id Domo reports using), and \code{usage}
#'   (a list with \code{inputTokens}, \code{outputTokens},
#'   \code{totalTokens}, \code{reasoningTokens}).
#'
#' @details
#' Everything in \code{prompt} is sent to Domo's hosted model. Mask or
#' aggregate client/matter/employee identifiers before calling this on
#' protected data.
#'
#' Each call is independent; there is no conversation state, so resend any
#' prior context in \code{prompt}. Generation is not deterministic, so
#' validate any names or figures in the output against the source data.
#'
#' Failures raise an error that includes the HTTP status and Domo's own
#' message and error code. The prompt itself is never included in the
#' error.
#'
#' @examples
#' \dontrun{
#' res <- textgen_generate(
#'   prompt = "Summarize in one sentence: 3 of 40 records were flagged.",
#'   developer_token = Sys.getenv("DOMO_TOKEN"),
#'   instance = "https://company.domo.com"
#' )
#' res$text
#' }
#'
#' @export

textgen_generate <- function(
  prompt,
  developer_token,
  instance,
  model = NULL,
  parameters = NULL
) {

  # Check Required Packages
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package \"httr2\" must be installed to use this function.",
         call. = FALSE)
  }

  if (!is.character(prompt) || length(prompt) != 1 || is.na(prompt) ||
      !nzchar(trimws(prompt))) {
    stop("`prompt` must be a single non-empty string.", call. = FALSE)
  }

  if (!is.character(developer_token) || length(developer_token) != 1 ||
      is.na(developer_token) || !nzchar(developer_token)) {
    stop("`developer_token` must be a single non-empty string.",
         call. = FALSE)
  }

  body <- list(input = prompt)
  if (!is.null(model)) body$model <- model
  if (!is.null(parameters)) body$parameters <- parameters

  url <- paste0(
    .normalize_domo_instance(instance),
    "/api/ai/v1/text/generation"
  )

  request <- httr2::request(url)
  request <- httr2::req_headers(
    request,
    `X-DOMO-Developer-Token` = developer_token,
    Accept = "application/json"
  )
  request <- httr2::req_body_json(request, body)
  # Retry only transient failures (429/503); a 4xx like a bad token or
  # model should fail immediately.
  request <- httr2::req_retry(
    request,
    max_tries = 3,
    is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 503)
  )
  request <- .textgen_req_error(request)

  response <- httr2::req_perform(request)

  # A malformed or unrecognized token can come back as HTTP 200 with an
  # empty / non-JSON body (observed live) rather than a 401.
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

  list(
    text = if (is.null(parsed$output)) NA_character_ else parsed$output,
    model = if (is.null(parsed$modelId)) NA_character_ else parsed$modelId,
    usage = parsed$modelProviderUsage
  )
}
