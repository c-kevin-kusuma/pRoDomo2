mock_ok <- function() {
  httr2::response_json(body = list(
    output = "Hello there, how are you?",
    modelId = "domo.domo_ai.domogpt-medium-v2.2:anthropic",
    modelProviderUsage = list(
      inputTokens = 14, outputTokens = 10, totalTokens = 24,
      reasoningTokens = NULL
    )
  ))
}

test_that("textgen_generate validates inputs before any request", {
  expect_error(textgen_generate("", "tok", "x.domo.com"), "non-empty")
  expect_error(textgen_generate("   ", "tok", "x.domo.com"), "non-empty")
  expect_error(textgen_generate(c("a", "b"), "tok", "x.domo.com"), "single")
  expect_error(textgen_generate(NA_character_, "tok", "x.domo.com"), "single")
  expect_error(textgen_generate("hi", "", "x.domo.com"), "developer_token")
})

test_that("textgen_generate returns text, model and usage and builds the request", {
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    mock_ok()
  })

  res <- textgen_generate(
    "Say hello", "secret-token", "x.domo.com",
    model = "m1", parameters = list(temperature = 0)
  )

  expect_identical(res$text, "Hello there, how are you?")
  expect_identical(res$model, "domo.domo_ai.domogpt-medium-v2.2:anthropic")
  expect_equal(res$usage$totalTokens, 24)

  expect_identical(seen$url, "https://x.domo.com/api/ai/v1/text/generation")
  expect_identical(
    httr2::req_get_headers(seen, redacted = "reveal")[["X-DOMO-Developer-Token"]],
    "secret-token"
  )
  expect_identical(seen$body$data$input, "Say hello")
  expect_identical(seen$body$data$model, "m1")
  expect_identical(seen$body$data$parameters$temperature, 0)
})

test_that("textgen_generate omits model/parameters when not supplied", {
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    mock_ok()
  })
  textgen_generate("hi", "tok", "x.domo.com")
  expect_named(seen$body$data, "input")
})

test_that("textgen_generate errors carry Domo's message and never the prompt", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(
      status_code = 401,
      body = list(
        status = 401,
        message = "Full authentication is required to access this resource"
      )
    )
  })
  err <- tryCatch(
    textgen_generate("CONFIDENTIAL-PROMPT", "bad", "x.domo.com"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "401")
  expect_match(err, "Full authentication is required")
  expect_false(grepl("CONFIDENTIAL-PROMPT", err, fixed = TRUE))
})

test_that("textgen_generate includes Domo's error code when present", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(
      status_code = 404,
      body = list(message = "Not Found", errorCode = "DS-0043",
                  localizedMessage = "ML Model does not exist")
    )
  })
  err <- tryCatch(
    textgen_generate("hi", "tok", "x.domo.com", model = "nope"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "ML Model does not exist \\(DS-0043\\)")
})

test_that("textgen_generate gives a clear error on an empty non-JSON 200", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200)
  })
  expect_error(
    textgen_generate("hi", "bad", "x.domo.com"),
    "non-JSON response"
  )
})
