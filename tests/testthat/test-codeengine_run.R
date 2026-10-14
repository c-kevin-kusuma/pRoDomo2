token_of <- function(req) {
  httr2::req_get_headers(req, redacted = "reveal")[["X-DOMO-Developer-Token"]]
}

run_it <- function(...) {
  codeengine_run("pkg-1", "1.0.4", "doIt", list(a = "x"), "secret-token", "x.domo.com", ...)
}

test_that("codeengine_run validates inputs before any request", {
  expect_error(codeengine_run("", "1", "f", list(), "tok", "x.domo.com"), "package_id")
  expect_error(codeengine_run("p", NA_character_, "f", list(), "tok", "x.domo.com"), "version")
  expect_error(codeengine_run("p", "1", c("a", "b"), list(), "tok", "x.domo.com"), "function_name")
  expect_error(codeengine_run("p", "1", "f", list(), "", "x.domo.com"), "developer_token")
  expect_error(codeengine_run("p", "1", "f", list(1), "tok", "x.domo.com"), "named list")
  expect_error(codeengine_run("p", "1", "f", list(), "tok", "x.domo.com", timeout = 0), "timeout")
})

test_that("codeengine_run builds the request and returns the result", {
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    httr2::response_json(body = list(
      executionId = "e-1", status = "SUCCESS",
      result = list(success = TRUE, status = "ok", insightCount = 6),
      errorInformation = NULL
    ))
  })
  res <- run_it()
  expect_identical(seen$url, "https://x.domo.com/api/codeengine/v2/packages/pkg-1/versions/1.0.4/functions/doIt")
  expect_identical(token_of(seen), "secret-token")
  expect_identical(seen$body$data$inputVariables$a, "x")
  expect_identical(res$status, "SUCCESS")
  expect_identical(res$execution_id, "e-1")
  expect_identical(res$result$insightCount, 6L)
  expect_true(is.na(res$error))
})

test_that("no inputs are sent as an empty object", {
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    httr2::response_json(body = list(executionId = "e", status = "SUCCESS", result = list()))
  })
  codeengine_run("p", "1", "f", developer_token = "tok", instance = "x.domo.com")
  expect_identical(jsonlite::toJSON(seen$body$data, auto_unbox = TRUE)[[1]], '{"inputVariables":{}}')
})

test_that("a failed execution raises by default and is returned with check = FALSE", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(
      executionId = "e-2", status = "FAILED", result = NULL,
      errorInformation = list(message = "boom")
    ))
  })
  expect_error(run_it(), "FAILED: boom")
  res <- run_it(check = FALSE)
  expect_identical(res$status, "FAILED")
  expect_identical(res$error, "boom")
})

test_that("HTTP errors carry Domo's message and never the token", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(status_code = 404, body = list(message = "No such package"))
  })
  err <- tryCatch(run_it(), error = function(e) conditionMessage(e))
  expect_match(err, "404")
  expect_match(err, "No such package")
  expect_false(grepl("secret-token", err, fixed = TRUE))
})

test_that("a non-JSON 200 response gives a clear error", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200, headers = list(`Content-Type` = "text/html"), body = charToRaw("x"))
  })
  expect_error(run_it(), "non-JSON")
})

test_that(".resp_has_body works without httr2::resp_has_body", {
  expect_true(.resp_has_body(httr2::response_json(body = list(a = 1))))
  expect_false(.resp_has_body(httr2::response(status_code = 200, body = raw(0))))
})
