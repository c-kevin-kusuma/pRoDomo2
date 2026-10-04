exec_json <- function(id = 101, state = "SUCCESS", ...) {
  c(list(
    id = id, state = state, failed = identical(state, "FAILED"),
    activationType = "MANUAL",
    beginTime = 1791142000000, endTime = 1791142060000,
    totalRowsRead = 500, totalRowsWritten = 480
  ), list(...))
}

token_of <- function(req) {
  httr2::req_get_headers(req, redacted = "reveal")[["X-DOMO-Developer-Token"]]
}

test_that("dataflow functions validate inputs before any request", {
  expect_error(dataflow_get("abc", "tok", "x.domo.com"), "dataflow_id")
  expect_error(dataflow_get(c(1, 2), "tok", "x.domo.com"), "dataflow_id")
  expect_error(dataflow_get(NA, "tok", "x.domo.com"), "dataflow_id")
  expect_error(dataflow_get(449, "", "x.domo.com"), "developer_token")
  expect_error(dataflow_run(449, NA_character_, "x.domo.com"), "developer_token")
  expect_error(dataflow_executions(449, "tok", "x.domo.com", limit = 0), "limit")
  expect_error(dataflow_executions(449, "tok", "x.domo.com", limit = 1.5), "limit")
  expect_error(dataflow_wait(449, "x", "tok", "x.domo.com"), "execution_id")
  expect_error(dataflow_wait(449, 1, "tok", "x.domo.com", timeout = -1), "timeout")
})

test_that("dataflow_get builds the request and returns the definition", {
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    httr2::response_json(body = list(id = 449, name = "Flow", actions = list(1, 2)))
  })
  res <- dataflow_get(449, "secret-token", "x.domo.com")
  expect_identical(seen$url, "https://x.domo.com/api/dataprocessing/v1/dataflows/449")
  expect_identical(token_of(seen), "secret-token")
  expect_length(res$actions, 2)
})

test_that("a numeric id above 5 digits is not turned into scientific notation", {
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    httr2::response_json(body = list())
  })
  dataflow_get(1234567, "tok", "x.domo.com")
  expect_match(seen$url, "dataflows/1234567$")
})

test_that("dataflow_executions returns a tidy tibble, newest first as given", {
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    httr2::response_json(body = list(
      exec_json(id = 102, state = "FAILED"),
      exec_json(id = 101, state = "SUCCESS")
    ))
  })
  res <- dataflow_executions(449, "tok", "x.domo.com", limit = 2)

  expect_match(seen$url, "dataflows/449/executions\\?limit=2$")
  expect_s3_class(res, "tbl_df")
  expect_identical(res$execution_id, c("102", "101"))
  expect_identical(res$state, c("FAILED", "SUCCESS"))
  expect_identical(res$failed, c(TRUE, FALSE))
  expect_equal(res$rows_written, c(480, 480))
  expect_s3_class(res$begin_time, "POSIXct")
  expect_identical(
    format(res$begin_time[1], tz = "UTC", usetz = FALSE),
    format(as.POSIXct(1791142000, origin = "1970-01-01", tz = "UTC"), tz = "UTC")
  )
})

test_that("dataflow_executions returns zero rows for a flow that never ran", {
  httr2::local_mocked_responses(function(req) httr2::response_json(body = list()))
  res <- dataflow_executions(449, "tok", "x.domo.com")
  expect_equal(nrow(res), 0)
  expect_true(all(c("execution_id", "state", "begin_time") %in% names(res)))
})

test_that("a running execution has NA end_time", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(list(
      id = 5, state = "RUNNING_DATA_FLOW", failed = FALSE, beginTime = 1791142000000
    )))
  })
  res <- dataflow_executions(449, "tok", "x.domo.com")
  expect_true(is.na(res$end_time))
  expect_true(is.na(res$rows_read))
})

test_that("dataflow_run posts to the executions endpoint and returns the id", {
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    httr2::response_json(body = list(id = 25565325, state = "CREATED"))
  })
  res <- dataflow_run(997, "secret-token", "x.domo.com")

  expect_identical(seen$url, "https://x.domo.com/api/dataprocessing/v1/dataflows/997/executions")
  expect_identical(httr2::req_get_method(seen), "POST")
  expect_identical(token_of(seen), "secret-token")
  expect_identical(res$execution_id, "25565325")
  expect_identical(res$state, "CREATED")
})

test_that("dataflow_run errors if Domo returns no execution id", {
  httr2::local_mocked_responses(function(req) httr2::response_json(body = list(state = "CREATED")))
  expect_error(dataflow_run(997, "tok", "x.domo.com"), "execution id")
})

test_that("dataflow_wait polls until a final state and returns the execution", {
  states <- c("CREATED", "RUNNING_DATA_FLOW", "RUNNING_DATA_FLOW", "SUCCESS")
  n <- 0
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    n <<- n + 1
    urls <<- c(urls, req$url)
    httr2::response_json(body = exec_json(id = 7, state = states[n]))
  })
  res <- dataflow_wait(997, 7, "tok", "x.domo.com", poll_interval = 0)

  expect_equal(n, 4)
  expect_true(all(urls == "https://x.domo.com/api/dataprocessing/v1/dataflows/997/executions/7"))
  expect_identical(res$state, "SUCCESS")
  expect_equal(nrow(res), 1)
})

test_that("dataflow_wait returns a failed run instead of raising", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = exec_json(id = 7, state = "FAILED"))
  })
  res <- dataflow_wait(997, 7, "tok", "x.domo.com", poll_interval = 0)
  expect_identical(res$state, "FAILED")
  expect_true(res$failed)
})

test_that("dataflow_wait times out with the last state in the message", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = exec_json(id = 7, state = "RUNNING_DATA_FLOW"))
  })
  expect_error(
    dataflow_wait(997, 7, "tok", "x.domo.com", timeout = 0, poll_interval = 0),
    "still RUNNING_DATA_FLOW"
  )
})

test_that("dataflow_wait reports state changes unless quiet", {
  states <- c("CREATED", "CREATED", "SUCCESS")
  n <- 0
  httr2::local_mocked_responses(function(req) {
    n <<- n + 1
    httr2::response_json(body = exec_json(id = 7, state = states[n]))
  })
  msgs <- testthat::capture_messages(
    dataflow_wait(997, 7, "tok", "x.domo.com", poll_interval = 0, quiet = FALSE)
  )
  expect_length(msgs, 2)  # CREATED once, then SUCCESS
})

test_that("HTTP errors carry Domo's message and never the token", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(
      status_code = 404,
      body = list(message = "Dataflow not found")
    )
  })
  err <- tryCatch(dataflow_get(1, "super-secret-token", "x.domo.com"), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "Dataflow not found")
  expect_false(grepl("super-secret-token", conditionMessage(err), fixed = TRUE))
})

test_that("a 200 with a non-JSON body (malformed token) gets a clear error", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200, headers = list(`Content-Type` = "text/html"), body = raw(0))
  })
  expect_error(dataflow_get(449, "bad", "x.domo.com"), "non-JSON")
})
