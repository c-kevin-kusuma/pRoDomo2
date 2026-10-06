id <- "7fdc119c-df91-4906-9b85-72fb998c8a16"

token_of <- function(req) {
  httr2::req_get_headers(req, redacted = "reveal")[["X-DOMO-Developer-Token"]]
}

test_that("tag functions validate inputs before any request", {
  expect_error(dataset_tags_get("not-a-uuid", "tok", "x.domo.com"), "dataset_id")
  expect_error(dataset_tags_get(c(id, id), "tok", "x.domo.com"), "dataset_id")
  expect_error(dataset_tags_get(id, "", "x.domo.com"), "developer_token")
  expect_error(dataset_tags_add(id, character(0), "tok", "x.domo.com"), "tags")
  expect_error(dataset_tags_add(id, c("a", NA), "tok", "x.domo.com"), "tags")
  expect_error(dataset_tags_add(id, " ", "tok", "x.domo.com"), "tags")
})

test_that("dataset_tags_get reads a JSON-string tag field", {
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    httr2::response_json(body = list(id = id, tags = "[\"Custom Connector\",\"Finance\"]"))
  })
  res <- dataset_tags_get(id, "secret-token", "x.domo.com")
  expect_identical(seen$url, paste0("https://x.domo.com/api/data/v3/datasources/", id))
  expect_identical(token_of(seen), "secret-token")
  expect_identical(res, c("Custom Connector", "Finance"))
})

test_that("dataset_tags_get returns character(0) when there are no tags", {
  httr2::local_mocked_responses(function(req) httr2::response_json(body = list(id = id)))
  expect_identical(dataset_tags_get(id, "tok", "x.domo.com"), character(0))
  httr2::local_mocked_responses(function(req) httr2::response_json(body = list(id = id, tags = "[]")))
  expect_identical(dataset_tags_get(id, "tok", "x.domo.com"), character(0))
})

test_that("dataset_tags_add posts the existing tags plus the new one", {
  calls <- list()
  stored <- "[\"Finance\"]"
  httr2::local_mocked_responses(function(req) {
    calls[[length(calls) + 1]] <<- req
    if (identical(httr2::req_get_method(req), "POST")) {
      stored <<- as.character(jsonlite::toJSON(unlist(req$body$data), auto_unbox = FALSE))
      return(httr2::response_json(body = list()))
    }
    httr2::response_json(body = list(id = id, tags = stored))
  })
  res <- dataset_tags_add(id, "Custom Connector", "tok", "x.domo.com")
  posts <- Filter(function(r) identical(httr2::req_get_method(r), "POST"), calls)

  expect_length(posts, 1)
  expect_identical(posts[[1]]$url, paste0("https://x.domo.com/api/data/ui/v3/datasources/", id, "/tags"))
  expect_identical(unlist(posts[[1]]$body$data), c("Finance", "Custom Connector"))
  expect_identical(res, c("Finance", "Custom Connector"))
})

test_that("adding a tag that is already there sends nothing", {
  posts <- 0
  httr2::local_mocked_responses(function(req) {
    if (identical(httr2::req_get_method(req), "POST")) posts <<- posts + 1
    httr2::response_json(body = list(id = id, tags = "[\"Custom Connector\"]"))
  })
  res <- dataset_tags_add(id, "Custom Connector", "tok", "x.domo.com")
  expect_identical(posts, 0)
  expect_identical(res, "Custom Connector")
})

test_that("an accepted request that did not stick is reported", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(id = id, tags = "[]"))
  })
  expect_error(dataset_tags_add(id, "Custom Connector", "tok", "x.domo.com"), "still missing")
})
