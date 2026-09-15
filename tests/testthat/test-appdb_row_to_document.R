test_that("appdb_row_to_document keeps a multi-select list-column as a flat array", {
  # A data frame with a list-column, as produced e.g. by splitting a
  # "; "-joined multi-select field back into a character vector per
  # row. as.list() on a single-row slice wraps that vector in an extra
  # list() layer; left unhandled, jsonlite::toJSON(auto_unbox = TRUE)
  # would serialize it as a doubly-nested array (e.g. [["a","b"]])
  # instead of the flat array the AppDB API expects (["a","b"]).
  data <- tibble::tibble(
    project_name = "Project A",
    stakeholders = list(c("Alice", "Bob", "Carol"))
  )

  doc <- appdb_row_to_document(data, 1)

  expect_identical(doc$content$project_name, "Project A")
  expect_identical(doc$content$stakeholders, c("Alice", "Bob", "Carol"))
  expect_false(is.list(doc$content$stakeholders))

  json <- jsonlite::toJSON(doc$content, auto_unbox = TRUE)
  expect_identical(
    unclass(json),
    '{"project_name":"Project A","stakeholders":["Alice","Bob","Carol"]}'
  )
})

test_that("appdb_row_to_document passes ordinary scalar columns through unchanged", {
  data <- tibble::tibble(project_name = "Project A", status = "Active")

  doc <- appdb_row_to_document(data, 1)

  expect_identical(doc$content$project_name, "Project A")
  expect_identical(doc$content$status, "Active")
})

test_that("appdb_row_to_document rejects an out-of-range row_number", {
  data <- tibble::tibble(project_name = "Project A")

  expect_error(appdb_row_to_document(data, 2), "out of range")
  expect_error(appdb_row_to_document(data, 0), "out of range")
})
