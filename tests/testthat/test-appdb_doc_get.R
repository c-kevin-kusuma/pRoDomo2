test_that(".appdb_normalize_record collapses a field to one scalar per shape", {
  # Empty/missing -> NA
  expect_identical(
    pRoDomo2:::.appdb_normalize_record(list(vendor_supported = NULL))$vendor_supported,
    NA_character_
  )
  expect_identical(
    pRoDomo2:::.appdb_normalize_record(list(vendor_supported = character(0)))$vendor_supported,
    NA_character_
  )

  # Multi-value (JSON array) -> "; "-joined string
  expect_identical(
    pRoDomo2:::.appdb_normalize_record(
      list(stakeholders = list("Alice", "Bob", "Carol"))
    )$stakeholders,
    "Alice; Bob; Carol"
  )

  # Single scalar -> passed through unchanged
  expect_identical(
    pRoDomo2:::.appdb_normalize_record(list(department_supported = "Finance"))$department_supported,
    "Finance"
  )
})

test_that("appdb_doc_get's flatten step combines mixed multi-select shapes without erroring", {
  # Reproduces a domoPMProjects-style collection where a multi-select
  # field (stakeholders) is a JSON array on some documents, a single
  # scalar on others, and absent/empty on others. Before normalization,
  # dplyr::bind_rows() on the raw `content` lists fails with errors like
  # "Can't recycle `stakeholders` (size 4) to match `vendor_supported`
  # (size 0)" or "Can't combine ..1$stakeholders <list> and
  # ..4$stakeholders <character>".
  all_documents <- list(
    list(
      id = "doc-1",
      content = list(
        project_name = "Project A",
        stakeholders = list("Alice", "Bob", "Carol", "Dave"),
        vendor_supported = character(0)
      )
    ),
    list(
      id = "doc-2",
      content = list(
        project_name = "Project B",
        stakeholders = "Alice",
        vendor_supported = "Acme Corp"
      )
    ),
    list(
      id = "doc-3",
      content = list(
        project_name = "Project C",
        vendor_supported = list("Acme Corp", "Globex")
      )
    )
  )

  data <- dplyr::bind_rows(
    purrr::map(
      all_documents,
      function(x) {
        record <- pRoDomo2:::.appdb_normalize_record(x$content)
        record$appdb_document_id <- x$id
        record
      }
    )
  )

  expect_equal(nrow(data), 3)
  expect_identical(data$stakeholders, c("Alice; Bob; Carol; Dave", "Alice", NA_character_))
  expect_identical(data$vendor_supported, c(NA_character_, "Acme Corp", "Acme Corp; Globex"))
})
