#' Convert Data Frame Row to AppDB Document
#'
#' Converts a single row from a data frame into the
#' AppDB document structure required by the Domo AppDB API.
#'
#' @param data A data frame containing the source data.
#' @param row_number The row number to convert.
#'
#' @details
#' If \code{data} has a list-column (e.g. holding a multi-select
#' field's values as a character vector per row), \code{as.list()} on
#' a single-row slice wraps that value in an extra layer of
#' \code{list()}. Left as-is, \code{httr2::req_body_json(auto_unbox =
#' TRUE)} would serialize it as a doubly-nested array (e.g.
#' \code{[["a","b"]]}) instead of the flat array the AppDB API expects
#' (\code{["a","b"]}). Each field is unwrapped one level before the
#' document is returned so list-column values serialize as a flat
#' array (or a bare scalar, via \code{auto_unbox}, when there's only
#' one value).
#'
#' @examples
#' new_data <- appdb_row_to_document(
#'   data = clients,
#'   row_number = 1
#' )
#'
#' @export

appdb_row_to_document <- function(
  data,
  row_number
) {

  if (row_number < 1 || row_number > nrow(data)) {
    stop(
      "row_number (", row_number, ") is out of range for `data` (",
      nrow(data), " row(s)).",
      call. = FALSE
    )
  }

  record <- as.list(data[row_number, , drop = FALSE])

  # Undo the extra list() layer that as.list() adds around list-column
  # values (see @details), so multi-value fields serialize as a flat
  # JSON array rather than a doubly-nested one.
  record <- lapply(record, function(v) {
    if (is.list(v) && length(v) == 1) v[[1]] else v
  })

  list(content = record)

}
