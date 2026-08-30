#' Convert Data Frame Row to AppDB Document
#'
#' Converts a single row from a data frame into the
#' AppDB document structure required by the Domo AppDB API.
#'
#' @param data A data frame containing the source data.
#' @param row_number The row number to convert.
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

  list(
    content = as.list(
      data[row_number, , drop = FALSE]
    )
  )

}
