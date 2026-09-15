#' Convert Data Frame Rows to AppDB Documents
#'
#' Converts every row of a data frame into the AppDB document structure
#' required by \code{appdb_doc_bulk_create()}/\code{appdb_doc_bulk_update()}
#' — the data-frame equivalent of calling \code{appdb_row_to_document()}
#' once per row.
#'
#' @param data A data frame containing the source data.
#' @param id_column Optional name of a column in \code{data} holding
#'   each row's existing AppDB document ID. When supplied, that column
#'   is excluded from the document's \code{content} and attached
#'   instead as the document's \code{id} field, which is what
#'   \code{appdb_doc_bulk_update()} needs to update (rather than
#'   create) each document. Leave \code{NULL} (the default) for
#'   \code{appdb_doc_bulk_create()}, where every row is a new
#'   document.
#'
#' @return A list of document payloads, one per row of \code{data}, in
#'   row order.
#'
#' @examples
#' # For appdb_doc_bulk_create():
#' new_docs <- appdb_rows_to_documents(need_new)
#'
#' # For appdb_doc_bulk_update():
#' update_docs <- appdb_rows_to_documents(need_update, id_column = "appdb_document_id")
#'
#' @export

appdb_rows_to_documents <- function(data, id_column = NULL) {

  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package \"purrr\" must be installed to use this function.",
         call. = FALSE)
  }

  if (!is.null(id_column) && !id_column %in% names(data)) {
    stop("`id_column` (", id_column, ") is not a column of `data`.", call. = FALSE)
  }

  ids <- if (!is.null(id_column)) data[[id_column]] else NULL
  content_data <- if (!is.null(id_column)) {
    data[, setdiff(names(data), id_column), drop = FALSE]
  } else {
    data
  }

  purrr::map(seq_len(nrow(data)), function(i) {
    doc <- appdb_row_to_document(content_data, i)
    if (!is.null(id_column)) doc$id <- ids[i]
    doc
  })

}
