#' List a Dataflow's Recent Executions
#'
#' Returns the most recent runs of a dataflow from
#' \code{/api/dataprocessing/v1/dataflows/\{id\}/executions}, newest first.
#'
#' @inheritParams dataflow_get
#' @param limit Maximum number of executions to return. Default 10.
#'
#' @return A tibble with one row per execution: \code{execution_id},
#'   \code{state} (for example \code{"SUCCESS"}, \code{"FAILED"}), \code{failed},
#'   \code{activation_type} (how it was started), \code{begin_time},
#'   \code{end_time} (both POSIXct, UTC; \code{end_time} is \code{NA} while
#'   running), \code{rows_read} and \code{rows_written}. Zero rows if the
#'   dataflow has never run.
#'
#' @examples
#' \dontrun{
#' dataflow_executions(449, Sys.getenv("DOMO_TOKEN"), "https://company.domo.com", limit = 3)
#' }
#'
#' @export

dataflow_executions <- function(dataflow_id, developer_token, instance,
                                limit = 10) {
  id <- .dataflow_check_id(dataflow_id)
  .dataflow_check_token(developer_token)
  if (!is.numeric(limit) || length(limit) != 1 || is.na(limit) ||
      limit < 1 || limit != round(limit)) {
    stop("`limit` must be a single positive whole number.", call. = FALSE)
  }

  url <- paste0(.dataflow_url(instance, id, "/executions"), "?limit=", limit)
  executions <- .dataflow_call(url, developer_token)

  if (length(executions) == 0) {
    return(.dataflow_execution_row(list())[0, ])
  }
  dplyr::bind_rows(lapply(executions, .dataflow_execution_row))
}
