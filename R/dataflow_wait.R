#' Wait for a Dataflow Execution to Finish
#'
#' Polls one execution until it reaches a final state (\code{SUCCESS},
#' \code{FAILED}, \code{KILLED}, ...) or the timeout passes.
#'
#' @inheritParams dataflow_get
#' @param execution_id The execution id returned by \code{\link{dataflow_run}}
#'   or shown by \code{\link{dataflow_executions}}.
#' @param timeout Seconds to wait before giving up. Default 3600.
#' @param poll_interval Seconds between checks. Default 15.
#' @param quiet If \code{FALSE}, prints a line each time the state changes.
#'
#' @return A one-row tibble in the same shape as
#'   \code{\link{dataflow_executions}}, describing the finished execution.
#'   A run that ends in \code{FAILED} or \code{KILLED} is still returned (check
#'   \code{state}), not raised as an error, so a caller can decide what to do.
#'
#' @details
#' If the timeout passes first, an error is raised that includes the last
#' state seen. The run itself is not cancelled.
#'
#' @examples
#' \dontrun{
#' run <- dataflow_run(449, Sys.getenv("DOMO_TOKEN"), "https://company.domo.com")
#' res <- dataflow_wait(449, run$execution_id, Sys.getenv("DOMO_TOKEN"),
#'                      "https://company.domo.com", quiet = FALSE)
#' res$state
#' }
#'
#' @export

dataflow_wait <- function(dataflow_id, execution_id, developer_token, instance,
                          timeout = 3600, poll_interval = 15, quiet = TRUE) {
  id <- .dataflow_check_id(dataflow_id)
  exec_id <- .dataflow_check_id(execution_id, "execution_id")
  .dataflow_check_token(developer_token)
  if (!is.numeric(timeout) || length(timeout) != 1 || is.na(timeout) ||
      timeout < 0) {
    stop("`timeout` must be a single non-negative number of seconds.",
         call. = FALSE)
  }
  if (!is.numeric(poll_interval) || length(poll_interval) != 1 ||
      is.na(poll_interval) || poll_interval < 0) {
    stop("`poll_interval` must be a single non-negative number of seconds.",
         call. = FALSE)
  }

  url <- .dataflow_url(instance, id, paste0("/executions/", exec_id))
  started <- Sys.time()
  last_state <- NULL

  repeat {
    execution <- .dataflow_call(url, developer_token)
    state <- .dataflow_chr(execution$state)

    if (!quiet && !identical(state, last_state)) {
      message(format(Sys.time(), "%H:%M:%S"), "  ", state)
    }
    last_state <- state

    if (state %in% .dataflow_final_states || isTRUE(execution$failed)) {
      return(.dataflow_execution_row(execution))
    }
    if (as.numeric(difftime(Sys.time(), started, units = "secs")) >= timeout) {
      stop("Execution ", exec_id, " of dataflow ", id, " was still ", state,
           " after ", timeout, " seconds.", call. = FALSE)
    }
    Sys.sleep(poll_interval)
  }
}
