#' Start a Dataflow Run
#'
#' Starts a manual run of a dataflow by posting to
#' \code{/api/dataprocessing/v1/dataflows/\{id\}/executions}. It returns as
#' soon as Domo accepts the run; use \code{\link{dataflow_wait}} to wait for
#' it to finish.
#'
#' @inheritParams dataflow_get
#'
#' @return A list with \code{execution_id} (a string) and \code{state}
#'   (normally \code{"CREATED"}).
#'
#' @details
#' This changes data: the dataflow rewrites its output datasets and any
#' dataflow triggered by them will start in turn. Domo does not stop you
#' from starting a run while one is already in progress.
#'
#' @examples
#' \dontrun{
#' run <- dataflow_run(449, Sys.getenv("DOMO_TOKEN"), "https://company.domo.com")
#' dataflow_wait(449, run$execution_id, Sys.getenv("DOMO_TOKEN"),
#'               "https://company.domo.com")
#' }
#'
#' @export

dataflow_run <- function(dataflow_id, developer_token, instance) {
  id <- .dataflow_check_id(dataflow_id)
  .dataflow_check_token(developer_token)

  parsed <- .dataflow_call(
    .dataflow_url(instance, id, "/executions"),
    developer_token,
    method = "POST"
  )
  if (is.null(parsed$id)) {
    stop("Domo accepted the request but did not return an execution id.",
         call. = FALSE)
  }
  list(execution_id = as.character(parsed$id), state = .dataflow_chr(parsed$state))
}
