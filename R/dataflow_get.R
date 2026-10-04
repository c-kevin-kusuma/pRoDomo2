#' Get a Dataflow Definition
#'
#' Fetches a dataflow's definition from
#' \code{/api/dataprocessing/v1/dataflows/\{id\}}: its input datasets, its
#' tiles (\code{actions}), and its output datasets.
#'
#' @param dataflow_id The dataflow's id, as a whole number or numeric string.
#' @param developer_token A valid Domo Developer Token. The OAuth
#'   client id / secret pair used by the dataset functions is not accepted by
#'   this endpoint.
#' @param instance Domo instance URL. For example:
#'   \code{"https://company.domo.com"}. A bare hostname is also accepted;
#'   \code{https://} is added automatically when no scheme is present.
#'
#' @return The parsed definition as a nested list (the API response, unchanged).
#'
#' @details
#' The definition can contain filter values and expressions typed into
#' tiles, so treat it as potentially sensitive before printing it.
#'
#' @examples
#' \dontrun{
#' flow <- dataflow_get(449, Sys.getenv("DOMO_TOKEN"), "https://company.domo.com")
#' length(flow$actions)
#' }
#'
#' @export

dataflow_get <- function(dataflow_id, developer_token, instance) {
  id <- .dataflow_check_id(dataflow_id)
  .dataflow_check_token(developer_token)
  .dataflow_call(.dataflow_url(instance, id), developer_token)
}
