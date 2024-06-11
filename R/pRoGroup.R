#' Group Management
#'
#' Maintain the memberships of groups based on a certain table.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param data_table A dataframe/tibble that contains two required fields:
#' \enumerate{
#' \item group_id
#' \item user_id}
#' @examples pRoGroup(client_id = client_id, secret = secret, data_table = data_table)
#' @export


pRoGroup <- function(client_id, secret, data_table) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("data.table", quietly = TRUE)) {stop("Package \"data.table\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("foreach", quietly = TRUE)) {stop("Package \"foreach\" must be installed to use this function.", call. = FALSE)}

  # Check the data_table
  data_table <- dplyr::select(data_table, c('group_id', 'user_id'))


  # Group List
  group_list <- data_table %>% dplyr::select(group_id) %>% unique()

  for (x in 1:nrow(group_list)) {

  cur_members <- group_user_get(client_id = client_id, secret = secret, group_id = group_list$group_id[x])
  cur_members <- dplyr::tibble(group_id = cur_members$group_id, user_id = unlist(cur_members$user_id))
  cor_members <- data_table %>% dplyr::filter(group_id == group_list$group_id[x])

  addList <- anti_join(cor_members, cur_members, by = join_by(group_id, user_id))
  for (i in 1:nrow(addList)) {group_user_add(client_id = client_id, secret = secret, group_id = addList$group_id[i], user_id = addList$user_id[i])}

  deleteList <- anti_join(cur_members, cor_members, by = join_by(group_id, user_id))
  for (i in 1:nrow(deleteList)) {group_user_remove(client_id = client_id, secret = secret, group_id = deleteList$group_id[i], user_id = deleteList$user_id[i])}

  }
}
