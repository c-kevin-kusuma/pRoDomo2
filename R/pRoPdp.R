#' Manage Personalized Data Permission (PDP) at Scale
#'
#' Manage Personalized Data Permission (PDP) at Scale with a master table that contains certain fields.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param data_table A data frame/tibble that contains 5 required fields:
#' \enumerate{
#'  \item `Dataset ID`
#'  \item `Policy Name`
#'  \item `Policy Column`
#'  \item `User ID`
#'  \item `Policy Value`}
#' A combination of `Dataset ID`+`Policy Name`+`Policy Column` makes a unique key of the table and each unique key represents one policy.
#' @param parallel Leveraging the \code{\link{foreach}} package to simultaneously update the PDPs of multiple datasets.
#' @param n_core It is a required field if parallel is TRUE.
#' @param ignore_policy The operation will ignore all policies that contains the specified string, defaulted to 'AA - Restricted'.
#' @examples
#' data_table <- dplyr::tibble(`Dataset ID` = '824025cb-ba76-41b4-a38d-625a9cc1dfed',
#'   `Policy Name` = c('pol_1', 'pol_2'),
#'   `Policy Column` = c('col_1', 'col_2'),
#'   `User ID` = 123456789,
#'   `Policy Value` = c('val_1', 'val_2')))
#' pRoPdp(client_id = client_id,
#'   secret = secret,
#'   data_table = data_table)
#' @export


pRoPdp <- function(client_id, secret, data_table, parallel = FALSE, n_core = NULL, ignore_policy = 'AA - Restricted') {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("readr", quietly = TRUE)) {stop("Package \"readr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("magrittr", quietly = TRUE)) {stop("Package \"magrittr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rlist", quietly = TRUE)) {stop("Package \"rlist\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("data.table", quietly = TRUE)) {stop("Package \"data.table\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("foreach", quietly = TRUE)) {stop("Package \"foreach\" must be installed to use this function.", call. = FALSE)}

  # Data
  pdpData <- data_table %>% dplyr::select(`Dataset ID`, `Policy Name`, `Policy Column`, `User ID`, `Policy Value`) %>% dplyr::mutate(across(everything(), as.character))
  pdpData[pdpData == ''] <- NA
  if(nrow(pdpData) != nrow(na.omit(pdpData))) {stop('Missing value detected in the data_table', call. = FALSE)}
  if(nrow(pdpData) != nrow(unique(pdpData))) {stop('Duplicates detected in the data_table', call. = FALSE)}
  pdpDs <- pdpData %>%  dplyr::filter(!is.na(`Dataset ID`) & `Dataset ID` != '' ) %>% dplyr::select(`Dataset ID`) %>% unique()
  if(nrow(pdpDs) == 0) {stop('No `Dataset ID` can be found', call. = FALSE)}

  # Functions
  `%dopar%` <- foreach::`%dopar%`
  `%!in%` <- Negate(`%in%`)
  `%!like%` <- Negate(data.table::`%like%`)
  extractPdp <- function(x) {
    if(length(x)==0) {break}
    for (i in 1:length(x)) {
      if(length(x[[i]]$users) == 0){users <- dplyr::tibble(users = '')} else{users <- dplyr::tibble(users = x[[i]]$users) %>% dplyr::mutate(users = as.character(users)) %>% dplyr::arrange(users)} # Extract Users
      if(length(x[[i]]$filters) == 0){filters <- dplyr::tibble(column = '', values = '')} else{filters <- x[[i]]$filters %>% rlist::list.stack() %>% dplyr::select(column, values) %>% dplyr::mutate(values = as.character(values)) %>% dplyr::arrange(values)}
      x[[i]] <- dplyr::tibble(`Policy ID` = x[[i]]$id, `Policy Name` = x[[i]]$name) %>% merge(users) %>% merge(filters) %>% dplyr::rename(`Policy Column` = column, `User ID` = users, `Policy Value` = values)}

    y <- dplyr::bind_rows(x)
  }
  createPdpList <- function(x){
    if('Policy ID' %in% colnames(x)){id <- as.integer(x$`Policy ID`)} else{id <- NULL}
    filters <- list()
    users <- list()
    longFilters <- x$`Policy Value` %>% strsplit('|', fixed = TRUE) %>% unlist()
    longUsers <- x$`User ID` %>% strsplit('|', fixed = TRUE) %>% unlist()
    for (i in 1:length(longFilters)) {filters[[i]] <- list(column = x$`Policy Column`, values = list(longFilters[i]), operator = 'EQUALS', not = FALSE) } # Create Filters
    for (i in 1:length(longUsers)) {users[[i]] <- as.integer(longUsers) } # Create Users
    pdpList <- list(id = id, type = 'user', name = x$`Policy Name`, filters = filters, users = users, virtualUsers = list(), groups = list())
    if('Policy ID' %!in% colnames(x)){pdpList$id <- NULL}
    return(pdpList)
  }
  massUpdate <- function(x){
    dsID = x
    # Current PDP List
    curPolicy <- pdp_get_all(client_id = client_id, secret = secret, dataset_id = dsID)
    curPolicyLong <- extractPdp(curPolicy) %>% dplyr::mutate(across(everything(), as.character)) %>% dplyr::filter(`Policy Name` %!like% ignore_policy & `Policy Name` != 'All Rows')
    curPolicyWide <- curPolicyLong %>% dplyr::select(`Policy ID`, `Policy Name`, `Policy Column`) %>% unique()

    # Correct PDP List
    corPolicyLong <- pdpData %>% dplyr::filter(`Dataset ID` == dsID) %>% select(-`Dataset ID`)
    corPolicyWide <- corPolicyLong %>% dplyr::arrange(`Policy Name`, `User ID`, `Policy Value`) %>% dplyr::group_by(`Policy Name`, `Policy Column`) %>% dplyr::summarise(`User ID` = paste(unique(`User ID`), collapse = '|'), `Policy Value` = paste(unique(`Policy Value`), collapse = '|'), .groups = 'drop')

    # IF NO current policies can be found on the dataset
    if(nrow(curPolicyWide) == 0) {
      if(nrow(corPolicyWide) == 0) {break}
      else {for (i in 1:nrow(corPolicyWide)) {pdp_create(client_id, secret = secret, dataset_id = dsID, body = createPdpList(corPolicyWide[i,]))}} }
    else{
      # Add policies
      addList <- dplyr::anti_join(corPolicyWide, curPolicyWide, by = c('Policy Name'='Policy Name', 'Policy Column' = 'Policy Column'))
      if(nrow(addList) > 0) {for (i in 1:nrow(addList)) {pdp_create(client_id, secret = secret, dataset_id = dsID, body = createPdpList(addList[i,]))} }

      # Delete Policies
      dupList <- curPolicyWide %>% dplyr::group_by(`Policy Name`, `Policy Column`) %>% dplyr::mutate(rowN = dplyr::row_number()) %>% dplyr::filter(rowN > 1) %>% dplyr::ungroup() %>% dplyr::select(`Policy ID`)
      delList <- dplyr::anti_join(curPolicyWide, corPolicyWide, by = c('Policy Name'='Policy Name', 'Policy Column' = 'Policy Column')) %>% dplyr::select(`Policy ID`)
      delList1 <- dplyr::bind_rows(delList, dupList) %>% unique()
      if(nrow(delList1) > 0) {for (i in 1:nrow(delList1)) {pdp_delete(client_id, secret = secret, dataset_id = dsID, pdp_id = delList1$`Policy ID`[i])} }

      # Update Policies
      addList2 <- addList %>% dplyr::select(`Policy Name`, `Policy Column`) %>% unique()
      delList2 <- delList1 %>% dplyr::left_join(curPolicyWide, by = join_by(`Policy ID`)) %>% dplyr::select(`Policy Name`, `Policy Column`) %>% unique()

      updList <- dplyr::anti_join(corPolicyLong, curPolicyLong, by = join_by(`Policy Name`, `Policy Column`, `Policy Value`)) %>%
        dplyr::select(`Policy Name`, `Policy Column`) %>% unique() %>%
        dplyr::anti_join(delList2, by = join_by(`Policy Name`, `Policy Column`)) %>%
        dplyr::anti_join(addList2, by = join_by(`Policy Name`, `Policy Column`)) %>%
        dplyr::left_join(curPolicyWide, by = join_by(`Policy Name`, `Policy Column`)) %>%
        dplyr::left_join(corPolicyWide, by = join_by(`Policy Name`, `Policy Column`))
      if(nrow(updList) > 0) {for (i in 1:nrow(updList)) {pdp_update(client_id, secret = secret, dataset_id = dsID, pdp_id = updList$`Policy ID`[i], body = createPdpList(updList[i,]))} } }
  }


  # Parallel is TRUE
  if(parallel){

    #Check For Parallelism
    if(is.null(n_core)) {stop('Parallel == TRUE needs to be accompanied by a value (>1) in n_core parameter.', call. = FALSE)}
    if(parallel::detectCores() < n_core) {stop(paste0("You only have ", parallel::detectCores(), " cores, please reduce the value of the n_core."), call. = FALSE)}

    # Create clusters
    my.cluster <- parallel::makeCluster(n_core, type = "PSOCK"); doParallel::registerDoParallel(cl = my.cluster)
    foreach::foreach(a = 1:nrow(pdpDs), .packages = c('magrittr', 'dplyr', 'pRoDomo2')) %dopar% {dsID <- pdpDs$`Dataset ID`[a]; massUpdate(dsID)}
    parallel::stopCluster(cl = my.cluster)
    }

  # No Parallel
  if(!parallel) { for(b in 1:nrow(pdpDs)) {
    dsID <- pdpDs$`Dataset ID`[b]
    print(paste0(b,' of ', nrow(pdpDs), ': ', dsID))
    massUpdate(dsID)}}

}
