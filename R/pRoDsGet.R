#' Query a DataSet
#'
#' Queries the data in an existing Domo DataSet.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param dataset_id The unique ID of the dataset.
#' @param sql_query SQL query, defaulted to: \code{select * from table}.
#' @param parallel Leveraging the \code{\link{foreach}} package to export data by breaking it into smaller chunks.
#' @param n_core It is a required field if parallel is TRUE.
#' @examples dataset_query(client_id = client_id,
#'   secret = secret,
#'   dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4',
#'   sql_query = 'select * from')
#' @export


pRoDsGet <- function(client_id, secret, dataset_id, sql_query = NULL, parallel = FALSE, n_core = NULL) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("data.table", quietly = TRUE)) {stop("Package \"data.table\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("foreach", quietly = TRUE)) {stop("Package \"foreach\" must be installed to use this function.", call. = FALSE)}

  # Functions
  `%dopar%` <- foreach::`%dopar%`
  `%!like%` <- Negate(data.table::`%like%`)

  # Access
  access <- httr::content(
    httr::GET(url = 'https://api.domo.com/oauth/token',
              config = httr::add_headers(c(Authorization=paste('Basic',RCurl::base64(paste(client_id,secret,sep=':'))[[1]], sep=' '))),
              query = list(grant_type='client_credentials')))

  cur <- dataset_get_info(client_id = client_id, secret = secret, dataset_id = dataset_id)
  print(paste0('Total Rows: ', cur$rows))


  # SQL Query
  if(is.null(sql_query)) {
    sql_query <- "select * from table"
    print(paste0('Defaulted to: ',sql_query, ' limit ', cur$rows))}
    else {sql_query <- string::str_replace_all(sql_query, '\n',' ')}


  # Parallel is TRUE
  if(parallel == TRUE){

    # Create Iterations
    a <- cur$rows
    b <- ceiling(a/n_core)
    iteration <- list()
    skip <- 0
    for (i in 1:n_core) {
      end = skip + b
      end = ifelse((end-skip) %% 2 == 0, end + 1, end)
      iteration[[i]] <- dplyr::tibble(iteration = i, skip = skip, end = end)
      skip = end}
    iteration <- dplyr::bind_rows(iteration) %>% dplyr::mutate(rows = end - skip)


    # Check For Parallelism
    if(is.null(n_core)) {stop('Parallel == TRUE needs to be accompanied by a value (>1) in n_core parameter.', call. = FALSE)}
    if(parallel::detectCores() < n_core) {stop(paste0("You only have ", parallel::detectCores(), " cores, please reduce the value of the n_core."), call. = FALSE)}

    # Create clusters
    my.cluster <- parallel::makeCluster(n_core, type = "PSOCK")
    doParallel::registerDoParallel(cl = my.cluster)

    data <- foreach::foreach(x = 1:nrow(iteration), .combine = 'rbind', .packages = c('magrittr', 'dplyr')) %dopar% {
      #Body
      content <- httr::content(
        httr::POST(url = paste0('https://api.domo.com/v1/datasets/query/execute/', dataset_id),
                   body = paste0('{"sql": ','"',sql_query,' limit ', iteration$skip[x],',',iteration$rows[x],'"}'),
                   config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
                   httr::content_type("application/json"),
                   httr:: accept("application/json"),
                   encode = 'json'))

      data <- rlist::list.stack(content$rows)
      colnames(data) <- content$columns
      data <- dplyr::tibble(data)
    }

    parallel::stopCluster(cl = my.cluster)
  }


  # No Parallel Processing
  else {
    print('No Parallel Processing')
    #Body
    content <- httr::content(
      httr::POST(url = paste0('https://api.domo.com/v1/datasets/query/execute/', dataset_id),
                 body = paste0('{"sql": ','"',sql_query,'"}'),
                 config = httr::add_headers(c(Authorization=paste('bearer',access$access_token,sep=' '))),
                 httr::content_type("application/json"),
                 httr:: accept("application/json"),
                 encode = 'json'))

    data <- rlist::list.stack(content$rows)
    colnames(data) <- content$columns
  }

  return(data)
}
