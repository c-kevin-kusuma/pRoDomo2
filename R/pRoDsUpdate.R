#' Create A New Dataset Via Stream API
#'
#' It is best practice to use this function to create a new dataset as it leverages STREAM API that loads data parts at scale.
#' @param client_id A client_id that can be created on the developer.domo.com page.
#' @param secret A secret that can created on the developer.domo.com page.
#' @param nameLike If included, will limit the list of DataSets to those with names that contain the string passed in. nameLike is case insensitive.
#' @param sort The DataSet field to sort by. Fields prefixed with a negative sign reverses the sort (i.e. '-name' does a reverse sort by the name of the DataSets).
#' @examples dataset_get_info_all(client_id = client_id,
#'   secret = secret,
#'   nameLike = 'first',
#'   sort = 'name')
#' @export


pRoDsUpdate <- function(client_id, secret, dataset_id, dataset, parallel = FALSE, n_core = NULL) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("readr", quietly = TRUE)) {stop("Package \"readr\" must be installed to use this function.", call. = FALSE)}

  # Check dataset_id
  stream.searched <- stream_search(client_id = client_id, secret = secret, dataset_id = dataset_id)
  if (length(stream.searched) == 0) {stop("dataset_id does not exist!", call. = FALSE)}

  # Functions
  estimate_rows <- function (data) {
    sz <- as.numeric(pryr::object_size(data)) / 1000
    targetSize <- 30000
    if (sz > targetSize)
      return(floor(nrow(data)*(targetSize) / (sz)))
    return(nrow(data))
  }
  data_type_extract <- function(x){
    b <- colnames(x)
    schema <- list()
    for (i in 1:ncol(x)) {
      name = b[i]
      type = class(x[,i])[1]

      schema[[i]] <- list(
        type = ifelse(type == 'numeric', 'DOUBLE',
                      ifelse(type == 'Date', 'DATE',
                             ifelse(type == 'POSIXct', 'DATETIME',
                                    ifelse(type == 'POSIXlt', 'DATETIME',
                                           ifelse(type == 'integer', 'LONG',
                                                  'STRING'))))), name = name) }
    schema = list(columns = schema)
    return(schema)
  }

  # Extract Schema
  dataset.current <- dataset_get_info(client_id = client_id, secret = secret, dataset_id = dataset_id)
  schema_current <- dataset.current$schema
  schema <- data_type_extract(dataset)

  # Row Estimation For Partitioning
  estimated.rows <- estimate_rows(dataset)
  n.row <- nrow(dataset)
  part.total <- ceiling(n.row / estimated.rows)

  # Update Schema If Necessary
  if(!identical(schema, schema_current)) {
    print('Schema Changed!')
    body <- list(name = dataset.current$name, description = dataset.current$description, pdpEnabled = dataset.current$pdpEnabled, schema = schema)
    dataset_update_info(client_id = client_id, secret = secret, dataset_id = dataset_id, body = body)}

  # Current Stream & Create Execution
  stream.id = stream.searched[[1]]$id
  created.execution <- stream_execution_create(client_id = client_id, secret = secret, stream_id = stream.id) # Create Execution
  execution.id <- created.execution$id


  # Upload Data
  start <- 1
  for (i in 1:part.total) {
    end <- ifelse(estimated.rows * i < n.row, estimated.rows * i, n.row)
    # print(paste('Part', paste0(i,':'), paste0(start,'-', end)))
    data <- dataset[start:end,]
    start <- estimated.rows * i + 1

    FNAME <- tempfile(pattern="domo", fileext=".gz")
    z <- gzfile(FNAME, "wb")
    readr::write_csv(as.data.frame(data),file=z,col_names=FALSE,na='\\N')
    close(z)

    size <- file.info(FNAME)$size
    b <- readBin(f <- file(FNAME, "rb"), "raw", n=size)
    close(f)

    stream_uploadPart(client_id = client_id, secret = secret, stream_id = stream.id, execution_id = execution.id, part_id = i, body = b)
    unlink(FNAME)
  }

  # Commit Execution
  stream.commit <- stream_execution_commit(client_id = client_id, secret = secret, stream_id = stream.id, execution_id = execution.id)

}
