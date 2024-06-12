#' Create A New Dataset Via Stream API
#'
#' It is best practice to use this function to create a new dataset as it leverages STREAM API that loads data parts at scale.
#' @param client_id A client_id that can be created on the \url{developer.domo.com} page.
#' @param secret A secret that can be created on the \url{developer.domo.com} page.
#' @param dataset The data frame that will be uploaded to DOMO.
#' @param dataset_name The name of the dataset.
#' @param dataset_description (Optional) The description of the dataset.
#' @param update_method The method in which the dataset will be updated, either "REPLACE" or "APPEND".
#' @param parallel (Optional) If TRUE, the function will use multiple cores to upload the data frame. `n_core` is required when `parallel` = TRUE.
#' @param n_core (Optional) The number of cores to use from your device. It's recommended to leave one core free. If you have 8 cores, enter `n_core` = 7.
#' @examples
#' pRoDsCreate(client_id = client_id,
#'   secret = secret,
#'   dataset = dataset,
#'   dataset_name = 'The Name of the Dataset',
#'   dataset_description = 'This is the description',
#'   update_method = 'REPLACE')
#' @export


pRoDsCreate <- function(client_id, secret, dataset, dataset_name, dataset_description = '', update_method = 'REPLACE', parallel = FALSE, n_core = NULL) {

  # Check Required Packages
  if (!requireNamespace("RCurl", quietly = TRUE)) {stop("Package \"RCurl\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("readr", quietly = TRUE)) {stop("Package \"readr\" must be installed to use this function.", call. = FALSE)}

  # Functions
  estimate_rows <- function (data) {
    sz <- as.numeric(pryr::object_size(dataset)) / 1000
    targetSize <- 30000
    if (sz > targetSize)
      return(floor(nrow(data)*(targetSize) / (sz)))
    return(nrow(data))
  }
  data_type_extract <- function(x){
    b <- colnames(x)
    b1 <- sapply(x, class)
    schema <- list()
    for (i in 1:ncol(x)) {
      type = b1[[i]][1]

      schema[[i]] <- list(
        name = b[i],
        type = ifelse(type == 'numeric', 'DOUBLE',
                      ifelse(type == 'Date', 'DATE',
                             ifelse(type == 'POSIXct', 'DATETIME',
                                    ifelse(type == 'POSIXlt', 'DATETIME',
                                           ifelse(type == 'integer', 'LONG',
                                                  'STRING')))))) }
    schema = list(columns = schema)
    return(schema)
  }

  # Extract Schema
  schema <- data_type_extract(dataset)
  body <- list(dataSet = list(name = dataset_name, description = dataset_description, schema = schema), updateMethod = update_method)

  # Create Stream and Execution
  created.stream <- stream_create(client_id = client_id, secret = secret, body = body) # Create Stream
  stream.id <- created.stream$id
  created.execution <- stream_execution_create(client_id = client_id, secret = secret, stream_id = stream.id) # Create Execution
  dataset.id <- created.stream$dataSet$id
  execution.id <- created.execution$id

  # Row Estimation For Partitioning
  estimated.rows <- estimate_rows(dataset)
  n.row <- nrow(dataset)
  part.total <- ceiling(n.row / estimated.rows)

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

  print(dataset.id)
}

