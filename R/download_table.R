#' nea_table
#'
#' A wrapper function for download any table in NASA Exoplanets Archive
#'
#' @param table A string containing the table name. For the available tables check 'details' below.
#' @param columns Specify the columns that you want to download in the table. Define the columns
#' in a vector, for example, c("col1", "col2", "col3"). Use '"all" to download all available columns
#' and "default" for the default columns, see more in 'details'.
#' @param format The archive extension. "csv" (default), "json", "xml", "ascii", "ipac", "bar"
#'  and "pipe".
#' @param data_folder Create a separate folder for you data. If FALSE, the
#' data will be download in your root.
#' @param force Check if the data already exists. If TRUE, the data will
#' download and replace.
#'
#' @details You can check the available tables in
#' https://exoplanetarchive.ipac.caltech.edu/docs/program_interfaces.html#data.
#' In this link you can also check the table documentation and there will be specified all
#' available columns and what are the 'default columns'.
#'
#' @examples
#' # download default columns for the exoplanets table
#' nea_table("exoplanets")
#'
#' # download specific columns
#' nea_table("q1_q17_dr24_tce", columns = c("kepid", "av_training_set"))
#'
#' @export
nea_table <- function(table,
                      columns = "default",
                      format = "csv",
                      data_folder = TRUE,
                      force = FALSE){

  base_url = "https://exoplanetarchive.ipac.caltech.edu/cgi-bin/nstedAPI/nph-nstedAPI?"
  dest_folder <- ""

  # Create the folder to storage the data
  if (data_folder){
    if (!dir.exists(table)) {
      dir.create(table)
      dest_folder <- paste0(table, "/")
    }
    dest_folder <- paste0(table, "/")
  }

  # Add table and columns to url
if (length(columns) == 1){
  if(columns == "default"){
    url <- paste0(base_url, "table=", table)
  } else if (columns == "all"){
      columns <- read.table(paste0(base_url, "table=", table, "&getAllColumns"))
      columns <- as.character(columns$V1)
      url <- paste0(base_url, "table=", table, "&select=", columns)
    } else url <- paste0(base_url, "table=", table, "&select=", columns)
  } else{
    url <- paste0(base_url, "table=", table, "&select=", columns[1])
    for(i in 2:length(columns)){
      url <- paste0(url, ",", columns[i])
    }
  }

  # Add the format
  if (format != "csv"){
    url <- paste0(url, "&format=", format)
  }

  # Check if data exists or download if not
  destfile <- paste0(dest_folder, table, ".", format)
  if (file.exists(destfile)){
    if (force == FALSE){
      warning("Your data already exists. If you want to replace it, please use force = TRUE")
      } else download.file(url = url, destfile = destfile)
   } else download.file(url = url, destfile = destfile)
}



#' nea_read_table
#'
#' This is a simple function for read the tables downloaded
#' by NASA Exoplanet Archive. You can use read.csv() function if
#' you need to be more especific about your data.
#'
#' @param table A string containing the table name. For the available tables check 'details' below.
#' @param folder If NULL (default) this function will search for
#' the folder created by nea_table(). If the data is in a different directory,
#' pass the PATH_TO_YOUR_FOLDER as a string.
#'
#' @details Use before nea_table() for download the data that you want
#' to work. You can check the available tables in
#' https://exoplanetarchive.ipac.caltech.edu/docs/program_interfaces.html#data.
#'
#' @return
#' A data frame containing the data
#'
#' @examples
#' nea_table("q1_q17_dr24_tce") # Download your data
#' nea_read_table("q1_q17_dr24_tce")
#'
#' @export
nea_read_table <- function(table, folder = NULL){

  if(dir.exists(table)) {
    nea_table <- read.csv(paste0(table,"/",table, ".csv"))
  } else {
    nea_table <- read.csv(paste0(folder, "/", table, ".csv"))
  }
  nea_table
}


