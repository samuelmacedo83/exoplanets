download_table <- function(table,
                           columns = NULL,
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
  #browser()
 if (is.null(columns)) {
    url <- paste0(base_url, "table=", table)
  } else if (length(columns) == 1){
    if (columns == "all"){
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
