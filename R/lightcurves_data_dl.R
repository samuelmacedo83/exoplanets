#' lightcurves_data_dl
#'
#' Download the lightcurve data according to the kepid provided
#'
#' @param kepid_table A data frame containing a column name kepid
#'
#' @importFrom magrittr %>%
#' @export
lightcurves_data <- function(kepid_table){

 #verify if lightcurves folder exists, if not, creates
  if (!dir.exists("lightcurves")){
    dir.create("lightcurves")
  }

 # verify the kepid column
 if (!any(grepl("kepid", names(kepid_table))) == TRUE){
   stop("There is no kepid column in your data")
 }

# remove duplicate
kepid_table <- kepid_table %>%
  dplyr::select(kepid) %>%
  dplyr::distinct()

# padronizing the star name by nine digits. "000847632"
kepid_table <-  purrr::map_chr(kepid_table$kepid,
                               stringr::str_pad,
                               width = 9, pad = "0")

# kepler lightcurve website
base_url <- "http://archive.stsci.edu/pub/kepler/lightcurves/"
kepid_url <- paste0(base_url,
                    substring(kepid_table[1], 1, 4), "/",
                    kepid_table[1], "/")

# Link to download lightcurve data
kepid_html <- xml2::read_html(kepid_url)
kepid_fits <- kepid_html %>% rvest::html_nodes("a") %>% rvest::html_text()
kepid_lc <- purrr::map_lgl(kepid_fits, grepl, pattern ="_lc_")
kepid_tar <- kepid_fits[kepid_lc]

# Create the folder to storage the data, in necessary
kepid_folder <- paste0("lightcurves/",kepid_table[1])

if (dir.exists(kepid_folder)) {
    cat(paste0("The ", kepid_table[1], " directory already exists"))
  }  else {
  browser()
  utils::download.file(url = paste0(kepid_url, kepid_tar),
                       destfile = "lightcurves/temp.tar",
                       method = "curl")
  utils::untar("lightcurves/temp.tar",
               exdir = "lightcurves")
  file.remove("lightcurves/temp.tar")
}

browser()

}
