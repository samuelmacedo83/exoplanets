#' lightcurves_data_dl
#'
#' Download the lightcurve data according to the kepid provided
#'
#' @param nea_table data frame containing a column name kepid
#'
#' @importFrom magrittr %>%
#' @export
lightcurves_data <- function(kepid_table){

 # verify the kepid column
 if (!any(grepl("kepid", names(kepid_table))) == TRUE){
   stop("There is no kepid variable in your table")
 }

# remove duplicate
kepid_table <- kepid_table %>%
  dplyr::select(kepid) %>%
  dplyr::distinct()

kepid_table <-  purrr::map_chr(kepid_table$kepid,
                               stringr::str_pad,
                               width = 9, pad = "0")
kepid_table
}
