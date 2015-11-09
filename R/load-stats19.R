#' Import and format UK 'Stats19' road traffic casualty data
#'
#' @section Details:
#'
#' @param dataset
#' @export
#' @examples
read_stats19 <- function(data_dir, zip_url = paste0("http://data.dft.gov.uk.s3.amazonaws.com/",
  "road-accidents-safety-data/Stats19_Data_2005-2014.zip")){

  # download and unzip the data if it's not present
  if(exists("data_dir")){
    data_dir <- tempdir()
    destfile <- file.path(data_dir, "Stats19_Data_2005-2014.zip")
    downloader::download(zip_url, destfile)
    unzip(destfile, exdir = data_dir)
  }

  # read-in the data

}