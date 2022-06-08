#' Import and format Australian Bureau of Statistics (ABS) TableBuilder files
#'
#' @section Details:
#' The Australian Bureau of Statistics (ABS) provides customised tables for
#' census and other datasets in a format that is difficult to use in R
#' because it contains rows with additional information.
#' This function imports the original (unzipped) TableBuilder files in .csv
#' or .xlsx format before creating an R dataframe with the data.
#'
#' Note: we recommend using the
#' [readabs](https://github.com/mattcowgill/readabs)
#' package for this purpose.
#'
#' @param dataset Either a dataframe containing the original data from
#' TableBuilder or a character string containing the path of the
#' unzipped TableBuilder file.
#' @param filetype A character string containing the filetype. Valid values
#' are 'csv', 'legacycsv' and 'xlsx' (default = 'csv'). Required even when
#' dataset is a dataframe. Use 'legacycsv' for csv files derived from earlier
#' versions of TableBuilder for which csv outputs were csv versions of the
#' xlsx files. Current csv output from TableBuilder follow a more standard
#' csv format.
#' @param sheet An integer value containing the index of the sheet in the
#' xlsx file (default = 1).
#' @param removeTotal A boolean value. If TRUE removes the rows and columns
#' with totals (default = TRUE).
#' @family data
#' @export
read_table_builder <- function(dataset, filetype = "csv", sheet = 1, removeTotal = TRUE) {
  .Deprecated("See https://github.com/mattcowgill/readabs for reading ABS data.")
}
