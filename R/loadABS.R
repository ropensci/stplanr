#' Import and format Australian Bureau of Statistics (ABS) TableBuilder files
#'
#' @section Details:
#' The Australian Bureau of Statistics (ABS) provides customised tables for
#' census and other datasets in a format that is difficult to use in R
#' because it contains rows with additional information.
#' This function imports the original (unzipped) TableBuilder files in .csv
#' or .xlsx format before creating an R dataframe with the data.
#'
#' @param dataset Either a dataframe containing the original data from
#' TableBuilder or a character string containing the path of the
#' unzipped TableBuilder file.
#' @param filetype A character string containing the filetype. Valid values
#' are 'csv' and 'xlsx' (default = 'csv'). Not used if dataset is a dataframe.
#' @param sheet An integer value containing the index of the sheet in the
#' xlsx file (default = 1).
#' @param removeTotal A boolean value. If TRUE removes the rows and columns
#' with totals (default = TRUE).
#' @import openxlsx
#' @export
#' @examples \dontrun{
#' readTableBuilder('employmentSLA.csv')
#' readTableBuilder('employmentSLA.xlsx',filetype='xlsx',sheet=1,removeTotal=TRUE)
#' employment <- read.csv('employmentSLA.csv')
#' readTableBuilder(employment)
#' }
readTableBuilder <- function(dataset, filetype="csv",sheet=1,removeTotal=TRUE) {
  if (missing(dataset)) {
    stop("Dataset is missing")
  }
  if (is.data.frame(dataset)) {
    tbfile <- dataset
  } else if (is.character(dataset)) {
    if (filetype=="xlsx") {
      tbfile <- openxlsx::readWorkbook(dataset,sheet = sheet, colNames = FALSE)
    } else {
      tbfile <- read.csv(dataset,header=FALSE)
    }
  } else {
    stop("Dataset not data.frame or character string")
  }
  if (is.null(tbfile) == TRUE) {
    stop("File could not be loaded")
  } else {
    tbfile[,1] <- NULL
    tbfile <- tbfile[which(rowSums(is.na(tbfile)) != ncol(tbfile)),]
    valuecols <- which(!is.na(tbfile[1,]))
    colnames(tbfile) <- unlist(c(tbfile[2,which(!is.na(tbfile[2,]))],tbfile[1,which(!is.na(tbfile[1,]))]))
    tbfile <- tbfile[3:nrow(tbfile),]
    tbfile <- tbfile[which(rowSums(is.na(tbfile)) != ncol(tbfile)-1),]
    i <- 1
    while (sum(is.na(tbfile[,i])) != 0) {
      tbfile[,i] <- rep(
        unique(tbfile[which(is.na(tbfile[,i])==FALSE),i]),
        each=nrow(tbfile)/length(tbfile[which(is.na(tbfile[,i])==FALSE),i]),
        times=length(tbfile[which(is.na(tbfile[,i])==FALSE),i])/length(unique(tbfile[which(is.na(tbfile[,i])==FALSE),i]))
      )
      i <- i + 1
    }
    if (removeTotal == TRUE) {
      tbfile <- tbfile[,which(colnames(tbfile) != "Total")]
      tbfile <- tbfile[which(tbfile[,1] != "Total"),]
    }
    tbfile[valuecols[which(valuecols <= ncol(tbfile))]] <- sapply(tbfile[valuecols[which(valuecols <= ncol(tbfile))]],as.character)
    tbfile[valuecols[which(valuecols <= ncol(tbfile))]] <- sapply(tbfile[valuecols[which(valuecols <= ncol(tbfile))]],as.numeric)
    row.names(tbfile) <- NULL
  }
  return(tbfile)
}