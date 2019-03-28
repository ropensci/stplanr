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
#' @examples
#' data_dir <- system.file("extdata", package = "stplanr")
#' t1 <- read_table_builder(file.path(data_dir, "SA1Population.csv"))
#' t2 <- read_table_builder(file.path(data_dir, "SA1Population.xlsx"),
#'   filetype = "xlsx", sheet = 1, removeTotal = TRUE
#' )
#' sa1pop <- read.csv(file.path(data_dir, "SA1Population.csv"), header = FALSE)
#' t3 <- read_table_builder(sa1pop)
read_table_builder <- function(dataset, filetype = "csv", sheet = 1, removeTotal = TRUE) {
  if (missing(dataset)) {
    stop("Dataset is missing")
  }
  if (is.data.frame(dataset)) {
    tbfile <- dataset
  } else if (is.character(dataset)) {
    if (filetype == "xlsx") {
      tbfile <- openxlsx::readWorkbook(dataset, sheet = sheet, colNames = FALSE)
    } else {
      tbfile <- read.csv(dataset, header = FALSE)
    }
  } else {
    stop("Dataset not data.frame or character string")
  }
  if (is.null(tbfile) == TRUE) {
    stop("File could not be loaded")
  } else {
    if (filetype == "xlsx" | filetype == "legacycsv") {
      tbfile[tbfile == ""] <- NA
      tbfile <- tbfile[, which(!(colSums(is.na(tbfile)) == nrow(tbfile)))]
      if (is.na(tbfile[which(rowSums(is.na(tbfile[, 2:ncol(tbfile)])) == min(rowSums(is.na(tbfile[, 2:ncol(tbfile)])))), ][1, 1]) == TRUE) {
        tbfile[, 1] <- NULL
      }
      else {
        tbfile <- tbfile[which(rowSums(is.na(tbfile)) < (ncol(tbfile) - 1)), ]
      }
      tbfile <- tbfile[which(rowSums(is.na(tbfile)) != ncol(tbfile)), ]
      valuecols <- which(!is.na(tbfile[1, ]))
      valuecols <- valuecols[which(valuecols > 1)]
      valuecols <- valuecols[!valuecols %in% which(!is.na(tbfile[2, ]))]
      colnames(tbfile) <- c(as.character(unlist(unname(tbfile[2, which(!is.na(tbfile[2, ]))]))), as.character(unlist(unname(tbfile[1, valuecols]))))
      tbfile <- tbfile[3:nrow(tbfile), ]
      tbfile <- tbfile[which(rowSums(is.na(tbfile)) != ncol(tbfile) - 1), ]
      if (length(valuecols) > 1) {
        tbfile <- tbfile[which(!rowSums(is.na(tbfile[, valuecols])) == length(valuecols)), ]
      }
      else {
        tbfile <- tbfile[which(is.na(tbfile[, valuecols]) != TRUE), ]
      }
      i <- 1
      while (sum(is.na(tbfile[, i])) != 0) {
        tbfile[, i] <- rep(
          unique(tbfile[which(is.na(tbfile[, i]) == FALSE), i]),
          each = nrow(tbfile) / length(tbfile[which(is.na(tbfile[, i]) == FALSE), i]),
          times = length(tbfile[which(is.na(tbfile[, i]) == FALSE), i]) / length(unique(tbfile[which(is.na(tbfile[, i]) == FALSE), i]))
        )
        i <- i + 1
      }
      if (removeTotal == TRUE) {
        tbfile <- tbfile[, which(colnames(tbfile) != "Total")]
        tbfile <- tbfile[which(tbfile[, 1] != "Total"), ]
      }
      tbfile[valuecols[which(valuecols <= ncol(tbfile))]] <- sapply(tbfile[valuecols[which(valuecols <= ncol(tbfile))]], function(x) {
        as.numeric(as.character(x))
      })
      row.names(tbfile) <- NULL
    } else {
      colnamevals <- c(as.character(unname(unlist(tbfile[(min(which(is.na(tbfile[, ncol(tbfile)]) == FALSE)) - 1), 1:(ncol(tbfile) - 1)]))), "value")
      tbfile <- tbfile[which(is.na(tbfile[, ncol(tbfile)]) == FALSE), ]
      colnames(tbfile) <- colnamevals
      if (removeTotal == TRUE) {
        tbfile <- tbfile[apply(tbfile, 1, function(x) all(x != "Total")), ]
      }
      row.names(tbfile) <- NULL
      tbfile$value <- as.numeric(as.character(tbfile$value))
    }
  }
  return(tbfile)
}
