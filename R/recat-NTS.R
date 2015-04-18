#' Recategorise UK National Travel Survey factor variables
#'
#' \code{age_recat}, \code{age_recat2} and \code{disab_recat}
#' are used to re-categorise the UK National Travel Survey (NTS) age
#' band and disability variables for comparison with other datasets.
#' See \url{http://discover.ukdataservice.ac.uk/series/?sn=2000037}
#' for more on accessing the NTS.
#'
#' See Crawford and Lovelace (2015) for an application of these functions.
#'
#' @param a The factor variable supplied by the UK's NTS
#' @return A factor with new categories for analysis
#'
#' @references
#'
#' Crawford, F., and Lovelace, R. (2015). The benefits of getting England cycling. Retrieved from http://www.ctc.org.uk/news/20150120-research-shows-growth-cycling-worth-%C2%BC-trillion-england%E2%80%99s-economy
#'
#' @export
#' @examples
#'
#'
#' age_recat("70 + years")
age_recat <- function(a){
    a2 <- factor(rep(NA, length(a)), levels = c("0-14", "15-29", "30-44", "45-59", "60-69", "70-79", "80+"))
    a2[a == "0 - 4 years" | a == "5 - 10 years" | a == "11 - 15 years"] <- "0-14"
    a2[a == "16 - 19 years" | a == "20 - 29 years"] <- "15-29"
    a2[a == "30 - 39 years" ] <- "30-44"
    f40_t49 <- which(a == "40 - 49 years") # we need to split this variable in two
    o30_44 <- sample(f40_t49, size = length(f40_t49) / 2)
    o45_59 <- f40_t49[!f40_t49 %in% o30_44]
    a2[o30_44] <- "30-44"
    a2[o45_59] <- "45-59"
    a2[a == "50 - 59 years" ] <- "45-59"
    a2[a == "60 - 69 years" ] <- "60-69"
    a2[a == "70 + years" ] <- "70-79" # all people over 70...
    over70 <- which(a == "70 + years")
    over80 <- sample(over70, size = (length(over70) * 0.397))
    a2[over80] <- "80+" # all people over 70...
  a2
}

#' @rdname age_recat
#' @export
age_recat2 <- function(a){
  a2 <- factor(rep(NA, length(a)), levels = c("0-16", "17-20", "21-29", "30-39","40-49",
    "50-59", "60-69", "70+"))
  a2[a == "Less than 1 year" | a == "1 - 2 years" | a == "3 - 4 years"| a == "5 - 10 years" | a == "11 - 15 years"| a == "16 years"] <- "0-16"
  a2[a == "17 years" | a == "18 years" | a == "19 years"| a == "20 years"] <- "17-20"
  a2[a == "21 - 25 years" | a == "26 - 29 years"] <- "21-29"
  a2[a == "30 - 39 years"] <- "30-39"
  a2[a == "40 - 49 years"] <- "40-49"
  a2[a == "50 - 59 years"] <- "50-59"
  a2[a == "60 - 64 years" | a == "65 - 69 years"] <- "60-69"
  a2[a == "70 - 74 years" | a == "75 - 79 years" | a == "80 - 84 years"| a == "85 years +"] <- "70+" # all people over 70...
  a2
}

#' @rdname age_recat
#' @export
disab_recat <- function(a){
  a2 <- factor(rep(NA, length(a)), levels = c("Yes", "No"))
  a2[a == "DNA" | a == "No difficulties"] <- "No"
  a2[a == "NA"] <- "No"
  a2[grepl("Foot|Bus|Car", a, ignore.case = T)] <- "Yes"
  a2
}