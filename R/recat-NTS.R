#' Recategorises UK Census age bands.
#'
#' \code{age_recat}, \code{age_recat2} and \code{disab_recat}
#' are used to re-categorise the National Travel Survey age
#' band and disability variables for comparison with other datasets.
#'
#' @param a The age variable supplied by the UK's NTS.
#' @return A factor with new age categories.
#'
#' @examples
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
age_recat2 <- function(a){
  c2 <- factor(rep(NA, length(c)), levels = c("0-16", "17-20", "21-29", "30-39","40-49",
    "50-59", "60-69", "70+"))
  c2[c == "Less than 1 year" | c == "1 - 2 years" | c == "3 - 4 years"| c == "5 - 10 years" | c == "11 - 15 years"| c == "16 years"] <- "0-16"
  c2[c == "17 years" | c == "18 years" | c == "19 years"| c == "20 years"] <- "17-20"
  c2[c == "21 - 25 years" | c == "26 - 29 years"] <- "21-29"
  c2[c == "30 - 39 years"] <- "30-39"
  c2[c == "40 - 49 years"] <- "40-49"
  c2[c == "50 - 59 years"] <- "50-59"
  c2[c == "60 - 64 years" | c == "65 - 69 years"] <- "60-69"
  c2[c == "70 - 74 years" | c == "75 - 79 years" | c == "80 - 84 years"| c == "85 years +"] <- "70+" # all people over 70...
  c2
}

#' @rdname age_recat
disab_recat <- function(a){
  b2 <- factor(rep(NA, length(b)), levels = c("Yes", "No"))
  b2[a == "DNA" | a == "No difficulties"] <- "No"
  b2[a == "NA"] <- "No"
  b2[grepl("Foot|Bus|Car", a, ignore.case = T)] <- "Yes"
  b2
}