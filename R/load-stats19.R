#' Download Stats19 data
#'
#' @section Details:
#' This convenience function downloads and unzips UK road traffic casualty data.
#' It results in unzipped .csv data in R's temporary directory.
#'
#' Ensure you have a fast internet connection and at least 100 Mb space
#'
#' @param zip_url The url where the data is stored
#' @export
#' @examples
#' \dontrun{
#' dl_stats19()
#'
#' # Load all stats19 datasets
#' ac <- read_stats19_ac()
#' ca <- read_stats19_ca()
#' ve <- read_stats19_ve()
#'
#' merge_stats19 <- function(ca, ve, ac){
#' all_stats19 <- dplyr::inner_join(ve, ca)
#' all_stats19 <- dplyr::inner_join(all_stats19, ac)
#' }
#' }
dl_stats19 <- function(zip_url = paste0("http://data.dft.gov.uk.s3.amazonaws.com/",
  "road-accidents-safety-data/Stats19_Data_2005-2014.zip")){

  # download and unzip the data if it's not present
  if(!"Accidents0514.csv" %in% list.files(tempdir())){
    data_dir <- tempdir()
    destfile <- file.path(data_dir, "Stats19_Data_2005-2014.zip")
    downloader::download(zip_url, destfile)
    unzip(destfile, exdir = data_dir)
  }

  list.files(tempdir())
  print(paste0("Data saved at: ", list.files(tempdir(),
              pattern = "csv", full.names = TRUE)))

}

#' Import and format UK 'Stats19' road traffic casualty data
#'
#' @section Details:
#' This is a wrapper function to access and load stats 19 data in a user-friendly way.
#' The function returns a data frame, in which each record is a reported incident in the
#' stats19 dataset.
#'
#' Ensure you have a fast internet connection and at least 100 Mb space.
#'
#' @param data_dir Character string representing where the data is stored.
#' If empty, R will attempt to download and unzip the data for you.
#' @param filename Character string of the filename of the .csv to read in - default values
#' are those downloaded from the UK Department for Transport (DfT).
#'
#' @export
#' @examples
#' \dontrun{
#' ac <- read_stats19_ac()
#' }
read_stats19_ac <- function(data_dir = tempdir(), filename = "Accidents0514.csv"){
  if(!filename %in% list.files(data_dir)){
    dl_stats19()
  }

  # read the data in
  ac <- readr::read_csv(file.path(data_dir, "Accidents0514.csv"))
#   ve <- readr::read_csv(file.path(data_dir, "Vehicles0514.csv"))
#   ca <- readr::read_csv(file.path(data_dir, "Casualties0514.csv"))

  # format ac data
  ac <- format_stats19_ac(ac)

  ac

}

#' Format UK 'Stats19' road traffic casualty data
#'
#' @section Details:
#' This is a helper function to format raw stats19 data
#'
#' @param ac Dataframe representing the raw Stats19 data read-in with \code{read_csv()}.
#' @param wb List of dataframes containing data labels for stats19 data
#'
#' @export
#' @examples
#' \dontrun{
#' ac <- format_stats19_ac(ac)
#' }
format_stats19_ac <- function(ac){

  data("wb")

  ac$Accident_Severity <-
    factor(ac$Accident_Severity, labels = c("Fatal", "Serious", "Slight"))
  ac$Police_Force <-
    factor(ac$Police_Force,
           labels =
             c("Metropolitan Police", "Cumbria", "Lancashire", "Merseyside",
               "Greater Manchester", "Cheshire", "Northumbria", "Durham", "North Yorkshire",
               "West Yorkshire", "South Yorkshire", "Humberside", "Cleveland",
               "West Midlands", "Staffordshire", "West Mercia", "Warwickshire",
               "Derbyshire", "Nottinghamshire", "Lincolnshire", "Leicestershire",
               "Northamptonshire", "Cambridgeshire", "Norfolk", "Suffolk", "Bedfordshire",
               "Hertfordshire", "Essex", "Thames Valley", "Hampshire", "Surrey",
               "Kent", "Sussex", "City of London", "Devon and Cornwall", "Avon and Somerset",
               "Gloucestershire", "Wiltshire", "Dorset", "North Wales", "Gwent",
               "South Wales", "Dyfed-Powys", "Northern", "Grampian", "Tayside",
               "Fife", "Lothian and Borders", "Central", "Strathclyde", "Dumfries and Galloway"
             ))
  ac$`1st_Road_Class` <-
    factor(ac$`1st_Road_Class`,
           labels = c("Motorway", "A(M)", "A", "B", "C", "Unclassified"))
  ac$Road_Type <-
    factor(ac$Road_Type,
           labels = c("Roundabout", "One way street", "Dual carriageway", "Single carriageway",
                                    "Slip road", "Unknown"))
  ac$Junction_Detail <-
    factor(ac$Junction_Detail, labels =
             c("Data missing or out of range", "Not at junction or within 20 metres",
               "Roundabout", "Mini-roundabout", "T or staggered junction", "Slip road",
               "Crossroads", "More than 4 arms (not roundabout)", "Private drive or entrance",
               "Other junction"))
  ac$Light_Conditions <-
    factor(ac$Light_Conditions,
           labels = c("Daylight", "Darkness - lights lit", "Darkness - lights unlit",
                      "Darkness - no lighting", "Darkness - lighting unknown"))
  ac$Weather_Conditions <-
    factor(ac$Weather_Conditions,
           labels = c("Data missing or out of range", "Fine no high winds", "Raining no high winds",
                      "Snowing no high winds", "Fine + high winds", "Raining + high winds",
                      "Snowing + high winds", "Fog or mist", "Other", "Unknown"))
  ac$Road_Surface_Conditions <-
    factor(ac$Road_Surface_Conditions,
           labels = c("Data missing or out of range", "Dry", "Wet or damp", "Snow",
                      "Frost or ice", "Flood over 3cm. deep"))
  ac$Time <-
    lubridate::hm(ac$Time)
  # hist(ac$Time@hour) # verify times
  ac$Date <- lubridate::dmy(ac$Date)
  # barplot(table(lubridate::wday(ac$Date, label = TRUE)))

  ac

}

#' Import and format UK 'Stats19' road traffic casualty data
#'
#' @section Details:
#' This is a wrapper function to access and load stats 19 data in a user-friendly way.
#' The function returns a data frame, in which each record is a reported incident in the
#' stats19 dataset.
#'
#' Ensure you have a fast internet connection and at least 100 Mb space.
#'
#' @inheritParams read_stats19_ac
#' @export
#' @examples
#' \dontrun{
#' ve <- read_stats19_ve()
#' }
read_stats19_ve <- function(data_dir = tempdir(), filename = "Vehicles0514.csv"){
  if(!filename %in% list.files(data_dir)){
    dl_stats19()
  }

  # read the data in
  #   ac <- readr::read_csv(file.path(data_dir, "Accidents0514.csv"))
  ve <- readr::read_csv(file.path(data_dir, "Vehicles0514.csv"))
  #   ca <- readr::read_csv(file.path(data_dir, "Casualties0514.csv"))

  # format ac data
  ve <- format_stats19_ve(ve)

  ve

}

#' Format UK 'Stats19' road traffic casualty data
#'
#' @section Details:
#' This is a helper function to format raw stats19 data
#'
#' @param ve Dataframe representing the raw Stats19 data read-in with \code{read_csv()}.
#' @param wb List of dataframes containing data labels for stats19 data
#' @export
#' @examples
#' \dontrun{
#' ve <- format_stats19_ve(ve)
#' }
format_stats19_ve <- function(ve, wb){

  data("wb")

  tfact <-
    wb$Vehicle.Type$label[ as.character(wb$Vehicle.Type$code) %in%
                             levels(as.factor(ve$Vehicle_Type)) ]
  ve$Vehicle_Type <- factor(ve$Vehicle_Type,
                          labels=tfact[c(20, 1:19, 21)])
  # summary(ve$Vehicle_Type)
  ve$Vehicle_Manoeuvre <-
    factor(ve$Vehicle_Manoeuvre, labels = wb$Vehicle.Manoeuvre$label[c(19,1:18)])
  # summary(ve$Vehicle_Manoeuvre)
  ve$Journey_Purpose_of_Driver <-
    factor(ve$Journey_Purpose_of_Driver, labels = wb$Journey.Purpose$label[c(8,1:7)])
  # summary(ve$Journey_Purpose_of_Driver)
  ve$Sex_of_Driver <- factor(ve$Sex_of_Driver , labels = wb$Sex.of.Driver$label[c(4,1:3)])
  levels(ve$Sex_Driver_f)[1] <- levels(ve$Sex_Driver_f)[4]
  summary(ve$Sex_of_Driver)

  wb$IMD.Decile <- dplyr::rename(wb$IMD.Decile, Driver_IMD_Decile = code, IMD_Decile = label)

  ve$Driver_IMD_Decile <- factor(dplyr::inner_join(ve, wb$IMD.Decile)$IMD_Decile)

  ve

}

#' Import and format UK 'Stats19' road traffic casualty data
#'
#' @section Details:
#' This is a wrapper function to access and load stats 19 data in a user-friendly way.
#' The function returns a data frame, in which each record is a reported incident in the
#' stats19 dataset.
#'
#' Ensure you have a fast internet connection and at least 100 Mb space.
#'
#' @inheritParams read_stats19_ac
#' @export
#' @examples
#' \dontrun{
#' ca <- read_stats19_ca()
#' }
read_stats19_ca <- function(data_dir = tempdir(), filename = "Casualties0514.csv"){
  if(!filename %in% list.files(data_dir)){
    dl_stats19()
  }

  # read the data in
  #   ac <- readr::read_csv(file.path(data_dir, "Accidents0514.csv"))
  #   ve <- readr::read_csv(file.path(data_dir, "Vehicles0514.csv"))
  ca <- readr::read_csv(file.path(data_dir, "Casualties0514.csv"))

  # format ca data
  ca <- format_stats19_ca(ca)

  ca

}

#' Format UK 'Stats19' road traffic casualty data
#'
#' @section Details:
#' This is a helper function to format raw stats19 data
#'
#' @param ca Dataframe representing the raw Stats19 data read-in with \code{read_csv()}.
#' @param wb List of dataframes containing data labels for stats19 data
#' @export
#' @examples
#' \dontrun{
#' ca <- format_stats19_ca(ca)
#' }
format_stats19_ca <- function(ca, wb){

  data("wb")

  # nrow(ca) / nrow(ac) # 1.3 casualties per incident: reasonable
  ca$Casualty_Class <- factor(ca$Casualty_Class, labels = wb$Casualty.Class$label)

  ca$Sex_of_Casualty <- factor(ca$Sex_of_Casualty, labels = wb$Sex.of.Casualty$label[c(3,1,2)])
  levels(ca$Sex_of_Casualty)[1] <- "Not known"

  ca$Age_Band_of_Casualty <- factor(ca$Age_Band_of_Casualty, labels = c("na", wb$Age.Band$label[c(1:11)]))

  summary(as.factor(ca$Casualty_Severity))
  ca$Casualty_Severity <- factor(ca$Casualty_Severity, labels = wb$Casualty.Severity$label)

  ca$Casualty_Type <- as.factor(ca$Casualty_Type)
  ca_type_labs <-
    wb$Casualty.Type$label[match(levels(ca$Casualty_Type), (wb$Casualty.Type$code))]
  ca$Type <- factor(ca$Casualty_Type, labels = ca_type_labs)

  ca

}

