#' Download Stats19 data
#'
#' @section Details:
#' This convenience function downloads and unzips UK road traffic casualty data.
#' It results in unzipped .csv data in R's temporary directory.
#'
#' Ensure you have a fast internet connection and at least 100 Mb space
#'
#' @param zip_url The url where the data is stored
#' @param data_dir Directory to which to download the file
#' @export
#' @examples
#' \dontrun{
#' dl_stats19()
#'
#' # Load all stats19 datasets
#' ac <- read_stats19_ac()
#' ca <- read_stats19_ca()
#' ve <- read_stats19_ve()
#' # now you can analyse the UK's stats19 data in a single table
#' }
dl_stats19 <- function(zip_url = paste0(
                         "http://data.dft.gov.uk.s3.amazonaws.com/",
                         "road-accidents-safety-data/Stats19_Data_2005-2014.zip"
                       ), data_dir = tempdir()) {

  # download and unzip the data if it's not present
  if (!"Accidents0514.csv" %in% list.files(data_dir)) {
    destfile <- file.path(data_dir, "Stats19_Data_2005-2014.zip")
    download.file(zip_url, destfile)
    unzip(destfile, exdir = data_dir)
  }

  print(paste0("Data saved at: ", list.files(data_dir,
    pattern = "csv", full.names = TRUE
  )))
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
read_stats19_ac <- function(data_dir = tempdir(), filename = "Accidents0514.csv") {
  if (!filename %in% list.files(data_dir)) {
    dl_stats19()
  }

  # read the data in
  ac <- readr::read_csv(file.path(data_dir, "Accidents0514.csv"), col_types = readr::cols(
    .default = readr::col_integer(),
    Accident_Index = readr::col_character(),
    Longitude = readr::col_double(),
    Latitude = readr::col_double(),
    Date = readr::col_character(),
    Time = readr::col_character(),
    `Local_Authority_(Highway)` = readr::col_character(),
    LSOA_of_Accident_Location = readr::col_character()
  ))
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
#' @param ac Dataframe representing the raw Stats19 data read-in with `read_csv()`.
#'
#' @export
#' @examples
#' \dontrun{
#' ac <- format_stats19_ac(ac)
#' }
format_stats19_ac <- function(ac) {
  ac$Accident_Severity <-
    factor(ac$Accident_Severity, labels = c("Fatal", "Serious", "Slight"))
  ac$Police_Force <-
    factor(ac$Police_Force,
      labels =
        c(
          "Metropolitan Police", "Cumbria", "Lancashire", "Merseyside",
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
        )
    )
  ac$`1st_Road_Class` <-
    factor(ac$`1st_Road_Class`,
      labels = c("Motorway", "A(M)", "A", "B", "C", "Unclassified")
    )
  ac$Road_Type <-
    factor(ac$Road_Type,
      labels = c(
        "Roundabout", "One way street", "Dual carriageway", "Single carriageway",
        "Slip road", "Unknown"
      )
    )
  ac$Junction_Detail <-
    factor(ac$Junction_Detail,
      labels =
        c(
          "Data missing or out of range", "Not at junction or within 20 metres",
          "Roundabout", "Mini-roundabout", "T or staggered junction", "Slip road",
          "Crossroads", "More than 4 arms (not roundabout)", "Private drive or entrance",
          "Other junction"
        )
    )
  ac$Light_Conditions <-
    factor(ac$Light_Conditions,
      labels = c(
        "Daylight", "Darkness - lights lit", "Darkness - lights unlit",
        "Darkness - no lighting", "Darkness - lighting unknown"
      )
    )
  ac$Weather_Conditions <-
    factor(ac$Weather_Conditions,
      labels = c(
        "Data missing or out of range", "Fine no high winds", "Raining no high winds",
        "Snowing no high winds", "Fine + high winds", "Raining + high winds",
        "Snowing + high winds", "Fog or mist", "Other", "Unknown"
      )
    )
  ac$Road_Surface_Conditions <-
    factor(ac$Road_Surface_Conditions,
      labels = c(
        "Data missing or out of range", "Dry", "Wet or damp", "Snow",
        "Frost or ice", "Flood over 3cm. deep"
      )
    )
  ac$Date <- lubridate::dmy(ac$Date)
  # barplot(table(lubridate::wday(ac$Date, label = TRUE)))

  names(ac)[1] <- "Accident_Index" # rename faulty index name

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
read_stats19_ve <- function(data_dir = tempdir(), filename = "Vehicles0514.csv") {
  if (!filename %in% list.files(data_dir)) {
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
#' @param ve Dataframe representing the raw Stats19 data read-in with `read_csv()`.
#' @export
#' @examples
#' \dontrun{
#' ve <- format_stats19_ve(ve)
#' }
format_stats19_ve <- function(ve) {
  ve$Vehicle_Type <- factor(ve$Vehicle_Type,
    labels = c(
      "Goods vehicle - unknown weight", "Pedal cycle", "Motorcycle 50cc and under",
      "Motorcycle 125cc and under", "Motorcycle over 125cc and up to 500cc",
      "Motorcycle over 500cc", "Taxi/Private hire car", "Car", "Minibus (8 - 16 passenger seats)",
      "Bus or coach (17 or more pass seats)", "Ridden horse", "Agricultural vehicle",
      "Tram", "Van / Goods 3.5 tonnes mgw or under", "Goods over 3.5t. and under 7.5t",
      "Goods 7.5 tonnes mgw and over", "Mobility scooter", "Electric motorcycle",
      "Other vehicle", "Motorcycle - unknown cc", "Data missing or out of range"
    )
  )
  # summary(ve$Vehicle_Type)
  ve$Vehicle_Manoeuvre <-
    factor(ve$Vehicle_Manoeuvre,
      labels = c(
        "Data missing or out of range", "Reversing", "Parked", "Waiting to go - held up",
        "Slowing or stopping", "Moving off", "U-turn", "Turning left",
        "Waiting to turn left", "Turning right", "Waiting to turn right",
        "Changing lane to left", "Changing lane to right", "Overtaking moving vehicle - offside",
        "Overtaking static vehicle - offside", "Overtaking - nearside",
        "Going ahead left-hand bend", "Going ahead right-hand bend",
        "Going ahead other"
      )
    )
  # summary(ve$Vehicle_Manoeuvre)
  ve$Journey_Purpose_of_Driver <-
    factor(ve$Journey_Purpose_of_Driver,
      labels = c(
        "Data missing or out of range", "Journey as part of work",
        "Commuting to/from work", "Taking pupil to/from school", "Pupil riding to/from school",
        "Other", "Not known", "Other/Not known (2005-10)"
      )
    )
  # summary(ve$Journey_Purpose_of_Driver)
  ve$Sex_of_Driver <- factor(ve$Sex_of_Driver,
    labels =
      c("Data missing or out of range", "Male", "Female", "Not known")
  )
  levels(ve$Sex_of_Driver)[1] <- levels(ve$Sex_of_Driver)[4]
  # summary(ve$Sex_of_Driver)

  ve$Age_Band_of_Driver <- factor(ve$Age_Band_of_Driver,
    labels = c(
      NA, "0 - 5", "6 - 10", "11 - 15", "16 - 20", "21 - 25", "26 - 35",
      "37 - 45", "46 - 55", "56 - 65", "66 - 75", "Over 75"
    )
  )

  ve$Driver_IMD_Decile <- factor(dplyr::inner_join(ve, structure(list(Driver_IMD_Decile = c(
    1, 2, 3, 4, 5, 6, 7, 8,
    9, 10, -1
  ), IMD_Decile = c(
    "Most deprived 10%", "More deprived 10-20%",
    "More deprived 20-30%", "More deprived 30-40%", "More deprived 40-50%",
    "Less deprived 40-50%", "Less deprived 30-40%", "Less deprived 20-30%",
    "Less deprived 10-20%", "Least deprived 10%", "Data missing or out of range"
  )), row.names = c(NA, -11L), class = "data.frame", .Names = c(
    "Driver_IMD_Decile",
    "IMD_Decile"
  )))$IMD_Decile)

  names(ve)[1] <- "Accident_Index" # rename faulty index name

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
read_stats19_ca <- function(data_dir = tempdir(), filename = "Casualties0514.csv") {
  if (!filename %in% list.files(data_dir)) {
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
#' @param ca Dataframe representing the raw Stats19 data read-in with `read_csv()`.
#' @export
#' @examples
#' \dontrun{
#' ca <- format_stats19_ca(ca)
#' }
format_stats19_ca <- function(ca) {

  # nrow(ca) / nrow(ac) # 1.3 casualties per incident: reasonable
  ca$Casualty_Class <- factor(ca$Casualty_Class,
    labels = c("Driver or rider", "Passenger", "Pedestrian")
  )

  ca$Sex_of_Casualty <- factor(ca$Sex_of_Casualty,
    labels = c("Data missing or out of range", "Male", "Female")
  )
  levels(ca$Sex_of_Casualty)[1] <- "Not known"

  ca$Age_Band_of_Casualty <- factor(ca$Age_Band_of_Casualty,
    labels = c(
      NA, "0 - 5", "6 - 10", "11 - 15", "16 - 20", "21 - 25", "26 - 35",
      "36 - 45", "46 - 55", "56 - 65", "66 - 75", "Over 75"
    )
  )

  summary(as.factor(ca$Casualty_Severity))
  ca$Casualty_Severity <- factor(ca$Casualty_Severity,
    labels = c("Fatal", "Serious", "Slight")
  )

  ca$Casualty_Type <- as.factor(ca$Casualty_Type)
  ca$Casualty_Type <- factor(ca$Casualty_Type,
    labels = c(
      "Pedestrian", "Cyclist", "Motorcycle 50cc and under rider or passenger",
      "Motorcycle 125cc and under rider or passenger", "Motorcycle over 125cc and up to 500cc rider or  passenger",
      "Motorcycle over 500cc rider or passenger", "Taxi/Private hire car occupant",
      "Car occupant", "Minibus (8 - 16 passenger seats) occupant",
      "Bus or coach occupant (17 or more pass seats)", "Horse rider",
      "Agricultural vehicle occupant", "Tram occupant", "Van / Goods vehicle (3.5 tonnes mgw or under) occupant",
      "Goods vehicle (over 3.5t. and under 7.5t.) occupant", "Goods vehicle (7.5 tonnes mgw and over) occupant",
      "Mobility scooter rider", "Electric motorcycle rider or passenger",
      "Other vehicle occupant", "Motorcycle - unknown cc rider or passenger",
      "Goods vehicle (unknown weight) occupant"
    )
  )

  names(ca)[1] <- "Accident_Index" # rename faulty index name


  ca
}
