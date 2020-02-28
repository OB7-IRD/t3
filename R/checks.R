#' @name check_trip_id
#' @title Attribut "trip_id" verification
#' @param trip_id (character) Trip identification.
#' @description Check if the item "trip_id" have one unique class identified as character.
check_trip_id <- function(trip_id) {
  if (length(class(trip_id)) != 1 || class(trip_id) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"trip_id\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_wellplan_id
#' @title Attribut "wellplan_id" verification
#' @param wellplan_id (character) Wellplan identification.
#' @description Check if the item "wellplan_id" have one unique class identified as character.
check_wellplan_id <- function(wellplan_id) {
  if (length(class(wellplan_id)) != 1 || class(wellplan_id) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"wellplan_id\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_id
#' @title Attribut "sample_id" verification
#' @param sample_id (character) Sample identification.
#' @description Check if the item "sample_id" have one unique class identified as character.
check_sample_id <- function(sample_id) {
  if (length(class(sample_id)) != 1 || class(sample_id) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_id\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_well_id
#' @title Attribut "well_id" verification
#' @param well_id (character) Well identification.
#' @description Check if the item "well_id" have one unique class identified as character.
check_well_id <- function(well_id) {
  if (length(class(well_id)) != 1 || class(well_id) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"well_id\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_activity_id
#' @title Attribut "activity_id" verification
#' @param activity_id (character) Activity identification.
#' @description Check if the item "activity_id" have one unique class identified as character.
check_activity_id <- function(activity_id) {
  if (length(class(activity_id)) != 1 || class(activity_id) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"activity_id\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_elementarylanding_id
#' @title Attribut "elementarylanding_id" verification
#' @param elementarylanding_id (character) Elementary landing identification.
#' @description Check if the item "elementarylanding_id" have one unique class identified as character.
check_elementarylanding_id <- function(elementarylanding_id) {
  if (length(class(elementarylanding_id)) != 1 || class(elementarylanding_id) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"elementarylanding_id\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_elementarycatch_id
#' @title Attribut "elementarycatch_id" verification
#' @param elementarycatch_id (character) Elementary catch identification.
#' @description Check if the item "elementarycatch_id" have one unique class identified as character.
check_elementarycatch_id <- function(elementarycatch_id) {
  if (length(class(elementarycatch_id)) != 1 || class(elementarycatch_id) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"elementarycatch_id\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_sub_sample_id
#' @title Attribut "sub_sample_id" verification
#' @param sub_sample_id (integer) Sub sample identification.
#' @description Check if the item "sub_sample_id" have one unique class identified as integer.
check_sub_sample_id <- function(sub_sample_id) {
  if (length(class(sub_sample_id)) != 1 || class(sub_sample_id) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sub_sample_id\" argument\nclass integer expected\n",
        sep = "")
    stop()
  }
}

#' @name check_landing_category
#' @title Attribut "landing_category" verification
#' @param landing_category (integer) Landing category identification.
#' @description Check if the item "landing_category" have one unique class identified as integer.
check_landing_category <- function(landing_category) {
  if (length(class(landing_category)) != 1 || class(landing_category) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"landing_category\" argument\nclass integer expected\n",
        sep = "")
    stop()
  }
}

#' @name check_landing_category_name
#' @title Attribut "landing_category_name" verification
#' @param landing_category_name (character) Landing category name identification.
#' @description Check if the item "landing_category_name" have one unique class identified as character.
check_landing_category_name <- function(landing_category_name) {
  if (length(class(landing_category_name)) != 1 || class(landing_category_name) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"landing_category_name\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_landing_date
#' @title Attribut "landing_date" verification
#' @param landing_date (date and hours) Landing date in format ymd_hms UTC.
#' @description Check if the item "landing_date" is in date format ymd_hms and check if the value is inferior to the actual date.
#' @importFrom lubridate ymd
check_landing_date <- function(landing_date) {
  if (is.na(lubridate::ymd_hms(landing_date, quiet = TRUE, tz = "UTC"))) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"landing_date\" argument\nat least one item failed to parse with format ymd_hms\n",
        sep = "")
    stop()
  } else if (lubridate::ymd_hms(landing_date, quiet = TRUE, tz = "UTC") > as.POSIXct(Sys.time(), tz = "UTC")) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: at least one \"landing_date\" is superior to the actual date\n",
        sep = "")
    stop()
  }
}

#' @name check_activity_date
#' @title Attribut "activity_date" verification
#' @param activity_date (date) Activity date in format year month day.
#' @description Check if the item "activity_date" is in date format year month day, if the value is inferior to the actual date and if it is inferior to the landing_date (if there are provided).
#' @importFrom lubridate ymd
check_activity_date <- function(activity_date, landing_date = NULL) {
  if (is.na(lubridate::ymd(activity_date, quiet = TRUE))) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"activity_date\" argument\nAt least one item failed to parse with format ymd\n",
        sep = "")
    stop()
  } else if (lubridate::ymd(activity_date, quiet = TRUE) > Sys.Date()) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: at least one item \"activity_date\" is superior to the actual date\n",
        sep = "")
    stop()
  } else if (! is.null(landing_date)) {
    if (is.na(lubridate::ymd_hms(landing_date, quiet = TRUE, tz = "UTC"))) {
      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: invalide \"landing_date\" argument\nat least one item failed to parse with format ymd_hms\n",
          sep = "")
      stop()
    } else if (lubridate::ymd(activity_date, quiet = TRUE) > lubridate::ymd_hms(landing_date, quiet = TRUE, tz = "UTC")) {
      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: At least one \"activity_date\" is superior to \"landing_date\"\n",
          sep = "")
      stop()
    }
  }
}

#' @name check_departure_date
#' @title Attribut "departure_date" verification
#' @param departure_date (date and hours) Departure date in format year month day hour minute second.
#' @description Check if the item "departure_date" is in date format year month day hour minute second, if the value is inferior to the actual date and if it is inferior to the landing_date (if there are provided).
#' @importFrom lubridate ymd
check_departure_date <- function(departure_date, landing_date = NULL) {
  if (is.na(lubridate::ymd_hms(departure_date, quiet = TRUE, tz = "UTC"))) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"departure_date\" argument\nAt least one item failed to parse with format year month day hour minute second\n",
        sep = "")
    stop()
  } else if (lubridate::ymd_hms(departure_date, quiet = TRUE, tz = "UTC") > as.POSIXct(Sys.time(), tz = "UTC")) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: at least one item \"departure_date\" is superior to the actual date\n",
        sep = "")
    stop()
  } else if (! is.null(landing_date)) {
    if (is.na(lubridate::ymd_hms(landing_date, quiet = TRUE, tz = "UTC"))) {
      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: invalide \"landing_date\" argument\nat least one item failed to parse with format year month day hour minute second\n",
          sep = "")
      stop()
    } else if (lubridate::ymd_hms(departure_date, quiet = TRUE, tz = "UTC") > lubridate::ymd_hms(landing_date, quiet = TRUE, tz = "UTC")) {
      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: At least one \"departure_date\" is superior to \"landing_date\"\n",
          sep = "")
      stop()
    }
  }
}

#' @name check_logbook_availability
#' @title Attribut "logbook_availability" verification
#' @param logbook_availability (integer) Logbook availability value, 1 for available and 0 for not.
#' @description Check if the item "logbook_availability" is class integer and if all values are 1 or 0.
check_logbook_availability <- function(logbook_availability) {
  if (length(class(logbook_availability)) != 1 || class(logbook_availability) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"logbook_availability\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (! logbook_availability %in% c(0, 1)) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"logbook_availability\" argument\nvalue 0 or 1 expected\n",
        sep = "")
    stop()
  }
}

#' @name check_fish_hold_empty
#' @title Attribut "fish_hold_empty" verification
#' @param fish_hold_empty (integer) Informe if the fish hold empty at the end of the trip, 1 for yes and 0 for not.
#' @description Check if the item "fish_hold_empty" is class integer and if all values are 1 or 0.
check_fish_hold_empty <- function(fish_hold_empty) {
  if (length(class(fish_hold_empty)) != 1 || class(fish_hold_empty) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"fish_hold_empty\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (! fish_hold_empty %in% c(0, 1)) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"fish_hold_empty\" argument\nvalue 0 or 1 expected\n",
        sep = "")
    stop()
  }
}

#' @name check_vessel_id
#' @title Attribut "vessel_id" verification
#' @param vessel_id (integer) Vessel identification.
#' @description Check if the item "vessel_id" have one unique class identified as integer.
check_vessel_id <- function(vessel_id) {
  if (length(class(vessel_id)) != 1 || class(vessel_id) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"vessel_id\" argument\nclass integer expected\n",
        sep = "")
    stop()
  }
}

#' @name check_activity_number
#' @title Attribut "activity_number" verification
#' @param activity_number (integer) Activity number.
#' @description Check if the item "activity_number" have one unique class identified as integer.
check_activity_number <- function(activity_number) {
  if (length(class(activity_number)) != 1 || class(activity_number) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"activity_number\" argument\nclass integer expected\n",
        sep = "")
    stop()
  }
}

#' @name check_ocean
#' @title Attribut "ocean" verification
#' @param ocean (integer) Ocean identification.
#' @description Check if the item "ocean" is class integer and if all values are 1, 2, 3, 4 or 5.
check_ocean <- function(ocean) {
  if (length(class(ocean)) != 1 || class(ocean) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"ocean\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (! ocean %in% c(1, 2, 3, 4, 5)) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"ocean\" argument\nvalue 1, 2, 3, 4 or 5 expected\n",
        sep = "")
    stop()
  }
}

#' @name check_length_type
#' @title Attribut "length_type" verification
#' @param length_type (integer) Length type identification, 1 for LD1 and 2 for LF.
#' @description Check if the item "length_type" is class integer and if all values are 1 or 2.
check_length_type <- function(length_type) {
  if (length(class(length_type)) != 1 || class(length_type) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"length_type\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (! length_type %in% c(1, 2)) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"length_type\" argument\nvalue 1 or 2 expected\n",
        sep = "")
    stop()
  }
}

#' @name check_school_type
#' @title Attribut "school_type" verification
#' @param school_type (integer) School type identification.
#' @description Check if the item "school_type" is class integer and if all values are 1, 2, or 3.
check_school_type <- function(school_type) {
  if (length(class(school_type)) != 1 || class(school_type) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"school_type\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (! school_type %in% c(1, 2, 3)) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"school_type\" argument\nvalue 1, 2 or 3 expected\n",
        sep = "")
    stop()
  }
}

#' @name check_logbook_category
#' @title Attribut "logbook_category" verification
#' @param logbook_category (integer) Logbook weight category.
#' @description Check if the item "logbook_category" have one unique class identified as integer and if values are between 1 and 13.
check_logbook_category <- function(logbook_category) {
  if (length(class(logbook_category)) != 1 || class(logbook_category) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"logbook_category\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (! logbook_category %in% c(1:13)) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"logbook_category\" argument\nvalue from 1 to 13 expected\n",
        sep = "")
    stop()
  }
}

#' @name check_logbook_category_name
#' @title Attribut "logbook_category_name" verification
#' @param logbook_category_name (character) Logbook category name identification.
#' @description Check if the item "logbook_category_name" have one unique class identified as character.
check_logbook_category_name <- function(logbook_category_name) {
  if (length(class(logbook_category_name)) != 1 || class(logbook_category_name) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"logbook_category_name\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_specie_code3l
#' @title Attribut "specie_code3l" verification
#' @param specie_code3l (character) Specie code identification on 3 characters.
#' @description Check if the item "specie_code3l" have one unique class identified as character and 3 characters.
check_specie_code3l <- function(specie_code3l) {
  if (length(class(specie_code3l)) != 1 || class(specie_code3l) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"specie_code3l\" argument\nclass character expected\n",
        sep = "")
    stop()
  } else if (nchar(specie_code3l) != 3) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"specie_code3l\" argument\n3 character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_specie_code
#' @title Attribut "specie_code" verification
#' @param specie_code (integer) Specie code identification.
#' @description Check if the item "specie_code" have one unique class identified as integer and superior to zero.
check_specie_code <- function(specie_code) {
  if (length(class(specie_code)) != 1 || class(specie_code) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"specie_code\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (specie_code <= 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"specie_code\" argument\ninteger expected with value superior to zero\n",
        sep = "")
    stop()
  }
}

#' @name check_activity_name
#' @title Attribut "activity_name" verification
#' @param activity_name (character) Activity identification.
#' @description Check if the item "check_activity_name" have one unique class identified as character.
check_activity_name <- function(check_activity_name) {
  if (length(class(check_activity_name)) != 1 || class(check_activity_name) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"check_activity_name\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_fleet
#' @title Attribut "fleet" verification
#' @param fleet (character) Fleet identification.
#' @description Check if the item "fleet" have one unique class identified as character.
check_fleet <- function(fleet) {
  if (length(class(fleet)) != 1 || class(fleet) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"fleet\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_wellplan_weigth_category_label
#' @title Attribut "wellplan_weigth_category_label" verification
#' @param wellplan_weigth_category_label (character) Well plan weight category identification.
#' @description Check if the item "wellplan_weigth_category_label" have one unique class identified as character.
check_wellplan_weigth_category_label <- function(wellplan_weigth_category_label) {
  if (length(class(wellplan_weigth_category_label)) != 1 || class(wellplan_weigth_category_label) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"wellplan_weigth_category_label\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_activity_code
#' @title Attribut "activity_code" verification
#' @param activity_code (integer) Activity code identification.
#' @description Check if the item "activity_code" have one unique class identified as integer.
check_activity_code <- function(activity_code) {
  if (length(class(activity_code)) != 1 || class(activity_code) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"activity_code\" argument\nclass integer expected\n",
        sep = "")
    stop()
  }
}

#' @name check_wellplan_number
#' @title Attribut "wellplan_number" verification
#' @param wellplan_number (integer) Well plan number of individus.
#' @description Check if the item "wellplan_number" have one unique class identified as integer.
check_wellplan_number <- function(wellplan_number) {
  if (length(class(wellplan_number)) != 1 || class(wellplan_number) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"wellplan_number\" argument\nclass integer expected\n",
        sep = "")
    stop()
  }
}

#' @name check_catch_weight
#' @title Attribut "catch_weight" verification
#' @param catch_weight (numeric) Catch weight in tonnes.
#' @description Check if the item "catch_weight" have one unique class identified as numeric and if values are superior to zero.
check_catch_weight <- function(catch_weight) {
  if (length(class(catch_weight)) != 1 || class(catch_weight) != "numeric") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"catch_weight\" argument\nclass numeric expected\n",
        sep = "")
    stop()
  } else if (catch_weight <= 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"catch_weight\" argument\npositive value expected\n",
        sep = "")
    stop()
  }
}

#' @name check_well_minus10_weigth
#' @title Attribut "well_minus10_weigth" verification
#' @param well_minus10_weigth (integer) Catch weight of individus less than 10 tonnes (by well, in tonne, all species considerated).
#' @description Check if the item "well_minus10_weigth" have one unique class identified as integer and if value are >= 0.
check_well_minus10_weigth <- function(well_minus10_weigth) {
  if (length(class(well_minus10_weigth)) != 1 || class(well_minus10_weigth) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"well_minus10_weigth\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (well_minus10_weigth < 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"well_minus10_weigth\" argument\n0 or postive value expected\n",
        sep = "")
    stop()
  }
}

#' @name check_weighted_weight
#' @title Attribut "weighted_weight" verification
#' @param weighted_weight (numeric) Set weight weighted by all set in the well(s).
#' @description Check if the item "weighted_weight" have one unique class identified as numeric and if value are >= 0.
check_weighted_weight <- function(weighted_weight) {
  if (length(class(weighted_weight)) != 1 || class(weighted_weight) != "numeric") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"weighted_weight\" argument\nclass numeric expected\n",
        sep = "")
    stop()
  } else if (weighted_weight <= 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"weighted_weight\" argument\npostive value expected\n",
        sep = "")
    stop()
  }
}

#' @name check_well_plus10_weigth
#' @title Attribut "well_plus10_weigth" verification
#' @param well_plus10_weigth (integer) Catch weight of individus more than 10 tonnes (by well, in tonne, all species considerated).
#' @description Check if the item "well_plus10_weigth" have one unique class identified as integer and if value are >= 0.
check_well_plus10_weigth <- function(well_plus10_weigth) {
  if (length(class(well_plus10_weigth)) != 1 || class(well_plus10_weigth) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"well_plus10_weigth\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (well_plus10_weigth < 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"well_plus10_weigth\" argument\n0 or postive value expected\n",
        sep = "")
    stop()
  }
}

#' @name check_well_global_weigth
#' @title Attribut "well_global_weigth" verification
#' @param well_global_weigth (integer) Catch weight of individus (less and more 10 tonnes categories, by well, in tonne, all species considerated).
#' @description Check if the item "well_global_weigth" have one unique class identified as integer and if value are >= 0.
check_well_global_weigth <- function(well_global_weigth) {
  if (length(class(well_global_weigth)) != 1 || class(well_global_weigth) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"well_global_weigth\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (well_global_weigth < 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"well_global_weigth\" argument\n0 or postive value expected\n",
        sep = "")
    stop()
  }
}

#' @name check_landing_weight
#' @title Attribut "landing_weight" verification
#' @param landing_weight (numeric) Landing weight in tonnes.
#' @description Check if the item "landing_weight" have one unique class identified as numeric.
check_landing_weight <- function(landing_weight) {
  if (length(class(landing_weight)) != 1 || class(landing_weight) != "numeric") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"landing_weight\" argument\nclass numeric expected\n",
        sep = "")
    stop()
  }
}

#' @name check_set_count
#' @title Attribut "set_count" verification
#' @param set_count (integer) Number of set associated to the activity.
#' @description Check if the item "set_count" have one unique class identified as integer and if value are superior or equal to 0
check_set_count <- function(set_count) {
  if (length(class(set_count)) != 1 || class(set_count) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"set_count\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (set_count < 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"set_count\" argument\n0 or postive value expected\n",
        sep = "")
    stop()
  }
}

#' @name check_time_at_sea
#' @title Attribut "time_at_sea" verification
#' @param time_at_sea (integer) Time at sea in hours.
#' @description Check if the item "time_at_sea" have one unique class identified as integer and if all values are superior or egal to zero.
check_time_at_sea <- function(time_at_sea) {
  if (length(class(time_at_sea)) != 1 || class(time_at_sea) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"time_at_sea\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (time_at_sea < 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"time_at_sea\" argument\n0 or postive value expected\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_total_count
#' @title Attribut "sample_total_count" verification
#' @param sample_total_count (integer) Sample number of total individus counted.
#' @description Check if the item "sample_total_count" have one unique class identified as integer and if all values are positive and different of zero.
check_sample_total_count <- function(sample_total_count) {
  if (length(class(sample_total_count)) != 1 || class(sample_total_count) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_total_count\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (sample_total_count <= 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_total_count\" argument\npostive value (without zero) expected\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_number_measured
#' @title Attribut "sample_number_measured" verification
#' @param sample_number_measured (integer) Sample number of measured individus.
#' @description Check if the item "sample_number_measured" have one unique class identified as integer and if all values are positive and different of zero.
check_sample_number_measured <- function(sample_number_measured) {
  if (length(class(sample_number_measured)) != 1 || class(sample_number_measured) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_number_measured\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (sample_number_measured <= 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_number_measured\" argument\npostive value (without zero) expected\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_number_measured_extrapolated_lf
#' @title Attribut "sample_number_measured_extrapolated_lf" verification
#' @param sample_number_measured_extrapolated_lf (numeric) Sample number of measured individus extrapolated to all counted individus.
#' @description Check if the item "sample_number_measured_extrapolated_lf" have one unique class identified as numeric and if all values are positive and different of zero.
check_sample_number_measured_extrapolated_lf <- function(sample_number_measured_extrapolated_lf) {
  if (length(class(sample_number_measured_extrapolated_lf)) != 1 || class(sample_number_measured_extrapolated_lf) != "numeric") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_number_measured_extrapolated_lf\" argument\nclass numeric expected\n",
        sep = "")
    stop()
  } else if (sample_number_measured_extrapolated_lf <= 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_number_measured_extrapolated_lf\" argument\npostive value (without zero) expected\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_number_weighted
#' @title Attribut "sample_number_weighted" verification
#' @param sample_number_weighted (numeric) Sample number of measured individus extrapolated to all counted individus and weighted by set weight.
#' @description Check if the item "sample_number_weighted" have one unique class identified as numeric and if all values are positive and different of zero.
check_sample_number_weighted <- function(sample_number_weighted) {
  if (length(class(sample_number_weighted)) != 1 || class(sample_number_weighted) != "numeric") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_number_weighted\" argument\nclass numeric expected\n",
        sep = "")
    stop()
  } else if (sample_number_weighted <= 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_number_weighted\" argument\npostive value (without zero) expected\n",
        sep = "")
    stop()
  }
}

#' @name check_wellplan_weight
#' @title Attribut "wellplan_weight" verification
#' @param wellplan_weight (numeric) Weight in tonnes filled in the well plan.
#' @description Check if the item "wellplan_weight" have one unique class identified as numeric and if all values are positive and different of zero.
check_wellplan_weight <- function(wellplan_weight) {
  if (length(class(wellplan_weight)) != 1 || class(wellplan_weight) != "numeric") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"wellplan_weight\" argument\nclass numeric expected\n",
        sep = "")
    stop()
  } else if (wellplan_weight <= 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"wellplan_weight\" argument\npostive value (without zero) expected\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_length_class
#' @title Attribut "sample_length_class" verification
#' @param sample_length_class (integer) Sample length class of measured individus.
#' @description Check if the item "sample_length_class" have one unique class identified as integer and if all values are positive and different of zero.
check_sample_length_class <- function(sample_length_class) {
  if (length(class(sample_length_class)) != 1 || class(sample_length_class) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_length_class\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (sample_length_class <= 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_length_class\" argument\npostive value (without zero) expected\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_quality
#' @title Attribut "sample_quality" verification
#' @param sample_quality (integer) Sample quality identification.
#' @description Check if the item "sample_quality" have one unique class identified as integer and if all values are 1, 2, 3, 4 or 9.
check_sample_quality <- function(sample_quality) {
  if (length(class(sample_quality)) != 1 || class(sample_quality) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_quality\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (! sample_quality %in% c(1, 2, 3, 4, 9)) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_quality\" argument\nvalue 1, 2, 3, 4 or 9 expected\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_type
#' @title Attribut "sample_type" verification
#' @param sample_type (integer) Sample type identification.
#' @description Check if the item "sample_type" have one unique class identified as integer and if all values are 1, 2, 3, 4, 9 or 11.
check_sample_type <- function(sample_type) {
  if (length(class(sample_type)) != 1 || class(sample_type) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_type\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (! sample_type %in% c(1, 2, 3, 4, 9, 11)) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_type\" argument\nvalue 1, 2, 3, 4, 9 or 11 expected\n",
        sep = "")
    stop()
  }
}

#' @name check_wellplan_weigth_category_code
#' @title Attribut "wellplan_weigth_category_code" verification
#' @param wellplan_weigth_category_code (integer) Well plan category code identification.
#' @description Check if the item "wellplan_weigth_category_code" have one unique class identified as integer and if all values are 1, 2, 8 or 9.
check_wellplan_weigth_category_code <- function(wellplan_weigth_category_code) {
  if (length(class(wellplan_weigth_category_code)) != 1 || class(wellplan_weigth_category_code) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"wellplan_weigth_category_code\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (! wellplan_weigth_category_code %in% c(1, 2, 8, 9)) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"wellplan_weigth_category_code\" argument\nvalue 1, 2, 8 or 9 expected\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_standardised_length_class_lf
#' @title Attribut "sample_standardised_length_class_lf" verification
#' @param sample_standardised_length_class_lf (integer) Sample standardised length class length fork of measured individus.
#' @description Check if the item "sample_standardised_length_class_lf" have one unique class identified as integer and if all values are positive and different of zero.
check_sample_standardised_length_class_lf <- function(sample_standardised_length_class_lf) {
  if (length(class(sample_standardised_length_class_lf)) != 1 || class(sample_standardised_length_class_lf) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_standardised_length_class_lf\" argument\nclass integer expected\n",
        sep = "")
    stop()
  } else if (sample_standardised_length_class_lf < 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_standardised_length_class_lf\" argument\npostive value expected\n",
        sep = "")
    stop()
  }
}

#' @name check_activity_longitude
#' @title Attribut "activity_longitude" verification
#' @param activity_longitude (numeric) Longitude, in decimal degree, of the activity.
#' @description Check if the item "activity_longitude" have one unique class identified as numeric.
check_activity_longitude <- function(activity_longitude) {
  if (length(class(activity_longitude)) != 1 || class(activity_longitude) != "numeric") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"activity_longitude\" argument\nclass numeric expected\n",
        sep = "")
    stop()
  }
}

#' @name check_activity_latitude
#' @title Attribut "activity_latitude" verification
#' @param activity_latitude (numeric) Latitude, in decimal degree, of the activity.
#' @description Check if the item "activity_latitude" have one unique class identified as numeric.
check_activity_latitude <- function(activity_latitude) {
  if (length(class(activity_latitude)) != 1 || class(activity_latitude) != "numeric") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"activity_latitude\" argument\nclass numeric expected\n",
        sep = "")
    stop()
  }
}
