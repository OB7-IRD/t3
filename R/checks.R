#' @name check_trip_id
#' @title Attribute "trip_id" verification
#' @param trip_id Object of class {\link[base]{character}} expected. Trip identification.
#' @description Check if the item "trip_id" have one unique class identified as character.
check_trip_id <- function(trip_id) {
  if (length(x = class(x = trip_id)) != 1
      || class(x = trip_id) != "character") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"trip_id\" argument, class character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_wellplan_id
#' @title Attribute "wellplan_id" verification
#' @param wellplan_id Object of class {\link[base]{character}} expected. Wellplan identification.
#' @description Check if the item "wellplan_id" have one unique class identified as character.
check_wellplan_id <- function(wellplan_id) {
  if (length(x = class(x = wellplan_id)) != 1
      || class(x = wellplan_id) != "character") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"wellplan_id\" argument, class character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_id
#' @title Attribute "sample_id" verification
#' @param sample_id Object of class {\link[base]{character}} expected. Sample identification.
#' @description Check if the item "sample_id" have one unique class identified as character.
check_sample_id <- function(sample_id) {
  if (length(x = class(x = sample_id)) != 1
      || class(x = sample_id) != "character") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_id\" argument, class character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_elementarysampleraw_id
#' @title Attribute "elementarysampleraw_id" verification
#' @param elementarysampleraw_id Object of class {\link[base]{character}} expected. Elementary sample raw identification.
#' @description Check if the item "elementarysampleraw_id" have one unique class identified as character.
check_elementarysampleraw_id <- function(elementarysampleraw_id) {
  if (length(x = class(x = elementarysampleraw_id)) != 1
      || class(x = elementarysampleraw_id) != "character") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"elementarysampleraw_id\" argument, class character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_well_id
#' @title Attribute "well_id" verification
#' @param well_id Object of class {\link[base]{character}} expected. Well identification.
#' @description Check if the item "well_id" have one unique class identified as character.
check_well_id <- function(well_id) {
  if (length(x = class(x = well_id)) != 1
      || class(x = well_id) != "character") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"well_id\" argument, class character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_activity_id
#' @title Attribute "activity_id" verification
#' @param activity_id Object of class {\link[base]{character}} expected. Activity identification.
#' @description Check if the item "activity_id" have one unique class identified as character.
check_activity_id <- function(activity_id) {
  if (length(x = class(x = activity_id)) != 1
      || class(x = activity_id) != "character") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"activity_id\" argument, class character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_elementarylanding_id
#' @title Attribute "elementarylanding_id" verification
#' @param elementarylanding_id Object of class {\link[base]{character}} expected. Elementary landing identification.
#' @description Check if the item "elementarylanding_id" have one unique class identified as character.
check_elementarylanding_id <- function(elementarylanding_id) {
  if (length(x = class(x = elementarylanding_id)) != 1
      || class(x = elementarylanding_id) != "character") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"elementarylanding_id\" argument, class character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_elementarycatch_id
#' @title Attribute "elementarycatch_id" verification
#' @param elementarycatch_id Object of class {\link[base]{character}} expected. Elementary catch identification.
#' @description Check if the item "elementarycatch_id" have one unique class identified as character.
check_elementarycatch_id <- function(elementarycatch_id) {
  if (length(x = class(x = elementarycatch_id)) != 1
      || class(x = elementarycatch_id) != "character") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"elementarycatch_id\" argument, class character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_sub_sample_id
#' @title Attribute "sub_sample_id" verification
#' @param sub_sample_id Object of class {\link[base]{integer}} expected. Sub sample identification.
#' @description Check if the item "sub_sample_id" have one unique class identified as integer.
check_sub_sample_id <- function(sub_sample_id) {
  if (length(x = class(x = sub_sample_id)) != 1
      || class(x = sub_sample_id) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sub_sample_id\" argument, class integer expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_sub_sample_id_total_count
#' @title Attribute "sub_sample_id_total_count" verification
#' @param sub_sample_id_total_count Object of class {\link[base]{character}} expected. Sub sample identification bis in relation with the fish total count.
#' @description Check if the item "sub_sample_id_total_count" have one unique class identified as character.
check_sub_sample_id_total_count <- function(sub_sample_id_total_count) {
  if (length(x = class(x = sub_sample_id_total_count)) != 1
      || class(x = sub_sample_id_total_count) != "character") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sub_sample_id_total_count\" argument, class character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_landing_category
#' @title Attribute "landing_category" verification
#' @param landing_category Object of class {\link[base]{character}} expected. Landing category identification.
#' @description Check if the item "landing_category" have one unique class identified as integer.
check_landing_category <- function(landing_category) {
  if (length(x = class(x = landing_category)) != 1
      || class(x = landing_category) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"landing_category\" argument, class integer expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_landing_category_name
#' @title Attribute "landing_category_name" verification
#' @param landing_category_name Object of class {\link[base]{character}} expected. Landing category name identification.
#' @description Check if the item "landing_category_name" have one unique class identified as character.
check_landing_category_name <- function(landing_category_name) {
  if (length(x = class(x = landing_category_name)) != 1
      || class(x = landing_category_name) != "character") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"landing_category_name\" argument, class character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_landing_date
#' @title Attribute "landing_date" verification
#' @param landing_date Object of class {\link[base]{character}} expected. Landing date in format year month day hour minute second UTC.
#' @description Check if the item "landing_date" is in date format year month day hour minute second UTC and check if the value is inferior to the actual date.
#' @importFrom lubridate ymd
check_landing_date <- function(landing_date) {
  if (is.na(x = lubridate::ymd_hms(landing_date,
                                   quiet = TRUE,
                                   tz = "UTC"))) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"landing_date\" argument, at least one item failed to parse with the expected format.\n",
        sep = "")
    stop()
  } else if (lubridate::ymd_hms(landing_date,
                                quiet = TRUE,
                                tz = "UTC") > as.POSIXct(x = Sys.time(),
                                                         tz = "UTC")) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: at least one \"landing_date\" is superior to the actual date.\n",
        sep = "")
    stop()
  }
}

#' @name check_activity_date
#' @title Attribute "activity_date" verification
#' @param activity_date Object of class {\link[base]{character}} expected. Activity date in format year month day.
#' @param landing_date Object of class {\link[base]{character}} expected. Landing date in format year month day hour minute second UTC. By default NULL.
#' @description Check if the item "activity_date" is in date format year month day, if the value is inferior to the actual date and if it is inferior to the landing_date (if there are provided).
#' @importFrom lubridate ymd
check_activity_date <- function(activity_date,
                                landing_date = NULL) {
  if (is.na(x = lubridate::ymd(activity_date,
                               quiet = TRUE))) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"activity_date\" argument, At least one item failed to parse with format \"ymd\".\n",
        sep = "")
    stop()
  } else if (lubridate::ymd(activity_date,
                            quiet = TRUE) > Sys.Date()) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: at least one item \"activity_date\" is superior to the actual date.\n",
        sep = "")
    stop()
  } else if (! is.null(x = landing_date)) {
    if (is.na(lubridate::ymd_hms(landing_date,
                                 quiet = TRUE,
                                 tz = "UTC"))) {
      cat(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Error: invalide \"landing_date\" argument, at least one item failed to parse with the expected format.\n",
          sep = "")
      stop()
    } else if (lubridate::ymd(activity_date,
                              quiet = TRUE) > lubridate::ymd_hms(landing_date,
                                                                 quiet = TRUE,
                                                                 tz = "UTC")) {
      cat(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Error: At least one \"activity_date\" is superior to \"landing_date\".\n",
          sep = "")
      stop()
    }
  }
}

#' @name check_departure_date
#' @title Attribute "departure_date" verification
#' @param departure_date Object of class {\link[base]{character}} expected. Departure date in format year month day hour minute second UTC.
#' @param landing_date Object of class {\link[base]{character}} expected. Landing date in format year month day hour minute second UTC. By default NULL.
#' @description Check if the item "departure_date" is in date format year month day hour minute second UTC, if the value is inferior to the actual date and if it is inferior to the landing_date (if there are provided).
#' @importFrom lubridate ymd
check_departure_date <- function(departure_date,
                                 landing_date = NULL) {
  if (is.na(x = lubridate::ymd_hms(departure_date,
                                   quiet = TRUE,
                                   tz = "UTC"))) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"departure_date\" argument, at least one item failed to parse with the expected format.\n",
        sep = "")
    stop()
  } else if (lubridate::ymd_hms(departure_date,
                                quiet = TRUE,
                                tz = "UTC") > as.POSIXct(x = Sys.time(),
                                                         tz = "UTC")) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: at least one item \"departure_date\" is superior to the actual date.\n",
        sep = "")
    stop()
  } else if (! is.null(x = landing_date)) {
    if (is.na(lubridate::ymd_hms(landing_date,
                                 quiet = TRUE,
                                 tz = "UTC"))) {
      cat(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Error: invalide \"landing_date\" argument, at least one item failed to parse with the expected format.\n",
          sep = "")
      stop()
    } else if (lubridate::ymd_hms(departure_date,
                                  quiet = TRUE,
                                  tz = "UTC") > lubridate::ymd_hms(landing_date,
                                                                   quiet = TRUE,
                                                                   tz = "UTC")) {
      cat(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Error: At least one \"departure_date\" is superior to \"landing_date\".\n",
          sep = "")
      stop()
    }
  }
}

#' @name check_logbook_availability
#' @title Attribute "logbook_availability" verification
#' @param logbook_availability Object of class {\link[base]{integer}} expected. Logbook availability value, 1 for available and 0 for not.
#' @description Check if the item "logbook_availability" is class integer and if all values are 1 or 0.
check_logbook_availability <- function(logbook_availability) {
  if (length(x = class(x = logbook_availability)) != 1
      || class(x = logbook_availability) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"logbook_availability\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (! logbook_availability %in% c(0, 1)) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"logbook_availability\" argument, value 0 or 1 expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_fish_hold_empty
#' @title Attribute "fish_hold_empty" verification
#' @param fish_hold_empty Object of class {\link[base]{integer}} expected. Inform if the fish hold empty at the end of the trip, 1 for yes and 0 for not.
#' @description Check if the item "fish_hold_empty" is class integer and if all values are 1 or 0.
check_fish_hold_empty <- function(fish_hold_empty) {
  if (length(x = class(fish_hold_empty)) != 1
      || class(x = fish_hold_empty) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"fish_hold_empty\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (! fish_hold_empty %in% c(0, 1)) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"fish_hold_empty\" argument, value 0 or 1 expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_vessel_id
#' @title Attribute "vessel_id" verification
#' @param vessel_id Object of class {\link[base]{integer}} expected. Vessel identification.
#' @description Check if the item "vessel_id" have one unique class identified as integer.
check_vessel_id <- function(vessel_id) {
  if (length(x = class(x = vessel_id)) != 1
      || class(x = vessel_id) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"vessel_id\" argument, class integer expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_vessel_type
#' @title Attribute "vessel_type" verification
#' @param vessel_type Object of class {\link[base]{character}} expected. Vessel type identification.
#' @description Check if the item "vessel_type" have one unique class identified as character.
check_vessel_type <- function(vessel_type) {
  if (length(x = class(x = vessel_type)) != 1
      || class(x = vessel_type) != "character") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"vessel_type\" argument, class character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_activity_number
#' @title Attribute "activity_number" verification
#' @param activity_number Object of class {\link[base]{integer}} expected. Activity number.
#' @description Check if the item "activity_number" have one unique class identified as integer.
check_activity_number <- function(activity_number) {
  if (length(x = class(x = activity_number)) != 1
      || class(x = activity_number) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"activity_number\" argument, class integer expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_ocean
#' @title Attribute "ocean" verification
#' @param ocean Object of class {\link[base]{integer}} expected. Ocean identification.
#' @description Check if the item "ocean" is class integer and if all values are 1, 2, 3, 4 or 5.
check_ocean <- function(ocean) {
  if (length(x = class(x = ocean)) != 1
      || class(x = ocean) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"ocean\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (! ocean %in% c(1, 2, 3, 4, 5)) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"ocean\" argument, value 1, 2, 3, 4 or 5 expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_length_type
#' @title Attribute "length_type" verification
#' @param length_type Object of class {\link[base]{integer}} expected. Length type identification, 1 for LD1 and 2 for LF.
#' @description Check if the item "length_type" is class integer and if all values are 1 or 2.
check_length_type <- function(length_type) {
  if (length(x = class(x = length_type)) != 1
      || class(x = length_type) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"length_type\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (! length_type %in% c(1, 2)) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"length_type\" argument, value 1 or 2 expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_school_type
#' @title Attribute "school_type" verification
#' @param school_type Object of class {\link[base]{integer}} expected. School type identification.
#' @description Check if the item "school_type" is class integer and if all values are 1, 2, or 3.
check_school_type <- function(school_type) {
  if (length(x = class(x = school_type)) != 1
      || class(x = school_type) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"school_type\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (! school_type %in% c(1, 2, 3)) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"school_type\" argument, value 1, 2 or 3 expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_logbook_category
#' @title Attribute "logbook_category" verification
#' @param logbook_category Object of class {\link[base]{integer}} expected. Logbook weight category.
#' @description Check if the item "logbook_category" have one unique class identified as integer and if values are between 1 and 13.
check_logbook_category <- function(logbook_category) {
  if (length(x = class(x = logbook_category)) != 1
      || class(x = logbook_category) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"logbook_category\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (! logbook_category %in% c(1:13)) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"logbook_category\" argument, value from 1 to 13 expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_logbook_category_name
#' @title Attribute "logbook_category_name" verification
#' @param logbook_category_name Object of class {\link[base]{character}} expected. Logbook category name identification.
#' @description Check if the item "logbook_category_name" have one unique class identified as character.
check_logbook_category_name <- function(logbook_category_name) {
  if (length(x = class(x = logbook_category_name)) != 1
      || class(x = logbook_category_name) != "character") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"logbook_category_name\" argument, class character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_specie_code3l
#' @title Attribute "specie_code3l" verification
#' @param specie_code3l Object of class {\link[base]{character}} expected. Specie code identification on 3 characters.
#' @description Check if the item "specie_code3l" have one unique class identified as character and 3 characters.
check_specie_code3l <- function(specie_code3l) {
  if (length(x = class(x = specie_code3l)) != 1
      || class(x = specie_code3l) != "character") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"specie_code3l\" argument, class character expected.\n",
        sep = "")
    stop()
  } else if (nchar(x = specie_code3l) != 3) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"specie_code3l\" argument, 3 character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_specie_code
#' @title Attribute "specie_code" verification
#' @param specie_code Object of class {\link[base]{integer}} expected. Specie code identification.
#' @description Check if the item "specie_code" have one unique class identified as integer and superior to zero.
check_specie_code <- function(specie_code) {
  if (length(x = class(x = specie_code)) != 1
      || class(x = specie_code) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"specie_code\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (specie_code <= 0) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"specie_code\" argument, integer expected with value superior to zero.\n",
        sep = "")
    stop()
  }
}

#' @name check_activity_name
#' @title Attribute "activity_name" verification
#' @param activity_name Object of class {\link[base]{character}} expected. Activity identification.
#' @description Check if the item "check_activity_name" have one unique class identified as character.
check_activity_name <- function(activity_name) {
  if (length(x = class(x = activity_name)) != 1
      || class(x = activity_name) != "character") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"activity_name\" argument, class character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_fleet
#' @title Attribute "fleet" verification
#' @param fleet Object of class {\link[base]{character}} expected. Fleet identification.
#' @description Check if the item "fleet" have one unique class identified as character.
check_fleet <- function(fleet) {
  if (length(x = class(x = fleet)) != 1
      || class(fleet) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"fleet\" argument, class character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_wellplan_weigth_category_label
#' @title Attribute "wellplan_weigth_category_label" verification
#' @param wellplan_weigth_category_label Object of class {\link[base]{character}} expected. Well plan weight category identification.
#' @description Check if the item "wellplan_weigth_category_label" have one unique class identified as character.
check_wellplan_weigth_category_label <- function(wellplan_weigth_category_label) {
  if (length(x = class(x = wellplan_weigth_category_label)) != 1
      || class(x = wellplan_weigth_category_label) != "character") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"wellplan_weigth_category_label\" argument, class character expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_activity_code
#' @title Attribute "activity_code" verification
#' @param activity_code Object of class {\link[base]{integer}} expected. Activity code identification.
#' @description Check if the item "activity_code" have one unique class identified as integer.
check_activity_code <- function(activity_code) {
  if (length(x = class(x = activity_code)) != 1
      || class(activity_code) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"activity_code\" argument, class integer expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_wellplan_number
#' @title Attribute "wellplan_number" verification
#' @param wellplan_number Object of class {\link[base]{integer}} expected. Well plan number of individus.
#' @description Check if the item "wellplan_number" have one unique class identified as integer.
check_wellplan_number <- function(wellplan_number) {
  if (length(x = class(x = wellplan_number)) != 1
      || class(x = wellplan_number) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"wellplan_number\" argument, class integer expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_catch_weight
#' @title Attribute "catch_weight" verification
#' @param catch_weight Object of class {\link[base]{numeric}} expected. Catch weight in tonnes.
#' @description Check if the item "catch_weight" have one unique class identified as numeric and if values are superior to zero.
check_catch_weight <- function(catch_weight) {
  if (length(x = class(x = catch_weight)) != 1
      || class(x = catch_weight) != "numeric") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"catch_weight\" argument, class numeric expected.\n",
        sep = "")
    stop()
  } else if (catch_weight <= 0) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"catch_weight\" argument, positive value expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_well_minus10_weigth
#' @title Attribute "well_minus10_weigth" verification
#' @param well_minus10_weigth Object of class {\link[base]{integer}} expected. Catch weight of individus less than 10 tonnes (by well, in tonne, all species considerated).
#' @description Check if the item "well_minus10_weigth" have one unique class identified as integer and if value are >= 0.
check_well_minus10_weigth <- function(well_minus10_weigth) {
  if (length(class(well_minus10_weigth)) != 1 || class(well_minus10_weigth) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"well_minus10_weigth\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (well_minus10_weigth < 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"well_minus10_weigth\" argument, 0 or postive value expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_weighted_weight
#' @title Attribute "weighted_weight" verification
#' @param weighted_weight Object of class {\link[base]{numeric}} Set weight weighted by all set in the well(s).
#' @description Check if the item "weighted_weight" have one unique class identified as numeric and if value are >= 0.
check_weighted_weight <- function(weighted_weight) {
  if (length(class(weighted_weight)) != 1 || class(weighted_weight) != "numeric") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"weighted_weight\" argument, class numeric expected.\n",
        sep = "")
    stop()
  } else if (weighted_weight <= 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"weighted_weight\" argument, postive value expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_well_plus10_weigth
#' @title Attribute "well_plus10_weigth" verification
#' @param well_plus10_weigth Object of class {\link[base]{integer}} expected. Catch weight of individus more than 10 tonnes (by well, in tonne, all species considerated).
#' @description Check if the item "well_plus10_weigth" have one unique class identified as integer and if value are >= 0.
check_well_plus10_weigth <- function(well_plus10_weigth) {
  if (length(class(well_plus10_weigth)) != 1 || class(well_plus10_weigth) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"well_plus10_weigth\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (well_plus10_weigth < 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"well_plus10_weigth\" argument, 0 or postive value expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_well_global_weigth
#' @title Attribute "well_global_weigth" verification
#' @param well_global_weigth Object of class {\link[base]{integer}} expected. Catch weight of individus (less and more 10 tonnes categories, by well, in tonne, all species considerated).
#' @description Check if the item "well_global_weigth" have one unique class identified as integer and if value are >= 0.
check_well_global_weigth <- function(well_global_weigth) {
  if (length(class(well_global_weigth)) != 1 || class(well_global_weigth) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"well_global_weigth\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (well_global_weigth < 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"well_global_weigth\" argument, 0 or postive value expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_landing_weight
#' @title Attribute "landing_weight" verification
#' @param landing_weight Object of class {\link[base]{numeric}} expected. Landing weight in tonnes.
#' @description Check if the item "landing_weight" have one unique class identified as numeric.
check_landing_weight <- function(landing_weight) {
  if (length(x = class(x = landing_weight)) != 1
      || class(x = landing_weight) != "numeric") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"landing_weight\" argument, class numeric expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_set_count
#' @title Attribute "set_count" verification
#' @param set_count Object of class {\link[base]{integer}} expected. Number of set associated to the activity.
#' @description Check if the item "set_count" have one unique class identified as integer and if value are superior or equal to 0
check_set_count <- function(set_count) {
  if (length(x = class(x = set_count)) != 1
      || class(x = set_count) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"set_count\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (set_count < 0) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"set_count\" argument, 0 or postive value expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_time_at_sea
#' @title Attribute "time_at_sea" verification
#' @param time_at_sea Object of class {\link[base]{integer}} expected. Time at sea in hours.
#' @description Check if the item "time_at_sea" have one unique class identified as integer and if all values are superior or egal to zero.
check_time_at_sea <- function(time_at_sea) {
  if (length(x = class(x = time_at_sea)) != 1
      || class(x = time_at_sea) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"time_at_sea\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (time_at_sea < 0) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"time_at_sea\" argument, 0 or postive value expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_total_count
#' @title Attribute "sample_total_count" verification
#' @param sample_total_count Object of class {\link[base]{integer}} expected. Sample number of total individuals counted.
#' @description Check if the item "sample_total_count" have one unique class identified as integer and if all values are positive and different of zero.
check_sample_total_count <- function(sample_total_count) {
  if (length(x = class(x = sample_total_count)) != 1
      || class(x = sample_total_count) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_total_count\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (sample_total_count <= 0) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_total_count\" argument, postive value (without zero) expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_number_measured
#' @title Attribute "sample_number_measured" verification
#' @param sample_number_measured Object of class {\link[base]{integer}} expected. Sample number of measured individus.
#' @description Check if the item "sample_number_measured" have one unique class identified as integer and if all values are positive and different of zero.
check_sample_number_measured <- function(sample_number_measured) {
  if (length(x = class(x = sample_number_measured)) != 1
      || class(x = sample_number_measured) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_number_measured\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (sample_number_measured <= 0) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_number_measured\" argument, postive value (without zero) expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_number_measured_extrapolated_lf
#' @title Attribute "sample_number_measured_extrapolated_lf" verification
#' @param sample_number_measured_extrapolated_lf Object of class {\link[base]{numeric}} Sample number of measured individus extrapolated to all counted individus.
#' @description Check if the item "sample_number_measured_extrapolated_lf" have one unique class identified as numeric and if all values are positive and different of zero.
check_sample_number_measured_extrapolated_lf <- function(sample_number_measured_extrapolated_lf) {
  if (length(class(sample_number_measured_extrapolated_lf)) != 1 || class(sample_number_measured_extrapolated_lf) != "numeric") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_number_measured_extrapolated_lf\" argument, class numeric expected.\n",
        sep = "")
    stop()
  } else if (sample_number_measured_extrapolated_lf <= 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_number_measured_extrapolated_lf\" argument, postive value (without zero) expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_number_weighted
#' @title Attribute "sample_number_weighted" verification
#' @param sample_number_weighted Object of class {\link[base]{numeric}} Sample number of measured individus extrapolated to all counted individus and weighted by set weight.
#' @description Check if the item "sample_number_weighted" have one unique class identified as numeric and if all values are positive and different of zero.
check_sample_number_weighted <- function(sample_number_weighted) {
  if (length(class(sample_number_weighted)) != 1 || class(sample_number_weighted) != "numeric") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_number_weighted\" argument, class numeric expected.\n",
        sep = "")
    stop()
  } else if (sample_number_weighted <= 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_number_weighted\" argument, postive value (without zero) expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_wellplan_weight
#' @title Attribute "wellplan_weight" verification
#' @param wellplan_weight Object of class {\link[base]{numeric}} Weight in tonnes filled in the well plan.
#' @description Check if the item "wellplan_weight" have one unique class identified as numeric and if all values are positive and different of zero.
check_wellplan_weight <- function(wellplan_weight) {
  if (length(x = class(x = wellplan_weight)) != 1
      || class(x = wellplan_weight) != "numeric") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"wellplan_weight\" argument, class numeric expected.\n",
        sep = "")
    stop()
  } else if (wellplan_weight <= 0) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"wellplan_weight\" argument, postive value (without zero) expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_length_class
#' @title Attribute "sample_length_class" verification
#' @param sample_length_class Object of class {\link[base]{integer}} expected. Sample length class of measured individus.
#' @description Check if the item "sample_length_class" have one unique class identified as integer and if all values are positive and different of zero.
check_sample_length_class <- function(sample_length_class) {
  if (length(x = class(x = sample_length_class)) != 1
      || class(x = sample_length_class) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_length_class\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (sample_length_class <= 0) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_length_class\" argument, postive value (without zero) expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_quality
#' @title Attribute "sample_quality" verification
#' @param sample_quality Object of class {\link[base]{integer}} expected. Sample quality identification.
#' @description Check if the item "sample_quality" have one unique class identified as integer and if all values are 1, 2, 3, 4 or 9.
check_sample_quality <- function(sample_quality) {
  if (length(x = class(x = sample_quality)) != 1
      || class(x = sample_quality) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_quality\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (! sample_quality %in% c(1, 2, 3, 4, 9)) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_quality\" argument, value 1, 2, 3, 4 or 9 expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_type
#' @title Attribute "sample_type" verification
#' @param sample_type Object of class {\link[base]{integer}} expected. Sample type identification.
#' @description Check if the item "sample_type" have one unique class identified as integer and if all values are 1, 2, 3, 4, 9 or 11.
check_sample_type <- function(sample_type) {
  if (length(x = class(x = sample_type)) != 1
      || class(x = sample_type) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_type\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (! sample_type %in% c(1, 2, 3, 4, 9, 11)) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_type\" argument, value 1, 2, 3, 4, 9 or 11 expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_wellplan_weigth_category_code
#' @title Attribute "wellplan_weigth_category_code" verification
#' @param wellplan_weigth_category_code Object of class {\link[base]{integer}} expected. Well plan category code identification.
#' @description Check if the item "wellplan_weigth_category_code" have one unique class identified as integer and if all values are 1, 2, 8 or 9.
check_wellplan_weigth_category_code <- function(wellplan_weigth_category_code) {
  if (length(x = class(x = wellplan_weigth_category_code)) != 1
      || class(x = wellplan_weigth_category_code) != "integer") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"wellplan_weigth_category_code\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (! wellplan_weigth_category_code %in% c(1, 2, 8, 9)) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"wellplan_weigth_category_code\" argument, value 1, 2, 8 or 9 expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_sample_standardised_length_class_lf
#' @title Attribute "sample_standardised_length_class_lf" verification
#' @param sample_standardised_length_class_lf Object of class {\link[base]{integer}} expected. Sample standardised length class length fork of measured individus.
#' @description Check if the item "sample_standardised_length_class_lf" have one unique class identified as integer and if all values are positive and different of zero.
check_sample_standardised_length_class_lf <- function(sample_standardised_length_class_lf) {
  if (length(class(sample_standardised_length_class_lf)) != 1 || class(sample_standardised_length_class_lf) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_standardised_length_class_lf\" argument, class integer expected.\n",
        sep = "")
    stop()
  } else if (sample_standardised_length_class_lf < 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"sample_standardised_length_class_lf\" argument, postive value expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_activity_longitude
#' @title Attribute "activity_longitude" verification
#' @param activity_longitude Object of class {\link[base]{numeric}} expected. Longitude, in decimal degree, of the activity.
#' @description Check if the item "activity_longitude" have one unique class identified as numeric.
check_activity_longitude <- function(activity_longitude) {
  if (length(x = class(x = activity_longitude)) != 1
      || class(x = activity_longitude) != "numeric") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"activity_longitude\" argument, class numeric expected.\n",
        sep = "")
    stop()
  }
}

#' @name check_activity_latitude
#' @title Attribute "activity_latitude" verification
#' @param activity_latitude Object of class {\link[base]{numeric}} expected. Latitude, in decimal degree, of the activity.
#' @description Check if the item "activity_latitude" have one unique class identified as numeric.
check_activity_latitude <- function(activity_latitude) {
  if (length(x = class(x = activity_latitude)) != 1
      || class(x = activity_latitude) != "numeric") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"activity_latitude\" argument, class numeric expected.\n",
        sep = "")
    stop()
  }
}
