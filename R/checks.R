#' @name check_trip_id
#' @title Attribut "trip_id" verification
#' @param trip_id (character) Trip identification
#' @description Check if the item "trip_id" have one unique class identified as character
# check trip_id ----
check_trip_id = function(trip_id) {
  if (length(class(trip_id)) != 1 || class(trip_id) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"trip_id\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_landing_date
#' @title Attribut "landing_date" verification
#' @param landing_date (date) Landing date in format ymd
#' @description Check if the item "landing_date" is in date format ymd and check if the value is inferior to the actual date
#' @importFrom lubridate ymd
# check landing_date ----
check_landing_date = function(landing_date) {
  if (is.na(lubridate::ymd(landing_date, quiet = TRUE))) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"landing_date\" argument\nat least one item failed to parse with format ymd\n",
        sep = "")
    stop()
  } else if (lubridate::ymd(landing_date, quiet = TRUE) > Sys.Date()) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: at least one \"landing_date\" is superior to the actual date\n",
        sep = "")
    stop()
  }
}

#' @name check_activity_date
#' @title Attribut "activity_date" verification
#' @param activity_date (date) Activity date in format ymd
#' @description Check if the item "activity_date" is in date format ymd, if the value is inferior to the actual date and if it is inferior to the landing_date (if there are provided)
#' @importFrom lubridate ymd
# check activity_date ----
check_activity_date = function(activity_date, landing_date = NULL) {
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
    if (is.na(lubridate::ymd(landing_date, quiet = TRUE))) {
      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: invalide \"landing_date\" argument\nat least one item failed to parse with format ymd\n",
          sep = "")
      stop()
    } else if (lubridate::ymd(activity_date, quiet = TRUE) > lubridate::ymd(landing_date, quiet = TRUE)) {
      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: At least one \"activity_date\" is superior to \"landing_date\"\n",
          sep = "")
      stop()
    }
  }
}

#' @name check_fishing_time
#' @title Attribut "fishing_time" verification
#' @param fishing_time (numeric or integer) Fishing time value
#' @description Check if the item "fishing_time" is class integer or numeric and if all values are postives
# check fishing_time ----
check_fishing_time = function(fishing_time) {
  if (length(class(fishing_time)) != 1 || ! class(fishing_time) %in% c("integer", "numeric")) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"fishing_time\" argument\nclass numeric or integer expected\n",
        sep = "")
    stop()
  } else if (fishing_time < 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: attribut \"fishing_time\" have at least one negative value\n",
        sep = "")
    stop()
  }
}

#' @name check_time_at_sea
#' @title Attribut "time_at_sea" verification
#' @param time_at_sea (numeric or integer) Time at sea value
#' @description Check if the item "time_at_sea" is class integer or numeric and if all values are postives
# check time_at_sea ----
check_time_at_sea = function(time_at_sea) {
  if (time_at_sea < 0) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: attribut \"time_at_sea\" have at least one negative value\n",
        sep = "")
    stop()
  }
}

#' @name check_logbook_availability
#' @title Attribut "logbook_availability" verification
#' @param logbook_availability (integer) Logbook availability value, 1 for available and 0 for not
#' @description Check if the item "logbook_availability" is class integer and if all values are 1 or 0
# check logbook_availability ----
check_logbook_availability = function(logbook_availability) {
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
#' @param fish_hold_empty (integer) Informe if the fish hold empty at the end of the trip, 1 for yes and 0 for not
#' @description Check if the item "fish_hold_empty" is class integer and if all values are 1 or 0
# check fish_hold_empty ----
check_fish_hold_empty = function(fish_hold_empty) {
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

#' @name vessel_id
#' @title Attribut "vessel_id" verification
#' @param vessel_id (integer) Vessel identification
#' @description Check if the item "vessel_id" have one unique class identified as integer
# check vessel_id ----
check_vessel_id = function(vessel_id) {
  if (length(class(vessel_id)) != 1 || class(vessel_id) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"vessel_id\" argument\nclass integer expected\n",
        sep = "")
    stop()
  }
}

#' @name activity_number
#' @title Attribut "activity_number" verification
#' @param activity_number (integer) Activity number
#' @description Check if the item "activity_number" have one unique class identified as integer
# check activity_number ----
check_activity_number = function(activity_number) {
  if (length(class(activity_number)) != 1 || class(activity_number) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"activity_number\" argument\nclass integer expected\n",
        sep = "")
    stop()
  }
}

#' @name check_landing_harbour_id
#' @title Attribut "landing_harbour_id" verification
#' @param landing_harbour_id (character) Landing harbour identification
#' @description Check if the item "landing_harbour_id" have one unique class identified as character
# check landing_harbour_id ----
check_landing_harbour_id = function(landing_harbour_id) {
  if (length(class(landing_harbour_id)) != 1 || class(landing_harbour_id) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"landing_harbour_id\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_departure_harbour_id
#' @title Attribut "departure_harbour_id" verification
#' @param departure_harbour_id (character) Departure harbour identification
#' @description Check if the item "departure_harbour_id" have one unique class identified as character
# check departure_harbour_id ----
check_departure_harbour_id = function(departure_harbour_id) {
  if (length(class(departure_harbour_id)) != 1 || class(departure_harbour_id) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"departure_harbour_id\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_ocean
#' @title Attribut "ocean" verification
#' @param ocean (integer) Ocean identification
#' @description Check if the item "ocean" is class integer and if all values are 1, 2, 3, 4 or 5
# check ocean ----
check_ocean = function(ocean) {
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

#' @name check_school_type
#' @title Attribut "school_type" verification
#' @param school_type (integer) School type identification
#' @description Check if the item "school_type" is class integer and if all values are 1, 2, or 3
# check school_type ----
check_school_type = function(school_type) {
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

#' @name logbook_category
#' @title Attribut "logbook_category" verification
#' @param logbook_category (integer) Logbook weight category
#' @description Check if the item "logbook_category" have one unique class identified as integer
# check logbook_category ----
check_logbook_category = function(logbook_category) {
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
#' @param logbook_category_name (character) Logbook category name identification
#' @description Check if the item "logbook_category_name" have one unique class identified as character
# check logbook_category_name ----
check_logbook_category_name = function(logbook_category_name) {
  if (length(class(logbook_category_name)) != 1 || class(logbook_category_name) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"logbook_category_name\" argument\nclass character expected\n",
        sep = "")
    stop()
  }
}

#' @name check_specie_code3l
#' @title Attribut "specie_code3l" verification
#' @param specie_code3l (character) Specie code identification on 3 characters
#' @description Check if the item "specie_code3l" have one unique class identified as character and 3 characters
# check specie_code3l ----
check_specie_code3l = function(specie_code3l) {
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

#' @name catch_weight
#' @title Attribut "catch_weight" verification
#' @param catch_weight (integer) Catch weight in tonne
#' @description Check if the item "catch_weight" have one unique class identified as integer
# check catch_weight ----
check_catch_weight = function(catch_weight) {
  if (length(class(catch_weight)) != 1 || class(catch_weight) != "integer") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"catch_weight\" argument\nclass integer expected\n",
        sep = "")
    stop()
  }
}

#' @name landing_weight
#' @title Attribut "landing_weight" verification
#' @param landing_weight (numeric) Landing weight in tonne
#' @description Check if the item "landing_weight" have one unique class identified as numeric
# check landing_weight ----
check_landing_weight = function(landing_weight) {
  if (length(class(landing_weight)) != 1 || class(landing_weight) != "numeric") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalide \"landing_weight\" argument\nclass numeric expected\n",
        sep = "")
    stop()
  }
}
