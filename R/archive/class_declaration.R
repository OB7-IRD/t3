#' @name .__C__trip
#' @title Trip class declaration
#' @description An S4 class to represent a trip
#' @slot trip_id Character vector with trip identitication
#' @slot landing_date A voir
# Trip ----
methods::setClass(Class = "trip",
                  representation = methods::representation(trip_id = "character",
                                                           landing_date = "Date",
                                                           fishing_time = "integer",
                                                           time_at_sea = "integer",
                                                           logbook_availability = "integer",
                                                           fish_hold_empty = "integer",
                                                           vessel_id = "character",
                                                           landing_harbour_id = "character",
                                                           departure_harbour_id = "character"),
                  validity = function(object) {
                    # Attribut "fishing_time" verification
                    if (any(object@fishing_time < 0)) {
                      stop("Attribut \"fishing_time\" have at least one negative value")
                    }
                    # Attribut "time_at_sea" verification
                    if (any(object@time_at_sea < 0)) {
                      stop("Attribut \"time_at_sea\" have at least one negative value")
                    }
                    # Attribut "landing_date" verification
                    if (any(object@landing_date > Sys.Date())) {
                      stop("At least one attribut \"landing_date\" is superior to the actual date")
                    }
                  })

#' @name .__C__trips
#' @title Trips class declaration
#' @description An S4 class to represent trips
#' @slot trip_id Character vector with trip identitication
#' @slot landing_date A voir
# Trips ----
methods::setClass(Class = "trips",
                  representation = methods::representation(trips = "S4"),
                  validity = function(object) {
                    # Attribut "trips" verification
                    if (class(object) != "trip") {
                      stop("At least one trip don't belong to class \"trip\"")
                    }
                  })
