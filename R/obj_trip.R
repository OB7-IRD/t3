#' @name obj_trip
#' @title Objet trip creation
#' @author Observatory of Exploited Tropical Pelagic Ecosystems \email{ob7@@ird.fr}
#' @param data A data frame or a list with data in trip table format of a T3 database. See detail section for more informations.
#' @description Generate objet trip with S4 method (from T3 database format).
#' @references \url{https://github.com/OB7-IRD/t3}
#' @details To do after dude !
#' @export
obj_trip <- function(data) {
  # Check data input format ----
  if (! class(data) %in% c("data.frame", "list")) {
    stop("Your input data is not in a data frame or a list format")
  }
  # Unclass for optimization ----
  class(data) <- NULL
  # Class declaration ----
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
  # Class creation ----
  obj_trip <- methods::new(Class = "trip",
                           trip_id = data[["trip_id"]],
                           landing_date = data[["landing_date"]],
                           fishing_time = data[["fishing_time"]],
                           time_at_sea = data[["time_at_sea"]],
                           logbook_availability = data[["logbook_availability"]],
                           fish_hold_empty = data[["fish_hold_empty"]],
                           vessel_id = data[["vessel_id"]],
                           landing_harbour_id = data[["landing_harbour_id"]],
                           departure_harbour_id = data[["departure_harbour_id"]])
}
