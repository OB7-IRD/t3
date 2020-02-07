#' @name object_trip
#' @title R6 object trip creation
#' @description Creation object trip in relation with R6 reference object class trip
#' @param trip_id trip_id
#' @param fleet fleet
#' @param departure_date departure_date
#' @param landing_date landing_date
#' @param logbook_availability logbook_availability
#' @param fish_hold_empty fish_hold_empty
#' @param vessel_id vessel_id
#' @return A R6 reference object with data related to one trip
#' @seealso \code{\link{trip}}
#' @export
object_trip <- function(trip_id,
                        fleet,
                        departure_date,
                        landing_date,
                        logbook_availability,
                        fish_hold_empty,
                        vessel_id) {
  t3:::trip$new(trip_id = trip_id,
                fleet = fleet,
                departure_date = departure_date,
                landing_date = landing_date,
                logbook_availability = logbook_availability,
                fish_hold_empty = fish_hold_empty,
                vessel_id = vessel_id)
}
