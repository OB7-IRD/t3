#' @name object_trip
#' @title R6 object trip creation
#' @description Creation object trip in relation with R6 reference object class trip
#' @param trip_id trip_id
#' @param landing_date landing_date
#' @param fishing_time fishing_time
#' @param time_at_sea time_at_sea
#' @param logbook_availability logbook_availability
#' @param fish_hold_empty fish_hold_empty
#' @param vessel_id vessel_id
#' @param landing_harbour_id landing_harbour_id
#' @param departure_harbour_id departure_harbour_id
#' @return A R6 reference object with data related to one trip
#' @seealso \code{\link{trip}}
#' @export
object_trip <- function(trip_id,
                        landing_date,
                        fishing_time,
                        time_at_sea,
                        logbook_availability,
                        fish_hold_empty,
                        vessel_id,
                        landing_harbour_id,
                        departure_harbour_id) {
  t3:::trip$new(trip_id = trip_id,
                landing_date = landing_date,
                fishing_time = fishing_time,
                time_at_sea = time_at_sea,
                logbook_availability = logbook_availability,
                fish_hold_empty = fish_hold_empty,
                vessel_id = vessel_id,
                landing_harbour_id = landing_harbour_id,
                departure_harbour_id = departure_harbour_id)
}
