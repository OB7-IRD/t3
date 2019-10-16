#' @name obj_trips
#' @title Object trips creation
#' @description Generate objet trips with S4 method (from T3 database format).
#' @param data To do
#' @export
#' @importFrom methods new
trips <- function(data,
                      remove_obj = T) {
  # Check input ----
  if (missing(remove_obj) || class(remove_obj) != "logical") {
    stop("Invalid \"remove_obj\" argument")
  }
  # Unclass for optimization ----
  data <- unclass(data)
  # Object creation ----
  trips <- list()
  for (i in 1:length(data[["trip_id"]])) {
    obj_trip <- methods::new(Class = "trip",
                             trip_id = data[["trip_id"]][i],
                             landing_date = data[["landing_date"]][i],
                             fishing_time = data[["fishing_time"]][i],
                             time_at_sea = data[["time_at_sea"]][i],
                             logbook_availability = data[["logbook_availability"]][i],
                             fish_hold_empty = data[["fish_hold_empty"]][i],
                             vessel_id = data[["vessel_id"]][i],
                             landing_harbour_id = data[["landing_harbour_id"]][i],
                             departure_harbour_id = data[["departure_harbour_id"]][i])
    trips <- c(trips, obj_trip)
  }
  return(trips)
}
