#' @name obj_trip
#' @title Object trip creation
#' @description Generate objet trip with S4 method (from T3 database format).
#' @param data (data.frame) A data frame or a list with data in T3 database trip table format. See detail section for more informations.
#' @export
#' @importFrom methods new
obj_trip <- function(data) {
  # Check data input format ----
  if (missing(data) || ! class(data) %in% c("data.frame", "list")) {
    stop("Invalid \"data\" argument\nA data frame or a list format is expected")
  }
  if (class(data) == "list" & length(unique(summary(data)[1:length(data)])) != 1) {
    stop("Invalid \"data\" argument\nThe list's elements have different size")
  }
  # Unclass for optimization ----
  data <- unclass(data)
  # Object creation ----
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
  return(obj_trip)
}
