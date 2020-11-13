#' @name trip
#' @title R6 class trip
#' @description Create R6 reference object class trip
#' @importFrom R6 R6Class
#' @importFrom lubridate ymd
# trip ----
trip <- R6::R6Class(classname = "trip",
                    public = list(
                      # initialize ----
                      #' @description Initialize function for R6 trip class.
                      #' @param trip_id Object of class {\link[base]{character}} expected. Trip identification.
                      #' @param fleet Object of class {\link[base]{character}} expected. Fleet identification.
                      #' @param landing_date Object of class {\link[base]{character}} expected. Landing date in format ymd_hms UTC.
                      #' @param departure_date Object of class {\link[base]{character}} expected. Departure date in format year month day hour minute second.
                      #' @param logbook_availability Object of class {\link[base]{integer}} expected. Logbook availability value, 1 for available and 0 for not.
                      #' @param fish_hold_empty Object of class {\link[base]{integer}} expected. Informe if the fish hold empty at the end of the trip, 1 for yes and 0 for not.
                      #' @param vessel_id Object of class {\link[base]{integer}} expected. Vessel identification.
                      #' @param vessel_type Object of class {\link[base]{character}} expected. Vessel type identification.
                      initialize = function(trip_id,
                                            fleet,
                                            landing_date,
                                            departure_date,
                                            logbook_availability,
                                            fish_hold_empty,
                                            vessel_id,
                                            vessel_type) {
                        # attribute "trip_id" verification
                        t3:::check_trip_id(trip_id)
                        # attribute "fleet" verification
                        t3:::check_fleet(fleet)
                        # attribute "landing_date" verification
                        t3:::check_landing_date(landing_date)
                        # attribute "departure_date" verification
                        t3:::check_departure_date(departure_date, landing_date)
                        # attribute "logbook_availability" verification
                        t3:::check_logbook_availability(logbook_availability)
                        # attribute "fish_hold_empty" verification
                        t3:::check_fish_hold_empty(fish_hold_empty)
                        # attribute "vessel_id" verification
                        t3:::check_vessel_id(vessel_id)
                        # attribute "vessel_type" verification
                        t3:::check_vessel_type(vessel_type)
                        # attributions
                        private$trip_id <- trip_id
                        private$fleet <- fleet
                        private$landing_date <- lubridate::ymd_hms(landing_date, quiet = TRUE, tz = "UTC")
                        private$departure_date <- lubridate::ymd_hms(departure_date, quiet = TRUE, tz = "UTC")
                        private$logbook_availability <- logbook_availability
                        private$fish_hold_empty <- fish_hold_empty
                        private$vessel_id <- vessel_id
                        private$vessel_type <- vessel_type}),
                    private = list(
                      trip_id = NULL,
                      fleet = NULL,
                      departure_date = NULL,
                      landing_date = NULL,
                      fishing_time = NULL,
                      time_at_sea = NULL,
                      searching_time = NULL,
                      logbook_availability = NULL,
                      fish_hold_empty = NULL,
                      vessel_id = NULL,
                      vessel_type = NULL,
                      rf1 = NULL,
                      statut_rf1 = NULL,
                      rf2 = NULL,
                      statut_rf2 = NULL,
                      activities = NULL,
                      elementarylandings = NULL,
                      wells = NULL))
