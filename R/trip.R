#' @name trip
#' @title R6 class trip creation
#' @description Create R6 reference object class trip
#' @importFrom R6 R6Class
#' @importFrom lubridate ymd
# trip ----
trip <- R6::R6Class(classname = "trip",
                    public = list(
                      initialize = function(trip_id,
                                            landing_date,
                                            fishing_time,
                                            time_at_sea,
                                            logbook_availability,
                                            fish_hold_empty,
                                            vessel_id,
                                            landing_harbour_id,
                                            departure_harbour_id) {
                        # attribut "trip_id" verification
                        t3:::check_trip_id(trip_id)
                        # attribut "landing_date" verification
                        t3:::check_landing_date(landing_date)
                        # attribut "fishing_time" verification
                        t3:::check_fishing_time(fishing_time)
                        # attribut "time_at_sea" verification
                        t3:::check_time_at_sea(time_at_sea)
                        # attribut "logbook_availability" verification
                        t3:::check_logbook_availability(logbook_availability)
                        # attribut "fish_hold_empty" verification
                        t3:::check_fish_hold_empty(fish_hold_empty)
                        # attribut "vessel_id" verification
                        t3:::check_vessel_id(vessel_id)
                        # attribut "landing_harbour_id" verification
                        t3:::check_landing_harbour_id(landing_harbour_id)
                        # attribut "departure_harbour_id" verification
                        t3:::check_departure_harbour_id(departure_harbour_id)
                        # attributions
                        private$trip_id <- trip_id
                        private$landing_date <- lubridate::ymd(landing_date, quiet = TRUE)
                        private$fishing_time <- fishing_time
                        private$time_at_sea <- time_at_sea
                        private$logbook_availability <- logbook_availability
                        private$fish_hold_empty <- fish_hold_empty
                        private$vessel_id <- vessel_id
                        private$landing_harbour_id <- landing_harbour_id
                        private$departure_harbour_id <- departure_harbour_id}),
                    private = list(
                      trip_id = NULL,
                      landing_date = NULL,
                      fishing_time = NULL,
                      time_at_sea = NULL,
                      logbook_availability = NULL,
                      fish_hold_empty = NULL,
                      vessel_id = NULL,
                      landing_harbour_id = NULL,
                      departure_harbour_id = NULL,
                      rf1 = NULL,
                      statut_rf1 = NULL,
                      rf2 = NULL,
                      statut_rf2 = NULL,
                      activities = NULL,
                      elementarylandings = NULL))
