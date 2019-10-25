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
                        # attribut "fishing_time" verification
                        if (fishing_time < 0) {
                          stop("Attribut \"fishing_time\" have at least one negative value")
                        } else {
                          private$fishing_time <- fishing_time
                        }
                        # attribut "time_at_sea" verification
                        if (time_at_sea < 0) {
                          stop("Attribut \"time_at_sea\" have at least one negative value")
                        } else {
                          private$time_at_sea <- time_at_sea
                        }
                        # attribut "landing_date" verification
                        if (is.na(lubridate::ymd(landing_date, quiet = TRUE))) {
                          stop("invalide \"landing_date\" argument\nAt least one item failed to parse with format ymd")
                        } else if (lubridate::ymd(landing_date, quiet = TRUE) > Sys.Date()) {
                          stop("At least one item \"landing_date\" is superior to the actual date")
                        } else {
                          private$landing_date <- lubridate::ymd(landing_date, quiet = TRUE)
                        }
                        private$trip_id <- trip_id
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
                      rf2 = NULL,
                      elementarycatches = NULL,
                      elementarylandings = NULL))
