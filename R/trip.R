#' @name trip
#' @title R6 class trip
#' @description Create R6 reference object class trip
#' @importFrom R6 R6Class
#' @importFrom codama r_type_checking
#' @importFrom lubridate parse_date_time
trip <- R6::R6Class(classname = "trip",
                    public = list(
                      # initialize ----
                      #' @description Initialize function for R6 trip class.
                      #' @param trip_id Object of class {\link[base]{character}} expected. Trip identification.
                      #' @param flag_code Object of class {\link[base]{character}} expected. Fleet identification.
                      #' @param departure_date Object of class {\link[base]{character}} expected. Departure date in format year month day hour minute second.
                      #' @param trip_end_date Object of class {\link[base]{character}} expected. Landing date in format ymd UTC.
                      #' @param logbook_availability_code Object of class {\link[base]{integer}} expected. Logbook availability value, 1 for available and 0 for not.
                      #' @param landing_well_content_code Object of class {\link[base]{integer}} expected. Informe if the fish hold empty at the end of the trip, 1 for yes and 0 for not.
                      #' @param vessel_code Object of class {\link[base]{integer}} expected. Vessel identification.
                      #' @param vessel_type_code Object of class {\link[base]{character}} expected. Vessel type identification.
                      initialize = function(trip_id,
                                            flag_code,
                                            departure_date,
                                            trip_end_date,
                                            logbook_availability_code,
                                            landing_well_content_code,
                                            vessel_code,
                                            vessel_type_code) {
                        # 1 - Arguments verifications ----
                        codama::r_type_checking(r_object = trip_id,
                                                type = "character",
                                                length = 1L)
                        codama::r_type_checking(r_object = flag_code,
                                                type = "character",
                                                length = 1L)
                        codama::r_type_checking(r_object = departure_date,
                                                type = "character",
                                                length = 1L)
                        codama::r_type_checking(r_object = trip_end_date,
                                                type = "character",
                                                length = 1L)
                        codama::r_type_checking(r_object = logbook_availability_code,
                                                type = "integer",
                                                length = 1L)
                        codama::r_type_checking(r_object = landing_well_content_code,
                                                type = "integer",
                                                length = 1L)
                        codama::r_type_checking(r_object = vessel_code,
                                                type = "integer",
                                                length = 1L)
                        codama::r_type_checking(r_object = vessel_type_code,
                                                type = "integer",
                                                length = 1L)
                        # 2 - Attributions ----
                        private$trip_id <- trip_id
                        private$flag_code <- flag_code
                        private$departure_date <- lubridate::parse_date_time(departure_date,
                                                                             orders = c("ymd_HMS",
                                                                                        "ymd"),
                                                                             tz = "UTC",
                                                                             quiet = TRUE)
                        private$trip_end_date <- lubridate::parse_date_time(trip_end_date,
                                                                            orders = c("ymd_HMS",
                                                                                       "ymd"),
                                                                            tz = "UTC",
                                                                            quiet = TRUE)
                        private$logbook_availability_code <- logbook_availability_code
                        private$landing_well_content_code <- landing_well_content_code
                        private$vessel_code <- vessel_code
                        private$vessel_type_code <- vessel_type_code}),
                    private = list(
                      trip_id = NULL,
                      flag_code = NULL,
                      departure_date = NULL,
                      trip_end_date = NULL,
                      fishing_time = NULL,
                      time_at_sea = NULL,
                      searching_time = NULL,
                      logbook_availability_code = NULL,
                      landing_well_content_code = NULL,
                      vessel_code = NULL,
                      vessel_type_code = NULL,
                      rf1 = NULL,
                      statut_rf1 = NULL,
                      rf2 = NULL,
                      statut_rf2 = NULL,
                      activities = NULL,
                      elementarylandings = NULL,
                      wells = NULL))
