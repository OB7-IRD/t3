#' @name activity
#' @title R6 class activity
#' @description Create R6 reference object class activity
activity <- R6::R6Class(classname = "activity",
                        public = list(
                          # initialize ----
                          #' @description Initialize function for R6 activities class.
                          #' @param trip_id Object of class {\link[base]{character}} expected. Trip identification.
                          #' @param activity_id Object of class {\link[base]{character}} expected. Activity identification.
                          #' @param ocean_code Object of class {\link[base]{integer}} expected. Ocean identification.
                          #' @param activity_date Object of class {\link[base]{character}} expected. Activity date in format year month day.
                          #' @param activity_number Object of class {\link[base]{integer}} expected. Activity number.
                          #' @param activity_latitude Object of class {\link[base]{numeric}} expected. Latitude, in decimal degree, of the activity.
                          #' @param activity_longitude Object of class {\link[base]{numeric}} expected. Longitude, in decimal degree, of the activity.
                          #' @param set_count Object of class {\link[base]{integer}} expected. Number of set associated to the activity.
                          #' @param school_type_code Object of class {\link[base]{integer}} expected. School type identification.
                          #' @param activity_code Object of class {\link[base]{integer}} expected. Activity code identification.
                          #' @param activity_label Object of class {\link[base]{character}} expected. Activity identification.
                          #' @param time_at_sea Object of class {\link[base]{integer}} expected. Time at sea in hours.
                          initialize = function(trip_id,
                                                activity_id,
                                                ocean_code,
                                                activity_date,
                                                activity_number,
                                                activity_latitude,
                                                activity_longitude,
                                                set_count,
                                                school_type_code,
                                                activity_code,
                                                activity_label,
                                                time_at_sea) {
                            # 1 - Arguments verifications ----
                            codama::r_type_checking(r_object = trip_id,
                                                    type = "character",
                                                    length = 1L)
                            codama::r_type_checking(r_object = activity_id,
                                                    type = "character",
                                                    length = 1L)
                            codama::r_type_checking(r_object = ocean_code,
                                                    type = "integer",
                                                    length = 1L)
                            codama::r_type_checking(r_object = activity_date,
                                                    type = "character",
                                                    length = 1L)
                            codama::r_type_checking(r_object = activity_number,
                                                    type = "integer",
                                                    length = 1L)
                            codama::r_type_checking(r_object = activity_latitude,
                                                    type = "numeric",
                                                    length = 1L)
                            codama::r_type_checking(r_object = activity_longitude,
                                                    type = "numeric",
                                                    length = 1L)
                            codama::r_type_checking(r_object = set_count,
                                                    type = "integer",
                                                    length = 1L)
                            codama::r_type_checking(r_object = school_type_code,
                                                    type = "integer",
                                                    length = 1L)
                            codama::r_type_checking(r_object = activity_code,
                                                    type = "integer",
                                                    length = 1L)
                            codama::r_type_checking(r_object = activity_label,
                                                    type = "character",
                                                    length = 1L)
                            codama::r_type_checking(r_object = time_at_sea,
                                                    type = "integer",
                                                    length = 1L)
                            # 2 - Attributions ----
                            private$trip_id <- trip_id
                            private$activity_id <- activity_id
                            private$ocean_code <- ocean_code
                            private$activity_date <- lubridate::parse_date_time(activity_date,
                                                                                orders = c("ymd_HMS",
                                                                                           "ymd"),
                                                                                tz = "UTC",
                                                                                quiet = TRUE)
                            private$activity_number <- activity_number
                            private$activity_longitude <- activity_longitude
                            private$activity_latitude <- activity_latitude
                            private$set_count <- set_count
                            private$school_type_code <- school_type_code
                            private$activity_code <- activity_code
                            private$activity_label <- activity_label
                            private$time_at_sea <- time_at_sea}),
                        private = list(
                          trip_id = NULL,
                          activity_id = NULL,
                          ocean_code = NULL,
                          activity_date = NULL,
                          activity_number = NULL,
                          activity_latitude = NULL,
                          activity_longitude = NULL,
                          set_count = NULL,
                          school_type_code = NULL,
                          activity_code = NULL,
                          activity_label = NULL,
                          time_at_sea = NULL,
                          positive_set_count = NULL,
                          set_duration = NULL,
                          elementarycatches = NULL))
