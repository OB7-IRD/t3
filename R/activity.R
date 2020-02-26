#' @name activity
#' @title R6 class activity creation
#' @description Create R6 reference object class activity
#' @importFrom R6 R6Class
#' @importFrom lubridate ymd
activity <- R6::R6Class(classname = "activity",
                        public = list(
                          # initialize ----
                          #' @description Initialize function for R6 activities class.
                          #' @param trip_id (character) Trip identification.
                          #' @param activity_id (character) Activity identification.
                          #' @param ocean (integer) Ocean identification.
                          #' @param activity_date (date) Activity date in format year month day.
                          #' @param activity_number (integer) Activity number.
                          #' @param activity_latitude (numeric) Latitude, in decimal degree, of the activity.
                          #' @param activity_longitude (numeric) Longitude, in decimal degree, of the activity.
                          #' @param set_count (integer) Number of set associated to the activity.
                          #' @param school_type (integer) School type identification.
                          #' @param activity_code (integer) Activity code identification.
                          #' @param activity_name (character) Activity identification.
                          #' @param time_at_sea (integer) Time at sea in hours.
                          initialize = function(trip_id,
                                                activity_id,
                                                ocean,
                                                activity_date,
                                                activity_number,
                                                activity_latitude,
                                                activity_longitude,
                                                set_count,
                                                school_type,
                                                activity_code,
                                                activity_name,
                                                time_at_sea) {
                            # attribut "trip_id" verification
                            t3:::check_trip_id(trip_id)
                            # attribut "activity_id" verification
                            t3:::check_activity_id(activity_id)
                            # attribut "ocean" verification
                            t3:::check_ocean(ocean)
                            # attribut "activity_date" verification
                            t3:::check_activity_date(activity_date = activity_date)
                            # attribut "check_activity_number" verification
                            t3:::check_activity_number(activity_number)
                            # attribut "check_activity_latitude" verification
                            t3:::check_activity_latitude(activity_latitude)
                            # attribut "check_activity_longitude" verification
                            t3:::check_activity_longitude(activity_longitude)
                            # attribut "set_count" verification
                            t3:::check_set_count(set_count)
                            # attribut "school_type" verification
                            t3:::check_school_type(school_type)
                            # attribut "activity_code" verification
                            t3:::check_activity_code(activity_code)
                            # attribut "school_type" verification
                            t3:::check_activity_name(activity_name)
                            # attribut "time_at_sea" verification
                            t3:::check_time_at_sea(time_at_sea)
                            # attributions
                            private$trip_id <- trip_id
                            private$activity_id <- activity_id
                            private$ocean <- ocean
                            private$activity_date <- lubridate::ymd(activity_date, quiet = TRUE)
                            private$activity_number <- activity_number
                            private$activity_longitude <- activity_longitude
                            private$activity_latitude <- activity_latitude
                            private$set_count <- set_count
                            private$school_type <- school_type
                            private$activity_code <- activity_code
                            private$activity_name <- activity_name
                            private$time_at_sea <- time_at_sea}),
                        private = list(
                          trip_id = NULL,
                          activity_id = NULL,
                          ocean = NULL,
                          activity_date = NULL,
                          activity_number = NULL,
                          activity_latitude = NULL,
                          activity_longitude = NULL,
                          set_count = NULL,
                          school_type = NULL,
                          activity_code = NULL,
                          activity_name = NULL,
                          time_at_sea = NULL,
                          positive_set_count = NULL,
                          set_duration = NULL,
                          elementarycatches = NULL))
