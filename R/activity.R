#' @name activity
#' @title R6 class activity creation
#' @description Create R6 reference object class activity
#' @importFrom R6 R6Class
#' @importFrom lubridate ymd
# activity ----
activity <- R6::R6Class(classname = "activity",
                        public = list(
                          initialize = function(trip_id,
                                                activity_id,
                                                ocean,
                                                activity_date,
                                                activity_number,
                                                set_count,
                                                school_type,
                                                activity_code,
                                                activity_name) {
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
                            # attribut "set_count" verification
                            t3:::check_set_count(set_count)
                            # attribut "school_type" verification
                            t3:::check_school_type(school_type)
                            # attribut "activity_code" verification
                            t3:::check_activity_code(activity_code)
                            # attribut "school_type" verification
                            t3:::check_activity_name(activity_name)
                            # attributions
                            private$trip_id <- trip_id
                            private$activity_id <- activity_id
                            private$ocean <- ocean
                            private$activity_date <- lubridate::ymd(activity_date, quiet = TRUE)
                            private$activity_number <- activity_number
                            private$set_count <- set_count
                            private$school_type <- school_type
                            private$activity_code <- activity_code
                            private$activity_name <- activity_name}),
                        private = list(
                          trip_id = NULL,
                          activity_id = NULL,
                          ocean = NULL,
                          activity_date = NULL,
                          activity_number = NULL,
                          set_count = NULL,
                          school_type = NULL,
                          activity_code = NULL,
                          activity_name = NULL,
                          positive_set_count = NULL,
                          set_duration = NULL,
                          elementarycatches = NULL))
