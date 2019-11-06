#' @name activity
#' @title R6 class activity creation
#' @description Create R6 reference object class activity
#' @importFrom R6 R6Class
#' @importFrom lubridate ymd
# activity ----
activity <- R6::R6Class(classname = "activity",
                        public = list(
                          initialize = function(vessel_id,
                                                landing_date,
                                                activity_date,
                                                activity_number,
                                                set_count) {
                            # attribut "vessel_id" verification
                            t3:::check_vessel_id(vessel_id)
                            # attribut "landing_date" verification
                            t3:::check_landing_date(landing_date)
                            # attribut "activity_date" verification
                            t3:::check_activity_date(activity_date = activity_date,
                                                     landing_date = landing_date)
                            # attribut "check_activity_number" verification
                            t3:::check_activity_number(activity_number)
                            # attribut "set_count" verification
                            t3:::check_set_count(set_count)
                            # attributions
                            private$vessel_id <- vessel_id
                            private$landing_date <- lubridate::ymd(landing_date, quiet = TRUE)
                            private$activity_date <- lubridate::ymd(activity_date, quiet = TRUE)
                            private$activity_number <- activity_number
                            private$set_count <- set_count}),
                        private = list(
                          vessel_id = NULL,
                          landing_date = NULL,
                          activity_date = NULL,
                          activity_number = NULL,
                          set_count = NULL,
                          positive_set_count = NULL,
                          set_duration = NULL))
