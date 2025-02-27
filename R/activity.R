#' @name activity
#' @title R6 class activity
#' @description Create R6 reference object class activity
#' @importFrom R6 R6Class
#' @importFrom codama r_type_checking
activity <- R6::R6Class(classname = "activity",
                        public = list(
                          # initialize ----
                          #' @description Initialize function for R6 activities class.
                          #' @param trip_id Object of class {\link[base]{character}} expected. Trip identification (unique topiaid from database).
                          #' @param activity_id Object of class {\link[base]{character}} expected. Activity identification (unique topiaid from database).
                          #' @param ocean_code Object of class {\link[base]{integer}} expected. Ocean identification.
                          #' @param activity_date Object of class {\link[base]{character}} expected. Activity date in format year month day.
                          #' @param activity_number Object of class {\link[base]{integer}} expected. Activity number.
                          #' @param activity_latitude Object of class {\link[base]{numeric}} expected. Latitude, in decimal degree, of the activity.
                          #' @param activity_longitude Object of class {\link[base]{numeric}} expected. Longitude, in decimal degree, of the activity.
                          #' @param time_at_sea Object of class {\link[base]{integer}} expected. Time at sea in hours.
                          #' @param set_count Object of class {\link[base]{integer}} expected. Number of set associated to the activity.
                          #' @param set_success_status_code Object of class {\link[base]{integer}} expected. Code status of the set.
                          #' @param set_success_status_label Object of class {\link[base]{character}} expected. Label status of the set.
                          #' @param school_type_code Object of class {\link[base]{integer}} expected. School type identification.
                          #' @param activity_code Object of class {\link[base]{integer}} expected. Activity code identification.
                          #' @param activity_label Object of class {\link[base]{character}} expected. Activity identification.
                          #' @param objectoperation_code Object of class {\link[base]{integer}} expected. Operation on floating object code identification.
                          #' @param objectoperation_label Object of class {\link[base]{character}} expected. Operation on floating object identification.
                          #' @param objectoperation_id Object of class {\link[base]{character}} expected. Operation on floating object identification (unique topiaid from database).
                          #' @param elementarycatches Object of type \link[base]{data.frame} or \link[tibble]{tbl_df} with elementarycatch(es) data expected for catch activities and NULL for other activities (default).
                          #' @details
                          #' Mandatory column names and types of elementarycatch(es) data:
                          #' \itemize{
                          #' \item{elementarycatch_id: } Elementary catch identification (class{\link[base]{character}}).
                          #' \item{ocean_code: } Ocean identification (class {\link[base]{integer}}).
                          #' \item{school_type_code: } School type identification (class {\link[base]{integer}}).
                          #' \item{weight_category_code: } Logbook weight category (class {\link[base]{integer}}).
                          #' \item{weight_category_label: } Logbook weight category name identification (class{\link[base]{character}}).
                          #' \item{species_code: } Species code identification (class {\link[base]{integer}}).
                          #' \item{species_fao_code: } Species code identification on 3 characters (class{\link[base]{character}}).
                          #' \item{species_fate_code: } Species fate code identification (class {\link[base]{integer}}).
                          #' \item{catch_weight:} Catch weight in tonnes (class {\link[base]{numeric}}).
                          #' \item{catch_count:} Catch count in number of specimens caught (class {\link[base]{integer}}).
                          #' }
                          initialize = function(trip_id,
                                                activity_id,
                                                ocean_code,
                                                activity_date,
                                                activity_number,
                                                activity_latitude,
                                                activity_longitude,
                                                time_at_sea,
                                                set_count,
                                                set_success_status_code,
                                                set_success_status_label,
                                                school_type_code,
                                                activity_code,
                                                activity_label,
                                                objectoperation_code,
                                                objectoperation_label,
                                                objectoperation_id,
                                                elementarycatches) {
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
                            codama::r_type_checking(r_object = set_success_status_code,
                                                    type = "integer",
                                                    length = 1L)
                            codama::r_type_checking(r_object = set_success_status_label,
                                                    type = "character",
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
                            codama::r_type_checking(r_object = objectoperation_code,
                                                    type = "integer",
                                                    length = 1L)
                            codama::r_type_checking(r_object = objectoperation_label,
                                                    type = "character",
                                                    length = 1L)
                            codama::r_type_checking(r_object = objectoperation_id,
                                                    type = "character",
                                                    length = 1L)
                            codama::r_type_checking(r_object = time_at_sea,
                                                    type = "integer",
                                                    length = 1L)
                            if(!is.null(elementarycatches)){
                            codama::r_table_checking(r_table=as.data.frame(elementarycatches),
                                                     type="data.frame",
                                                     column_name=c("activity_id",
                                                                   "elementarycatch_id",
                                                                   "ocean_code",
                                                                   "school_type_code",
                                                                   "weight_category_code",
                                                                   "weight_category_label",
                                                                   "species_fao_code",
                                                                   "catch_weight",
                                                                   "catch_count",
                                                                   "species_fate_code"),
                                                     column_type=c("character",
                                                                   "character",
                                                                   "integer",
                                                                   "integer",
                                                                   "character",
                                                                   "character",
                                                                   "character",
                                                                   "numeric",
                                                                   "integer",
                                                                   "integer"))
                            }
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
                            private$time_at_sea <- time_at_sea
                            private$set_count <- set_count
                            private$set_success_status_code <- set_success_status_code
                            private$set_success_status_label <- set_success_status_label
                            private$school_type_code <- school_type_code
                            private$activity_code <- activity_code
                            private$activity_label <- activity_label
                            private$objectoperation_code <- objectoperation_code
                            private$objectoperation_label <- objectoperation_label
                            private$objectoperation_id <- objectoperation_id
                            private$elementarycatches <- elementarycatches
                            }),
                        private = list(
                          trip_id = NULL,
                          activity_id = NULL,
                          ocean_code = NULL,
                          activity_date = NULL,
                          activity_number = NULL,
                          activity_latitude = NULL,
                          activity_longitude = NULL,
                          set_count = NULL,
                          set_success_status_code = NULL,
                          set_success_status_label = NULL,
                          school_type_code = NULL,
                          activity_code = NULL,
                          activity_label = NULL,
                          objectoperation_code = NULL,
                          objectoperation_label = NULL,
                          objectoperation_id = NULL,
                          time_at_sea = NULL,
                          fishing_time = NULL,
                          searching_time = NULL,
                          positive_set_count = NULL,
                          set_duration = NULL,
                          elementarycatches = NULL))
