#' @name elementarycatch
#' @title R6 class elementarycatch creation
#' @description Create R6 reference object class elementarycatch
#' @importFrom R6 R6Class
elementarycatch <- R6::R6Class(classname = "elementarycatch",
                               public = list(
                                 initialize = function(ocean,
                                                       trip_id,
                                                       landing_date,
                                                       activity_date,
                                                       activity_number,
                                                       school_type,
                                                       logbook_category,
                                                       logbook_category_name,
                                                       vessel_id,
                                                       specie_code3l,
                                                       catch_weight) {
                                   # attribut "ocean" verification
                                   t3:::check_ocean(ocean)
                                   # attribut "trip_id" verification
                                   t3:::check_trip_id(trip_id)
                                   # attribut "landing_date" verification
                                   t3:::check_landing_date(landing_date)
                                   # attribut "activity_date" verification
                                   t3:::check_activity_date(activity_date = activity_date,
                                                            landing_date = landing_date)
                                   # attribut "activity_number" verification
                                   t3:::check_activity_number(activity_number)
                                   # attribut "school_type" verification
                                   t3:::check_school_type(school_type)
                                   # attribut "logbook_category" verification
                                   t3:::check_logbook_category(logbook_category)
                                   # attribut "logbook_category_name" verification
                                   t3:::check_logbook_category_name(logbook_category_name)
                                   # attribut "vessel_id" verification
                                   t3:::check_vessel_id(vessel_id)
                                   # attribut "specie_code3l" verification
                                   t3:::check_specie_code3l(specie_code3l)
                                   # attribut "catch_weight" verification
                                   t3:::check_catch_weight(catch_weight)
                                   # attributions
                                   private$ocean <- ocean
                                   private$trip_id <- trip_id
                                   private$landing_date <- lubridate::ymd(landing_date, quiet = TRUE)
                                   private$activity_date <- lubridate::ymd(activity_date, quiet = TRUE)
                                   private$activity_number <- activity_number
                                   private$school_type <- school_type
                                   private$logbook_category <- logbook_category
                                   private$logbook_category_name <- logbook_category_name
                                   private$vessel_id <- vessel_id
                                   private$specie_code3l <- specie_code3l
                                   private$catch_weight <- catch_weight
                                 }),
                               private = list(
                                 ocean = NULL,
                                 trip_id = NULL,
                                 landing_date = NULL,
                                 activity_date = NULL,
                                 activity_number = NULL,
                                 school_type = NULL,
                                 logbook_category = NULL,
                                 logbook_category_name = NULL,
                                 vessel_id = NULL,
                                 specie_code3l = NULL,
                                 catch_weight = NULL,
                                 catch_weight_rf1 = NULL,
                                 catch_weight_rf2 = NULL,
                                 corrected_logbook_category = NULL,
                                 catch_weight_category_corrected = NULL
                               ))
