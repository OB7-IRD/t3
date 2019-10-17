#' @name elementarycatch
#' @title R6 class elementarycatch creation
#' @description Create R6 reference object class elementarycatch
#' @importFrom R6 R6Class
#' @importFrom lubridate ymd
elementarycatch <- R6::R6Class(classname = "elementarycatch",
                               public = list(
                                 initialize = function(trip_id,
                                                       landing_date,
                                                       activity_date,
                                                       activity_number,
                                                       logbook_category,
                                                       vessel_id,
                                                       specie_code3l,
                                                       catch_weight) {
                                   # attribut "trip_id" verification
                                   if (class(trip_id) != "character") {
                                     stop("invalide \"trip_id\" argument\nclass character expected")
                                   } else {
                                     private$trip_id <- trip_id
                                   }
                                   # attribut "landing_date" verification
                                   if (is.na(lubridate::ymd(landing_date, quiet = TRUE))) {
                                     stop("invalide \"landing_date\" argument\nAt least one item failed to parse with format ymd")
                                   } else if (lubridate::ymd(landing_date, quiet = TRUE) > Sys.Date()) {
                                     stop("At least one \"landing_date\" is superior to the actual date")
                                   } else {
                                     private$landing_date <- lubridate::ymd(landing_date, quiet = TRUE)
                                   }
                                   # attribut "activity_date" verification
                                   if (is.na(lubridate::ymd(activity_date, quiet = TRUE))) {
                                     stop("invalide \"activity_date\" argument\nAt least one item failed to parse with format ymd")
                                   } else if (lubridate::ymd(activity_date, quiet = TRUE) > Sys.Date()) {
                                     stop("At least one item \"activity_date\" is superior to the actual date")
                                   } else if (lubridate::ymd(activity_date, quiet = TRUE) > lubridate::ymd(landing_date, quiet = TRUE)) {
                                     stop("At least one \"activity_date\" is superior to \"landing_date\"")
                                   } else {
                                     private$activity_date <- lubridate::ymd(activity_date, quiet = TRUE)
                                   }
                                   # attribut "activity_number" verification
                                   if (class(activity_number) != "integer") {
                                     stop("invalide \"activity_number\" argument\nclass integer expected")
                                   } else {
                                     private$activity_number <- activity_number
                                   }
                                   # attribut "logbook_category" verification
                                   if (class(logbook_category) != "character") {
                                     stop("invalide \"logbook_category\" argument\nclass character expected")
                                   } else {
                                     private$logbook_category <- logbook_category
                                   }
                                   # attribut "vessel_id" verification
                                   if (class(vessel_id) != "integer") {
                                     stop("invalide \"vessel_id\" argument\nclass integer expected")
                                   } else {
                                     private$vessel_id <- vessel_id
                                   }
                                   # attribut "specie_code3l" verification
                                   if (class(specie_code3l) != "character" || nchar(specie_code3l) != 3) {
                                     stop("invalide \"specie_code3l\" argument\n3 characters expected")
                                   } else {
                                     private$specie_code3l <- specie_code3l
                                   }
                                   # attribut "catch_weight" verification
                                   if (class(catch_weight) != "integer") {
                                     stop("invalide \"catch_weight\" argument\nclass integer expected")
                                   } else {
                                     private$catch_weight <- catch_weight
                                   }
                                 }),
                               private = list(
                                 trip_id = NULL,
                                 landing_date = NULL,
                                 activity_date = NULL,
                                 activity_number = NULL,
                                 logbook_category = NULL,
                                 vessel_id = NULL,
                                 specie_code3l = NULL,
                                 catch_weight = NULL
                               ))
