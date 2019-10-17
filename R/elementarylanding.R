#' @name elementarylanding
#' @title R6 class elementarylanding creation
#' @description Create R6 reference object class elementarylanding
#' @importFrom R6 R6Class
#' @importFrom lubridate ymd
elementarylanding <- R6::R6Class(classname = "elementarylanding",
                                 public = list(
                                   initialize = function(trip_id,
                                                         landing_date,
                                                         vessel_id,
                                                         landing_category,
                                                         specie_code3l,
                                                         landing_weight) {
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
                                     # attribut "vessel_id" verification
                                     if (class(vessel_id) != "integer") {
                                       stop("invalide \"vessel_id\" argument\nclass integer expected")
                                     } else {
                                       private$vessel_id <- vessel_id
                                     }
                                     # attribut "landing_category" verification
                                     if (class(landing_category) != "character") {
                                       stop("invalide \"landing_category\" argument\nclass character expected")
                                     } else {
                                       private$landing_category <- landing_category
                                     }
                                     # attribut "specie_code3l" verification
                                     if (class(specie_code3l) != "character" || nchar(specie_code3l) != 3) {
                                       stop("invalide \"specie_code3l\" argument\n3 characters expected")
                                     } else {
                                       private$specie_code3l <- specie_code3l
                                     }
                                     # attribut "landing_weight" verification
                                     if (class(landing_weight) != "numeric") {
                                       stop("invalide \"landing_weight\" argument\nclass numeric expected")
                                     } else {
                                       private$landing_weight <- landing_weight
                                     }
                                   }),
                                 private = list(
                                   trip_id = NULL,
                                   landing_date = NULL,
                                   vessel_id = NULL,
                                   landing_category = NULL,
                                   specie_code3l = NULL,
                                   landing_weight = NULL
                                 ))
