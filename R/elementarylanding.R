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
                                     t3:::check_trip_id(trip_id)
                                     # attribut "landing_date" verification
                                     t3:::check_landing_date(landing_date)
                                     # attribut "vessel_id" verification
                                     t3:::check_vessel_id(vessel_id)
                                     # attribut "landing_category" verification
                                     if (class(landing_category) != "character") {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalide \"landing_category\" argument\nclass character expected\n",
                                           sep = "")
                                       stop()
                                     }
                                     # attribut "specie_code3l" verification
                                     t3:::check_specie_code3l(specie_code3l)
                                     # attribut "landing_weight" verification
                                     t3:::check_landing_weight(landing_weight)
                                     # attributions
                                     private$trip_id <- trip_id
                                     private$landing_date <- lubridate::ymd(landing_date, quiet = TRUE)
                                     private$vessel_id <- vessel_id
                                     private$landing_category <- landing_category
                                     private$specie_code3l <- specie_code3l
                                     private$landing_weight <- landing_weight
                                   }),
                                 private = list(
                                   trip_id = NULL,
                                   landing_date = NULL,
                                   vessel_id = NULL,
                                   landing_category = NULL,
                                   specie_code3l = NULL,
                                   landing_weight = NULL
                                 ))
