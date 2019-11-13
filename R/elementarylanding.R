#' @name elementarylanding
#' @title R6 class elementarylanding creation
#' @description Create R6 reference object class elementarylanding
#' @importFrom R6 R6Class
#' @importFrom lubridate ymd
elementarylanding <- R6::R6Class(classname = "elementarylanding",
                                 public = list(
                                   initialize = function(trip_id,
                                                         landing_category,
                                                         landing_category_name,
                                                         specie_code3l,
                                                         landing_weight) {
                                     # attribut "trip_id" verification
                                     t3:::check_trip_id(trip_id)
                                     # attribut "landing_category" verification
                                     t3:::check_landing_category(landing_category)
                                     # attribut "landing_category_name" verification
                                     t3:::check_landing_category_name(landing_category_name)
                                     # attribut "specie_code3l" verification
                                     t3:::check_specie_code3l(specie_code3l)
                                     # attribut "landing_weight" verification
                                     t3:::check_landing_weight(landing_weight)
                                     # attributions
                                     private$trip_id <- trip_id
                                     private$landing_category <- landing_category
                                     private$landing_category_name <- landing_category_name
                                     private$specie_code3l <- specie_code3l
                                     private$landing_weight <- landing_weight
                                   }),
                                 private = list(
                                   trip_id = NULL,
                                   landing_category = NULL,
                                   landing_category_name = NULL,
                                   specie_code3l = NULL,
                                   landing_weight = NULL
                                 ))
