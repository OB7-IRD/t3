#' @name elementarylanding
#' @title R6 class elementarylanding
#' @description Create R6 reference object class elementarylanding
#' @importFrom R6 R6Class
#' @importFrom lubridate ymd
elementarylanding <- R6::R6Class(classname = "elementarylanding",
                                 public = list(
                                   # initialize ----
                                   #' @description Initialize function for R6 elementarylanding class.
                                   #' @param trip_id Object of class {\link[base]{character}} expected. Trip identification.
                                   #' @param elementarylanding_id Object of class {\link[base]{character}} expected. Elementary landing identification.
                                   #' @param landing_category Object of class {\link[base]{integer}} expected. Landing category identification.
                                   #' @param landing_category_name Object of class {\link[base]{character}} expected. Landing category name identification.
                                   #' @param specie_code Object of class {\link[base]{integer}} expected. Specie code identification.
                                   #' @param specie_code3l Object of class {\link[base]{character}} expected. Specie code identification on 3 characters.
                                   #' @param landing_weight Object of class {\link[base]{numeric}} expected. Landing weight in tonnes.
                                   initialize = function(trip_id,
                                                         elementarylanding_id,
                                                         landing_category,
                                                         landing_category_name,
                                                         specie_code,
                                                         specie_code3l,
                                                         landing_weight) {
                                     # attribute "trip_id" verification
                                     t3:::check_trip_id(trip_id)
                                     # attribute "elementarylanding_id" verification
                                     t3:::check_elementarylanding_id(elementarylanding_id)
                                     # attribute "landing_category" verification
                                     t3:::check_landing_category(landing_category)
                                     # attribute "landing_category_name" verification
                                     t3:::check_landing_category_name(landing_category_name)
                                     # attribute "specie_code" verification
                                     t3:::check_specie_code(specie_code)
                                     # attribute "specie_code3l" verification
                                     t3:::check_specie_code3l(specie_code3l)
                                     # attribute "landing_weight" verification
                                     t3:::check_landing_weight(landing_weight)
                                     # attributeions
                                     private$trip_id <- trip_id
                                     private$elementarylanding_id <- elementarylanding_id
                                     private$landing_category <- landing_category
                                     private$landing_category_name <- landing_category_name
                                     private$specie_code <- specie_code
                                     private$specie_code3l <- specie_code3l
                                     private$landing_weight <- landing_weight
                                   }),
                                 private = list(
                                   trip_id = NULL,
                                   elementarylanding_id = NULL,
                                   landing_category = NULL,
                                   landing_category_name = NULL,
                                   specie_code = NULL,
                                   specie_code3l = NULL,
                                   landing_weight = NULL
                                 ))
