#' @name elementarylanding
#' @title R6 class elementarylanding
#' @description Create R6 reference object class elementarylanding
#' @importFrom R6 R6Class
#' @importFrom codama r_type_checking
elementarylanding <- R6::R6Class(classname = "elementarylanding",
                                 public = list(
                                   # initialize ----
                                   #' @description Initialize function for R6 elementarylanding class.
                                   #' @param trip_id Object of class {\link[base]{character}} expected. Trip identification.
                                   #' @param elementarylanding_id Object of class {\link[base]{character}} expected. Elementary landing identification.
                                   #' @param weight_category_code Object of class {\link[base]{integer}} expected. Logbook weight category.
                                   #' @param weight_category_label Object of class {\link[base]{character}} expected. Logbook category name identification.
                                   #' @param species_fao_code Object of class {\link[base]{character}} expected. Species code identification on 3 characters.
                                   #' @param landing_weight Object of class {\link[base]{numeric}} expected. Landing weight in tonnes.
                                   initialize = function(trip_id,
                                                         elementarylanding_id,
                                                         weight_category_code,
                                                         weight_category_label,
                                                         species_fao_code,
                                                         landing_weight) {
                                     # 1 - Arguments verifications ----
                                     codama::r_type_checking(r_object = trip_id,
                                                             type = "character",
                                                             length = 1L)
                                     codama::r_type_checking(r_object = elementarylanding_id,
                                                             type = "character",
                                                             length = 1L)
                                     codama::r_type_checking(r_object = weight_category_code,
                                                             type = "character",
                                                             length = 1L)
                                     codama::r_type_checking(r_object = weight_category_label,
                                                             type = "character",
                                                             length = 1L)
                                     codama::r_type_checking(r_object = species_fao_code,
                                                             type = "character",
                                                             length = 1L)
                                     codama::r_type_checking(r_object = landing_weight,
                                                             type = "numeric",
                                                             length = 1L)
                                     # 2 - Attributions ----
                                     private$trip_id <- trip_id
                                     private$elementarylanding_id <- elementarylanding_id
                                     private$weight_category_code <- weight_category_code
                                     private$weight_category_label <- weight_category_label
                                     private$species_fao_code <- species_fao_code
                                     private$landing_weight <- landing_weight
                                   }),
                                 private = list(
                                   trip_id = NULL,
                                   elementarylanding_id = NULL,
                                   weight_category_code = NULL,
                                   weight_category_label = NULL,
                                   species_fao_code = NULL,
                                   landing_weight = NULL
                                 ))
