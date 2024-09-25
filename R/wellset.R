#' @name wellset
#' @title R6 class wellset
#' @description Create R6 reference object class wellset
#' @importFrom R6 R6Class
#' @importFrom codama r_type_checking
wellset <- R6::R6Class(classname = "wellset",
                       public = list(
                         # initialize ----
                         #' @description Initialize function for R6 wellset class.
                         #' @param trip_id Object of class {\link[base]{character}} expected. Trip identification.
                         #' @param activity_id Object of class {\link[base]{character}} expected. Activity identification.
                         #' @param well_id Object of class {\link[base]{character}} expected. Well identification.
                         #' @param sample_id Object of class {\link[base]{character}} expected. Sample identification.
                         #' @param weighted_weight Object of class {\link[base]{numeric}} expected. Set weight weighted by all set in the well(s).
                         #' @param weighted_weight_minus10 Object of class {\link[base]{numeric}} expected. Set weight of individuals less than 10 kg (weighted by all set in the well(s))
                         #' @param weighted_weight_plus10 Object of class {\link[base]{numeric}} expected. Set weight of individuals plus than 10 kg (weighted by all set in the well(s))
                         initialize = function(trip_id,
                                               activity_id,
                                               well_id,
                                               sample_id,
                                               weighted_weight,
                                               weighted_weight_minus10,
                                               weighted_weight_plus10) {
                           # 1 - Arguments verifications ----
                           codama::r_type_checking(r_object = trip_id,
                                                   type = "character",
                                                   length = 1L)
                           codama::r_type_checking(r_object = activity_id,
                                                   type = "character",
                                                   length = 1L)
                           codama::r_type_checking(r_object = well_id,
                                                   type = "character",
                                                   length = 1L)
                           codama::r_type_checking(r_object = sample_id,
                                                   type = "character",
                                                   length = NULL)
                           codama::r_type_checking(r_object = weighted_weight,
                                                   type = "numeric",
                                                   length = 1L)
                           codama::r_type_checking(r_object = weighted_weight_minus10,
                                                   type = "numeric",
                                                   length = 1L)
                           codama::r_type_checking(r_object = weighted_weight_plus10,
                                                   type = "numeric",
                                                   length = 1L)
                           # 2 - Attributions ----
                           private$trip_id <- trip_id
                           private$activity_id <- activity_id
                           private$well_id <- well_id
                           private$sample_id <- sample_id
                           private$weighted_weight <- weighted_weight
                           private$weighted_weight_minus10 <- weighted_weight_minus10
                           private$weighted_weight_plus10 <- weighted_weight_plus10
                         }),
                       private = list(
                         trip_id = NULL,
                         activity_id = NULL,
                         well_id = NULL,
                         sample_id = NULL,
                         weighted_weight = NULL,
                         prop_weighted_weight = NULL,
                         weighted_weight_minus10 = NULL,
                         weighted_weight_plus10 = NULL,
                         weighted_samples_minus10 = NULL,
                         weighted_samples_plus10 = NULL,
                         weighted_samples_total = NULL,
                         rf_minus10 = NULL,
                         rf_plus10 = NULL,
                         rf_total = NULL,
                         rf_validation = NULL))
