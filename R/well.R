#' @name well
#' @title R6 class well
#' @description Create R6 reference object class well
#' @importFrom R6 R6Class
#' @importFrom codama r_type_checking
well <- R6::R6Class(classname = "well",
                    public = list(
                      # initialize ----
                      #' @description Initialize function for R6 well class.
                      #' @param trip_id Object of class {\link[base]{character}} expected. Trip identification.
                      #' @param well_id Object of class {\link[base]{character}} expected. Well identification.
                      #' @param well_minus10_weight Object of class {\link[base]{integer}} expected. Catch weight of individus less than 10 tonnes (by well, in tonne, all species considerated).
                      #' @param well_plus10_weight Object of class {\link[base]{integer}} expected. Catch weight of individus more than 10 tonnes (by well, in tonne, all species considerated).
                      #' @param well_global_weight Object of class {\link[base]{integer}} expected. Catch weight of individus (less and more 10 tonnes categories, by well, in tonne, all species considerated).
                      initialize = function(trip_id,
                                            well_id,
                                            well_minus10_weight,
                                            well_plus10_weight,
                                            well_global_weight) {
                        # 1 - Arguments verifications ----
                        codama::r_type_checking(r_object = trip_id,
                                                type = "character",
                                                length = 1L)
                        codama::r_type_checking(r_object = well_id,
                                                type = "character",
                                                length = 1L)
                        codama::r_type_checking(r_object = well_minus10_weight,
                                                type = "numeric",
                                                length = 1L)
                        codama::r_type_checking(r_object = well_plus10_weight,
                                                type = "numeric",
                                                length = 1L)
                        codama::r_type_checking(r_object = well_global_weight,
                                                type = "numeric",
                                                length = 1L)
                        # 2 - Attributions ----
                        private$trip_id <- trip_id
                        private$well_id <- well_id
                        private$well_minus10_weight <- well_minus10_weight
                        private$well_plus10_weight <- well_plus10_weight
                        private$well_global_weight <- well_global_weight}),
                    private = list(
                      trip_id = NULL,
                      well_id = NULL,
                      well_minus10_weight = NULL,
                      well_plus10_weight = NULL,
                      well_global_weight = NULL,
                      elementarysampleraw = NULL,
                      elementarysample = NULL,
                      standardisedsample = NULL,
                      standardisedsampleset = NULL,
                      wellsets = NULL,
                      proportion_verification = NULL,
                      well_prop_minus10_weight = NULL,
                      well_prop_plus10_weight = NULL,
                      wellplan = NULL))
