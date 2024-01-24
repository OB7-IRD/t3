#' @name elementarywellplan
#' @title R6 class elementarywellplan
#' @description Create R6 reference object class elementarywellplan
elementarywellplan <- R6::R6Class(classname = "elementarywellplan",
                                  public = list(
                                    #' @description Initialize function for R6 elementarywellplan class.
                                    #' @param wellplan_id Object of class {\link[base]{character}} expected. Wellplan identification.
                                    #' @param well_id Object of class {\link[base]{character}} expected. Well identification.
                                    #' @param activity_id Object of class {\link[base]{character}} expected. Activity identification.
                                    #' @param sample_id Object of class {\link[base]{character}} expected. Sample identification.
                                    #' @param species_code Object of class {\link[base]{integer}} expected. Specie code identifiation.
                                    #' @param species_fao_code Object of class {\link[base]{character}} expected. Specie code identification on 3 characters.
                                    #' @param wellplan_weight (numeric) Weight in tonnes filled in the well plan.
                                    #' @param wellplan_count Object of class {\link[base]{integer}} expected. Well plan number of individus.
                                    #' @param weight_category_code Object of class {\link[base]{character}} expected. Well plan category code identification.
                                    #' @param weight_category_label Object of class {\link[base]{character}} expected. Well plan weight category identification.
                                    initialize = function(wellplan_id,
                                                          well_id,
                                                          activity_id,
                                                          sample_id,
                                                          species_code,
                                                          species_fao_code,
                                                          wellplan_weight,
                                                          wellplan_count,
                                                          weight_category_code,
                                                          weight_category_label) {
                                      # 1 - Arguments verifications ----
                                      codama::r_type_checking(r_object = wellplan_id,
                                                              type = "character",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = well_id,
                                                              type = "character",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = activity_id,
                                                              type = "character",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = sample_id,
                                                              type = "character",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = species_code,
                                                              type = "integer",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = species_fao_code,
                                                              type = "character",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = wellplan_weight,
                                                              type = "numeric",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = weight_category_code,
                                                              type = "character",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = weight_category_label,
                                                              type = "character",
                                                              length = 1L)
                                      # 2 - Attributions ----
                                      private$wellplan_id <- wellplan_id
                                      private$well_id <- well_id
                                      private$activity_id <- activity_id
                                      private$sample_id <- sample_id
                                      private$species_code <- species_code
                                      private$species_fao_code <- species_fao_code
                                      private$wellplan_weight <- wellplan_weight
                                      private$weight_category_code <- weight_category_code
                                      private$weight_category_label <- weight_category_label
                                    }),
                                  private = list(
                                    wellplan_id = NULL,
                                    well_id = NULL,
                                    activity_id = NULL,
                                    sample_id = NULL,
                                    species_code = NULL,
                                    species_fao_code = NULL,
                                    wellplan_weight = NULL,
                                    weight_category_code = NULL,
                                    weight_category_label = NULL))
