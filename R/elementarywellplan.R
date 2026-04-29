#' @name elementarywellplan
#' @title R6 class elementarywellplan
#' @description Create R6 reference object class elementarywellplan
#' @importFrom R6 R6Class
#' @importFrom codama r_type_checking
elementarywellplan <- R6::R6Class(classname = "elementarywellplan",
                                  public = list(
                                    #' @description Initialize function for R6 elementarywellplan class.
                                    #' @param wellplan_id Object of class {\link[base]{character}} expected. Wellplan identification.
                                    #' @param well_id Object of class {\link[base]{character}} expected. Well identification (topiaid from Observe database).
                                    #' @param well_id_bis Object of class {\link[base]{character}} expected. Second well identification created by t3 process such as :
                                    #'  well_id_bis="fr.ird.data.ps.logbook.Well#trip_id#well_label" for Observe database and well_id_bis="fr.ird.avdth.entities.data.Well#vessel_code#trip_end_date#N_CUVE.F_POS_CUVE" for AVDTH database.
                                    #' @param activity_id Object of class {\link[base]{character}} expected. Activity identification.
                                    #' @param school_type_code Object of class {\link[base]{character}} expected. School type identification.
                                    #' @param sample_id Object of class {\link[base]{character}} expected. Sample identification.
                                    #' @param species_fao_code Object of class {\link[base]{character}} expected. Specie code identification on 3 characters.
                                    #' @param wellplan_weight (numeric) Weight in tonnes filled in the well plan.
                                    #' @param wellplan_count Object of class {\link[base]{integer}} expected. Well plan number of individus.
                                    #' @param weight_category_code Object of class {\link[base]{character}} expected. Well plan category code identification.
                                    #' @param weight_category_label Object of class {\link[base]{character}} expected. Well plan weight category identification.
                                    initialize = function(wellplan_id,
                                                          well_id,
                                                          well_id_bis,
                                                          activity_id,
                                                          school_type_code,
                                                          sample_id,
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
                                      codama::r_type_checking(r_object = well_id_bis,
                                                              type = "character",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = activity_id,
                                                              type = "character",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = school_type_code,
                                                              type = "character",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = sample_id,
                                                              type = "character",
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
                                      private$well_id_bis <- well_id_bis
                                      private$activity_id <- activity_id
                                      private$school_type_code <- school_type_code
                                      private$sample_id <- sample_id
                                      private$species_fao_code <- species_fao_code
                                      private$wellplan_weight <- wellplan_weight
                                      private$weight_category_code <- weight_category_code
                                      private$weight_category_label <- weight_category_label
                                    }),
                                  private = list(
                                    wellplan_id = NULL,
                                    well_id = NULL,
                                    well_id_bis = NULL,
                                    activity_id = NULL,
                                    school_type_code = NULL,
                                    sample_id = NULL,
                                    species_fao_code = NULL,
                                    wellplan_weight = NULL,
                                    weight_category_code = NULL,
                                    weight_category_label = NULL))
