#' @name standardisedsampleset
#' @title R6 class standardisedsampleset
#' @description Create R6 reference object class standardisedsampleset
standardisedsampleset <- R6::R6Class(classname = "standardisedsampleset",
                                  public = list(
                                    # initialize ----
                                    #' @description Initialize function for R6 standardisedsampleset class.
                                    #' @param trip_id Object of class {\link[base]{character}} expected. Trip identification.
                                    #' @param activity_id Object of class {\link[base]{character}} expected. Activity identification.
                                    #' @param well_id Object of class {\link[base]{character}} expected. Well identification.
                                    #' @param sample_id Object of class {\link[base]{character}} expected. Sample identification.
                                    #' @param sample_quality_code Object of class {\link[base]{integer}} expected. Sample quality identification.
                                    #' @param sample_type_code Object of class {\link[base]{integer}} expected. Sample type identification.
                                    #' @param species_code Object of class {\link[base]{integer}} expected. Specie code identification.
                                    #' @param species_fao_code Object of class {\link[base]{character}} expected. Specie code identification on 3 characters.
                                    #' @param sample_standardised_length_class_lf Object of class {\link[base]{integer}} expected. Sample standardised length class length fork of measured individus.
                                    #' @param sample_number_weighted Object of class {\link[base]{numeric}} expected. Sample number of measured individus extrapolated to all counted individus and weighted by set weight.
                                    #' @param sample_weigth Object of class {\link[base]{numeric}} expected. Weight (kg) of the sample number of measured individus extrapolated to all counted individus (conversion by length weight relationship: coeficient a * length class lf ^ coeficient b).
                                    #' @param sample_weight_unit Object of class {\link[base]{numeric}} or NA expected. Weight (kg) of one individu calculated by the length weight relationship: coeficient a * length class lf ^ coeficient b).
                                    #' @param sample_category Object of class {\link[base]{character}} expected. Sample category: -10kg or +10kg.
                                    initialize = function(trip_id,
                                                          activity_id,
                                                          well_id,
                                                          sample_id,
                                                          sample_quality_code,
                                                          sample_type_code,
                                                          species_code,
                                                          species_fao_code,
                                                          sample_standardised_length_class_lf,
                                                          sample_number_weighted,
                                                          sample_weigth,
                                                          sample_weight_unit,
                                                          sample_category) {
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
                                                              length = 1L)
                                      codama::r_type_checking(r_object = sample_quality_code,
                                                              type = "integer",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = sample_type_code,
                                                              type = "integer",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = species_code,
                                                              type = "integer",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = species_fao_code,
                                                              type = "character",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = sample_standardised_length_class_lf,
                                                              type = "integer",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = sample_number_weighted,
                                                              type = "numeric",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = sample_weigth,
                                                              type = "numeric",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = sample_weight_unit,
                                                              type = "numeric",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = sample_category,
                                                              type = "character",
                                                              length = 1L)
                                      # 2 - Attributions ----
                                      private$trip_id <- trip_id
                                      private$activity_id <- activity_id
                                      private$well_id <- well_id
                                      private$sample_id <- sample_id
                                      private$sample_quality_code <- sample_quality_code
                                      private$sample_type_code <- sample_type_code
                                      private$species_code <- species_code
                                      private$species_fao_code <- species_fao_code
                                      private$sample_standardised_length_class_lf <- sample_standardised_length_class_lf
                                      private$sample_number_weighted <- sample_number_weighted
                                      private$sample_weigth <- sample_weigth
                                      private$sample_weight_unit <- sample_weight_unit
                                      private$sample_category <- sample_category
                                    }),
                                  private = list(
                                    trip_id = NULL,
                                    activity_id = NULL,
                                    well_id = NULL,
                                    sample_id = NULL,
                                    sample_quality_code = NULL,
                                    sample_type_code = NULL,
                                    species_code = NULL,
                                    species_fao_code = NULL,
                                    sample_standardised_length_class_lf = NULL,
                                    sample_number_weighted = NULL,
                                    sample_weigth = NULL,
                                    sample_number_weighted_set = NULL,
                                    sample_weigth_set = NULL,
                                    sample_weight_unit = NULL,
                                    sample_category = NULL))
