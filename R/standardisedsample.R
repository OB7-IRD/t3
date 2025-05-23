#' @name standardisedsample
#' @title R6 class standardisedsample
#' @description Create R6 reference object class standardisedsample
#' @importFrom R6 R6Class
#' @importFrom codama r_type_checking
standardisedsample <- R6::R6Class(classname = "standardisedsample",
                                  public = list(
                                    # initialize ----
                                    #' @description Initialize function for R6 standardisedsample class.
                                    #' @param trip_id (character) Trip identification.
                                    #' @param well_id (character) Well identification.
                                    #' @param sample_id (character) Sample identification.
                                    #' @param sample_quality_code Object of class {\link[base]{integer}} expected. Sample quality identification.
                                    #' @param sample_type_code Object of class {\link[base]{integer}} expected. Sample type identification.
                                    #' @param species_fao_code (character) Specie code identification on 3 characters.
                                    #' @param sample_standardised_length_class_lf Object of class {\link[base]{integer}} expected. Sample standardised length class length fork of measured individus.
                                    #' @param sample_number_measured_extrapolated_lf Object of class {\link[base]{numeric}} expected. Sample number of measured individus extrapolated to all counted individus.
                                    #' @param sample_total_count Object of class {\link[base]{integer}} expected. Sample number of total individus counted.
                                    initialize = function(trip_id,
                                                          well_id,
                                                          sample_id,
                                                          sample_quality_code,
                                                          sample_type_code,
                                                          species_fao_code,
                                                          sample_standardised_length_class_lf,
                                                          sample_number_measured_extrapolated_lf,
                                                          sample_total_count) {
                                      # 1 - Arguments verifications ----
                                      codama::r_type_checking(r_object = trip_id,
                                                              type = "character",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = well_id,
                                                              type = "character",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = sample_id,
                                                              type = "character",
                                                              length = NULL)
                                      codama::r_type_checking(r_object = sample_quality_code,
                                                              type = "integer",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = sample_type_code,
                                                              type = "integer",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = species_fao_code,
                                                              type = "character",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = sample_standardised_length_class_lf,
                                                              type = "integer",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = sample_number_measured_extrapolated_lf,
                                                              type = "numeric",
                                                              length = 1L)
                                      codama::r_type_checking(r_object = sample_total_count,
                                                              type = "integer",
                                                              length = 1L)
                                      # 2 - Attributions ----
                                      private$trip_id <- trip_id
                                      private$well_id <- well_id
                                      private$sample_id <- sample_id
                                      private$sample_quality_code <- sample_quality_code
                                      private$sample_type_code <- sample_type_code
                                      private$species_fao_code <- species_fao_code
                                      private$sample_standardised_length_class_lf <- sample_standardised_length_class_lf
                                      private$sample_number_measured_extrapolated_lf <- sample_number_measured_extrapolated_lf
                                      private$sample_total_count <- sample_total_count
                                    }),
                                  private = list(
                                    trip_id = NULL,
                                    well_id = NULL,
                                    sample_id = NULL,
                                    sample_quality_code = NULL,
                                    sample_type_code = NULL,
                                    species_fao_code = NULL,
                                    sample_standardised_length_class_lf = NULL,
                                    sample_number_measured_extrapolated_lf = NULL,
                                    sample_total_count = NULL))
