#' @name standardisedsample
#' @title R6 class standardisedsample
#' @description Create R6 reference object class standardisedsample
#' @importFrom R6 R6Class
standardisedsample <- R6::R6Class(classname = "standardisedsample",
                                  public = list(
                                    # initialize ----
                                    #' @description Initialize function for R6 standardisedsample class.
                                    #' @param trip_id (character) Trip identification.
                                    #' @param well_id (character) Well identification.
                                    #' @param sample_id (character) Sample identification.
                                    #' @param sample_quality Object of class {\link[base]{integer}} expected. Sample quality identification.
                                    #' @param sample_type Object of class {\link[base]{integer}} expected. Sample type identification.
                                    #' @param specie_code Object of class {\link[base]{integer}} expected. Specie code identification.
                                    #' @param specie_code3l (character) Specie code identification on 3 characters.
                                    #' @param sample_standardised_length_class_lf Object of class {\link[base]{integer}} expected. Sample standardised length class length fork of measured individus.
                                    #' @param sample_number_measured_extrapolated_lf Object of class {\link[base]{numeric}} expected. Sample number of measured individus extrapolated to all counted individus.
                                    #' @param sample_total_count Object of class {\link[base]{integer}} expected. Sample number of total individus counted.
                                    initialize = function(trip_id,
                                                          well_id,
                                                          sample_id,
                                                          sample_quality,
                                                          sample_type,
                                                          specie_code,
                                                          specie_code3l,
                                                          sample_standardised_length_class_lf,
                                                          sample_number_measured_extrapolated_lf,
                                                          sample_total_count) {
                                      # attribute "trip_id" verification
                                      t3:::check_trip_id(trip_id)
                                      # attribute "well_id" verification
                                      t3:::check_well_id(well_id)
                                      # attribute "sample_id" verification
                                      t3:::check_sample_id(sample_id)
                                      # attribute "sample_quality" verification
                                      t3:::check_sample_quality(sample_quality)
                                      # attribute "sample_type" verification
                                      t3:::check_sample_type(sample_type)
                                      # attribute "specie_code" verification
                                      t3:::check_specie_code(specie_code)
                                      # attribute "specie_code3l" verification
                                      t3:::check_specie_code3l(specie_code3l)
                                      # attribute "sample_standardised_length_class_lf" verification
                                      t3:::check_sample_standardised_length_class_lf(sample_standardised_length_class_lf)
                                      # attribute "sample_number_measured_extrapolated_lf" verification
                                      t3:::check_sample_number_measured_extrapolated_lf(sample_number_measured_extrapolated_lf)
                                      # attribute "sample_total_count" verification
                                      t3:::check_sample_total_count(sample_total_count)
                                      # attributeions
                                      private$trip_id <- trip_id
                                      private$well_id <- well_id
                                      private$sample_id <- sample_id
                                      private$sample_quality <- sample_quality
                                      private$sample_type <- sample_type
                                      private$specie_code <- specie_code
                                      private$specie_code3l <- specie_code3l
                                      private$sample_standardised_length_class_lf <- sample_standardised_length_class_lf
                                      private$sample_number_measured_extrapolated_lf <- sample_number_measured_extrapolated_lf
                                      private$sample_total_count <- sample_total_count
                                    }),
                                  private = list(
                                    trip_id = NULL,
                                    well_id = NULL,
                                    sample_id = NULL,
                                    sample_quality = NULL,
                                    sample_type = NULL,
                                    specie_code = NULL,
                                    specie_code3l = NULL,
                                    sample_standardised_length_class_lf = NULL,
                                    sample_number_measured_extrapolated_lf = NULL,
                                    sample_total_count = NULL))
