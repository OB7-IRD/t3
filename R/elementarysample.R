#' @name elementarysample
#' @title R6 class trip creation
#' @description Create R6 reference object class elementarysample
#' @importFrom R6 R6Class
# elementarysample ----
elementarysample <- R6::R6Class(classname = "elementarysample",
                                  public = list(
                                    initialize = function(trip_id,
                                                          well_id,
                                                          sample_id,
                                                          sub_sample_id,
                                                          sample_quality,
                                                          sample_type,
                                                          specie_code3l,
                                                          sample_standardised_length_class_lf,
                                                          sample_number_measured_extrapolated_lf,
                                                          sample_total_count,
                                                          elementarysampleraw) {
                                      # attribut "trip_id" verification
                                      t3:::check_trip_id(trip_id)
                                      # attribut "well_id" verification
                                      t3:::check_well_id(well_id)
                                      # attribut "sample_id" verification
                                      t3:::check_sample_id(sample_id)
                                      # attribut "sub_sample_id" verification
                                      t3:::check_sub_sample_id(sub_sample_id)
                                      # attribut "sample_quality" verification
                                      t3:::check_sample_quality(sample_quality)
                                      # attribut "sample_type" verification
                                      t3:::check_sample_type(sample_type)
                                      # attribut "specie_code3l" verification
                                      t3:::check_specie_code3l(specie_code3l)
                                      # attribut "sample_standardised_length_class_lf" verification
                                      t3:::check_sample_standardised_length_class_lf(sample_standardised_length_class_lf)
                                      # attribut "sample_number_measured_extrapolated_lf" verification
                                      t3:::check_sample_number_measured_extrapolated_lf(sample_number_measured_extrapolated_lf)
                                      # attribut "sample_total_count" verification
                                      t3:::check_sample_total_count(sample_total_count)
                                      # attributions
                                      private$trip_id <- trip_id
                                      private$well_id <- well_id
                                      private$sample_id <- sample_id
                                      private$sub_sample_id <- sub_sample_id
                                      private$sample_quality <- sample_quality
                                      private$sample_type <- sample_type
                                      private$specie_code3l <- specie_code3l
                                      private$sample_standardised_length_class_lf <- sample_standardised_length_class_lf
                                      private$sample_number_measured_extrapolated_lf <- sample_number_measured_extrapolated_lf
                                      private$sample_total_count <- sample_total_count
                                      private$elementarysampleraw <- elementarysampleraw
                                    }),
                                  private = list(
                                    trip_id = NULL,
                                    well_id = NULL,
                                    sample_id = NULL,
                                    sub_sample_id = NULL,
                                    sample_quality = NULL,
                                    sample_type = NULL,
                                    specie_code3l = NULL,
                                    sample_standardised_length_class_lf = NULL,
                                    sample_number_measured_extrapolated_lf = NULL,
                                    sample_total_count = NULL,
                                    elementarysampleraw = NULL))
