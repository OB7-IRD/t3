#' @name standardisedsampleset
#' @title R6 class standardisedsampleset creation
#' @description Create R6 reference object class standardisedsampleset
#' @importFrom R6 R6Class
# standardisedsampleset ----
standardisedsampleset <- R6::R6Class(classname = "standardisedsampleset",
                                  public = list(
                                    initialize = function(trip_id,
                                                          activity_id,
                                                          well_id,
                                                          sample_id,
                                                          sample_quality,
                                                          sample_type,
                                                          specie_code3l,
                                                          sample_standardised_length_class_lf,
                                                          sample_number_weighted,
                                                          sample_weigth,
                                                          sample_weight_unit,
                                                          sample_category,
                                                          standardisedsample) {
                                      # attribut "trip_id" verification
                                      t3:::check_trip_id(trip_id)
                                      # attribut "activity_id" verification
                                      t3:::check_activity_id(activity_id)
                                      # attribut "well_id" verification
                                      t3:::check_well_id(well_id)
                                      # attribut "sample_id" verification
                                      t3:::check_sample_id(sample_id)
                                      # attribut "sample_quality" verification
                                      t3:::check_sample_quality(sample_quality)
                                      # attribut "sample_type" verification
                                      t3:::check_sample_type(sample_type)
                                      # attribut "specie_code3l" verification
                                      t3:::check_specie_code3l(specie_code3l)
                                      # attribut "sample_standardised_length_class_lf" verification
                                      t3:::check_sample_standardised_length_class_lf(sample_standardised_length_class_lf)
                                      # attribut "sample_number_weighted" verification
                                      t3:::check_sample_number_weighted(sample_number_weighted)
                                      # attributions
                                      private$trip_id <- trip_id
                                      private$activity_id <- activity_id
                                      private$well_id <- well_id
                                      private$sample_id <- sample_id
                                      private$sample_quality <- sample_quality
                                      private$sample_type <- sample_type
                                      private$specie_code3l <- specie_code3l
                                      private$sample_standardised_length_class_lf <- sample_standardised_length_class_lf
                                      private$sample_number_weighted <- sample_number_weighted
                                      private$sample_weigth <- sample_weigth
                                      private$sample_weight_unit <- sample_weight_unit
                                      private$sample_category <- sample_category
                                      private$standardisedsample <- standardisedsample
                                    }),
                                  private = list(
                                    trip_id = NULL,
                                    activity_id = NULL,
                                    well_id = NULL,
                                    sample_id = NULL,
                                    sample_quality = NULL,
                                    sample_type = NULL,
                                    specie_code3l = NULL,
                                    sample_standardised_length_class_lf = NULL,
                                    sample_number_weighted = NULL,
                                    sample_weigth = NULL,
                                    sample_number_weighted_set = NULL,
                                    sample_weigth_set = NULL,
                                    sample_weight_unit = NULL,
                                    sample_category = NULL,
                                    standardisedsample = NULL))
