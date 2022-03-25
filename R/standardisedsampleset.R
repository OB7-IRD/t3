#' @name standardisedsampleset
#' @title R6 class standardisedsampleset
#' @description Create R6 reference object class standardisedsampleset
#' @importFrom R6 R6Class
standardisedsampleset <- R6::R6Class(classname = "standardisedsampleset",
                                  public = list(
                                    # initialize ----
                                    #' @description Initialize function for R6 standardisedsampleset class.
                                    #' @param trip_id Object of class {\link[base]{character}} expected. Trip identification.
                                    #' @param activity_id Object of class {\link[base]{character}} expected. Activity identification.
                                    #' @param well_id Object of class {\link[base]{character}} expected. Well identification.
                                    #' @param sample_id Object of class {\link[base]{character}} expected. Sample identification.
                                    #' @param sample_quality Object of class {\link[base]{integer}} expected. Sample quality identification.
                                    #' @param sample_type Object of class {\link[base]{integer}} expected. Sample type identification.
                                    #' @param specie_code Object of class {\link[base]{integer}} expected. Specie code identification.
                                    #' @param specie_code3l Object of class {\link[base]{character}} expected. Specie code identification on 3 characters.
                                    #' @param sample_standardised_length_class_lf Object of class {\link[base]{integer}} expected. Sample standardised length class length fork of measured individus.
                                    #' @param sample_number_weighted Object of class {\link[base]{numeric}} expected. Sample number of measured individus extrapolated to all counted individus and weighted by set weight.
                                    #' @param sample_weigth Object of class {\link[base]{numeric}} expected. Weight (kg) of the sample number of measured individus extrapolated to all counted individus (conversion by length weight relationship: coeficient a * length class lf ^ coeficient b).
                                    #' @param sample_weight_unit Object of class {\link[base]{numeric}} or NA expected. Weight (kg) of one individu calculated by the length weight relationship: coeficient a * length class lf ^ coeficient b).
                                    #' @param sample_category Object of class {\link[base]{character}} expected. Sample category: -10kg or +10kg.
                                    initialize = function(trip_id,
                                                          activity_id,
                                                          well_id,
                                                          sample_id,
                                                          sample_quality,
                                                          sample_type,
                                                          specie_code,
                                                          specie_code3l,
                                                          sample_standardised_length_class_lf,
                                                          sample_number_weighted,
                                                          sample_weigth,
                                                          sample_weight_unit,
                                                          sample_category) {
                                      # attribute "trip_id" verification
                                      t3:::check_trip_id(trip_id)
                                      # attribute "activity_id" verification
                                      t3:::check_activity_id(activity_id)
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
                                      # attribute "sample_number_weighted" verification
                                      t3:::check_sample_number_weighted(sample_number_weighted)
                                      # attributeions
                                      private$trip_id <- trip_id
                                      private$activity_id <- activity_id
                                      private$well_id <- well_id
                                      private$sample_id <- sample_id
                                      private$sample_quality <- sample_quality
                                      private$sample_type <- sample_type
                                      private$specie_code <- specie_code
                                      private$specie_code3l <- specie_code3l
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
                                    sample_quality = NULL,
                                    sample_type = NULL,
                                    specie_code = NULL,
                                    specie_code3l = NULL,
                                    sample_standardised_length_class_lf = NULL,
                                    sample_number_weighted = NULL,
                                    sample_weigth = NULL,
                                    sample_number_weighted_set = NULL,
                                    sample_weigth_set = NULL,
                                    sample_weight_unit = NULL,
                                    sample_category = NULL))
