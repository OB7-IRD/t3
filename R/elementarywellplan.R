#' @name elementarywellplan
#' @title R6 class elementarywellplan creation
#' @description Create R6 reference object class elementarywellplan
#' @importFrom R6 R6Class
# elementarywellplan ----
elementarywellplan <- R6::R6Class(classname = "elementarywellplan",
                                  public = list(
                                    # initialize ----
                                    #' @description Initialize function for R6 elementarywellplan class.
                                    #' @param wellplan_id (character) Wellplan identification.
                                    #' @param well_id (character) Well identification.
                                    #' @param activity_id (character) Activity identification.
                                    #' @param sample_id (character) Sample identification.
                                    #' @param specie_code3l (character) Specie code identification on 3 characters.
                                    #' @param wellplan_weight (numeric) Weight in tonnes filled in the well plan.
                                    #' @param wellplan_number (integer) Well plan number of individus.
                                    #' @param wellplan_weigth_category_code (integer) Well plan category code identification.
                                    #' @param wellplan_weigth_category_label (character) Well plan weight category identification.
                                    initialize = function(wellplan_id,
                                                          well_id,
                                                          activity_id,
                                                          sample_id,
                                                          specie_code3l,
                                                          wellplan_weight,
                                                          wellplan_number,
                                                          wellplan_weigth_category_code,
                                                          wellplan_weigth_category_label) {
                                      # attribut "wellplan_id" verification
                                      t3:::check_wellplan_id(wellplan_id)
                                      # attribut "well_id" verification
                                      t3:::check_well_id(well_id)
                                      # attribut "activity_id" verification
                                      t3:::check_activity_id(activity_id)
                                      # attribut "sample_id" verification
                                      t3:::check_sample_id(sample_id)
                                      # attribut "specie_code3l" verification
                                      t3:::check_specie_code3l(specie_code3l)
                                      # attribut "wellplan_weight" verification
                                      t3:::check_wellplan_weight(wellplan_weight)
                                      # attribut "wellplan_number" verification
                                      t3:::check_wellplan_number(wellplan_number)
                                      # attribut "wellplan_weigth_category_code" verification
                                      t3:::check_wellplan_weigth_category_code(wellplan_weigth_category_code)
                                      # attribut "wellplan_weigth_category_label" verification
                                      t3:::check_wellplan_weigth_category_label(wellplan_weigth_category_label)
                                      # attributions
                                      private$wellplan_id <- wellplan_id
                                      private$well_id <- well_id
                                      private$activity_id <- activity_id
                                      private$sample_id <- sample_id
                                      private$specie_code3l <- specie_code3l
                                      private$wellplan_weight <- wellplan_weight
                                      private$wellplan_number <- wellplan_number
                                      private$wellplan_weigth_category_code <- wellplan_weigth_category_code
                                      private$wellplan_weigth_category_label <- wellplan_weigth_category_label}),
                                  private = list(
                                    wellplan_id = NULL,
                                    well_id = NULL,
                                    activity_id = NULL,
                                    sample_id = NULL,
                                    specie_code3l = NULL,
                                    wellplan_weight = NULL,
                                    wellplan_number = NULL,
                                    wellplan_weigth_category_code = NULL,
                                    wellplan_weigth_category_label = NULL))
