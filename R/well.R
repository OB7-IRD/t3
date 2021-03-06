#' @name well
#' @title R6 class well creation
#' @description Create R6 reference object class well
#' @importFrom R6 R6Class
# well ----
well <- R6::R6Class(classname = "well",
                    public = list(
                      # initialize ----
                      #' @description Initialize function for R6 well class.
                      #' @param trip_id (character) Trip identification.
                      #' @param well_id (character) Well identification.
                      #' @param well_minus10_weigth (integer) Catch weight of individus less than 10 tonnes (by well, in tonne, all species considerated).
                      #' @param well_plus10_weigth (integer) Catch weight of individus more than 10 tonnes (by well, in tonne, all species considerated).
                      #' @param well_global_weigth (integer) Catch weight of individus (less and more 10 tonnes categories, by well, in tonne, all species considerated).
                      initialize = function(trip_id,
                                            well_id,
                                            well_minus10_weigth,
                                            well_plus10_weigth,
                                            well_global_weigth) {
                        # attribut "trip_id" verification
                        t3:::check_trip_id(trip_id)
                        # attribut "well_id" verification
                        t3:::check_well_id(well_id)
                        # attribut "well_minus10_weigth" verification
                        t3:::check_well_minus10_weigth(well_minus10_weigth)
                        # attribut "well_plus10_weigth" verification
                        t3:::check_well_plus10_weigth(well_plus10_weigth)
                        # attribut "well_global_weigth" verification
                        t3:::check_well_global_weigth(well_global_weigth)
                        # attributions
                        private$trip_id <- trip_id
                        private$well_id <- well_id
                        private$well_minus10_weigth <- well_minus10_weigth
                        private$well_plus10_weigth <- well_plus10_weigth
                        private$well_global_weigth <- well_global_weigth}),
                    private = list(
                      trip_id = NULL,
                      well_id = NULL,
                      well_minus10_weigth = NULL,
                      well_plus10_weigth = NULL,
                      well_global_weigth = NULL,
                      elementarysampleraw = NULL,
                      elementarysample = NULL,
                      standardisedsample = NULL,
                      standardisedsampleset = NULL,
                      wellsets = NULL,
                      proportion_verification = NULL,
                      well_prop_minus10_weigth = NULL,
                      well_prop_plus10_weigth = NULL,
                      wellplan = NULL))
