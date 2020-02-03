#' @name wellset
#' @title R6 class wellset creation
#' @description Create R6 reference object class wellset
#' @importFrom R6 R6Class
# wellset ----
wellset <- R6::R6Class(classname = "wellset",
                       public = list(
                         initialize = function(trip_id,
                                               activity_id,
                                               well_id,
                                               sample_id,
                                               weighted_weight,
                                               weighted_weight_minus10,
                                               weighted_weight_plus10) {
                           # attribut "trip_id" verification
                           t3:::check_trip_id(trip_id)
                           # attribut "well_id" verification
                           t3:::check_well_id(well_id)
                           # attribut "activity_id" verification
                           t3:::check_activity_id(activity_id)
                           # attribut "sample_id" verification
                           t3:::check_sample_id(sample_id)
                           # attribut "weighted_weight" verification
                           t3:::check_weighted_weight(weighted_weight)
                           # attributions
                           private$trip_id <- trip_id
                           private$activity_id <- activity_id
                           private$well_id <- well_id
                           private$sample_id <- sample_id
                           private$weighted_weight <- weighted_weight
                           private$weighted_weight_minus10 <- weighted_weight_minus10
                           private$weighted_weight_plus10 <- weighted_weight_plus10
                         }),
                       private = list(
                         trip_id = NULL,
                         activity_id = NULL,
                         well_id = NULL,
                         sample_id = NULL,
                         weighted_weight = NULL,
                         prop_weighted_weight = NULL,
                         weighted_weight_minus10 = NULL,
                         weighted_weight_plus10 = NULL,
                         weighted_samples_minus10 = NULL,
                         weighted_samples_plus10 = NULL,
                         weighted_samples_total = NULL,
                         rf_minus10 = NULL,
                         rf_plus10 = NULL,
                         rf_total = NULL,
                         rf_validation = NULL))
