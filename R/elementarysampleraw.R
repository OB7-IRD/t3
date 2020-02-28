#' @name elementarysampleraw
#' @title R6 class trip creation
#' @description Create R6 reference object class elementarysampleraw
#' @importFrom R6 R6Class
# elementarysampleraw ----
elementarysampleraw <- R6::R6Class(classname = "elementarysampleraw",
                                   public = list(
                                     # initialize ----
                                     #' @description Initialize function for R6 elementarysampleraw class.
                                     #' @param trip_id (character) Trip identification.
                                     #' @param well_id (character) Well identification.
                                     #' @param sample_id (character) Sample identification.
                                     #' @param sample_quality (integer) Sample quality identification.
                                     #' @param sub_sample_id (integer) Sub sample identification.
                                     #' @param sample_type (integer) Sample type identification.
                                     #' @param specie_code (integer) Specie code identification.
                                     #' @param specie_code3l (character) Specie code identification on 3 characters.
                                     #' @param length_type (integer) Length type identification, 1 for LD1 and 2 for LF.
                                     #' @param sample_total_count (integer) Sample number of total individus counted.
                                     #' @param sample_number_measured (integer) Sample number of measured individus.
                                     #' @param sample_length_class (integer) Sample length class of measured individus.
                                     initialize = function(trip_id,
                                                           well_id,
                                                           sample_id,
                                                           sub_sample_id,
                                                           sample_quality,
                                                           sample_type,
                                                           specie_code,
                                                           specie_code3l,
                                                           length_type,
                                                           sample_total_count,
                                                           sample_number_measured,
                                                           sample_length_class) {
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
                                       # attribut "specie_code" verification
                                       t3:::check_specie_code(specie_code)
                                       # attribut "specie_code3l" verification
                                       t3:::check_specie_code3l(specie_code3l)
                                       # attribut "length_type" verification
                                       t3:::check_length_type(length_type)
                                       # attribut "sample_total_count" verification
                                       t3:::check_sample_total_count(sample_total_count)
                                       # attribut "sample_number_measured" verification
                                       t3:::check_sample_number_measured(sample_number_measured)
                                       # attribut "sample_length_class" verification
                                       t3:::check_sample_length_class(sample_length_class)
                                       # attributions
                                       private$trip_id <- trip_id
                                       private$well_id <- well_id
                                       private$sample_id <- sample_id
                                       private$sub_sample_id <- sub_sample_id
                                       private$sample_quality <- sample_quality
                                       private$sample_type <- sample_type
                                       private$specie_code <- specie_code
                                       private$specie_code3l <- specie_code3l
                                       private$length_type <- length_type
                                       private$sample_total_count <- sample_total_count
                                       private$sample_number_measured <- sample_number_measured
                                       private$sample_length_class <- sample_length_class
                                     }),
                                   private = list(
                                     trip_id = NULL,
                                     well_id = NULL,
                                     sample_id = NULL,
                                     sub_sample_id = NULL,
                                     sample_quality = NULL,
                                     sample_type = NULL,
                                     specie_code = NULL,
                                     specie_code3l = NULL,
                                     length_type = NULL,
                                     sample_total_count = NULL,
                                     sample_number_measured = NULL,
                                     rf4 = NULL,
                                     sample_number_measured_extrapolated = NULL,
                                     sample_number_measured_extrapolated_lf = NULL,
                                     sample_length_class = NULL,
                                     sample_length_class_lf = NULL))
