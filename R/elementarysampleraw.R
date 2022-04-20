#' @name elementarysampleraw
#' @title R6 class trip
#' @description Create R6 reference object class elementarysampleraw
#' @importFrom R6 R6Class
# elementarysampleraw ----
elementarysampleraw <- R6::R6Class(classname = "elementarysampleraw",
                                   public = list(
                                     # initialize ----
                                     #' @description Initialize function for R6 elementarysampleraw class.
                                     #' @param trip_id Object of class {\link[base]{character}} expected. Trip identification.
                                     #' @param well_id Object of class {\link[base]{character}} expected. Well identification.
                                     #' @param sample_id Object of class {\link[base]{character}} expected. Sample identification.
                                     #' @param sample_quality Object of class {\link[base]{integer}} expected. Sample quality identification.
                                     #' @param sub_sample_id Object of class {\link[base]{integer}} expected. Sub sample identification.
                                     #' @param sub_sample_id_total_count Object of class {\link[base]{integer}} expected. Sub sample identification bis in relation with the fish total count.
                                     #' @param elementarysampleraw_id Object of class {\link[base]{character}} expected. Elementary sample raw identification.
                                     #' @param sample_type Object of class {\link[base]{integer}} expected. Sample type identification.
                                     #' @param specie_code Object of class {\link[base]{integer}} expected. Specie code identification.
                                     #' @param specie_code3l Object of class {\link[base]{character}} expected. Specie code identification on 3 characters.
                                     #' @param length_type Object of class {\link[base]{integer}} expected. Length type identification, 1 for LD1 and 2 for LF.
                                     #' @param sample_total_count Object of class {\link[base]{integer}} expected. Sample number of total individus counted.
                                     #' @param sample_number_measured Object of class {\link[base]{integer}} expected. Sample number of measured individus.
                                     #' @param sample_length_class Object of class {\link[base]{integer}} expected. Sample length class of measured individus.
                                     initialize = function(trip_id,
                                                           well_id,
                                                           sample_id,
                                                           sub_sample_id,
                                                           sub_sample_id_total_count,
                                                           elementarysampleraw_id,
                                                           sample_quality,
                                                           sample_type,
                                                           specie_code,
                                                           specie_code3l,
                                                           length_type,
                                                           sample_total_count,
                                                           sample_number_measured,
                                                           sample_length_class) {
                                       # attribute "trip_id" verification
                                       check_trip_id(trip_id)
                                       # attribute "well_id" verification
                                       check_well_id(well_id)
                                       # attribute "sample_id" verification
                                       check_sample_id(sample_id)
                                       # attribute "sub_sample_id" verification
                                       check_sub_sample_id(sub_sample_id)
                                       # attribute "sub_sample_id_total_count" verification
                                       check_sub_sample_id_total_count(sub_sample_id_total_count)
                                       # attribute "elementarysampleraw_id" verification
                                       check_elementarysampleraw_id(elementarysampleraw_id)
                                       # attribute "sample_quality" verification
                                       check_sample_quality(sample_quality)
                                       # attribute "sample_type" verification
                                       check_sample_type(sample_type)
                                       # attribute "specie_code" verification
                                       check_specie_code(specie_code)
                                       # attribute "specie_code3l" verification
                                       check_specie_code3l(specie_code3l)
                                       # attribute "length_type" verification
                                       check_length_type(length_type)
                                       # attribute "sample_total_count" verification
                                       check_sample_total_count(sample_total_count)
                                       # attribute "sample_number_measured" verification
                                       check_sample_number_measured(sample_number_measured)
                                       # attribute "sample_length_class" verification
                                       check_sample_length_class(sample_length_class)
                                       # attributeions
                                       private$trip_id <- trip_id
                                       private$well_id <- well_id
                                       private$sample_id <- sample_id
                                       private$sub_sample_id <- sub_sample_id
                                       private$sub_sample_id_total_count <- sub_sample_id_total_count
                                       private$elementarysampleraw_id <- elementarysampleraw_id
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
                                     sub_sample_id_total_count = NULL,
                                     elementarysampleraw_id = NULL,
                                     sample_quality = NULL,
                                     sample_type = NULL,
                                     specie_code = NULL,
                                     specie_code3l = NULL,
                                     length_type = NULL,
                                     sample_total_count = NULL,
                                     sample_number_measured = NULL,
                                     rf4 = NULL,
                                     sample_number_measured_lf = NULL,
                                     sample_number_measured_extrapolated_lf = NULL,
                                     sample_length_class = NULL,
                                     sample_length_class_lf = NULL))
