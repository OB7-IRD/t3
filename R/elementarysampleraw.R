#' @name elementarysampleraw
#' @title R6 class trip
#' @description Create R6 reference object class elementarysampleraw
#' @importFrom R6 R6Class
#' @importFrom codama r_type_checking
elementarysampleraw <- R6::R6Class(classname = "elementarysampleraw",
                                   public = list(
                                     # initialize ----
                                     #' @description Initialize function for R6 elementarysampleraw class.
                                     #' @param trip_id Object of class {\link[base]{character}} expected. Trip identification.
                                     #' @param well_id Object of class {\link[base]{character}} expected. Well identification.
                                     #' @param sample_id Object of class {\link[base]{character}} expected. Sample identification.
                                     #' @param sub_sample_id Object of class {\link[base]{integer}} expected. Sub sample identification.
                                     #' @param sub_sample_total_count_id Object of class {\link[base]{integer}} expected. Sub sample identification bis in relation with the fish total count.
                                     #' @param elementarysampleraw_id Object of class {\link[base]{character}} expected. Elementary sample raw identification.
                                     #' @param sample_quality_code Object of class {\link[base]{integer}} expected. Sample quality identification.
                                     #' @param sample_type_code Object of class {\link[base]{integer}} expected. Sample type identification.
                                     #' @param species_code Object of class {\link[base]{integer}} expected. Specie code identification.
                                     #' @param species_fao_code Object of class {\link[base]{character}} expected. Specie code identification on 3 characters.
                                     #' @param size_measure_type_code Object of class {\link[base]{integer}} expected. Length type identification.
                                     #' @param sample_total_count Object of class {\link[base]{integer}} expected. Sample number of total individus counted.
                                     #' @param sample_number_measured Object of class {\link[base]{integer}} expected. Sample number of measured individus.
                                     #' @param sample_length_class Object of class {\link[base]{integer}} expected. Sample length class of measured individus.
                                     initialize = function(trip_id,
                                                           well_id,
                                                           sample_id,
                                                           sub_sample_id,
                                                           sub_sample_total_count_id,
                                                           elementarysampleraw_id,
                                                           sample_quality_code,
                                                           sample_type_code,
                                                           species_code,
                                                           species_fao_code,
                                                           size_measure_type_code,
                                                           sample_total_count,
                                                           sample_number_measured,
                                                           sample_length_class) {
                                       # 1 - Arguments verifications ----
                                       codama::r_type_checking(r_object = trip_id,
                                                               type = "character",
                                                               length = 1L)
                                       codama::r_type_checking(r_object = well_id,
                                                               type = "character",
                                                               length = 1L)
                                       codama::r_type_checking(r_object = sample_id,
                                                               type = "character",
                                                               length = 1L)
                                       codama::r_type_checking(r_object = sub_sample_id,
                                                               type = "integer",
                                                               length = 1L)
                                       codama::r_type_checking(r_object = sub_sample_total_count_id,
                                                               type = "character",
                                                               length = 1L)
                                       codama::r_type_checking(r_object = elementarysampleraw_id,
                                                               type = "character",
                                                               length = 1L)
                                       codama::r_type_checking(r_object = sample_quality_code,
                                                               type = "integer",
                                                               length = 1L)
                                       codama::r_type_checking(r_object = sample_type_code,
                                                               type = "integer",
                                                               length = 1L)
                                       codama::r_type_checking(r_object = species_code,
                                                               type = "integer",
                                                               length = 1L)
                                       codama::r_type_checking(r_object = species_fao_code,
                                                               type = "character",
                                                               length = 1L)
                                       codama::r_type_checking(r_object = size_measure_type_code,
                                                               type = "character",
                                                               length = 1L)
                                       codama::r_type_checking(r_object = sample_total_count,
                                                               type = "integer",
                                                               length = 1L)
                                       codama::r_type_checking(r_object = sample_number_measured,
                                                               type = "integer",
                                                               length = 1L)
                                       codama::r_type_checking(r_object = sample_length_class,
                                                               type = "numeric",
                                                               length = 1L)
                                       # 2 - Attributions ----
                                       private$trip_id <- trip_id
                                       private$well_id <- well_id
                                       private$sample_id <- sample_id
                                       private$sub_sample_id <- sub_sample_id
                                       private$sub_sample_total_count_id <- sub_sample_total_count_id
                                       private$elementarysampleraw_id <- elementarysampleraw_id
                                       private$sample_quality_code <- sample_quality_code
                                       private$sample_type_code <- sample_type_code
                                       private$species_code <- species_code
                                       private$species_fao_code <- species_fao_code
                                       private$size_measure_type_code <- size_measure_type_code
                                       private$sample_total_count <- sample_total_count
                                       private$sample_number_measured <- sample_number_measured
                                       private$sample_length_class <- sample_length_class
                                     }),
                                   private = list(
                                     trip_id = NULL,
                                     well_id = NULL,
                                     sample_id = NULL,
                                     sub_sample_id = NULL,
                                     sub_sample_total_count_id = NULL,
                                     elementarysampleraw_id = NULL,
                                     sample_quality_code = NULL,
                                     sample_type_code = NULL,
                                     species_code = NULL,
                                     species_fao_code = NULL,
                                     size_measure_type_code  = NULL,
                                     sample_total_count = NULL,
                                     sample_number_measured = NULL,
                                     rf4 = NULL,
                                     sample_number_measured_lf = NULL,
                                     sample_number_measured_extrapolated_lf = NULL,
                                     sample_length_class = NULL,
                                     sample_length_class_lf = NULL))
