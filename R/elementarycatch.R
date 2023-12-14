#' @name elementarycatch
#' @title R6 class elementarycatch
#' @description Create R6 reference object class elementarycatch
#' @importFrom R6 R6Class
elementarycatch <- R6::R6Class(classname = "elementarycatch",
                               public = list(
                                 # initialize ----
                                 #' @description Initialize function for R6 activities class.
                                 #' @param activity_id Object of class {\link[base]{character}} expected. Activity identification.
                                 #' @param elementarycatch_id Object of class {\link[base]{character}} expected. Elementary catch identification.
                                 #' @param ocean_code Object of class {\link[base]{integer}} expected. Ocean identification.
                                 #' @param school_type_code Object of class {\link[base]{integer}} expected. School type identification.
                                 #' @param weight_category_code Object of class {\link[base]{integer}} expected. Logbook weight category.
                                 #' @param weight_category_label Object of class {\link[base]{character}} expected. Logbook category name identification.
                                 #' @param species_code Object of class {\link[base]{integer}} expected. Species code identification.
                                 #' @param species_fao_code Object of class {\link[base]{character}} expected. Species code identification on 3 characters.
                                 #' @param species_fate_code Object of class {\link[base]{integer}} expected. Species fate code identification.
                                 #' @param catch_weight Object of class {\link[base]{numeric}} expected. Catch weight in tonnes.
                                 initialize = function(activity_id,
                                                       elementarycatch_id,
                                                       ocean_code,
                                                       school_type_code,
                                                       weight_category_code,
                                                       weight_category_label,
                                                       species_code,
                                                       species_fao_code,
                                                       species_fate_code,
                                                       catch_weight) {
                                   # 1 - Arguments verifications ----
                                   codama::r_type_checking(r_object = activity_id,
                                                           type = "character",
                                                           length = 1L)
                                   codama::r_type_checking(r_object = elementarycatch_id,
                                                           type = "character",
                                                           length = 1L)
                                   codama::r_type_checking(r_object = ocean_code,
                                                           type = "integer",
                                                           length = 1L)
                                   codama::r_type_checking(r_object = school_type_code,
                                                           type = "integer",
                                                           length = 1L)
                                   codama::r_type_checking(r_object = weight_category_code,
                                                           type = "character",
                                                           length = 1L)
                                   codama::r_type_checking(r_object = weight_category_label,
                                                           type = "character",
                                                           length = 1L)
                                   codama::r_type_checking(r_object = species_code,
                                                           type = "integer",
                                                           length = 1L)
                                   codama::r_type_checking(r_object = species_fao_code,
                                                           type = "character",
                                                           length = 1L)
                                   codama::r_type_checking(r_object = species_fate_code,
                                                           type = "integer",
                                                           length = 1L)
                                   codama::r_type_checking(r_object = catch_weight,
                                                           type = "numeric",
                                                           length = 1L)
                                   # 2 - Attributions ----
                                   private$activity_id <- activity_id
                                   private$elementarycatch_id <- elementarycatch_id
                                   private$ocean_code <- ocean_code
                                   private$school_type_code <- school_type_code
                                   private$weight_category_code <- weight_category_code
                                   private$weight_category_label <- weight_category_label
                                   private$species_code <- species_code
                                   private$species_fao_code <- species_fao_code
                                   private$species_fate_code <- species_fate_code
                                   private$catch_weight <- catch_weight
                                 }),
                               private = list(
                                 activity_id = NULL,
                                 elementarycatch_id = NULL,
                                 ocean_code = NULL,
                                 school_type_code = NULL,
                                 weight_category_code = NULL,
                                 weight_category_label = NULL,
                                 species_code = NULL,
                                 species_fao_code = NULL,
                                 species_fate_code = NULL,
                                 catch_weight = NULL,
                                 catch_weight_rf1 = NULL,
                                 catch_weight_rf2 = NULL,
                                 corrected_logbook_category = NULL,
                                 catch_weight_category_corrected = NULL
                               ))
