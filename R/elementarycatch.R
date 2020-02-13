#' @name elementarycatch
#' @title R6 class elementarycatch creation
#' @description Create R6 reference object class elementarycatch
#' @importFrom R6 R6Class
elementarycatch <- R6::R6Class(classname = "elementarycatch",
                               public = list(
                                 initialize = function(activity_id,
                                                       elementarycatch_id,
                                                       ocean,
                                                       school_type,
                                                       logbook_category,
                                                       logbook_category_name,
                                                       specie_code3l,
                                                       catch_weight) {
                                   # attribut "activity_id" verification
                                   t3:::check_activity_id(activity_id)
                                   # attribut "elementarycatch_id" verification
                                   t3:::check_elementarycatch_id(elementarycatch_id)
                                   # attribut "ocean" verification
                                   t3:::check_ocean(ocean)
                                   # attribut "school_type" verification
                                   t3:::check_school_type(school_type)
                                   # attribut "logbook_category" verification
                                   t3:::check_logbook_category(logbook_category)
                                   # attribut "logbook_category_name" verification
                                   t3:::check_logbook_category_name(logbook_category_name)
                                   # attribut "specie_code3l" verification
                                   t3:::check_specie_code3l(specie_code3l)
                                   # attribut "catch_weight" verification
                                   t3:::check_catch_weight(catch_weight)
                                   # attributions
                                   private$activity_id <- activity_id
                                   private$elementarycatch_id <- elementarycatch_id
                                   private$ocean <- ocean
                                   private$school_type <- school_type
                                   private$logbook_category <- logbook_category
                                   private$logbook_category_name <- logbook_category_name
                                   private$specie_code3l <- specie_code3l
                                   private$catch_weight <- catch_weight
                                 }),
                               private = list(
                                 activity_id = NULL,
                                 elementarycatch_id = NULL,
                                 ocean = NULL,
                                 school_type = NULL,
                                 logbook_category = NULL,
                                 logbook_category_name = NULL,
                                 specie_code3l = NULL,
                                 catch_weight = NULL,
                                 catch_weight_rf1 = NULL,
                                 catch_weight_rf2 = NULL,
                                 corrected_logbook_category = NULL,
                                 catch_weight_category_corrected = NULL
                               ))
