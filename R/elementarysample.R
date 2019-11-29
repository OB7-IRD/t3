#' @name elementarysample
#' @title R6 class trip creation
#' @description Create R6 reference object class elementarysample
#' @importFrom R6 R6Class
# elementarysample ----
elementarysample <- R6::R6Class(classname = "elementarysample",
                                public = list(
                                  initialize = function(trip_id,
                                                        sample_id,
                                                        sub_sample_id,
                                                        specie_code3l,
                                                        length_type,
                                                        sample_total_count,
                                                        sample_number_measured,
                                                        sample_length_class) {
                                    # attribut "trip_id" verification
                                    t3:::check_trip_id(trip_id)
                                    # attribut "sample_id" verification
                                    t3:::check_sample_id(sample_id)
                                    # attribut "sub_sample_id" verification
                                    t3:::check_sub_sample_id(sub_sample_id)
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
                                    private$sample_id <- sample_id
                                    private$sub_sample_id <- sub_sample_id
                                    private$specie_code3l <- specie_code3l
                                    private$length_type <- length_type
                                    private$sample_total_count <- sample_total_count
                                    private$sample_number_measured <- sample_number_measured
                                    private$sample_length_class <- sample_length_class
                                  }),
                                private = list(
                                  trip_id = NULL,
                                  sample_id = NULL,
                                  sub_sample_id = NULL,
                                  specie_code3l = NULL,
                                  length_type = NULL,
                                  sample_total_count = NULL,
                                  sample_number_measured = NULL,
                                  rf4 = NULL,
                                  sample_number_measured_extrapolated = NULL,
                                  sample_number_measured_extrapolated_lf = NULL,
                                  sample_length_class = NULL,
                                  sample_length_class_lf = NULL))
