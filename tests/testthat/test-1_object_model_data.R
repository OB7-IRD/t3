# 1 - Checking object_model_data ----
load(system.file("test_data",
                 "data_test.RData",
                 package = "t3"))
object_model_data <- t3:::object_model_data$new()
# 1.1 - Comparison number of activities between data extracted and data model ----
capture.output(object_model_data$activities_object_creation(data_source = "rdata",
                                                            data_path = system.file("test_data",
                                                                                    "data_test.RData",
                                                                                    package = "t3")),
               file = "NUL")
test_that(desc = "comparison_number_activities_data_extracted_vs_data_model",
          code = {
            expect_equal(object = length(unique(activities$activity_id)),
                         expected = length(object_model_data$.__enclos_env__$private$activities$.__enclos_env__$private$data))
          })

# 1.2 - Comparison number of elementary catches between data extracted and data model ----
capture.output(object_model_data$elementarycatches_object_creation(data_source = "rdata",
                                                                   data_path = system.file("test_data",
                                                                                           "data_test.RData",
                                                                                           package = "t3")),
               file = "NUL")
test_that(desc = "comparison_number_elementarycatches_data_extracted_vs_data_model",
          code = {
            expect_equal(object = length(unique(elementary_catches$elementarycatch_id)),
                         expected = length(object_model_data$.__enclos_env__$private$elementarycatches$.__enclos_env__$private$data))
          })

# 1.3 - Comparison number of elementary landings between data extracted and data model ----
capture.output(object_model_data$elementarylandings_object_creation(data_source = "rdata",
                                                                    data_path = system.file("test_data",
                                                                                            "data_test.RData",
                                                                                            package = "t3")),
               file = "NUL")
test_that(desc = "comparison_number_elementarylandings_data_extracted_vs_data_model",
          code = {
            expect_equal(object = length(unique(elementary_landings$elementarylanding_id)),
                         expected = length(object_model_data$.__enclos_env__$private$elementarylandings$.__enclos_env__$private$data))
          })

# 1.4 - Comparison lenght step data between data extracted and data model ----
capture.output(object_model_data$lengthstep_data(data_source = "rdata",
                                                 data_path = system.file("test_data",
                                                                         "data_test.RData",
                                                                         package = "t3")),
               file = "NUL")
test_that(desc = "comparison_lengthstep_data_extracted_vs_data_model",
          code = {
            expect_equal(object = dim(length_step),
                         expected = dim(object_model_data$.__enclos_env__$private$lengthstep))
          })

# 1.5 - Comparison lenght weight relationships data between data extracted and data model ----
capture.output(object_model_data$lengthweightrelationship_data(data_source = "rdata",
                                                               data_path = system.file("test_data",
                                                                                       "data_test.RData",
                                                                                       package = "t3")),
               file = "NUL")
test_that(desc = "comparison_lengthweightrelationship_data_extracted_vs_data_model",
          code = {
            expect_equal(object = dim(length_weight_relationships),
                         expected = dim(object_model_data$.__enclos_env__$private$lengthweightrelationship))
          })

# 1.6 - Comparison samples data between data extracted and data model ----
capture.output(object_model_data$wells_object_creation(data_source = "rdata",
                                                       data_path_samples = system.file("test_data",
                                                                                       "data_test.RData",
                                                                                       package = "t3"),
                                                       data_path_wellplans = system.file("test_data",
                                                                                         "data_test.RData",
                                                                                         package = "t3")),
               file = "NUL")

test_that(desc = "comparison_number_well_samples_data_extracted_vs_data_model",
          code = {
            expect_equal(object = c(length(unique(samples$well_id)),
                                    nrow(samples)),
                         expected = c(length(object_model_data$.__enclos_env__$private$wells$.__enclos_env__$private$data),
                                      sum(sapply(X = seq_len(length.out = length(object_model_data$.__enclos_env__$private$wells$.__enclos_env__$private$data)),
                                                 FUN = function(a) {
                                                   length(unlist(object_model_data$.__enclos_env__$private$wells$.__enclos_env__$private$data[[a]]$.__enclos_env__$private$elementarysampleraw))
                                                 }))))
          })

# 1.7 - Comparison samples set data between data extracted and data model ----
capture.output(object_model_data$sampleset_data(data_source = "rdata",
                                                data_path = system.file("test_data",
                                                                        "data_test.RData",
                                                                        package = "t3")),
               file = "NUL")
test_that(desc = "comparison_sampleset_data_extracted_vs_data_model",
          code = {
            expect_equal(object = dim(samples_set),
                         expected = dim(object_model_data$.__enclos_env__$private$sampleset))
          })

# 1.8 - Comparison set duration data between data extracted and data model ----
capture.output(object_model_data$setduration_data(data_source = "rdata",
                                                  data_path = system.file("test_data",
                                                                          "data_test.RData",
                                                                          package = "t3")),
               file = "NUL")
test_that(desc = "comparison_setduration_data_extracted_vs_data_model",
          code = {
            expect_equal(object = dim(set_duration_ref),
                         expected = dim(object_model_data$.__enclos_env__$private$setdurationref))
          })

# 1.9 - Comparison number of trips between data extracted and data model ----
capture.output(object_model_data$trips_object_creation(data_source = "rdata",
                                                       data_path = system.file("test_data",
                                                                               "data_test.RData",
                                                                               package = "t3")),
               file = "NUL")
test_that(desc = "comparison_number_trips_data_extracted_vs_data_model",
          code = {
            expect_equal(object = length(unique(trips$trip_id)),
                         expected = length(object_model_data$.__enclos_env__$private$trips$.__enclos_env__$private$data))
          })
