# 1 - Checking object_model_data ----
source(system.file("test_data",
                   "test_object_model_data.R",
                   package = "t3"),
       local = TRUE)
# 1.1 - Comparison number of activities between data extracted and data model ----
test_that(desc = "comparison_number_activities_data_extracted_vs_data_model",
          code = {
            expect_equal(object = length(unique(activities$activity_id)),
                         expected = length(object_model_data$.__enclos_env__$private$activities$.__enclos_env__$private$data))
          })
# 1.2 - Comparison number of elementary catches between data extracted and data model ----
test_that(desc = "comparison_number_elementarycatches_data_extracted_vs_data_model",
          code = {
            expect_equal(object = length(unique(elementary_catches$elementarycatch_id)),
                         expected = length(object_model_data$.__enclos_env__$private$elementarycatches$.__enclos_env__$private$data))
          })
# 1.3 - Comparison number of elementary landings between data extracted and data model ----
test_that(desc = "comparison_number_elementarylandings_data_extracted_vs_data_model",
          code = {
            expect_equal(object = length(unique(elementary_landings$elementarylanding_id)),
                         expected = length(object_model_data$.__enclos_env__$private$elementarylandings$.__enclos_env__$private$data))
          })
# 1.4 - Comparison lenght step data between data extracted and data model ----
test_that(desc = "comparison_lengthstep_data_extracted_vs_data_model",
          code = {
            expect_equal(object = dim(length_step),
                         expected = dim(object_model_data$.__enclos_env__$private$lengthstep))
          })
# 1.5 - Comparison lenght weight relationships data between data extracted and data model ----
test_that(desc = "comparison_lengthweightrelationship_data_extracted_vs_data_model",
          code = {
            expect_equal(object = dim(length_weight_relationships),
                         expected = dim(object_model_data$.__enclos_env__$private$lengthweightrelationship))
          })
# 1.6 - Comparison samples data between data extracted and data model ----
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
test_that(desc = "comparison_sampleset_data_extracted_vs_data_model",
          code = {
            expect_equal(object = dim(samples_set),
                         expected = dim(object_model_data$.__enclos_env__$private$sampleset))
          })
# 1.8 - Comparison set duration data between data extracted and data model ----
test_that(desc = "comparison_setduration_data_extracted_vs_data_model",
          code = {
            expect_equal(object = dim(set_duration_ref),
                         expected = dim(object_model_data$.__enclos_env__$private$setdurationref))
          })
# 1.9 - Comparison number of trips between data extracted and data model ----
test_that(desc = "comparison_number_trips_data_extracted_vs_data_model",
          code = {
            expect_equal(object = length(unique(trips$trip_id)),
                         expected = length(object_model_data$.__enclos_env__$private$trips$.__enclos_env__$private$data))
          })
