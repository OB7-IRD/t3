# 1 - Checking object_model_data ----
load(file = system.file("test_data",
                        "observe_data_test.RData",
                        package = "t3"))
# 101 - Comparison activities between data extracted and data model ----
testthat::test_that(desc = "101. Comparison activities between data extracted and data model",
                    code = {
                      testthat::expect_equal(object = length(unique(activities$activity_id)),
                                             expected = length(object_model_data$.__enclos_env__$private$activities$.__enclos_env__$private$data))
                    })
# 102 - Comparison elementary catches between data extracted and data model ----
testthat::test_that(desc = "102 - Comparison elementary catches between data extracted and data model",
                    code = {
                      testthat::expect_equal(object = length(unique(elementarycatches$elementarycatch_id)),
                                             expected = length(unique(dplyr::bind_rows(object_model_data$.__enclos_env__$private$activities$extract_l1_element_value(element="elementarycatches"))$elementarycatch_id)))
                    })
# 103 - Comparison elementary landings between data extracted and data model ----
testthat::test_that(desc = "103 - Comparison elementary landings between data extracted and data model",
                    code = {
                      testthat::expect_equal(object = length(unique(elementarylandings$elementarylanding_id)),
                                             expected = length(object_model_data$.__enclos_env__$private$elementarylandings$.__enclos_env__$private$data))
                    })
# 104 - Comparison steps data between data extracted and data model ----
testthat::test_that(desc = "104 - Comparison steps data between data extracted and data model",
                    code = {
                      testthat::expect_equal(object = dim(lengthsteps),
                                             expected = dim(object_model_data$.__enclos_env__$private$lengthsteps))
                    })
# 105 - Comparison weight relationships data between data extracted and data model ----
testthat::test_that(desc = "105 - Comparison weight relationships data between data extracted and data model",
                    code = {
                      testthat::expect_equal(object = nrow(lengthweightrelationships),
                                             expected = nrow(object_model_data$.__enclos_env__$private$lengthweightrelationships))
                    })
# 106 - Comparison wells data between data extracted and data model ----
testthat::test_that(desc = "106 - Comparison wells data between data extracted and data model",
                    code = {
                      testthat::expect_gte(object = length(unique(samples$well_id)),
                                           expected = length(object_model_data$.__enclos_env__$private$wells$.__enclos_env__$private$data))
                    })
# 107 - Comparison samples data between data extracted and data model ----
testthat::test_that(desc = "107 - Comparison samples data between data extracted and data model",
                    code = {
                      testthat::expect_gte(object = nrow(samples),
                                           expected = sum(sapply(X = seq_len(length.out = length(object_model_data$.__enclos_env__$private$wells$.__enclos_env__$private$data)),
                                                                 FUN = function(a) {
                                                                   length(unlist(object_model_data$.__enclos_env__$private$wells$.__enclos_env__$private$data[[a]]$.__enclos_env__$private$elementarysampleraw))
                                                                 })))
                    })
# 108 - Comparison sample sets data between data extracted and data model ----
testthat::test_that(desc = "108 - Comparison sample sets data between data extracted and data model",
                    code = {
                      testthat::expect_equal(object = dim(samplesets),
                                             expected = dim(object_model_data$.__enclos_env__$private$samplesets))
                    })
# 109 - Comparison set duration references data between data extracted and data model ----
testthat::test_that(desc = "109 - Comparison set duration references data between data extracted and data model",
                    code = {
                      testthat::expect_equal(object = dim(setdurationrefs),
                                             expected = dim(object_model_data$.__enclos_env__$private$setdurationrefs))
                    })
# 110 - Comparison trips between data extracted and data model ----
testthat::test_that(desc = "110 - Comparison trips between data extracted and data model",
                    code = {
                      testthat::expect_equal(object = length(unique(trips$trip_id)),
                                             expected = length(object_model_data$.__enclos_env__$private$trips$.__enclos_env__$private$data))
                    })

# 111 - Comparison activity code referential data between data extracted and data model ----
testthat::test_that(desc = "111 - Comparison activity code referential data between data extracted and data model",
                    code = {
                      testthat::expect_equal(object = dim(activitycoderefs),
                                             expected = dim(object_model_data$.__enclos_env__$private$activitycoderefs))
                    })

