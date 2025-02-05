# 2 - Checking full_trips ----
load(file = system.file("test_data",
                        "observe_data_test.RData",
                        package = "t3"))
species_fao_codes_rf1_fr <- c("YFT", "SKJ", "BET", "ALB", "MIX", "LOT")
# model creation ----
# initialisation object for full trips class
object_full_trips <- t3:::full_trips$new()
# object full_trip creation
capture.output(object_full_trips$create_full_trips(object_trips = object_model_data$.__enclos_env__$private$trips),
               file = "NUL")
# add activities to trips selected
capture.output(object_full_trips$add_activities(object_activities = object_model_data$.__enclos_env__$private$activities),
               file = "NUL")
# filter on reference year
capture.output(object_full_trips$filter_by_years_period(years_period = as.integer(c(1884,
                                                                                    1885))),
               file = "NUL")
# add elementarycatches to trips selected
capture.output(object_full_trips$add_elementarycatches(object_elementarycatches = object_model_data$.__enclos_env__$private$elementarycatches),
               file = "NUL")
# add elementarylandings to trips selected
capture.output(object_full_trips$add_elementarylandings(object_elementarylandings = object_model_data$.__enclos_env__$private$elementarylandings),
               file = "NUL")
# add well(s) and sample(s) to trip(s) selected
capture.output(object_full_trips$add_wells_samples(object_wells = object_model_data$.__enclos_env__$private$wells),
               file = "NUL")
# level 1 process ----
# level 1.1: rf1
species_fao_codes_rf1 <- c("YFT", "SKJ", "BET", "ALB", "MIX", "LOT")
species_fate_codes_rf1 = as.integer(c(6, 11))
capture.output(object_full_trips$rf1(species_fao_codes_rf1 = species_fao_codes_rf1,
                                     species_fate_codes_rf1 = species_fate_codes_rf1),
               file = "NUL")
# level 1.2: rf2
capture.output(object_full_trips$rf2(),
               file = "NUL")
# level 1.3: logbook weigth categories conversion ----
capture.output(object_full_trips$conversion_weigth_category(),
               file = "NUL")
# level 1.4: set count ----
capture.output(object_full_trips$set_count(),
               file = "NUL")

# level 1.5: set duration ----
capture.output(object_full_trips$set_duration(set_duration_ref = object_model_data$.__enclos_env__$private$setdurationrefs),
               file = "NUL")
# level 1.6: time at sea ----
capture.output(object_full_trips$time_at_sea(),
               file = "NUL")
# level 1.7: fishing time ----
capture.output(object_full_trips$fishing_time(),
               file = "NUL")
# level 1.8: searching time ----
capture.output(object_full_trips$searching_time(),
               file = "NUL")
# level 2.1: sample length class ld1 to lf conversion ----
capture.output(object_full_trips$sample_length_class_ld1_to_lf(length_step = object_model_data$.__enclos_env__$private$lengthsteps),
               file = "NUL")
# level 2.2: sample number measured extrapolation ----
capture.output(object_full_trips$sample_number_measured_extrapolation(),
               file = "NUL")
# level 2.3: sample step length class standardisation ----
capture.output(object_full_trips$sample_length_class_step_standardisation(),
               file = "NUL")
# level 2.4: well set weight categories ----
capture.output(object_full_trips$well_set_weigth_categories(sample_set = object_model_data$.__enclos_env__$private$samplesets),
               file = "NUL")
# level 2.5: standardised sample creation ----
capture.output(object_full_trips$standardised_sample_creation(),
               file = "NUL")
# level 2.6: sample number standardisation ----
capture.output(object_full_trips$standardised_sample_set_creation(length_weight_relationship_data = object_model_data$.__enclos_env__$private$lengthweightrelationships),
               file = "NUL")
# level 2.7: raised factors determination ----
capture.output(object_full_trips$raised_factors_determination(),
               file = "NUL")

# level 2.8: samples number standardisation at set scale ----
capture.output(object_full_trips$raised_standardised_sample_set(),
               file = "NUL")
# path to level 3 ----
capture.output(object_full_trips$path_to_level3(),
               file = "NUL")
for (full_trip_id in seq_len(length.out = length(x = object_full_trips$.__enclos_env__$private$data_selected))) {
  capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                 file = "NUL")
  capture.output(current_trips$add(object_full_trips$.__enclos_env__$private$data_selected[[full_trip_id]]),
                 file = "NUL")
  current_sum_elementarycatches_rf1 <- 0
  current_sum_elementarycatches_rf2 <- 0
  current_sum_elementarycatches_rf2_all <- 0
  current_sum_elementarylandings <- 0
  current_sum_catch_weight_category_corrected <- 0
  for (partial_trip_id in seq_len(length.out = current_trips$count())) {
    current_trip <- current_trips$extract(id = partial_trip_id)[[1]]
    current_time_at_sea <- current_trip$.__enclos_env__$private$time_at_sea
    # 209 - Checking if variable "time_at_sea" is filled and in the correct format according to the process 1.6 ----
    testthat::test_that(desc = "209 - Checking if variable \"time_at_sea\" is filled and in the correct format according to the process 1.6",
                        code = {
                          testthat::expect_true(object = (is.na(current_time_at_sea)
                                                          || (is.numeric(current_time_at_sea)
                                                              & current_time_at_sea > 0)),
                                                label = paste0("issue with the full trip ", full_trip_id,
                                                               " and the partial trip ", partial_trip_id))
                        })
    current_fishing_time <- current_trip$.__enclos_env__$private$fishing_time
    # 210 - Checking if variable "fishing_time" is filled and in the correct format according to the process 1.7 ----
    testthat::test_that(desc = "210 - Checking if variable \"fishing_time\" is filled and in the correct format according to the process 1.7",
                        code = {
                          testthat::expect_true(object = (is.na(current_time_at_sea)
                                                          || (is.numeric(current_fishing_time)
                                                              & current_fishing_time > 0)),
                                                label = paste0("issue with the full trip ", full_trip_id,
                                                               " and the partial trip ", partial_trip_id))
                        })
    current_searching_time <- current_trip$.__enclos_env__$private$searching_time
    # 211 - Checking if variable "searching_time" is filled and in the correct format according to the process 1.8 ----
    testthat::test_that(desc = "211 - Checking if variable \"searching_time\" is filled and in the correct format according to the process 1.8",
                        code = {
                          testthat::expect_true(object = (is.na(current_time_at_sea)
                                                          || (is.numeric(current_searching_time)
                                                              & current_searching_time > 0)),
                                                label = paste0("issue with the full trip ", full_trip_id,
                                                               " and the partial trip ", partial_trip_id))
                        })
    current_status_rf1 <- current_trip$.__enclos_env__$private$statut_rf1
    current_status_rf2 <- current_trip$.__enclos_env__$private$statut_rf2
    current_rf1 <- current_trip$.__enclos_env__$private$rf1
    current_rf2 <- current_trip$.__enclos_env__$private$rf2
    # 201 - Checking if rf1 process was applied on all trips ----
    testthat::test_that(desc = "202 - Checking if rf1 process was applied on all trips",
                        code = {
                          testthat::expect_true(object = (is.na(current_rf1)
                                                          || (is.numeric(current_rf1)
                                                              & (current_trip$.__enclos_env__$private$statut_rf1 %in% c(1.1, 1.2, 1.3, 1.4, 2.1, 2.2, 2.3, 2.4)))),
                                                label = paste0("issue with the full trip ", full_trip_id,
                                                               " and the partial trip ", partial_trip_id))
                        })
    # 203 - Checking if rf2 process was applied on all trips ----
    testthat::test_that(desc = "203 - Checking if rf2 process was applied on all trips",
                        code = {
                          testthat::expect_true(object = (is.na(current_rf2)
                                                          || (is.numeric(current_rf2)
                                                              & (current_trip$.__enclos_env__$private$statut_rf2 %in% c(1, 2, 3)))),
                                                label = paste0("issue with the full trip ", full_trip_id,
                                                               " and the partial trip ", partial_trip_id))
                        })
    if (length(current_trip$.__enclos_env__$private$wells) != 0) {
      capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                     file = "NUL")
      capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                     file = "NUL")
      for (well_id in seq_len(length.out = current_wells$count())) {
        current_well <- current_wells$extract(id = well_id)[[1]]
        # 216 - Checking if process 2.3 ran correctly ----
        testthat::test_that(desc = "216 - Checking if process 2.3 ran correctly",
                            code = {
                              testthat::expect_true(object = ((paste0(class(x = current_well$.__enclos_env__$private$elementarysample),
                                                                      collapse = "_") == "elementarysamples_list_t3_R6")
                                                              || (class(x = current_well$.__enclos_env__$private$elementarysample) == "logical"
                                                                  && is.na(x = current_well$.__enclos_env__$private$elementarysample))),
                                                    label = paste0("issue with the full trip ", full_trip_id,
                                                                   ", the partial trip ", partial_trip_id,
                                                                   " and the well ", well_id))
                            })
        current_wellsets <- current_well$.__enclos_env__$private$wellsets
        # 218 - Checking if process 2.4 ran correctly ----
        testthat::test_that(desc = "218 - Checking if process 2.4 ran correctly",
                            code = {
                              testthat::expect_true(object = (all(class(x = current_wellsets) == c("wellsets",
                                                                                                   "list_t3",
                                                                                                   "R6"))
                                                              || (class(x = current_wellsets) == "logical"
                                                                  && is.na(x = current_wellsets))),
                                                    label = paste0("issue with the full trip ", full_trip_id,
                                                                   ", the partial trip ", partial_trip_id,
                                                                   " and the well ", well_id))
                            })
        if (all(class(current_wellsets) == c("wellsets",
                                             "list_t3",
                                             "R6"))) {
          # 219 - Checking if sum "prop_weighted_weight" is equal to 1 ----
          testthat::test_that(desc = "219 - Checking if sum \"prop_weighted_weight\" is equal to 1",
                              code = {
                                testthat::expect_equal(object = sum(unlist(current_wellsets$extract_l1_element_value(element = "prop_weighted_weight"))),
                                                       expected = 1,
                                                       label = paste0("issue with the full trip ", full_trip_id,
                                                                      ", partial trip ", partial_trip_id,
                                                                      "and well ", well_id))
                              })

          # 222 - Checking if process 2.7 ran correctly ----
          testthat::test_that(desc = "222 - Checking if process 2.7 ran correctly",
                              code = {
                                testthat::expect_true(object = all(unlist(current_wellsets$extract_l1_element_value(element = "rf_validation")) %in% c(1, 2, 3, 4, 5, NA)),
                                                      label = paste0("issue with the full trip ", full_trip_id,
                                                                     ", the partial trip ", partial_trip_id,
                                                                     " and the well ", well_id))
                              })
          if (length(x = current_wellsets$filter_l1(filter = "$path$rf_validation %in% c(1, 2, 3, 4, 5)")) != 0) {
            capture.output(current_wellsets_rf_validation <- t3::object_r6(class_name = "wellsets"),
                           file = "NUL")
            capture.output(current_wellsets_rf_validation$add(new_item = current_wellsets$filter_l1(filter = "$path$rf_validation %in% c(1, 2, 3, 4, 5)")),
                           file = "NUL")
            # 223 - Checking if variables "weighted_samples_minus10", "weighted_samples_plus10" and "weighted_samples_total" are numeric ----
            testthat::test_that(desc = "223 - Checking if variables \"weighted_samples_minus10\", \"weighted_samples_plus10\" and \"weighted_samples_total\" are numeric",
                                code = {
                                  testthat::expect_true(object = (all(is.numeric(unlist(current_wellsets_rf_validation$extract_l1_element_value(element = "weighted_samples_minus10"))))
                                                                  && all(is.numeric(unlist(current_wellsets_rf_validation$extract_l1_element_value(element = "weighted_samples_plus10"))))
                                                                  && all(is.numeric(unlist(current_wellsets_rf_validation$extract_l1_element_value(element = "weighted_samples_total"))))),
                                                        label = paste0("issue with the full trip ", full_trip_id,
                                                                       ", the partial trip ", partial_trip_id,
                                                                       " and the well ", well_id))
                                })
          }
        }
        current_standardisedsample <- current_well$.__enclos_env__$private$standardisedsample
        # 220 - Checking if process 2.5 ran correctly ----
        testthat::test_that(desc = "220 - Checking if process 2.5 ran correctly",
                            code = {
                              testthat::expect_true(object = (all(class(current_standardisedsample) == c("standardisedsamples",
                                                                                                         "list_t3",
                                                                                                         "R6"))
                                                              || (class(current_standardisedsample) == "logical"
                                                                  && is.na(current_wellsets))),
                                                    label = paste0("issue with the full trip ", full_trip_id,
                                                                   ", the partial trip ", partial_trip_id,
                                                                   " and the well ", well_id))
                            })
        current_standardisedsampleset <- current_well$.__enclos_env__$private$standardisedsampleset
        # 221 - Checking if process 2.6 ran correctly ----
        testthat::test_that(desc = "221 - Checking if process 2.6 ran correctly",
                            code = {
                              testthat::expect_true(object = (all(class(current_standardisedsampleset) == c("standardisedsamplesets",
                                                                                                            "list_t3",
                                                                                                            "R6"))
                                                              || (class(current_standardisedsampleset) == "logical"
                                                                  && is.na(current_standardisedsampleset))),
                                                    label = paste0("issue with the full trip ", full_trip_id,
                                                                   ", the partial trip ", partial_trip_id,
                                                                   " and the well ", well_id))
                            })
        if (all(class(current_standardisedsampleset) == c("standardisedsamplesets",
                                                          "list_t3",
                                                          "R6"))) {
          # 224 - Checking if process 2.8 ran correctly ----
          testthat::test_that(desc = "224 - Checking if process 2.8 ran correctly",
                              code = {
                                testthat::expect_true(object = (all(is.na(x = unlist(current_standardisedsampleset$extract_l1_element_value(element = "sample_number_weighted_set")))
                                                                    | is.numeric(x = unlist(current_standardisedsampleset$extract_l1_element_value(element = "sample_number_weighted_set"))))
                                                                & all(is.na(x = unlist(current_standardisedsampleset$extract_l1_element_value(element = "sample_weigth_set")))
                                                                      | is.numeric(x = unlist(current_standardisedsampleset$extract_l1_element_value(element = "sample_weigth_set"))))),
                                                      label = paste0("issue with the full trip ", full_trip_id,
                                                                     ", the partial trip ", partial_trip_id,
                                                                     " and the well ", well_id))
                              })
        }
        if (length(current_well$.__enclos_env__$private$elementarysampleraw) != 0) {
          capture.output(current_elementarysamplesraw <- t3::object_r6(class_name = "elementarysamplesraw"),
                         file = "NUL")
          capture.output(current_elementarysamplesraw$add(new_item = unlist(current_well$.__enclos_env__$private$elementarysampleraw)),
                         file = "NUL")
          if (paste0(class(x = current_well$.__enclos_env__$private$elementarysample),
                     collapse = "_") == "elementarysamples_list_t3_R6") {
            capture.output(current_elementarysamples <- t3::object_r6(class_name = "elementarysamples"),
                           file = "NUL")
            capture.output(current_elementarysamples$add(new_item = current_well$.__enclos_env__$private$elementarysample$.__enclos_env__$private$data),
                           file = "NUL")
            current_elementarysamplesraw_species_list <- unique(unlist(current_elementarysamplesraw$extract_l1_element_value(element = "species_fao_code")))
            for (specie in current_elementarysamplesraw_species_list) {
              capture.output(current_elementarysamplesraw_species <- t3::object_r6(class_name = "elementarysamplesraw"),
                             file = "NUL")
              capture.output(current_elementarysamplesraw_species$add(new_item = current_elementarysamplesraw$filter_l1(filter = paste0("$path$species_fao_code == \"",
                                                                                                                                         specie, "\""))),
                             file = "NUL")
              capture.output(current_elementarysamples_species <- t3::object_r6(class_name = "elementarysamples"),
                             file = "NUL")
              capture.output(current_elementarysamples_species$add(new_item = current_elementarysamples$filter_l1(filter = paste0("$path$species_fao_code == \"",
                                                                                                                                  specie, "\""))),
                             file = "NUL")
              # 217 - Checking if sum "sample_number_measured_extrapolated_lf" in object "elementarysamplesraw" is equal to "sample_number_measured" in object "elementarysamples" ----
              testthat::test_that(desc = "217 - Checking if sum \"sample_number_measured_extrapolated_lf\" in object \"elementarysamplesraw\" is equal to \"sample_number_measured\" in object \"elementarysamples\"",
                                  code = {
                                    testthat::expect_equal(object = sum(unlist(current_elementarysamplesraw_species$extract_l1_element_value(element = "sample_number_measured_extrapolated_lf"))),
                                                           expected = sum(unlist(current_elementarysamples_species$extract_l1_element_value(element = "sample_number_measured_extrapolated_lf"))),
                                                           label = paste0("issue with the full trip ", full_trip_id,
                                                                          ", the partial trip ", partial_trip_id,
                                                                          ", the well ", well_id,
                                                                          " and the specie ", specie))
                                  })

            }
          }
          current_sample_length_class_lf <- current_elementarysamplesraw$extract_l1_element_value(element = "sample_length_class_lf")
          current_sample_number_measured_lf <- current_elementarysamplesraw$extract_l1_element_value(element = "sample_number_measured_lf")
          # 212 - Checking if process 2.1 run on all data ----
          testthat::test_that(desc = "212 - Checking if process 2.1 run on all data",
                              code = {
                                testthat::expect_true(object = (all(unlist(lapply(X = current_sample_length_class_lf,
                                                                                  FUN = function(x) {
                                                                                    (! is.null(x)
                                                                                     && ((is.na(x)
                                                                                          || is.numeric(x))))
                                                                                  }))))
                                                      & (all(unlist(lapply(X = current_sample_number_measured_lf,
                                                                           FUN = function(x) {
                                                                             (! is.null(x)
                                                                              && ((is.na(x)
                                                                                   || is.numeric(x))))
                                                                           })))),
                                                      label = paste0("issue with the full trip ", full_trip_id,
                                                                     ", partial trip ", partial_trip_id,
                                                                     " and the well ", well_id))
                              })
          current_sample_rf4 <- current_elementarysamplesraw$extract_l1_element_value(element = "rf4")
          current_sample_sample_number_measured_extrapolated_lf <- current_elementarysamplesraw$extract_l1_element_value(element = "sample_number_measured_extrapolated_lf")
          # 214 - Checking if variables "rf4" and "sample_number_measured_extrapolated_lf" are filled and in the correct format according to the process 2.2 ----
          testthat::test_that(desc = "214 - Checking if variables \"rf4\" and \"sample_number_measured_extrapolated_lf\" are filled and in the correct format according to the process 2.2",
                              code = {
                                testthat::expect_true(object = (all(unlist(lapply(X = current_sample_rf4,
                                                                                  FUN = function(x) {
                                                                                    (! is.null(x)
                                                                                     && ((is.na(x)
                                                                                          || is.numeric(x))))
                                                                                  }))))
                                                      & (all(unlist(lapply(X = current_sample_sample_number_measured_extrapolated_lf,
                                                                           FUN = function(x) {
                                                                             (! is.null(x)
                                                                              && ((is.na(x)
                                                                                   || is.numeric(x))))
                                                                           })))),
                                                      label = paste0("issue with the full trip ", full_trip_id,
                                                                     ", partial trip ", partial_trip_id,
                                                                     " and the well ", well_id))
                              })
          current_elementarysampleraw_id <- unique(unlist(current_elementarysamplesraw$extract_l1_element_value(element = "elementarysampleraw_id")))
          for (elementarysampleraw_id in current_elementarysampleraw_id) {
            capture.output(current_elementarysamplesraw_id <- t3::object_r6(class_name = "elementarysamplesraw"),
                           file = "NUL")
            capture.output(current_elementarysamplesraw_id$add(new_item = unlist(current_elementarysamplesraw$filter_l1(filter = paste0("$path$elementarysampleraw_id == \"",
                                                                                                                                        elementarysampleraw_id,
                                                                                                                                        "\"")))),
                           file = "NUL")
            sum_sample_number_measured_lf <- sum(unlist(current_elementarysamplesraw_id$extract_l1_element_value(element = "sample_number_measured_lf")))
            sample_number_measured <- unique(unlist(current_elementarysamplesraw_id$extract_l1_element_value(element = "sample_number_measured")))
            if (! is.na(sum_sample_number_measured_lf)) {
              # 213 - Checking if sum "sample_number_measured_lf" is equal to "sample_number_measured" ----
              testthat::test_that(desc = "213 - Checking if sum \"sample_number_measured_lf\" is equal to \"sample_number_measured\"",
                                  code = {
                                    testthat::expect_equal(object = sum_sample_number_measured_lf,
                                                           expected = sample_number_measured,
                                                           label = paste0("issue with the full trip ", full_trip_id,
                                                                          ", partial trip ", partial_trip_id,
                                                                          ", well ", well_id,
                                                                          " and the elementary sample raw ", elementarysampleraw_id))
                                  })
            }
            for (sub_elementarysampleraw_id in seq_len(length.out = current_elementarysamplesraw_id$count())) {
              current_elementarysampleraw <- current_elementarysamplesraw_id$extract(id = sub_elementarysampleraw_id)[[1]]
              current_sample_number_measured_extrapolated_lf <- current_elementarysampleraw$.__enclos_env__$private$sample_number_measured_extrapolated_lf
              calculate_sample_number_measured_extrapolated_lf <- current_elementarysampleraw$.__enclos_env__$private$rf4 * current_elementarysampleraw$.__enclos_env__$private$sample_number_measured_lf
              if (! is.na(current_sample_number_measured_extrapolated_lf)
                  && ! is.na(calculate_sample_number_measured_extrapolated_lf)) {
                # 215 - Checking if "sample_number_measured_extrapolated_lf" equal "sample_number_measured_lf" multiply by "rf4" ----
                testthat::test_that(desc = "215 - Checking if \"sample_number_measured_extrapolated_lf\" equal \"sample_number_measured_lf\" multiply by \"rf4\"",
                                    code = {
                                      testthat::expect_equal(object = current_sample_number_measured_extrapolated_lf,
                                                             expected = calculate_sample_number_measured_extrapolated_lf,
                                                             label = paste0("issue with the full trip ", full_trip_id,
                                                                            ", partial trip ", partial_trip_id,
                                                                            ", well ", well_id,
                                                                            ", elementary sample raw ", elementarysampleraw_id,
                                                                            " and sub elementary sample raw ", sub_elementarysampleraw_id))
                                    })
              }
            }
          }
        }
      }
    }
    capture.output(current_activities <- t3::object_r6(class_name = "activities"),
                   file = "NUL")
    capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                   file = "NUL")
    current_activities_set_duration <- current_activities$extract_l1_element_value(element = "set_duration")
    # 208 - Checking if variable "set_duration" is filled and in the correct format according the process 1.5 ----
    testthat::test_that(desc = "208 - Checking if variable \"set_duration\" is filled and in the correct format according the process 1.5",
                        code = {
                          testthat::expect_true(object = (all(apply(X =  as.matrix(unlist(current_activities_set_duration)),
                                                                    MARGIN=1,
                                                                    FUN = function(x) (! is.null(x)
                                                                                       && ((is.na(x)
                                                                                            || (is.numeric(x)
                                                                                                & x >= 0))))))),
                                                label = paste0("issue with the full trip ", full_trip_id,
                                                               " and the partial trip ", partial_trip_id))
                        })
    current_activities_set_count <- unlist(current_activities$extract_l1_element_value(element = "positive_set_count"))
    # 207 - Checking if variable positive_set_count is filled and in the correct format according to the process 1.4 ----
    testthat::test_that(desc = "207 - Checking if variable positive_set_count is filled and in the correct format according to the process 1.4",
                        code = {
                          testthat::expect_true(object = (all(unlist(lapply(X = current_activities_set_count,
                                                                            FUN = function(x) (! is.null(x)
                                                                                               && ((is.na(x)
                                                                                                    || (is.numeric(x)
                                                                                                        & x >= 0)))))))),
                                                label = paste0("issue with the full trip ", full_trip_id,
                                                               " and the partial trip ", partial_trip_id))
                        })
    capture.output(current_elementarylandings <- t3::object_r6(class_name = "elementarylandings"),
                   file = "NUL")
    capture.output(current_elementarylandings$add(new_item = current_trip$.__enclos_env__$private$elementarylandings),
                   file = "NUL")
    if (current_activities$count() != 0) {
      for (activity_id in seq_len(length.out = current_activities$count())) {
        current_activity <- current_activities$extract(id = activity_id)[[1]]
        if (! is.null(current_activity$.__enclos_env__$private$elementarycatches)
            & length(current_activity$.__enclos_env__$private$elementarycatches) != 0) {
          capture.output(current_elementarycatches <- t3::object_r6(class_name = "elementarycatches"),
                         file = "NUL")
          capture.output(current_elementarycatches$add(new_item = current_activity$.__enclos_env__$private$elementarycatches),
                         file = "NUL")
          elementarycatches_ids <- unique(unlist(current_elementarycatches$extract_l1_element_value(element = "elementarycatch_id")))
          current_sum_catch_weight_category_corrected <- current_sum_catch_weight_category_corrected + sum(unlist(current_elementarycatches$extract_l1_element_value(element = "catch_weight_category_code_corrected")))
          for (elementarycatches_id in seq_len(length.out = length(elementarycatches_ids))) {
            elementarycatches_id <- elementarycatches_ids[elementarycatches_id]
            capture.output(current_elementarycatches_by_id <- t3::object_r6(class_name = "elementarycatches"),
                           file = "NUL")
            capture.output(current_elementarycatches_by_id$add(new_item = current_elementarycatches$filter_l1(filter = paste0("$path$elementarycatch_id == \"",
                                                                                                                              elementarycatches_id,
                                                                                                                              "\""))),
                           file = "NUL")
            if (unique(unlist(current_elementarycatches_by_id$extract_l1_element_value(element = "species_fao_code"))) %in% species_fao_codes_rf1_fr) {
              current_sum_elementarycatches_rf1 <- current_sum_elementarycatches_rf1 + unique(unlist(current_elementarycatches_by_id$extract_l1_element_value(element = "catch_weight_rf1")))
              current_sum_elementarycatches_rf2 <- current_sum_elementarycatches_rf2 + unique(unlist(current_elementarycatches_by_id$extract_l1_element_value(element = "catch_weight_rf2")))
            }
            current_sum_elementarycatches_rf2_all <- current_sum_elementarycatches_rf2_all + unique(unlist(current_elementarycatches_by_id$extract_l1_element_value(element = "catch_weight_rf2")))
            current_weight_category_code_corrected <- current_elementarycatches_by_id$extract_l1_element_value(element = "weight_category_code_corrected")
            # 205 - Checking if variable "weight_category_code_corrected" is equal to "<10kg", ">30kg", ">10kg" or "unknown" ----
            testthat::test_that(desc = "205 - Checking if variable \"weight_category_code_corrected\" is equal to \"<10kg\", \">30kg\", \">10kg\" or \"unknown\"",
                                code = {
                                  testthat::expect_true(object = (all(unlist(lapply(X = current_weight_category_code_corrected,
                                                                                    FUN = function(x) (! is.null(x)
                                                                                                       && ((is.na(x)
                                                                                                            || (x %in% c("<10kg",
                                                                                                                         "10-30kg",
                                                                                                                         ">30kg",
                                                                                                                         ">10kg",
                                                                                                                         "unknown"))))))))),
                                                        label = paste0("issue with the full trip ", full_trip_id,
                                                                       ", the partial trip ", partial_trip_id,
                                                                       ", the activity ", activity_id,
                                                                       " and the elementarycatch ", elementarycatches_id))
                                })
          }
        }
      }
    }
    if (current_elementarylandings$count() != 0) {
      capture.output(current_elementarylandings_rf1 <- t3::object_r6(class_name = "elementarylandings"),
                     file = "NUL")
      capture.output(current_elementarylandings_rf1$add(new_item = current_elementarylandings$filter_l1(filter = paste0("$path$species_fao_code %in% c(",
                                                                                                                        paste0("\"", species_fao_codes_rf1_fr, collapse = "\", \""),
                                                                                                                        "\")"))),
                     file = "NUL")
      current_sum_elementarylandings <- current_sum_elementarylandings + sum(unlist(current_elementarylandings_rf1$extract_l1_element_value(element = "landing_weight")))
    }
  }
  if (! is.na(current_sum_elementarycatches_rf1)) {
    # 201 - Checking if sum elementary catches corrected by rf1 is equal to sum elementary landings ----
    testthat::test_that(desc = "201 - Checking if sum elementary catches corrected equal to sum elementary landings",
                        code = {
                          testthat::expect_equal(object = current_sum_elementarylandings,
                                                 expected = current_sum_elementarycatches_rf1,
                                                 label = paste0("issue with full trip ", full_trip_id))
                        })
  }
  if (! is.na(current_sum_elementarycatches_rf2)) {
    # 204 - Checking if sum elementary catches corrected by rf2 is equal to sum elementary landings ----
    testthat::test_that(desc = "201 - Checking if sum elementary catches corrected equal to sum elementary landings",
                        code = {
                          testthat::expect_equal(object = current_sum_elementarylandings,
                                                 expected = current_sum_elementarycatches_rf2,
                                                 label = paste0("issue with the full trip ", full_trip_id))
                        })
  }
  if (!(is.na(current_sum_catch_weight_category_corrected) & is.na(current_sum_elementarycatches_rf2_all))) {
    # 206 - Checking if sum catch weight category corrected (process 1.3) is equal to all sum elementary catches corrected by rf2 ----
    testthat::test_that(desc = "206 - Checking if sum catch weight category corrected (process 1.3) is equal to all sum elementary catches corrected by rf2",
                        code = {
                          testthat::expect_equal(object = current_sum_catch_weight_category_corrected,
                                                 expected = current_sum_elementarycatches_rf2_all,
                                                 label = paste0("issue with the full trip ", full_trip_id))
                        })
  }
}

