# 2 - Checking full_trips ----
load(file = system.file("test_data",
                        "data_test.RData",
                        package = "t3"))
# model creation ----
# initialisation object for full trips class
object_full_trips <- t3:::full_trips$new()
# object full_trip creation
capture.output(object_full_trips$create_full_trips(object_trips = object_model_data$.__enclos_env__$private$trips),
               file = "NUL")
# filter on reference year
capture.output(object_full_trips$filter_by_periode(periode_reference = as.integer(c(1884,
                                                                                    1885))),
               file = "NUL")
# add activities to trips selected
capture.output(object_full_trips$add_activities(object_activities = object_model_data$.__enclos_env__$private$activities),
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
species_rf1 = as.integer(c(1, 2, 3, 4, 9, 11))
capture.output(object_full_trips$rf1(species_rf1 = species_rf1),
               file = "NUL")

# level 1.2: rf2
capture.output(object_full_trips$rf2(),
               file = "NUL")

# level 1.3: logbook weigth categories conversion ----
capture.output(object_full_trips$conversion_weigth_category(),
               file = "NUL")

for (a in seq_len(length.out = length(x = object_full_trips$.__enclos_env__$private$data_selected))) {
  capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                 file = "NUL")
  capture.output(current_trips$add(object_full_trips$.__enclos_env__$private$data_selected[[a]]),
                 file = "NUL")
  current_sum_elementarycatches_rf1 <- 0
  current_sum_elementarycatches_rf2 <- 0
  current_sum_elementarylandings <- 0
  current_sum_catch_weight_category_corrected <- 0
  for (b in seq_len(length.out = current_trips$count())) {
    current_trip <- current_trips$extract(id = b)[[1]]
    current_status_rf1 <- current_trip$.__enclos_env__$private$statut_rf1
    current_status_rf2 <- current_trip$.__enclos_env__$private$statut_rf2
    current_rf1 <- current_trip$.__enclos_env__$private$rf1
    current_rf2 <- current_trip$.__enclos_env__$private$rf2
    # 201 - Checking if rf1 process was applied on all trips ----
    testthat::test_that(desc = "202 - Checking if rf1 process was applied on all trips",
                        code = {
                          testthat::expect_true(object = (current_trip$.__enclos_env__$private$statut_rf1 %in% c(1.1, 1.2, 1.3, 1.4, 2.1, 2.2, 2.3, 2.4)))
                          testthat::expect_true(object = is.numeric(current_rf1))
                        })
    # 203 - Checking if rf2 process was applied on all trips ----
    testthat::test_that(desc = "203 - Checking if rf2 process was applied on all trips",
                        code = {
                          testthat::expect_true(object = (current_trip$.__enclos_env__$private$statut_rf2 %in% c(1, 2)))
                          testthat::expect_true(object = is.numeric(current_rf2))
                        })
    capture.output(current_activities <- t3::object_r6(class_name = "activities"),
                   file = "NUL")
    capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                   file = "NUL")
    capture.output(current_elementarylandings <- t3::object_r6(class_name = "elementarylandings"),
                   file = "NUL")
    capture.output(current_elementarylandings$add(new_item = current_trip$.__enclos_env__$private$elementarylandings),
                   file = "NUL")
    if (current_activities$count() != 0) {
      for (c in seq_len(length.out = current_activities$count())) {
        current_activity <- current_activities$extract(id = c)[[1]]
        if (! is.null(current_activity$.__enclos_env__$private$elementarycatches)
            & length(current_activity$.__enclos_env__$private$elementarycatches) != 0) {
          capture.output(current_elementarycatches <- t3::object_r6(class_name = "elementarycatches"),
                         file = "NUL")
          capture.output(current_elementarycatches$add(new_item = current_activity$.__enclos_env__$private$elementarycatches),
                         file = "NUL")
          elementarycatches_ids <- unique(unlist(current_elementarycatches$extract_l1_element_value(element = "elementarycatch_id")))
          current_sum_catch_weight_category_corrected <- current_sum_catch_weight_category_corrected + sum(unlist(current_elementarycatches$extract_l1_element_value(element = "catch_weight_category_corrected")))
          for (d in seq_len(length.out = length(elementarycatches_ids))) {
            elementarycatches_id <- elementarycatches_ids[d]
            capture.output(current_elementarycatches_by_id <- t3::object_r6(class_name = "elementarycatches"),
                           file = "NUL")
            capture.output(current_elementarycatches_by_id$add(new_item = current_elementarycatches$filter_l1(filter = paste0("$path$elementarycatch_id == \"",
                                                                                                                              elementarycatches_id,
                                                                                                                              "\""))),
                           file = "NUL")
            if (unique(unlist(current_elementarycatches_by_id$extract_l1_element_value(element = "specie_code"))) %in% species_rf1) {
              current_sum_elementarycatches_rf1 <- current_sum_elementarycatches_rf1 + unique(unlist(current_elementarycatches_by_id$extract_l1_element_value(element = "catch_weight_rf1")))
              current_sum_elementarycatches_rf2 <- current_sum_elementarycatches_rf2 + unique(unlist(current_elementarycatches_by_id$extract_l1_element_value(element = "catch_weight_rf2")))
            }
            current_corrected_logbook_category <- unique(unlist(current_elementarycatches_by_id$extract_l1_element_value(element = "corrected_logbook_category")))
            # 205 - Checking if variable "corrected_logbook_category" is equal to "<10kg", ">30kg", ">10kg" or "unknown" ----
            testthat::test_that(desc = "205 - Checking if variable \"corrected_logbook_category\" is equal to \"<10kg\", \">30kg\", \">10kg\" or \"unknown\"",
                                code = {
                                  testthat::expect_true(object = current_corrected_logbook_category %in% c("<10kg", "10-30kg", ">30kg", ">10kg", "unknown"),
                                                        label = current_corrected_logbook_category)
                                })
          }
        }
      }
    }
    if (current_elementarylandings$count() != 0) {
      capture.output(current_elementarylandings_rf1 <- t3::object_r6(class_name = "elementarylandings"),
                     file = "NUL")
      capture.output(current_elementarylandings_rf1$add(new_item = current_elementarylandings$filter_l1(filter = paste0("$path$specie_code %in% c(",
                                                                                                                                                  paste0(species_rf1, collapse = ", "),
                                                                                                                                                  ")"))),
                     file = "NUL")
      current_sum_elementarylandings <- current_sum_elementarylandings + sum(unlist(current_elementarylandings_rf1$extract_l1_element_value(element = "landing_weight")))
    }
  }
  # 202 - Checking if sum elementary catches corrected by rf1 is equal to sum elementary landings ----
  testthat::test_that(desc = "201 - Checking if sum elementary catches corrected equal to sum elementary landings",
                      code = {
                        testthat::expect_equal(object = current_sum_elementarylandings,
                                               expected = current_sum_elementarycatches_rf1)
                      })
  # 204 - Checking if sum elementary catches corrected by rf2 is equal to sum elementary landings ----
  testthat::test_that(desc = "201 - Checking if sum elementary catches corrected equal to sum elementary landings",
                      code = {
                        testthat::expect_equal(object = current_sum_elementarylandings,
                                               expected = current_sum_elementarycatches_rf2)
                      })
  # 206 - Checking if sum catch weight category corrected (process 1.3) is equal to sum elementary catches corrected by rf2 ----
  testthat::test_that(desc = "206 - Checking if sum catch weight category corrected (process 1.3) is equal to sum elementary catches corrected by rf2",
                      code = {
                        testthat::expect_equal(object = current_sum_catch_weight_category_corrected,
                                               expected = current_sum_elementarycatches_rf2)
                      })
}










