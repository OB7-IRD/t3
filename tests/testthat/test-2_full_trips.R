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
  current_trips <- object_full_trips$.__enclos_env__$private$data_selected[[a]]
  current_sum_elementarycatches_rf1 <- 0
  current_sum_elementarycatches_rf2 <- 0
  current_sum_elementarylandings <- 0
  for (b in seq_len(length.out = length(x = current_trips))) {
    current_trip <- current_trips[[b]]
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
    current_activities <- current_trip$.__enclos_env__$private$activities
    current_elementarylandings <- current_trip$.__enclos_env__$private$elementarylandings
    if (length(x = current_activities) != 0) {
      for (c in seq_len(length.out = length(x = current_activities))) {
        current_activity <- current_activities[[c]]
        if (! is.null(current_activity$.__enclos_env__$private$elementarycatches)) {
          current_elementarycatches <- current_activity$.__enclos_env__$private$elementarycatches
          for (d in seq_len(length.out = length(current_elementarycatches))) {
            if (current_elementarycatches[[d]]$.__enclos_env__$private$specie_code %in% species_rf1
                && ! is.null(current_elementarycatches[[d]]$.__enclos_env__$private$catch_weight_rf1)) {
              current_sum_elementarycatches_rf1 <- current_sum_elementarycatches_rf1 + current_elementarycatches[[d]]$.__enclos_env__$private$catch_weight_rf1
              current_sum_elementarycatches_rf2 <- current_sum_elementarycatches_rf2 + current_elementarycatches[[d]]$.__enclos_env__$private$catch_weight_rf2
            }
          }
        }
      }
    }
    if (length(x = current_elementarylandings) != 0) {
      for (e in seq_len(length.out = length(current_elementarylandings))) {
        if (current_elementarylandings[[e]]$.__enclos_env__$private$specie_code %in% species_rf1) {
          current_sum_elementarylandings <- current_sum_elementarylandings + current_elementarylandings[[e]]$.__enclos_env__$private$landing_weight
        }
      }
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
}










