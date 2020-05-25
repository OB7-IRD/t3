# 2 - Checking full_trips ----
source(system.file("test_data",
                   "test_full_trips.R",
                   package = "t3"),
       local = TRUE)
# 2.1 - Comparison number of trips between data model and full trips ----
test_that(desc = "comparison_number_trips_data_model_vs_full_trips",
          code = {
            expect_equal(object = length(x = object_model_data$.__enclos_env__$private$trips$.__enclos_env__$private$data),
                         expected = length(unlist(x = object_full_trips$.__enclos_env__$private$data)))
          })
