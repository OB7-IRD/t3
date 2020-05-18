# object_model_data output format ----
test_that("object_model_data output format",
          code = {
            expect_equal(object = class(t3:::object_model_data$new()),
                         expected = c("object_model_data", "R6"))
          })

# object trip output format ----
test_that(desc = "object trip output format",
          code = {
            capture.output(expect_equal(object = class(t3::object_r6(class_name = "trips")),
                                        expected = c("trips", "list_t3", "R6")),
                           file = "NUL")
          })



# compare number of trips between data trips extracted and data model (classes trip and trips) ----
object_model_data <- t3:::object_model_data$new()
capture.output(object_model_data$trips_object_creation(data_source = "csv_file",
                                                       data_path = system.file("test_data",
                                                                               "trips.csv",
                                                                               package = "t3")),
               file = "NUL")
test_that(desc = "number_trips_data_extracted_vs_data_model",
          code = {
            expect_equal(object = length(unique(read.csv2(file = system.file("test_data",
                                                                             "trips.csv",
                                                                             package = "t3"),
                                                          stringsAsFactors = FALSE)$trip_id)),
                         expected = length(object_model_data$.__enclos_env__$private$trips$.__enclos_env__$private$data))
          })

# compare number of activities between data activities extracted and data model (classes activity and activities) ----
capture.output(object_model_data$activities_object_creation(data_source = "csv_file",
                                                            data_path = system.file("test_data",
                                                                                    "activities.csv",
                                                                                    package = "t3")),
               file = "NUL")
test_that(desc = "number_activities_data_extracted_vs_data_model",
          code = {
            expect_equal(object = length(unique(read.csv2(file = system.file("test_data",
                                                                             "activities.csv",
                                                                             package = "t3"),
                                                          stringsAsFactors = FALSE)$activity_id)),
                         expected = length(object_model_data$.__enclos_env__$private$activities$.__enclos_env__$private$data))
          })

# compare number of elementary catches between data elementary catches extracted and data model (classes elementarycatch and elementarycatches) ----
capture.output(object_model_data$elementarycatches_object_creation(data_source = "csv_file",
                                                                   data_path = system.file("test_data",
                                                                                           "elementary_catches.csv",
                                                                                           package = "t3")),
               file = "NUL")

# compare number of elementary landings between data elementary landings extracted and data model (classes elementarylanding and elementarylandings) ----
capture.output(object_model_data$elementarylandings_object_creation(data_source = "csv_file",
                                                                    data_path = system.file("test_data",
                                                                                            "elementary_landings.csv",
                                                                                            package = "t3")),
               file = "NUL")

# compare number of samples and well plans between data samples and well plans extracted and data model (classes well, wells, elementarysampleraw and elementarywellplan) ----
capture.output(object_model_data$wells_object_creation(data_source = "csv_file",
                                                       data_path_samples = system.file("test_data",
                                                                                       "samples.csv",
                                                                                       package = "t3"),
                                                       data_path_wellplans = system.file("test_data",
                                                                                         "well_plans.csv",
                                                                                         package = "t3")),
               file = "NUL")

# compare number of set duration data between data set duration extracted and data model ----
capture.output(object_model_data$setduration_data(data_source = "csv_file",
                                                  data_path = system.file("test_data",
                                                                          "set_duration_ref.csv",
                                                                          package = "t3")),
               file = "NUL")
