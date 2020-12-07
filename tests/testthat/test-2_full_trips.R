# 2 - Checking full_trips ----
load(file = system.file("test_data",
                        "data_test.RData",
                        package = "t3"))
# model creation: initialisation object for full trips class ----
object_full_trips <- t3:::full_trips$new()
# model creation: object full_trip creation ----
object_full_trips$create_full_trips(object_trips = object_model_data$.__enclos_env__$private$trips)
# model creation: filter on reference year ----
object_full_trips$filter_by_periode(periode_reference = as.integer(c(1884, 1885)))
# model creation: add activities to trips selected ----
object_full_trips$add_activities(object_activities = object_model_data$.__enclos_env__$private$activities)
# model creation: add elementarycatches to trips selected ----
object_full_trips$add_elementarycatches(object_elementarycatches = object_model_data$.__enclos_env__$private$elementarycatches)
# model creation: add elementarylandings to trips selected ----
object_full_trips$add_elementarylandings(object_elementarylandings = object_model_data$.__enclos_env__$private$elementarylandings)
# model creation: add well(s) and sample(s) to trip(s) selected ----
object_full_trips$add_wells_samples(object_wells = object_model_data$.__enclos_env__$private$wells)
