source(system.file("test_data",
                   "test_object_model_data.R",
                   package = "t3"),
       local = TRUE)
object_full_trips <- t3:::full_trips$new()
capture.output(object_full_trips$create_full_trips(object_trips = object_model_data$.__enclos_env__$private$trips),
               file = "NUL")
