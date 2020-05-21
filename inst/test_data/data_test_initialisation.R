# object_model_data initialisation ----
load(system.file("test_data",
                 "data_test.RData",
                 package = "t3"))
object_model_data <- t3:::object_model_data$new()
capture.output(object_model_data$activities_object_creation(data_source = "envir"),
               file = "NUL")
capture.output(object_model_data$elementarycatches_object_creation(data_source = "envir"),
               file = "NUL")
capture.output(object_model_data$elementarylandings_object_creation(data_source = "envir"),
               file = "NUL")
capture.output(object_model_data$lengthstep_data(data_source = "envir"),
               file = "NUL")
capture.output(object_model_data$lengthweightrelationship_data(data_source = "envir"),
               file = "NUL")
capture.output(object_model_data$wells_object_creation(data_source = "envir"),
               file = "NUL")
capture.output(object_model_data$sampleset_data(data_source = "envir"),
               file = "NUL")
capture.output(object_model_data$setduration_data(data_source = "envir"),
               file = "NUL")
capture.output(object_model_data$trips_object_creation(data_source = "envir"),
               file = "NUL")
