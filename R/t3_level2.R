#' @name t3_level2
#' @title T3 process level 2
#' @description Level 2 of t3 process (samples standardisation).
#' @param object_model_data Object of classes \code{\link[t3]{object_model_data}} and \code{\link[R6]{R6}} expected.
#' @param object_full_trips Object of classes \code{\link[t3]{full_trips}}, \code{\link[t3]{list_t3}} and \code{\link[R6]{R6}} expected.
#' @param maximum_lf_class Object of type \code{\link[base]{integer}} expected. Theorical maximum lf class that can occur (all species considerated). By default 500.
#' @param threshold_rf_minus10 Object of type \code{\link[base]{integer}} expected. Threshold limit value for raising factor on individuals category minus 10. By default 500.
#' @param threshold_rf_plus10 Object of type \code{\link[base]{integer}} expected. Threshold limit value for raising factor on individuals category plus 10. By default 500.
#' @param threshold_frequency_rf_minus10 Object of type \code{\link[base]{integer}} expected. Threshold limit frequency value for raising factor on individuals category minus 10. By default 75.
#' @param threshold_frequency_rf_plus10 Object of type \code{\link[base]{integer}} expected. Threshold limit frequency value for raising factor on individuals category plus 10. By default 75.
#' @param threshold_rf_total Object of type \code{\link[base]{integer}} expected. Threshold limit value for raising factor (all categories). By default 250.
#' @param log_file Object of type \code{\link[base]{logical}} expected. Initiation or not for log file creation. By default FALSE (no).
#' @param log_path Object of type \code{\link[base]{character}} expected. Path of the log file directory. By default NULL.
#' @param log_name Object of type \code{\link[base]{character}} expected. Name of the log file. By default "t3_level2".
#' @param output_path Object of class \code{\link[base]{character}} expected. Outputs path directory. By default NULL.
#' @param new_directory Object of class \code{\link[base]{logical}} expected. Initiate a new outputs directory of use an existing one. By default NULL.
#' @param integrated_process Object of class \code{\link[base]{logical}} expected. Indicate if the process is integrated in another (like the one in the function "t3_process"). By default FALSE.
#' @param referential_template Object of class \code{\link[base]{character}} expected. By default "observe". Referential template selected (for example regarding the activity_code). You can switch to "avdth".
#' @return The function a R6 reference object of class "object_full_trips".
#' @export
t3_level2 <- function(object_model_data,
                      object_full_trips,
                      maximum_lf_class = as.integer(500),
                      threshold_rf_minus10 = as.integer(500),
                      threshold_rf_plus10 = as.integer(500),
                      threshold_frequency_rf_minus10 = as.integer(75),
                      threshold_frequency_rf_plus10 = as.integer(75),
                      threshold_rf_total = as.integer(250),
                      log_file = FALSE,
                      log_path = NULL,
                      log_name = "t3_level2",
                      output_path = NULL,
                      new_directory = FALSE,
                      integrated_process = FALSE,
                      referential_template = "observe") {
  if (paste0(class(object_model_data),
             collapse = " ") != "object_model_data R6") {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Invalid \"object_model_data\" argument. Classes \"object_model_data\" and \"R6\" expected.")
  } else if (paste0(class(object_full_trips),
                    collapse = " ") != "full_trips list_t3 R6") {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Invalid \"object_full_trips\" argument. Classes \"full_trips\", \"list_t3\" and \"R6\" expected.")
  } else {
    # log file initialisation ----
    initiate_log_file(log_file = log_file,
                      log_path = log_path,
                      log_name = log_name)
    # directories initialization if outputs extraction ----
    if (integrated_process != TRUE
        && ! is.null(x = output_path)) {
      output_path <- initiate_directory(output_path = output_path,
                                        new_directory = new_directory,
                                        level = "level2")
    }
    message(format(x = Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            " - Start function t3 process level 2.")
    # level 2.1: sample length class ld1 to lf conversion ----
    object_full_trips$sample_length_class_ld1_to_lf(length_step = object_model_data$.__enclos_env__$private$lengthsteps,
                                                    global_output_path = output_path,
                                                    referential_template = referential_template)
    # level 2.2: sample number measured extrapolation ----
    object_full_trips$sample_number_measured_extrapolation(global_output_path = output_path)
    # level 2.3: sample step length class standardisation ----
    object_full_trips$sample_length_class_step_standardisation(maximum_lf_class = maximum_lf_class,
                                                               global_output_path = output_path)
    # level 2.4: sample weight categories ----
    object_full_trips$well_set_weight_categories(sample_set = object_model_data$.__enclos_env__$private$samplesets,
                                                 global_output_path = output_path,
                                                 referential_template = referential_template)
    # level 2.5: standardised sample creation ----
    object_full_trips$standardised_sample_creation(global_output_path = output_path)
    # level 2.6: sample number standardisation ----
    object_full_trips$standardised_sample_set_creation(length_weight_relationship_data = object_model_data$.__enclos_env__$private$lengthweightrelationships,
                                                       global_output_path = output_path)
    # level 2.7: raised factors determination ----
    object_full_trips$raised_factors_determination(threshold_rf_minus10 = threshold_rf_minus10,
                                                   threshold_rf_plus10 = threshold_rf_minus10,
                                                   threshold_frequency_rf_minus10 = threshold_rf_minus10,
                                                   threshold_frequency_rf_plus10 = threshold_rf_minus10,
                                                   threshold_rf_total = threshold_rf_minus10,
                                                   global_output_path = output_path)
    # level 2.8: samples number standardisation at set scale ----
    object_full_trips$raised_standardised_sample_set(global_output_path = output_path)
    # close, if necessary log file connection ----
    if (log_file == TRUE) {
      closeAllConnections()
    }
    message(format(x = Sys.time(),
                   format = "%Y-%m-%d %H:%M:%S"),
            " - Successful function t3 process level 2.")
    return(object_full_trips)
  }
}
