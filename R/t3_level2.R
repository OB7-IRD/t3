#' @name t3_level2
#' @title T3 process level 2
#' @description Level 2 of t3 process (samples standardisation).
#' @param length_step (data.frame) Data frame object with length ratio between ld1 and lf class.
#' @param sample_set (data.frame) Data frame object with weighted weigth of each set sampled.
#' @param length_weight_relationship_data (data.frame) Data frame object with parameters for length weight relationship.
#' @param log_file (logical) Initiation or not for log file creation. By default FALSE (no).
#' @param log_path (character) Path of the log file directory. By default NULL.
#' @param log_name (character) Name of the log file. By default "t3log".
#' @return The function a R6 reference object of class "object_full_trips".
#' @export
t3_level2 <- function(length_step,
                      sample_set,
                      length_weight_relationship_data,
                      log_file = FALSE,
                      log_path = NULL,
                      log_name = "t3_level2") {
  if ("object_full_trips" %in% ls(envir = .GlobalEnv)) {
    object_full_trips <- get(x = "object_full_trips", envir = .GlobalEnv)
    if (paste0(class(object_full_trips), collapse = " ") != "full_trips list_t3 R6") {
      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: invalid \"object_full_trips\" argument.",
          "Classes \"full_trips\", \"list_t3\" and \"R6\" expected.\n",
          sep = "")
      stop()
    } else {
      # log file initialisation ----
      t3::initiate_log_file(log_file = log_file,
                            log_path = log_path,
                            log_name = log_name)
      # level 2.1: sample number measured extrapolation ----
      object_full_trips$sample_number_measured_extrapolation()
      # level 2.2: sample length class ld1 to lf conversion ----
      object_full_trips$sample_length_class_ld1_to_lf(length_step = length_step)
      # level 2.3: sample step length class standardisation ----
      object_full_trips$sample_length_class_step_standardisation()
      # level 2.4: sample weight categories ----
      object_full_trips$well_set_weigth_categories(sample_set = sample_set)
      # level 2.5: standardised sample creation ----
      object_full_trips$standardised_sample_creation()
      # level 2.6: sample number standardisation ----
      object_full_trips$standardised_sample_set_creation(length_weight_relationship_data = length_weight_relationship_data)
      # level 2.7: raised factors determination ----
      object_full_trips$raised_factors_determination()
      # level 2.8: samples number standardisation at set scale ----
      object_full_trips$raised_standardised_sample_set()
      # close, if necessary log file connection ----
      if (log_file == TRUE) {
        closeAllConnections()
      }
      assign(x = "object_full_trips",
             value = object_full_trips,
             envir = .GlobalEnv)
    }
  } else {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: object \"object_full_trips\" don't exist in the global environment.\n",
        sep = "")
    stop()
  }
}
