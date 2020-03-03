#' @name t3_level1
#' @title T3 process level 1
#' @description Level 1 of t3 process (logbook standardisation).
#' @param log_file (logical) Initiation or not for log file creation. By default FALSE (no).
#' @param log_path (character) Path of the log file directory. By default NULL.
#' @param log_name (character) Name of the log file. By default "t3log".
#' @return The function a R6 reference object of class "object_full_trips".
#' @export
t3_level1 <- function(log_file = FALSE,
                      log_path = NULL,
                      log_name = "t3_level1") {
  if ("object_full_trips" %in% ls(envir = .GlobalEnv)) {
    object_full_trips <- get(x = "object_full_trips", envir = .GlobalEnv)
    if (any(! class(object_full_trips) %in% c("full_trips", "list_t3", "R6"))) {
      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: invalid \"object_full_trips\" argument\n",
          "classes \"full_trips\", \"list_t3\" and \"R6\" expected\n",
          sep = "")
      stop()
    } else {
      # log file initialisation ----
      t3::initiate_log_file(log_file = log_file,
                            log_path = log_path,
                            log_name = log_name)
      # level 1.1: rf1 ----
      object_full_trips$rf1(species_rf1 = species_rf1_fr)
      # level 1.2: rf2 ----
      object_full_trips$rf2()
      # level 1.3: logbook weigth categories conversion ----
      object_full_trips$conversion_weigth_category()
      # level 1.4: set count ----
      object_full_trips$set_count()
      # level 1.5: set duration ----
      object_full_trips$set_duration(set_duration_ref = object_model_data$.__enclos_env__$private$setdurationref)
      # level 1.6: time at sea ----
      object_full_trips$time_at_sea()
      # level 1.7: fishing time ----
      object_full_trips$fishing_time()
      # level 1.8: searching time ----
      object_full_trips$searching_time()
      if (log_file == TRUE) {
        closeAllConnections()
      }
      assign(x = "object_full_trips",
             value = object_full_trips,
             envir = .GlobalEnv)
    }
  } else {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: object \"object_full_trips\" don't exist in the global environment\n",
        sep = "")
    stop()
  }
}
