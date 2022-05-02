#' @name t3_level1
#' @title T3 process level 1
#' @description Level 1 of t3 process (logbooks standardisation).
#' @param object_model_data Object of classes \code{\link[t3]{object_model_data}} and \code{\link[R6]{R6}} expected.
#' @param object_full_trips Object of classes \code{\link[t3]{full_trips}}, \code{\link[t3]{list_t3}} and \code{\link[R6]{R6}} expected.
#' @param species_rf1 Object of type \code{\link[base]{integer}} expected. Specie(s) code(s) used for the RF1 process. By default 1 (YFT), 2 (SKJ), 3 (BET), 4 (ALB), 9 (MIX) and 11 (LOT).
#' @param rf1_lowest_limit Object of type \code{\link[base]{numeric}} expected. Verification value for the lowest limit of the RF1. By default 0.8.
#' @param rf1_highest_limit Object of type \code{\link[base]{numeric}} expected. Verification value for the highest limit of the RF1. By default 1.2.
#' @param sunrise_schema Object of class {\link[base]{character}} expected. Sunrise caracteristic. By default "sunrise" (top edge of the sun appears on the horizon). See function fishing_time() for more details.
#' @param sunset_schema Object of class {\link[base]{character}} expected. Sunset caracteristic. By default "sunset" (sun disappears below the horizon, evening civil twilight starts). See function fishing_time() for more details.
#' @param log_file Object of class {\link[base]{logical}} expected. Initiation or not for log file creation. By default FALSE (no).
#' @param log_path Object of class {\link[base]{character}} expected. Path of the log file directory. By default NULL.
#' @param log_name Object of class {\link[base]{character}} expected. Name of the log file. By default "t3_level1".
#' @param outputs_path Object of class \code{\link[base]{character}} expected. Outputs path directory. By default NULL.
#' @param new_directory Object of class \code{\link[base]{logical}} expected. Initiate a new outputs directory of use an existing one. By default NULL.
#' @param integrated_process Object of class \code{\link[base]{logical}} expected. Indicate if the process is integrated in another (like the one in the function "t3_process"). By default FALSE.
#' @return The function a R6 reference object of class "object_full_trips".
#' @export
t3_level1 <- function(object_model_data,
                      object_full_trips,
                      species_rf1 = as.integer(c(1, 2, 3, 4, 9, 11)),
                      rf1_lowest_limit = 0.8,
                      rf1_highest_limit = 1.2,
                      sunrise_schema = "sunrise",
                      sunset_schema = "sunset",
                      log_file = FALSE,
                      log_path = NULL,
                      log_name = "t3_level1",
                      outputs_path = NULL,
                      new_directory = NULL,
                      integrated_process = FALSE) {
  if (paste0(class(object_model_data),
             collapse = " ") != "object_model_data R6") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalid \"object_model_data\" argument.\n",
        "Classes \"object_model_data\" and \"R6\" expected.\n",
        sep = "")
    stop()
  } else if (paste0(class(object_full_trips),
                    collapse = " ") != "full_trips list_t3 R6") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Error: invalid \"object_full_trips\" argument.\n",
        "Classes \"full_trips\", \"list_t3\" and \"R6\" expected.\n",
        sep = "")
    stop()
  } else {
    # log file initialisation ----
    initiate_log_file(log_file = log_file,
                      log_path = log_path,
                      log_name = log_name)
    cat(format(Sys.time(),
               "%Y-%m-%d %H:%M:%S"),
        " - Start function t3 process level 1.\n",
        "[species rf1: ",
        paste0(species_rf1,
               collapse = ", "),
        "]\n",
        sep = "")
    # directories initialization if outputs extraction ----
    if (integrated_process != TRUE
        && ! is.null(x = outputs_path)) {
      outputs_path <- initiate_directories(outputs_path = outputs_path,
                                           new_directory = new_directory,
                                           level = "level1")
    }
    # level 1.1: rf1 ----
    object_full_trips$rf1(species_rf1 = species_rf1,
                          rf1_lowest_limit = rf1_lowest_limit,
                          rf1_highest_limit = rf1_highest_limit,
                          global_outputs_path = outputs_path)
    # level 1.2: rf2 ----
    object_full_trips$rf2(global_outputs_path = outputs_path)
    # level 1.3: logbook weigth categories conversion ----
    object_full_trips$conversion_weigth_category()
    # level 1.4: set count ----
    object_full_trips$set_count()
    # level 1.5: set duration ----
    object_full_trips$set_duration(set_duration_ref = object_model_data$.__enclos_env__$private$setdurationrefs)
    # level 1.6: time at sea ----
    object_full_trips$time_at_sea()
    # level 1.7: fishing time ----
    object_full_trips$fishing_time(sunrise_schema = "sunrise",
                                   sunset_schema = "sunset")
    # level 1.8: searching time ----
    object_full_trips$searching_time()
    # close, if necessary log file connection ----
    if (log_file == TRUE) {
      closeAllConnections()
    }
    return(object_full_trips)
  }
}
