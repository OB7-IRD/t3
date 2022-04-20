#' @name t3_process
#' @title T3 process compilation
#' @description Run the t3 process, with the possibility to run all the process or stop.
#' @param process Object of class {\link[base]{character}} expected. Specify here if you want to run the whole process or just a part of it. By default "all". Check detail section below for more information.
#' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "t3_db" but you can switch to "avdth_db".
#' @param db_con Database connection R object expected. Mandatory argument for data source "t3_db", "avdth_db" and "sql_query".
#' @param log_file Object of class {\link[base]{logical}} expected. Initiation or not for log file creation. By default FALSE (no).
#' @param log_path Object of class {\link[base]{character}} expected. Path of the log file directory. By default NULL.
#' @param outputs_path Object of class \code{\link[base]{character}} expected. Outputs path directory. By default NULL.
#' @param new_directory Object of class \code{\link[base]{logical}} expected. Initiate a new outputs directory of use an existing one. By default NULL.
#' @param periode_reference Object of class {\link[base]{integer}} expected. Year(s) of the reference period coded on 4 digits. By default NULL.
#' @param countries Object of class {\link[base]{character}} expected. ISO code on 3 letters related to one or more countries. By default NULL.
#' @param oceans Object of class {\link[base]{integer}} expected. Ocean(s) related to data coded on 1 digit. By default NULL.
#' @param sample_type (integer) Sample type identification (landing, observer, ...). By default NULL.
#' @param trips_selected Object of class {\link[base]{character}} expected. Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
#' @param species_rf1 Object of type \code{\link[base]{integer}} expected. Specie(s) code(s) used for the RF1 process. By default 1 (YFT), 2 (SKJ), 3 (BET), 4 (ALB), 9 (MIX) and 11 (LOT).
#' @param rf1_lowest_limit Object of type \code{\link[base]{numeric}} expected. Verification value for the lowest limit of the RF1. By default 0.8.
#' @param rf1_highest_limit Object of type \code{\link[base]{numeric}} expected. Verification value for the highest limit of the RF1. By default 1.2.
#' @param sunrise_schema Object of class {\link[base]{character}} expected. Sunrise caracteristic. By default "sunrise" (top edge of the sun appears on the horizon). See function fishing_time() for more details.
#' @param sunset_schema Object of class {\link[base]{character}} expected. Sunset caracteristic. By default "sunset" (sun disappears below the horizon, evening civil twilight starts). See function fishing_time() for more details.
#' @param maximum_lf_class Object of type \code{\link[base]{integer}} expected. Theorical maximum lf class that can occur (all species considerated). By default 500.
#' @param threshold_rf_minus10 Object of type \code{\link[base]{integer}} expected. Threshold limite value for raising factor on individuals category minus 10. By default 500.
#' @param threshold_rf_plus10 Object of type \code{\link[base]{integer}} expected. Threshold limite value for raising factor on individuals category plus 10. By default 500.
#' @param threshold_frequency_rf_minus10 Object of type \code{\link[base]{integer}} expected. Threshold limite frequency value for raising factor on individuals category minus 10. By default 75.
#' @param threshold_frequency_rf_plus10 Object of type \code{\link[base]{integer}} expected. Threshold limite frequency value for raising factor on individuals category plus 10. By default 75.
#' @param threshold_rf_total Object of type \code{\link[base]{integer}} expected. Threshold limite value for raising factor (all categories). By default 250.
#' @details
#' For the argument "process", you can choose between 5 modalities (descending size classification):
#' \itemize{
#'  \item{all: }{argument by default, you launch all the process}
#'  \item{level1: }{you launch data model initialisation and the process level 1}
#'  \item{level2: }{you launch data model initialisation and the process level 2}
#'  \item{until_level2: }{you launch data model initialisation, the process level 1 and 2}
#' }
#' @export
t3_process <- function(process = "all",
                       data_source = "t3_db",
                       db_con,
                       log_file = FALSE,
                       log_path = NULL,
                       outputs_path = NULL,
                       new_directory = NULL,
                       periode_reference,
                       countries,
                       oceans,
                       sample_type,
                       trips_selected = NULL,
                       species_rf1 = as.integer(c(1, 2, 3, 4, 9, 11)),
                       rf1_lowest_limit = 0.8,
                       rf1_highest_limit = 1.2,
                       sunrise_schema = "sunrise",
                       sunset_schema = "sunset",
                       maximum_lf_class = as.integer(500),
                       threshold_rf_minus10 = as.integer(500),
                       threshold_rf_plus10 = as.integer(500),
                       threshold_frequency_rf_minus10 = as.integer(75),
                       threshold_frequency_rf_plus10 = as.integer(75),
                       threshold_rf_total = as.integer(250)) {
  if (paste(class(process), collapse = " ") != "character"
      || length(process) != 1
      || ! process %in% c("all",
                          "level1",
                          "level2",
                          "until_level2")) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalid \"process\" argument.\n",
        "Classes \"character\" with one unique value (\"all\", \"level1\", \"level2\" or \"until_level2\") expected.\n",
        sep = "")
    stop()
  } else {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Ignition of the Tropical Tuna Treatment.\n",
        "Process could be long. Until reach 88 mph, take a coffee.\n",
        sep = "")
    t3_process <- data_model_initialisation(data_source = data_source,
                                            db_con = db_con,
                                            log_file = log_file,
                                            log_path = log_path,
                                            log_name = "data_model_initialisation",
                                            periode_reference = periode_reference,
                                            countries = countries,
                                            oceans = oceans,
                                            sample_type = sample_type,
                                            trips_selected = trips_selected)
    if (process == "level1") {
      new_directory_level1 <- new_directory
      outputs_path_level1 <- outputs_path
      integrated_process <- FALSE
    } else if (process == "level2") {
      new_directory_level2 <- new_directory
      outputs_path_level2 <- outputs_path
      integrated_process <- FALSE
    } else if (process %in% c("all",
                              "until_level2")) {
      if (! is.null(x = outputs_path)) {
        integrated_process <- TRUE
        outputs_path <- initiate_directories(outputs_path = outputs_path,
                                             new_directory = new_directory,
                                             level = process)
        new_directory_level1 <- FALSE
        outputs_path_level1 <- outputs_path
        new_directory_level2 <- FALSE
        outputs_path_level2 <- outputs_path
      } else {
        integrated_process <- FALSE
        new_directory_level1 <- new_directory
        outputs_path_level1 <- outputs_path
        new_directory_level2 <- new_directory
        outputs_path_level2 <- outputs_path
      }
    }
    if (process %in% c("all",
                       "level1",
                       "until_level2")) {
      t3_process[[2]] <- t3_level1(object_model_data = t3_process[[1]],
                                   object_full_trips = t3_process[[2]],
                                   log_file = log_file,
                                   log_path = log_path,
                                   log_name = "t3_level1",
                                   species_rf1 = species_rf1,
                                   rf1_lowest_limit = rf1_lowest_limit,
                                   rf1_highest_limit = rf1_highest_limit,
                                   sunrise_schema = sunrise_schema,
                                   sunset_schema = sunset_schema,
                                   new_directory = new_directory_level1,
                                   outputs_path = outputs_path_level1,
                                   integrated_process = integrated_process)
    }
    if (process %in% c("all",
                       "level2",
                       "until_level2")) {
      t3_process[[2]] <- t3_level2(object_model_data = t3_process[[1]],
                                   object_full_trips = t3_process[[2]],
                                   maximum_lf_class = maximum_lf_class,
                                   threshold_rf_minus10 = threshold_rf_minus10,
                                   threshold_rf_plus10 = threshold_rf_plus10,
                                   threshold_frequency_rf_minus10 = threshold_frequency_rf_minus10,
                                   threshold_frequency_rf_plus10 = threshold_frequency_rf_plus10,
                                   threshold_rf_total = threshold_rf_total,
                                   log_file = log_file,
                                   log_path = log_path,
                                   log_name = "t3_level2",
                                   new_directory = new_directory_level2,
                                   outputs_path = outputs_path_level2,
                                   integrated_process = integrated_process)
    }
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Successful process of the Tropical Tuna Treatment.\n",
        sep = "")
    return(t3_process)
  }
}
