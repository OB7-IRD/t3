#' @name t3_process
#' @title T3 process compilation
#' @description Run the t3 process, with the possibility to run all the process or stop.
#' @param process Object of class {\link[base]{character}} expected. Specify here if you want to run the whole process or just a part of it. By default "all". Check detail section below for more information.
#' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "t3_db" but you can switch to "avdth_db".
#' @param db_con Database connection R object expected. Mandatory argument for data source "t3_db", "avdth_db" and "sql_query".
#' @param log_file Object of class {\link[base]{logical}} expected. Initiation or not for log file creation. By default FALSE (no).
#' @param log_path Object of class {\link[base]{character}} expected. Path of the log file directory. By default NULL.
#' @param output_path Object of class \code{\link[base]{character}} expected. Outputs path directory. By default NULL.
#' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
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
#' @param periode_reference_level3 Object of type \code{\link[base]{integer}} expected. Year(s) period of reference for modelling estimation.
#' @param target_year Object of type \code{\link[base]{integer}} expected. Year of interest for the model estimation and prediction.Default value is current year -1.
#' @param period_duration Object of type \code{\link[base]{integer}} expected. number of years use for the modelling. The default value is 5
#' @param distance_maximum Object of type \code{\link[base]{integer}} expected. Maximum distance between all sets of a sampled well. By default 5.
#' @param number_sets_maximum Object of type \code{\link[base]{integer}} expected. Maximum number of sets allowed in mixture. By default 5.
#' @param set_weight_minimum Object of type \code{\link[base]{integer}} expected. Minimum set size considered. Remove smallest set for which sample could not be representative. By default 6 t.
#' @param minimum_set_frequency Object of type \code{\link[base]{numeric}} expected. Minimum threshold proportion of set in awell to be used for model training in the process. By default 0.1.
#' @param vessel_id_ignored Object of type \code{\link[base]{integer}} expected. Specify list of vessel(s) id(s) to be ignored in the model estimation and prediction .By default NULL.
#' @param num.trees Object of type \code{\link[base]{integer}} expected. Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times. The default value is 1000.
#' @param mtry Object of type \code{\link[base]{integer}} expected. Number of variables randomly sampled as candidates at each split. The default value is 2.
#' @param min.node.size Object of type \code{\link[base]{numeric}} expected. Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time).The default value is 5.
#' @param seed_number Object of type \code{\link[base]{integer}} expected. Set the initial seed for the modelling. The default value is 7.
#' @param small_fish_only Object of type \code{\link[base]{logical}} expected. Whether the model estimate proportion for small fish only (< 10 kg).
#' @param plot_sample \code{\link[base]{logical}}. Whether the sample figure is computed. Default value = F
#' @param avdth_patch_coord parameter waiting for coordinate conversion patch from avdth database
#' @param ci Object of type \code{\link[base]{logical}} expected. Logical indicating whether confidence interval is computed. The default value is FALSE as it is a time consuming step.
#' @param ci_type Type of confidence interval to compute. The default value is "all". Other options are "set" for ci on each set, "t1" for ci on nominal catch by species, "t1-fmod" for ci on nominal catch by species and fishing mode "t2" and "t2-fmod" for ci by 1 degree square and month. A vector of several ci option can be provided. ci_type are computed only if  the ci parameter is TRUE.
#' @param Nboot Object of type \code{\link[base]{numeric}} expected. The number of bootstrap samples desired for the ci computation. The default value is 10.
#' @param plot_predict Object of type \code{\link[base]{logical}} expected. Logical indicating whether maps of catch at size have to be done.
#' @details
#' For the argument "process", you can choose between 5 modalities (descending size classification):
#' \itemize{
#'  \item{all: }{argument by default, you launch all the process}
#'  \item{level1: }{you launch data model initialisation and the process level 1}
#'  \item{level2: }{you launch data model initialisation and the process level 2}
#'  \item{until_level2: }{you launch data model initialisation, the process level 1 and 2}
#' }
#' @importFrom codama r_type_checking
#' @export
t3_process <- function(process = "all",
                       data_source = "t3_db",
                       db_con,
                       log_file = FALSE,
                       log_path = NULL,
                       output_path = NULL,
                       output_format = "eu",
                       new_directory = TRUE,
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
                       threshold_rf_total = as.integer(250),
                       periode_reference_level3 = NULL,
                       target_year,
                       period_duration,
                       distance_maximum = as.integer(5),
                       number_sets_maximum = as.integer(5),
                       set_weight_minimum = as.integer(6),
                       minimum_set_frequency = 0.1,
                       vessel_id_ignored = NULL,
                       num.trees = 1000L,
                       mtry = 2L,
                       min.node.size = 5,
                       seed_number = 7L,
                       small_fish_only = FALSE,
                       plot_sample = FALSE,
                       avdth_patch_coord = FALSE,
                       ci = FALSE,
                       ci_type = "all",
                       Nboot = 50,
                       plot_predict = FALSE) {
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = process,
                              type = "character",
                              length = 1L,
                              allowed_value = c("all",
                                                "level1",
                                                "level2",
                                                "until_level2"),
                              output = "logical") != TRUE) {
    stop(codama::r_type_checking(r_object = process,
                                 type = "character",
                                 length = 1L,
                                 allowed_value = c("all",
                                                   "level1",
                                                   "level2",
                                                   "until_level2"),
                                 output = "message"))
  }
  # 2 - Process ----
  cat(format(x = Sys.time(),
             "%Y-%m-%d %H:%M:%S"),
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
    output_path_level1 <- output_path
    output_format_level1 <- output_format
    integrated_process <- FALSE
  } else if (process == "level2") {
    new_directory_level2 <- new_directory
    output_path_level2 <- output_path
    output_format_level2 <- output_format
    integrated_process <- FALSE
  } else if (process %in% c("all",
                            "until_level2")) {
    if (! is.null(x = output_path)) {
      integrated_process <- TRUE
      output_path <- initiate_directory(output_path = output_path,
                                        new_directory = new_directory,
                                        level = process)
      new_directory_level1 <- FALSE
      output_path_level1 <- output_path
      output_format_level1 <- output_format
      new_directory_level2 <- FALSE
      output_path_level2 <- output_path
      output_format_level2 <- output_format
      if (process == "all") {
        new_directory_level3 <- FALSE
        output_path_level3 <- output_path
        output_format_level3 <- output_format
      }
    } else {
      integrated_process <- FALSE
      new_directory_level1 <- new_directory
      output_path_level1 <- output_path
      output_format_level1 <- output_format
      new_directory_level2 <- new_directory
      output_path_level2 <- output_path
      output_format_level2 <- output_format
      if (process == "all") {
        new_directory_level3 <- new_directory
        output_path_level3 <- output_path
        output_format_level3 <- output_format
      }
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
                                 output_path = output_path_level1,
                                 output_format = output_format_level1,
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
                                 output_path = output_path_level2,
                                 output_format = output_format_level2,
                                 integrated_process = integrated_process)
  }
  if (process == "all") {
    t3_process[[3]] <- t3_process$object_full_trips$path_to_level3()
    names(t3_process)[3] <- "process_level3"
    t3_process[[3]] <- t3_level3(inputs_level3 = t3_process[[3]][[1]],
                                 inputs_level3_path = NULL,
                                 periode_reference_level3 = NULL,
                                 target_year,
                                 period_duration,
                                 distance_maximum = distance_maximum,
                                 number_sets_maximum = number_sets_maximum,
                                 set_weight_minimum = set_weight_minimum,
                                 minimum_set_frequency = minimum_set_frequency,
                                 vessel_id_ignored = vessel_id_ignored,
                                 num.trees = num.trees,
                                 mtry = mtry,
                                 min.node.size = min.node.size,
                                 seed_number = seed_number,
                                 small_fish_only = small_fish_only,
                                 plot_sample = plot_sample,
                                 avdth_patch_coord = avdth_patch_coord,
                                 ci = ci,
                                 ci_type = ci_type,
                                 Nboot = Nboot,
                                 plot_predict = plot_predict,
                                 log_file = log_file,
                                 log_path = log_path,
                                 log_name = "t3_level3",
                                 output_path = output_path_level3,
                                 output_format = output_format_level3,
                                 new_directory = new_directory_level3,
                                 integrated_process = integrated_process)
  }
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " - Successful process of the Tropical Tuna Treatment.\n",
      sep = "")
  return(t3_process)
}
