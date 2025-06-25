#' @name t3_level3
#' @title T3 process level 3
#' @description Level 3 of t3 process (modelislation).
#' @param inputs_level3 Object of type \code{\link[base]{data.frame}} expected. Inputs of levels 3 (see function path to level 3).
#' @param inputs_level3_path Object of type \code{\link[base]{character}} expected. Path to the folder containing yearly data output of the level 1 and 2 (output of the function the path to level 3). If provide, replace the inputs_level3 object.
#' @param periode_reference_level3 Object of type \code{\link[base]{integer}} expected. Year(s) period of reference for modelling estimation.
#' @param target_year Object of type \code{\link[base]{integer}} expected. Year of interest for the model estimation and prediction.Default value is current year -1.
#' @param target_ocean Object of type \code{\link[base]{integer}} expected. The code of ocean of interest.
#' @param period_duration Object of type \code{\link[base]{integer}} expected. number of years use for the modelling. The default value is 5
#' @param country_flag Three letters FAO flag code of country to estimate catches.
#' @param input_type Type of coding use in different databases. Default value is 'observe_database'. Values can be 'observe_database' or 'avdth_database'.
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
#' @param log_file Object of type \code{\link[base]{logical}} expected. Initiation or not for log file creation. By default FALSE (no).
#' @param log_path Object of type \code{\link[base]{character}} expected. Path of the log file directory. By default NULL.
#' @param log_name Object of type \code{\link[base]{character}} expected. Name of the log file. By default "t3_level2".
#' @param output_path Object of class \code{\link[base]{character}} expected. Outputs path directory. By default NULL.
#' @param new_directory Object of class \code{\link[base]{logical}} expected. Initiate a new outputs directory of use an existing one. By default NULL.
#' @param integrated_process Object of class \code{\link[base]{logical}} expected. Indicate if the process is integrated in another (like the one in the function "t3_process"). By default FALSE.
#' @export
t3_level3 <- function(inputs_level3,
                      inputs_level3_path = NULL,
                      periode_reference_level3 = NULL,
                      target_year,
                      target_ocean = NULL,
                      period_duration,
                      country_flag = NULL,
                      input_type = "observe_database",
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
                      plot_predict = FALSE,
                      log_file = FALSE,
                      log_path = NULL,
                      log_name = "t3_level3",
                      output_path = NULL,
                      new_directory = FALSE,
                      integrated_process = FALSE) {
  # 1 - Log file initialisation ----
  initiate_log_file(log_file = log_file,
                    log_path = log_path,
                    log_name = log_name)
  # 2 - Directories initialization if outputs extraction ----
  if (integrated_process != TRUE
      && ! is.null(x = output_path)) {
    output_path <- initiate_directory(output_path = output_path,
                                      new_directory = new_directory,
                                      level = "level3")
  }
  # 3 - Level 3 process ----
  message(format(x = Sys.time(),
                 "%Y-%m-%d %H:%M:%S"),
          " - Start function t3 process level 3.")
  # methodes initialisation
  object_full_trips <- full_trips$new()
  # level 3.1: data preparatory
  process_level3 <- object_full_trips$data_preparatory(inputs_level3 = inputs_level3,
                                                       inputs_level3_path = inputs_level3_path,
                                                       output_directory = output_path,
                                                       periode_reference_level3 = periode_reference_level3,
                                                       target_year = target_year,
                                                       target_ocean = target_ocean,
                                                       period_duration = period_duration,
                                                       distance_maximum = distance_maximum,
                                                       number_sets_maximum = number_sets_maximum,
                                                       set_weight_minimum = set_weight_minimum,
                                                       minimum_set_frequency = minimum_set_frequency,
                                                       vessel_id_ignored = vessel_id_ignored)
  # level 3.2: random forest models
  process_level3$output_level3_process2 <- object_full_trips$random_forest_models(output_level3_process1 = process_level3$output_level3_process1$data_lb_sample_screened$data4mod,
                                                                                  num.trees = num.trees,
                                                                                  mtry = mtry,
                                                                                  min.node.size = min.node.size,
                                                                                  seed_number = seed_number,
                                                                                  small_fish_only = small_fish_only)
  # level 3.3: models checking
  process_level3$output_level3_process3 <- object_full_trips$models_checking(output_level3_process2 = process_level3$output_level3_process2,
                                                                             output_directory = process_level3$output_directory,
                                                                             plot_sample = plot_sample,
                                                                             avdth_patch_coord = avdth_patch_coord)
  # level 3.4: data formatting for predictions
  process_level3$output_level3_process4 <- object_full_trips$data_formatting_for_predictions(inputs_level3 = process_level3$raw_inputs_level3,
                                                                                             output_level3_process1 = process_level3$output_level3_process1$data_lb_sample_screened$data4mod,
                                                                                             target_year = target_year,
                                                                                             vessel_id_ignored = vessel_id_ignored,
                                                                                             small_fish_only = small_fish_only,
                                                                                             country_flag = country_flag,
                                                                                             input_type = input_type)
  # level 3.5: predictions
  process_level3$output_level3_process5 <- object_full_trips$model_predictions(output_level3_process2 = process_level3$output_level3_process2,
                                                                               output_level3_process4 = process_level3$output_level3_process4,
                                                                               output_directory = process_level3$output_directory,
                                                                               ci = ci,
                                                                               ci_type = ci_type,
                                                                               Nboot = Nboot,
                                                                               plot_predict = plot_predict,
                                                                               country_flag = country_flag)
  # close, if necessary log file connection
  if (log_file == TRUE) {
    closeAllConnections()
  }
  message(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Successful function t3 process level 3.")
  return(process_level3)
}
