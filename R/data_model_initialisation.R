#' @name data_model_initialisation
#' @title Data model initialisation
#' @description Shortcut for initialisation of data's object model from a observe or a AVDTH database.
#' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "observe_database" but you can switch to "avdth_database".
#' @param database_connection Database connection R object expected.  Mandatory argument for data source "observe_database" and "avdth_database".
#' @param log_file Object of class {\link[base]{logical}} expected. By default FALSE. Initiation or not for log file creation.
#' @param log_path Object of class {\link[base]{character}} expected. By default NULL. Path of the log file directory.
#' @param log_name Object of class {\link[base]{character}} expected. By default "data_model_initialisation". Name of the log file.
#' @param time_period Object of class {\link[base]{integer}} expected. Year(s) of the reference time period coded on 4 digits.
#' @param fleet_code Object of class {\link[base]{character}} expected. Country(ies) code related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
#' @param ocean_code Object of class {\link[base]{integer}} expected. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "observe_database" and "avdth_database".
#' @param vessel_type_code Object of class {\link[base]{integer}} expected. Vessel type(s) related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
#' @param species_fate_code Object of class {\link[base]{integer}} expected. Specie fate(s) related to data extraction.
#' @param sample_type_code Object of class {\link[base]{integer}} expected. Sample type identification.
#' @param trip_id Object of class {\link[base]{character}} expected. By default NULL. Additional parameter only used with data source "observe_database". Use trip(s) identification(s) for selected trip(s) kept in the query. This argument overrides all others arguments like "time_periode", "country" or "ocean".
#' @return The function return a list with two R6 reference object inside: one class "object_model_data" and the second class "object_full_trips".
#' @export
data_model_initialisation <- function(data_source = "observe_database",
                                      database_connection,
                                      log_file = FALSE,
                                      log_path = NULL,
                                      log_name = "data_model_initialisation",
                                      time_period,
                                      fleet_code,
                                      ocean_code,
                                      vessel_type_code,
                                      species_fate_code,
                                      sample_type_code,
                                      trip_id = NULL) {
  # 1 - Arguments verifications ----
  codama::r_type_checking(r_object = data_source,
                          type = "character",
                          length = 1L,
                          allowed_value = c("observe_database",
                                            "avdth_database"))
  # 2 - Log file initialisation ----
  initiate_log_file(log_file = log_file,
                    log_path = log_path,
                    log_name = log_name)
  cat(format(Sys.time(),
             "%Y-%m-%d %H:%M:%S"),
      " - Start function data model initialisation.\n",
      "[time period: ",
      paste0(time_period,
             collapse = ", "),
      "; fleet code: ",
      paste0(fleet_code,
             collapse = ", "),
      "; ocean code: ",
      paste0(ocean_code,
             collapse = ", "),
      "; vessel type code: ",
      paste0(vessel_type_code,
             collapse = ", "),
      "; sample type code: ",
      sample_type_code,
      ifelse(test = is.null(x = trip_id),
             yes = "]\n",
             no = paste0(", trip(s) id selected: ",
                         paste0(trip_id,
                                collapse = ", "),
                         "]\n")),
      sep = "")
  # 3 - Data model initilization ----
  # initialisation object for data's object model
  object_model_data <- object_model_data$new()
  # model creation: object trips creation
  object_model_data$trips_object_creation(data_source = data_source,
                                          time_period = time_period,
                                          fleet_code = fleet_code,
                                          ocean_code = ocean_code,
                                          vessel_type_code = vessel_type_code,
                                          database_connection = database_connection,
                                          trip_id = trip_id)
  # model creation: object activites creation
  object_model_data$activities_object_creation(data_source = data_source,
                                               time_period = time_period,
                                               fleet_code = fleet_code,
                                               ocean_code = ocean_code,
                                               vessel_type_code = vessel_type_code,
                                               database_connection = database_connection,
                                               trip_id = trip_id)
  # model creation: object elementarycatches creation
  object_model_data$elementarycatches_object_creation(data_source = data_source,
                                                      time_period = time_period,
                                                      fleet_code = fleet_code,
                                                      ocean_code = ocean_code,
                                                      vessel_type_code = vessel_type_code,
                                                      species_fate_code = species_fate_code,
                                                      database_connection = database_connection,
                                                      trip_id = trip_id)
  # model creation: object elementarylandings creation
  object_model_data$elementarylandings_object_creation(data_source = data_source,
                                                       time_period = time_period,
                                                       fleet_code = fleet_code,
                                                       ocean_code = ocean_code,
                                                       vessel_type_code = vessel_type_code,
                                                       database_connection = database_connection,
                                                       trip_id = trip_id)
  # model creation: object wells creation
  object_model_data$wells_object_creation(data_source = data_source,
                                          time_period = time_period,
                                          fleet_code = fleet_code,
                                          ocean_code = ocean_code,
                                          vessel_type_code = vessel_type_code,
                                          sample_type_code = sample_type_code,
                                          database_connection = database_connection,
                                          trip_id = trip_id)
  # model creation: set durations data
  object_model_data$setdurationrefs_data(data_path = system.file("set_duration_ref.csv",
                                                                 package = "t3"))
  # model creation: length steps data
  object_model_data$lengthsteps_data(data_path = system.file("length_step.csv",
                                                             package = "t3"))
  # model creation: sample sets data
  object_model_data$samplesets_data(data_source = data_source,
                                    time_period = time_period,
                                    fleet_code = fleet_code,
                                    ocean_code = ocean_code,
                                    vessel_type_code = vessel_type_code,
                                    database_connection = database_connection,
                                    trip_id = trip_id)
  # model creation: length weight relationships data
  if (data_source == "avdth_database") {
    object_model_data$lengthweightrelationships_data(data_source = "csv_file",
                                                     data_path = system.file("length_weight_relationship.csv",
                                                                             package = "t3"))
  } else {
    object_model_data$lengthweightrelationships_data(database_connection = database_connection)
  }
  # 4 - Full trips model initilization ----
  # model creation: initialisation object for full trips class
  object_full_trips <- full_trips$new()
  # model creation: object full_trip creation
  object_full_trips$create_full_trips(object_trips = object_model_data$.__enclos_env__$private$trips)
  # model creation: add activities to trips selected
  object_full_trips$add_activities(object_activities = object_model_data$.__enclos_env__$private$activities)
  # model creation: filter by reference year
  object_full_trips$filter_by_time_period(time_period = time_period)
  # model creation: add elementarycatches to trips selected
  object_full_trips$add_elementarycatches(object_elementarycatches = object_model_data$.__enclos_env__$private$elementarycatches)
  # model creation: add elementarylandings to trips selected
  object_full_trips$add_elementarylandings(object_elementarylandings = object_model_data$.__enclos_env__$private$elementarylandings)
  # model creation: add well(s) and sample(s) to trip(s) selected
  object_full_trips$add_wells_samples(object_wells = object_model_data$.__enclos_env__$private$wells)
  # close, if necessary log file connection
  if (log_file == TRUE) {
    closeAllConnections()
  }
  return(list("object_model_data" = object_model_data,
              "object_full_trips" = object_full_trips))
}
