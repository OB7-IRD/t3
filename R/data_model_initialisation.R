#' @name data_model_initialisation
#' @title Data model initialisation
#' @description Shortcut for initialisation of data's object model from a observe or a AVDTH database.
#' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "observe_database" but you can switch to "avdth_database".
#' @param database_connection Database connection, list of one or more R object(s) expected. By default NULL.
#' Mandatory argument for data source "observe_database" ("PostgreSQLConnection" R object), corresponding to the second element of the object returned by \href{https://ob7-ird.github.io/furdeb/reference/postgresql_dbconnection.html}{`furdeb::postgresql_dbconnection()`}.
#' Or mandatory argument for data source"avdth_database" ("JDBCConnection" R object) corresponding to the second element of the object returned by \href{https://ob7-ird.github.io/furdeb/reference/access_dbconnection.html}{`furdeb::access_dbconnection()`}.
#' For data source "observe_database", a list of "PostgreSQLConnection" R objects can be specified to query data from different observe databases.
#' For example, a list of two database connection arguments for "observe_main" and "observe_acquisition" can be specified to simultaneously import and process recent data from acquisition database, which has not yet been imported into the main database, and older data from the main database.
#' @param log_file Object of class {\link[base]{logical}} expected. By default FALSE. Initiation or not for log file creation.
#' @param log_path Object of class {\link[base]{character}} expected. By default NULL. Path of the log file directory.
#' @param log_name Object of class {\link[base]{character}} expected. By default "data_model_initialisation". Name of the log file.
#' @param years_period Object of class {\link[base]{integer}} expected. By default NULL. Year(s) of the reference time period coded on 4 digits. Mandatory for data source "observe_database" and "avdth_database".
#' @param flag_codes Object of class {\link[base]{character}} expected. By default NULL. Country(ies) code related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
#' @param ocean_codes Object of class {\link[base]{integer}} expected. By default NULL. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "observe_database" and "avdth_database".
#' @param vessel_type_codes Object of class {\link[base]{integer}} expected. By default NULL. Vessel type(s) related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
#' @param species_fate_codes Object of class {\link[base]{integer}} expected. By default NULL. Specie fate(s) related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
#' @param sample_type_codes Object of class {\link[base]{integer}} expected. By default NULL. Sample type identification.
#' @param trip_ids Object of class {\link[base]{character}} expected. By default NULL. Additional parameter only used with data source "observe_database". Use trip(s) identification(s) for selected trip(s) kept in the query. This argument overrides all others arguments like "years_period", "country" or "ocean".
#' @return The function return a list with two R6 reference object inside: one class "object_model_data" and the second class "object_full_trips".
#' @importFrom codama r_type_checking
#' @export
data_model_initialisation <- function(data_source = "observe_database",
                                      database_connection,
                                      log_file = FALSE,
                                      log_path = NULL,
                                      log_name = "data_model_initialisation",
                                      years_period,
                                      flag_codes,
                                      ocean_codes,
                                      vessel_type_codes,
                                      species_fate_codes,
                                      sample_type_codes,
                                      trip_ids = NULL) {
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
  message(format(Sys.time(),
                 "%Y-%m-%d %H:%M:%S"),
          " - Start function data model initialisation.\n",
          "[year(s) period: ",
          paste0(years_period,
                 collapse = ", "),
          "; flag(s) code(s): ",
          paste0(flag_codes,
                 collapse = ", "),
          "; ocean(s) code(s): ",
          paste0(ocean_codes,
                 collapse = ", "),
          "; vessel(s) type(s) code(s): ",
          paste0(vessel_type_codes,
                 collapse = ", "),
          "; sample(s) type(s) code(s): ",
          sample_type_codes,
          ifelse(test = is.null(x = trip_ids),
                 yes = "]",
                 no = paste0(", trip(s) id(s) selected: ",
                             paste0(trip_ids,
                                    collapse = ", "),
                             "]")))
  # 3 - Data model initilization ----
  # initialisation object for data's object model
  object_model_data <- object_model_data$new()
  # model creation: object trips creation
  object_model_data$trips_object_creation(data_source = data_source,
                                          database_connection = database_connection,
                                          years_period = years_period,
                                          flag_codes = flag_codes,
                                          ocean_codes = ocean_codes,
                                          vessel_type_codes = vessel_type_codes,
                                          trip_ids = trip_ids)
  # model creation: object activites creation
  object_model_data$activities_object_creation(data_source = data_source,
                                               database_connection = database_connection,
                                               years_period = years_period,
                                               flag_codes = flag_codes,
                                               ocean_codes = ocean_codes,
                                               vessel_type_codes = vessel_type_codes,
                                               trip_ids = trip_ids)
  # model creation: object elementarycatches creation
  object_model_data$elementarycatches_object_creation(data_source = data_source,
                                                      database_connection = database_connection,
                                                      years_period = years_period,
                                                      flag_codes = flag_codes,
                                                      ocean_codes = ocean_codes,
                                                      vessel_type_codes = vessel_type_codes,
                                                      species_fate_codes = species_fate_codes,
                                                      trip_ids = trip_ids)
  # model creation: object elementarylandings creation
  object_model_data$elementarylandings_object_creation(data_source = data_source,
                                                       database_connection = database_connection,
                                                       years_period = years_period,
                                                       flag_codes = flag_codes,
                                                       ocean_codes = ocean_codes,
                                                       vessel_type_codes = vessel_type_codes,
                                                       trip_ids = trip_ids)
  # model creation: object wells creation
  object_model_data$wells_object_creation(data_source = data_source,
                                          database_connection = database_connection,
                                          years_period = years_period,
                                          flag_codes = flag_codes,
                                          ocean_codes = ocean_codes,
                                          vessel_type_codes = vessel_type_codes,
                                          sample_type_codes = sample_type_codes,
                                          trip_ids = trip_ids)
  # model creation: set durations data
  object_model_data$setdurationrefs_data(data_path = system.file("set_duration_ref.csv",
                                                                 package = "t3"))
  # model creation: length steps data
  object_model_data$lengthsteps_data(data_path = system.file("length_step.csv",
                                                             package = "t3"))
  # model creation: sample sets data
  object_model_data$samplesets_data(data_source = data_source,
                                    years_period = years_period,
                                    flag_codes = flag_codes,
                                    ocean_codes = ocean_codes,
                                    vessel_type_codes = vessel_type_codes,
                                    database_connection = database_connection,
                                    trip_ids = trip_ids)
  # model creation: length weight relationships data
  object_model_data$lengthweightrelationships_data(data_source = "csv_file",
                                                   data_path = system.file("length_weight_relationship.csv",
                                                                           package = "t3"))
  # 4 - Full trips model initilization ----
  # model creation: initialisation object for full trips class
  object_full_trips <- full_trips$new()
  # model creation: object full_trip creation
  object_full_trips$create_full_trips(object_trips = object_model_data$.__enclos_env__$private$trips)
  # model creation: add activities to trips selected
  object_full_trips$add_activities(object_activities = object_model_data$.__enclos_env__$private$activities)
  # model creation: filter by reference year
  object_full_trips$filter_by_years_period(years_period = years_period)
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
