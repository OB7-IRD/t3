#' @name data_model_initialisation
#' @title Data model initialisation
#' @description Shortcut for initialisation of data's object model from a t3 or a avdth database.
#' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "t3_db" but you can switch to "avdth_db".
#' @param db_con Database connection R object expected. Mandatory argument for data source "t3_db", "avdth_db" and "sql_query".
#' @param log_file Object of class {\link[base]{logical}} expected. Initiation or not for log file creation. By default FALSE (no).
#' @param log_path Object of class {\link[base]{character}} expected. Path of the log file directory. By default NULL.
#' @param log_name Object of class {\link[base]{character}} expected. Name of the log file. By default "data_model_initialisation".
#' @param periode_reference Object of class {\link[base]{integer}} expected. Year(s) of the reference period coded on 4 digits. By default NULL.
#' @param countries Object of class {\link[base]{character}} expected. ISO code on 3 letters related to one or more countries. By default NULL.
#' @param oceans Object of class {\link[base]{integer}} expected. Ocean(s) related to data coded on 1 digit. By default NULL.
#' @param sample_type (integer) Sample type identification (landing, observer, ...). By default NULL.
#' @param trips_selected Object of class {\link[base]{character}} expected. Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
#' @return The function return a list with two R6 reference object inside: one class "object_model_data" and the second class "object_full_trips".
#' @export
data_model_initialisation <- function(data_source = "t3_db",
                                      db_con,
                                      log_file = FALSE,
                                      log_path = NULL,
                                      log_name = "data_model_initialisation",
                                      periode_reference,
                                      countries,
                                      oceans,
                                      sample_type,
                                      trips_selected = NULL) {
  # log file initialisation ----
  initiate_log_file(log_file = log_file,
                    log_path = log_path,
                    log_name = log_name)
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " - Start function data model initialisation.\n",
      "[periode reference: ",
      paste0(periode_reference,
             collapse = ", "),
      "; countries: ",
      paste0(countries,
             collapse = ", "),
      "; oceans: ",
      paste0(oceans,
             collapse = ", "),
      "; sample type: ",
      sample_type,
      ifelse(test = is.null(trips_selected),
             yes = "]\n",
             no = paste0(", trips selected: ",
                         paste0(trips_selected,
                                collapse = ", "),
                         "]\n")),
      sep = "")
  # initialisation object for data's object model ----
  object_model_data <- object_model_data$new()
  # model creation: object trips creation ----
  object_model_data$trips_object_creation(data_source = data_source,
                                          db_con = db_con,
                                          periode_reference = periode_reference,
                                          countries = countries,
                                          oceans = oceans,
                                          trips_selected = trips_selected)
  # model creation: object activites creation ----
  object_model_data$activities_object_creation(data_source = data_source,
                                               db_con = db_con,
                                               periode_reference = periode_reference,
                                               countries = countries,
                                               oceans = oceans,
                                               trips_selected = trips_selected)
  # model creation: object elementarycatches creation ----
  object_model_data$elementarycatches_object_creation(data_source = data_source,
                                                      db_con = db_con,
                                                      periode_reference = periode_reference,
                                                      countries = countries,
                                                      oceans = oceans,
                                                      trips_selected = trips_selected)
  # model creation: object elementarylandings creation ----
  object_model_data$elementarylandings_object_creation(data_source = data_source,
                                                       db_con = db_con,
                                                       periode_reference = periode_reference,
                                                       countries = countries,
                                                       oceans = oceans,
                                                       trips_selected = trips_selected)
  # model creation: object wells creation ----
  object_model_data$wells_object_creation(data_source = data_source,
                                          db_con = db_con,
                                          periode_reference = periode_reference,
                                          countries = countries,
                                          oceans = oceans,
                                          sample_type = sample_type,
                                          trips_selected = trips_selected)
  # model creation: set durations data ----
  if (data_source == "avdth_db") {
    object_model_data$setdurationrefs_data(data_source = "csv",
                                           data_path = system.file("setdurationrefs.csv",
                                                                   package = "t3"),
                                           periode_reference = periode_reference,
                                           countries = countries)
  } else {
    object_model_data$setdurationrefs_data(data_source = data_source,
                                           db_con = db_con,
                                           periode_reference = periode_reference,
                                           countries = countries)
  }
  # model creation: length steps data ----
  if (data_source == "avdth_db") {
    object_model_data$lengthsteps_data(data_source = "csv",
                                       data_path = system.file("lengthsteps.csv",
                                                               package = "t3"))
  } else {
    object_model_data$lengthsteps_data(data_source = data_source,
                                       db_con = db_con)
  }
  # model creation: sample sets data ----
  object_model_data$samplesets_data(data_source = data_source,
                                    db_con = db_con,
                                    periode_reference = periode_reference,
                                    countries = countries,
                                    oceans = oceans,
                                    trips_selected = trips_selected)
  # model creation: length weight relationships data ----
  if (data_source == "avdth_db") {
    object_model_data$lengthweightrelationships_data(data_source = "csv",
                                                     data_path = system.file("lengthweightrelationships.csv",
                                                                             package = "t3"))
  } else {
    object_model_data$lengthweightrelationships_data(db_con = db_con)
  }
  # model creation: initialisation object for full trips class ----
  object_full_trips <- full_trips$new()
  # model creation: object full_trip creation ----
  object_full_trips$create_full_trips(object_trips = object_model_data$.__enclos_env__$private$trips)
  # model creation: add activities to trips selected ----
  object_full_trips$add_activities(object_activities = object_model_data$.__enclos_env__$private$activities)
  # model creation: filter by reference year
  object_full_trips$filter_by_time_period_reference(time_periode_reference = periode_reference)
  # model creation: add elementarycatches to trips selected ----
  object_full_trips$add_elementarycatches(object_elementarycatches = object_model_data$.__enclos_env__$private$elementarycatches)
  # model creation: add elementarylandings to trips selected ----
  object_full_trips$add_elementarylandings(object_elementarylandings = object_model_data$.__enclos_env__$private$elementarylandings)
  # model creation: add well(s) and sample(s) to trip(s) selected ----
  object_full_trips$add_wells_samples(object_wells = object_model_data$.__enclos_env__$private$wells)
  # close, if necessary log file connection ----
  if (log_file == TRUE) {
    closeAllConnections()
  }
  return(list("object_model_data" = object_model_data,
              "object_full_trips" = object_full_trips))
}
