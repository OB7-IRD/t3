#' @name data_model_initialisation
#' @title Data model initialisation
#' @description Shortcut for initialisation of data's object model from a t3 database.
#' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits.
#' @param countries (character) ISO code on 3 letters related to one or more countries.
#' @param oceans (integer) Ocean(s) related to data coded on 1 digit.
#' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a t3 database.
#' @param log_file (logical) Initiation or not for log file creation. By default FALSE (no).
#' @param log_path (character) Path of the log file directory. By default NULL.
#' @param log_name (character) Name of the log file. By default "data_model_initialisation".
#' @param trips_selected (character) Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference, countries and sample types). By default NULL.
#' @return The function return two R6 reference object, one class "object_model_data" and the second class "object_full_trips".
#' @export
data_model_initialisation <- function(periode_reference,
                                      countries,
                                      oceans,
                                      db_con,
                                      sample_type,
                                      log_file = FALSE,
                                      log_path = NULL,
                                      log_name = "data_model_initialisation",
                                      trips_selected = NULL) {
  # log file initialisation ----
  t3::initiate_log_file(log_file = log_file,
                        log_path = log_path,
                        log_name = log_name)
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " - Start function data model initialisation.\n",
      "[periode reference: ",
      paste0(periode_reference, collapse = ", "),
      "; countries: ",
      paste0(countries, collapse = ", "),
      "; oceans: ",
      paste0(oceans, collapse = ", "),
      "; sample type: ",
      sample_type,
      ifelse(test = is.null(trips_selected),
             yes = "]\n",
             no = paste0(", trips selected: ",
                         paste0(trips_selected, collapse = ", "),
                         "]\n")),
      sep = "")
  # initialisation object for data's object model ----
  object_model_data <- t3:::object_model_data$new()
  # model creation: object trips creation ----
  object_model_data$trips_object_creation(periode_reference = periode_reference,
                                          countries = countries,
                                          oceans = oceans,
                                          trips_selected = trips_selected,
                                          db_con = t3_con)
  # model creation: object activites creation ----
  object_model_data$activities_object_creation(periode_reference = periode_reference,
                                               countries = countries,
                                               oceans = oceans,
                                               trips_selected = trips_selected,
                                               db_con = t3_con)
  # model creation: object elementarycatches creation ----
  object_model_data$elementarycatches_object_creation(periode_reference = periode_reference,
                                                      countries = countries,
                                                      oceans = oceans,
                                                      trips_selected = trips_selected,
                                                      db_con = t3_con)
  # model creation: object elementarylandings creation ----
  object_model_data$elementarylandings_object_creation(periode_reference = periode_reference,
                                                       countries = countries,
                                                       oceans = oceans,
                                                       trips_selected = trips_selected,
                                                       db_con = t3_con)
  # model creation: object wells creation ----
  object_model_data$wells_object_creation(periode_reference = periode_reference,
                                          countries = countries,
                                          oceans = oceans,
                                          sample_type = sample_type,
                                          trips_selected = trips_selected,
                                          db_con = t3_con)
  # model creation: set durations data ----
  object_model_data$setdurations_data(periode_reference = periode_reference,
                                     countries = countries,
                                     db_con = t3_con)
  # model creation: length steps data ----
  object_model_data$lengthsteps_data(db_con = t3_con)
  # model creation: sample sets data ----
  object_model_data$samplesets_data(periode_reference = periode_reference,
                                    countries = countries,
                                    oceans = oceans,
                                    trips_selected = trips_selected,
                                    db_con = t3_con)
  # model creation: length weight relationships data ----
  object_model_data$lengthweightrelationships_data(db_con = t3_con)
  # model creation: initialisation object for full trips class ----
  object_full_trips <- t3:::full_trips$new()
  # model creation: object full_trip creation ----
  object_full_trips$create_full_trips(object_trips = object_model_data$.__enclos_env__$private$trips)
  # model creation: filter on reference year ----
  object_full_trips$filter_by_periode(periode_reference = periode_reference)
  # model creation: add activities to trips selected ----
  object_full_trips$add_activities(object_activities = object_model_data$.__enclos_env__$private$activities)
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
  assign(x = "object_model_data",
         value = object_model_data,
         envir = .GlobalEnv)
  assign(x = "object_full_trips",
         value = object_full_trips,
         envir = .GlobalEnv)
}
