#' @name t3_process
#' @title T3 process compilation
#' @description Run the t3 process, with the possibility to run all the process or stop.
#' @param process (character) Specify here if you want to run the whole process or just a part of it. By default "all". Check detail section below for more information.
#' @param log_file (logical) Initiation or not for log file creation. By default FALSE (no).
#' @param log_path (character) Path of the log file directory. By default NULL.
#' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits.
#' @param countries (character) ISO code on 3 letters related to one or more countries.
#' @param oceans (integer) Ocean(s) related to data coded on 1 digit.
#' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a t3 database.
#' @param trips_selected (character) Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference, countries and sample types). By default NULL.
#' @param species_rf1 (integer) Specie(s) code(s) used for the rf1 process.
#' @param set_duration_ref (data frame) Data and parameters for set duration calculation (by year, country, ocean and school type).
#' @return The function a R6 reference object of class "object_full_trips".
#' @param length_step (data.frame) Data frame object with length ratio between ld1 and lf class.
#' @param sample_set (data.frame) Data frame object with weighted weigth of each set sampled.
#' @param length_weight_relationship_data (data.frame) Data frame object with parameters for length weight relationship.
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
                       log_file = FALSE,
                       log_path = NULL,
                       db_con,
                       periode_reference,
                       countries,
                       oceans,
                       sample_type,
                       species_rf1,
                       set_duration_ref,
                       length_step,
                       sample_set,
                       length_weight_relationship_data,
                       trips_selected = NULL) {
  if (paste(class(process), collapse = " ") != "character"
      || length(process) != 1
      || ! process %in% c("all", "level1", "level2", "until_level2")) {
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
    t3::data_model_initialisation(periode_reference = periode_reference,
                                  countries = countries,
                                  oceans = oceans,
                                  db_con = db_con,
                                  sample_type = sample_type,
                                  trips_selected = NULL,
                                  log_file = log_file,
                                  log_path = log_path,
                                  log_name = "data_model_initialisation")
    if (process %in% c("all", "level1", "until_level2")) {
      t3::t3_level1(species_rf1 = species_rf1,
                    set_duration_ref = set_duration_ref,
                    log_file = log_file,
                    log_path = log_path,
                    log_name = "t3_level1")
    }
    if (process %in% c("all", "level2", "until_level2")) {
      t3::t3_level2(length_step,
                    sample_set,
                    length_weight_relationship_data,
                    log_file = log_file,
                    log_path = log_path,
                    log_name = "t3_level2")
    }
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Successful process of the Tropical Tuna Treatment.\n",
        sep = "")
  }
}
