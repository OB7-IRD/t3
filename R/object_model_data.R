#' @title R6 class object_model_data
#' @name object_model_data
#' @description Create R6 reference object class object_model_data
#' @importFrom R6 R6Class
#' @importFrom codama r_type_checking
#' @importFrom dplyr  first last tibble mutate case_when row_number filter
#' @importFrom DBI SQL sqlInterpolate dbGetQuery
object_model_data <- R6::R6Class(classname = "object_model_data",
                                 public = list(
                                   #' @description Creation of an R6 reference object class trips which contains one or more R6 reference object class trip.
                                   #' @param data_source Object of class {\link[base]{character}} expected. By default "observe_database". Identification of data source. You can switch between "observe_database", "avdth_database", "csv_file" (with separator ";" and decimal ","), "rdata_file" or "envir" (for an object in the R environment).
                                   #' @param database_connection Database connection, list of one or more R object(s) expected. By default NULL.
                                   #' Mandatory argument for data source "observe_database" ("PqConnection" R object), corresponding to the second element of the object returned by \href{https://ob7-ird.github.io/furdeb/reference/postgresql_dbconnection.html}{`furdeb::postgresql_dbconnection()`}.
                                   #' Or mandatory argument for data source"avdth_database" ("JDBCConnection" R object) corresponding to the second element of the object returned by \href{https://ob7-ird.github.io/furdeb/reference/access_dbconnection.html}{`furdeb::access_dbconnection()`}.
                                   #' For data source "observe_database", a list of "PqConnection" R objects can be specified to query data from different observe databases.
                                   #' For example, a list of two database connection arguments for "observe_main" and "observe_acquisition" can be specified to simultaneously import and process recent data from acquisition database, which has not yet been imported into the main database, and older data from the main database.
                                   #' @param years_period Object of class {\link[base]{integer}} expected. By default NULL. Year(s) of the reference time period coded on 4 digits. Mandatory for data source "observe_database" and "avdth_database".
                                   #' @param flag_codes Object of class {\link[base]{character}} expected. By default NULL. Three letters country(ies) ISO 3 code(s) related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param ocean_codes Object of class {\link[base]{integer}} expected. By default NULL. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param vessel_type_codes Object of class {\link[base]{integer}} expected. By default NULL. Vessel type(s) related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param trip_ids Object of class {\link[base]{character}} expected. By default NULL. Additional parameter only used with data source "observe_database". Use trip(s) identification(s) for selected trip(s) kept in the query. This argument overrides all others arguments like "years_period", "flag_codes" or "ocean_codes".
                                   #' @param data_path Object of class {\link[base]{character}} expected. By default NULL. Path of the data csv/RData file.
                                   #' @param envir Object of class {\link[base]{character}} expected. By default the first environment where data are found will be used. Specify an environment to look in for data source "envir".
                                   trips_object_creation = function(data_source = "observe_database",
                                                                    database_connection = NULL,
                                                                    years_period = NULL,
                                                                    flag_codes = NULL,
                                                                    ocean_codes = NULL,
                                                                    vessel_type_codes = NULL,
                                                                    trip_ids = NULL,
                                                                    data_path = NULL,
                                                                    envir = NULL) {
                                     # 1 - Arguments verifications ----
                                     if (data_source %in% c("observe_database",
                                                            "avdth_database")) {
                                       if(is.null(years_period)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"years_period\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(flag_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"flag_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(ocean_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"ocean_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(vessel_type_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"vessel_type_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       codama::r_type_checking(r_object = years_period,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = flag_codes,
                                                               type = "character")
                                       codama::r_type_checking(r_object = ocean_codes,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = vessel_type_codes,
                                                               type = "integer")
                                     } else if (data_source %in% c("csv_file",
                                                                   "rdata_file")) {
                                       codama::r_type_checking(r_object = data_path,
                                                               type = "character",
                                                               length = 1L)
                                     } else if (data_source != "envir") {
                                       stop(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Invalid \"data_source\" argument. Check function documention through ?object_model_data for more details.")
                                     }
                                     # 2 - Process for observe database ----
                                     if (data_source == "observe_database") {
                                       # specific argument verification for multiple query
                                       if (length(x = database_connection) > 1) {
                                         if( any(unlist(lapply(database_connection, class)) !=  "PqConnection")) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - Invalid \"database_connection\" argument. List of objects of class \"PqConnection\" expected for multiple observe databases query.")


                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from ", length(database_connection)  ," observe databases.\n", sep="")
                                       } else {
                                         # specific argument verification for simple query
                                         if (paste0(class(x = database_connection),
                                                    collapse = " ") != "PqConnection") {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - Invalid \"database_connection\" argument. Class \"PqConnection\" expected.")
                                         }
                                         # process beginning
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from an observe database.\n", sep="")
                                       }
                                       for(i in  1:length(database_connection)){
                                         if(length(database_connection)>1){
                                           database_conn <- database_connection[[i]]
                                         } else {
                                           database_conn <- database_connection
                                         }
                                         if (! is.null(x = trip_ids)) {
                                           codama::r_type_checking(r_object = trip_ids,
                                                                   type = "character")
                                           trip_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                  "observe",
                                                                                                  "observe_trips_selected_trips.sql",
                                                                                                  package = "t3")),
                                                                      collapse = "\n"))

                                           trip_sql_final <- DBI::sqlInterpolate(conn = database_conn,
                                                                                 sql = trip_sql,
                                                                                 trip_ids = DBI::SQL(paste0("'",
                                                                                                            paste0(trip_ids,
                                                                                                                   collapse = "', '"),
                                                                                                            "'")))

                                         } else {
                                           trip_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                  "observe",
                                                                                                  "observe_trips.sql",
                                                                                                  package = "t3")),
                                                                      collapse = "\n"))
                                           trip_sql_final <- DBI::sqlInterpolate(conn = database_conn,
                                                                                 sql = trip_sql,
                                                                                 begin_time_period = paste0((dplyr::first(years_period,
                                                                                                                          order_by = years_period) - 1),
                                                                                                            "-10-01"),
                                                                                 end_time_period = paste0((dplyr::last(years_period,
                                                                                                                       order_by = years_period) + 1),
                                                                                                          "-03-31"),
                                                                                 flag_codes = DBI::SQL(paste0("'",
                                                                                                              paste0(flag_codes,
                                                                                                                     collapse = "', '"),
                                                                                                              "'")),
                                                                                 ocean_codes = DBI::SQL(paste0("'",
                                                                                                               paste0(ocean_codes,
                                                                                                                      collapse = "', '"),
                                                                                                               "'")),
                                                                                 vessel_type_codes = DBI::SQL(paste0("'",
                                                                                                                     paste0(vessel_type_codes,
                                                                                                                            collapse = "', '"),
                                                                                                                     "'")))
                                         }
                                         cat("[",
                                             trip_sql_final,
                                             "]\n",
                                             sep = "")
                                         if(i >1){
                                           trip_data <- dplyr::full_join(trip_data, dplyr::tibble(DBI::dbGetQuery(conn = database_conn,
                                                                                                                  statement = trip_sql_final)))
                                         } else {
                                           trip_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_conn,
                                                                                      statement = trip_sql_final))
                                         }
                                       }
                                       if  (nrow(x = trip_data) == 0){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and parameters associated.")
                                       } else if (sum(duplicated(trip_data)) != 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " -  Duplicated imported trips, check the databases for the trip(s) : ",
                                              trip_data$trip_id[duplicated(trip_data)],
                                              ". A trip must not be recorded in more than one database.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful trip(s) data importation from observe database(s).\n", sep="")
                                       }
                                     } else if (data_source == "avdth_database") {
                                       # 3 - Process for AVDTH database ----
                                       # specific argument verification
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "JDBCConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument. Class \"JDBCConnection\" expected.")
                                       }
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start trip(s) data importation from an AVDTH database.\n")
                                       trip_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                              "avdth",
                                                                                              "avdth_trips.sql",
                                                                                              package = "t3")),
                                                                  collapse = "\n"))
                                       trip_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                             sql = trip_sql,
                                                                             begin_time_period  = DBI::SQL(paste0("#",
                                                                                                                  (dplyr::first(years_period,
                                                                                                                                order_by = years_period) - 1),
                                                                                                                  "-10-01#")),
                                                                             end_time_period = DBI::SQL(paste0("#",
                                                                                                               (dplyr::last(years_period,
                                                                                                                            order_by = years_period) + 1),
                                                                                                               "-03-31#")),
                                                                             flag_codes = DBI::SQL(paste0("'",
                                                                                                          paste0(flag_codes,
                                                                                                                 collapse = "', '"),
                                                                                                          "'")),
                                                                             ocean_codes = DBI::SQL(paste0(paste0(ocean_codes,
                                                                                                                  collapse = ", "))),
                                                                             vessel_type_codes = DBI::SQL(paste0(paste0(vessel_type_codes,
                                                                                                                        collapse = ", "))))
                                       cat("[",
                                           trip_sql_final,
                                           "]\n",
                                           sep = "")
                                       trip_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_connection,
                                                                                  statement = trip_sql_final)) %>%
                                         dplyr::mutate(trip_id = as.character(x = trip_id),
                                                       flag_code = as.character(x = flag_code),
                                                       departure_date = as.character(x = departure_date),
                                                       trip_end_date = as.character(x = trip_end_date),
                                                       logbook_availability_code = as.integer(x = logbook_availability_code),
                                                       landing_well_content_code = as.integer(x = landing_well_content_code),
                                                       vessel_code = as.integer(x = vessel_code),
                                                       vessel_type_code = as.integer(x = vessel_type_code))
                                       if (nrow(x = trip_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and query's parameters.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful trip(s) data importation from avdht database.\n", sep="")
                                       }
                                     } else if (data_source == "csv_file") {
                                       # 4 - Process for csv file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start trip(s) data importation from csv file.\n", sep="")
                                       trip_data <- dplyr::tibble(read.csv2(file = data_path,
                                                                            stringsAsFactors = FALSE))
                                       if (nrow(x = trip_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful trip(s) data importation from csv file.\n", sep="")
                                       }
                                     } else if (data_source == "rdata_file") {
                                       # 5 - Process for rdata file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start trip(s) data importation from RData file.\n", sep="")
                                       load(file = data_path,
                                            envir = tmp_envir <- new.env())
                                       if (exists(x = "trips",
                                                  envir = tmp_envir)) {
                                         trip_data <- dplyr::tibble(get(x = "trips",
                                                                        envir = tmp_envir))
                                         if (paste0(class(x = trip_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = trip_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid RData, no R object named \"trips\" available in the R environment provided.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful trip(s) data importation from RData file.\n", sep="")
                                     } else if (data_source == "envir") {
                                       # 6 - R environment source ----
                                       # specific argument verification
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "trip")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       # process beginning
                                       if (exists(x = "trip",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from R environment.\n")
                                         trip_data <- dplyr::tibble(get(x = "trip",
                                                                        envir = environment_name))
                                         if (paste0(class(x = trip_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = trip_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"trip\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful trip(s) data importation R environment.\n", sep="")
                                     }
                                     # 7 - Common data design ----
                                     trip_data <- unclass(x = trip_data)
                                     object_trips <- object_r6(class_name = "trips")
                                     T1 <- Sys.time()

                                     object_trips$add(lapply(cli::cli_progress_along(seq_len(length.out = length(x = trip_data[[1]])),
                                                                                     clear = getOption("cli.progress_clear", FALSE),
                                                                                     format = paste0(
                                                                                       format(x = Sys.time(),
                                                                                              "%Y-%m-%d %H:%M:%S"),
                                                                                       " - Importation of trip element ",
                                                                                       "[{cli::pb_current}/{cli::pb_total}], ",
                                                                                       "[{cli::pb_bar}{cli::pb_percent}",
                                                                                       "], Time remaining:{cli::pb_eta}"),,
                                                                                     total = length(x = trip_data[[1]])),
                                                             FUN = function(trip_id) {
                                                               Sys.sleep(3/100)
                                                               trip <- trip$new(trip_id = trip_data$trip_id[trip_id],
                                                                                flag_code = trip_data$flag_code[trip_id],
                                                                                departure_date = trip_data$departure_date[trip_id],
                                                                                trip_end_date = trip_data$trip_end_date[trip_id],
                                                                                logbook_availability_code = trip_data$logbook_availability_code[trip_id],
                                                                                landing_well_content_code = trip_data$landing_well_content_code[trip_id],
                                                                                vessel_code = trip_data$vessel_code[trip_id],
                                                                                vessel_type_code = trip_data$vessel_type_code[trip_id])
                                                               return(trip)
                                                             }))
                                     private$trips <- object_trips
                                     T2 <- Sys.time()
                                     elapsed_time <- format(round(T2-T1,2), units="secs")
                                     cli::cli_alert_info(paste0(format(x = Sys.time(),
                                                                  format = "%Y-%m-%d %H:%M:%S "),
                                                            cli::col_green(cli::symbol$tick)," Successful importation of ",
                                                            length(x = trip_data[[1]]),
                                                           " trips, in ", elapsed_time, "."))
                                     capture.output(gc(full=TRUE), file="NUL")
                                   },
                                   #' @description Creation of a R6 reference object of class activities which contain one or more R6 reference object of class activity, including related elementarycatch(es).
                                   #' @param data_source  Object of class {\link[base]{character}} expected. By default "observe_database". Identification of data source. You can switch between "observe_database", "avdth_database", "csv_file" (with separator ";" and decimal ","), "rdata_file" or "envir" (for an object in the R environment).
                                   #' @param database_connection Database connection, list of one or more R object(s) expected. By default NULL.
                                   #' Mandatory argument for data source "observe_database" ("PqConnection" R object), corresponding to the second element of the object returned by \href{https://ob7-ird.github.io/furdeb/reference/postgresql_dbconnection.html}{`furdeb::postgresql_dbconnection()`}.
                                   #' Or mandatory argument for data source"avdth_database" ("JDBCConnection" R object) corresponding to the second element of the object returned by \href{https://ob7-ird.github.io/furdeb/reference/access_dbconnection.html}{`furdeb::access_dbconnection()`}.
                                   #' For data source "observe_database", a list of "PqConnection" R objects can be specified to query data from different observe databases.
                                   #' For example, a list of two database connection arguments for "observe_main" and "observe_acquisition" can be specified to simultaneously import and process recent data from acquisition database, which has not yet been imported into the main database, and older data from the main database.
                                   #' @param years_period Object of class {\link[base]{integer}} expected. By default NULL. Year(s) of the reference time period coded on 4 digits. Mandatory for data source "observe_database" and "avdth_database".
                                   #' @param flag_codes Object of class {\link[base]{character}} expected. By default NULL. Country(ies) code related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param ocean_codes Object of class {\link[base]{integer}} expected. By default NULL. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param vessel_type_codes Object of class {\link[base]{integer}} expected. By default NULL. Vessel type(s) related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param species_fate_codes Object of class {\link[base]{integer}} expected. By default NULL. Specie fate(s) related to elementarycatch(es) data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param trip_ids Object of class {\link[base]{character}} expected. By default NULL. Additional parameter only used with data source "observe_database". Use trip(s) identification(s) for selected trip(s) kept in the query. This argument overrides all others arguments like "years_period", "country" or "ocean".
                                   #' @param data_path Object of class {\link[base]{character}} expected. By default NULL. Path of the data csv/RData file.
                                   #' @param envir Object of class {\link[base]{character}} expected. By default the first environment where data are found will be used. Specify an environment to look in for data source "envir".
                                   activities_object_creation = function(data_source = "observe_database",
                                                                         database_connection = NULL,
                                                                         years_period = NULL,
                                                                         flag_codes = NULL,
                                                                         ocean_codes = NULL,
                                                                         vessel_type_codes = NULL,
                                                                         species_fate_codes = NULL,
                                                                         trip_ids = NULL,
                                                                         data_path = NULL,
                                                                         envir = NULL) {
                                     # 1 - Arguments verifications ----
                                     if (data_source %in% c("observe_database",
                                                            "avdth_database")) {
                                       if(is.null(years_period)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"years_period\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(flag_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"flag_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(ocean_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"ocean_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(vessel_type_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"vessel_type_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(species_fate_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"species_fate_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       codama::r_type_checking(r_object = years_period,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = flag_codes,
                                                               type = "character")
                                       codama::r_type_checking(r_object = ocean_codes,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = vessel_type_codes,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = species_fate_codes,
                                                               type = "integer")
                                     } else if (data_source %in% c("csv_file",
                                                                   "rdata_file")) {
                                       codama::r_type_checking(r_object = data_path,
                                                               type = "character",
                                                               length = 1L)
                                     } else if (data_source != "envir") {
                                       stop(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Invalid \"data_source\" argument. Check function documention through ?object_model_data for more details.")
                                     }
                                     # 2 - Process for observe database ----
                                     if (data_source == "observe_database") {
                                       # specific argument verification for multiple query
                                       if (length(x = database_connection) > 1) {
                                         if( any(unlist(lapply(database_connection, class)) !=  "PqConnection")) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - Invalid \"database_connection\" argument. List of objects of class \"PqConnection\" expected for multiple observe databases query.")


                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start activity(ies) and elementary catch(es) data importation from ",
                                             length(database_connection)  ," observe databases.\n", sep="")
                                       } else {
                                         # specific argument verification for simple query
                                         if (paste0(class(x = database_connection),
                                                    collapse = " ") != "PqConnection") {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - Invalid \"database_connection\" argument. Class \"PqConnection\" expected.")
                                         }
                                         # process beginning
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start activity(ies) and elementary catch(es) data importation from an observe database.\n",
                                             sep="")
                                       }
                                       for(i in  1:length(database_connection)){
                                         if(length(database_connection)>1){
                                           database_conn <- database_connection[[i]]
                                         } else {
                                           database_conn <- database_connection
                                         }
                                         if (! is.null(x = trip_ids)) {
                                           codama::r_type_checking(r_object = trip_ids,
                                                                   type = "character")
                                           activity_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                      "observe",
                                                                                                      "observe_activities_selected_trips.sql",
                                                                                                      package = "t3")),
                                                                          collapse = "\n"))
                                           activity_sql_final <- DBI::sqlInterpolate(conn = database_conn,
                                                                                     sql = activity_sql,
                                                                                     trip_ids = DBI::SQL(paste0("'",
                                                                                                                paste0(trip_ids,
                                                                                                                       collapse = "', '"),
                                                                                                                "'")))
                                           elementarycatch_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                             "observe",
                                                                                                             "observe_elementarycatches_selected_trips.sql",
                                                                                                             package = "t3")),
                                                                                 collapse = "\n"))
                                           elementarycatch_sql_final <- DBI::sqlInterpolate(conn = database_conn,
                                                                                            sql = elementarycatch_sql,
                                                                                            trip_ids = DBI::SQL(paste0("'",
                                                                                                                       paste0(trip_ids,
                                                                                                                              collapse = "', '"),
                                                                                                                       "'")))
                                         } else {
                                           activity_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                      "observe",
                                                                                                      "observe_activities.sql",
                                                                                                      package = "t3")),
                                                                          collapse = "\n"))
                                           activity_sql_final <- DBI::sqlInterpolate(conn = database_conn,
                                                                                     sql = activity_sql,
                                                                                     begin_time_period = paste0((dplyr::first(years_period,
                                                                                                                              order_by = years_period) - 1),
                                                                                                                "-10-01"),
                                                                                     end_time_period = paste0((dplyr::last(years_period,
                                                                                                                           order_by = years_period) + 1),
                                                                                                              "-03-31"),
                                                                                     flag_codes = DBI::SQL(paste0("'",
                                                                                                                  paste0(flag_codes,
                                                                                                                         collapse = "', '"),
                                                                                                                  "'")),
                                                                                     ocean_codes = DBI::SQL(paste0("'",
                                                                                                                   paste0(ocean_codes,
                                                                                                                          collapse = "', '"),
                                                                                                                   "'")),
                                                                                     vessel_type_codes = DBI::SQL(paste0("'",
                                                                                                                         paste0(vessel_type_codes,
                                                                                                                                collapse = "', '"),
                                                                                                                         "'")))
                                           elementarycatch_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                             "observe",
                                                                                                             "observe_elementarycatches.sql",
                                                                                                             package = "t3")),
                                                                                 collapse = "\n"))
                                           elementarycatch_sql_final <- DBI::sqlInterpolate(conn = database_conn,
                                                                                            sql = elementarycatch_sql,
                                                                                            begin_time_period = paste0((dplyr::first(years_period,
                                                                                                                                     order_by = years_period) - 1),
                                                                                                                       "-10-01"),
                                                                                            end_time_period = paste0((dplyr::last(years_period,
                                                                                                                                  order_by = years_period) + 1),
                                                                                                                     "-03-31"),
                                                                                            flag_codes = DBI::SQL(paste0("'",
                                                                                                                         paste0(flag_codes,
                                                                                                                                collapse = "', '"),
                                                                                                                         "'")),
                                                                                            ocean_codes = DBI::SQL(paste0("'",
                                                                                                                          paste0(ocean_codes,
                                                                                                                                 collapse = "', '"),
                                                                                                                          "'")),
                                                                                            vessel_type_codes = DBI::SQL(paste0("'",
                                                                                                                                paste0(vessel_type_codes,
                                                                                                                                       collapse = "', '"),
                                                                                                                                "'")),
                                                                                            species_fate_codes = DBI::SQL(paste0("'",
                                                                                                                                 paste0(species_fate_codes,
                                                                                                                                        collapse = "', '"),
                                                                                                                                 "'")))

                                         }
                                         cat("[",
                                             activity_sql_final,
                                             "]\n")
                                         cat("[",
                                             elementarycatch_sql_final,
                                             "]\n")
                                         if(i >1){
                                           activity_data <- dplyr::full_join(activity_data,
                                                                             dplyr::tibble(DBI::dbGetQuery(conn = database_conn,
                                                                                                           statement = activity_sql_final)))
                                           elementarycatch_data <- dplyr::full_join(elementarycatch_data,
                                                                                    dplyr::tibble(DBI::dbGetQuery(conn = database_conn,
                                                                                                                  statement = elementarycatch_sql_final)))
                                         } else {
                                           activity_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_conn,
                                                                                          statement = activity_sql_final))
                                           elementarycatch_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_conn,
                                                                                                 statement = elementarycatch_sql_final))
                                         }
                                         # Format data to to obtain a single line for each activity_id
                                         # Gather on the same row FloatingObjects and objectoperation_codes associated to the same activity
                                         activity_data <- activity_data %>%
                                           dplyr::group_by(activity_id) %>%
                                           dplyr::mutate(objectoperation_id=paste0(objectoperation_id, collapse = ", "),
                                                         objectoperation_code=paste0(objectoperation_code, collapse = ", "),
                                                         objectoperation_label=paste0(objectoperation_label, collapse = ", ")) %>%
                                           dplyr::distinct()
                                       }
                                       if (nrow(x = elementarycatch_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No elementary catch(es) data imported, check the query and parameters associated.")
                                       } else if (nrow(x = elementarycatch_data) != length(unique(elementarycatch_data$elementarycatch_id))) {
                                         elementarycatch_id <- unique(elementarycatch_data$elementarycatch_id[duplicated(elementarycatch_data)])
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Duplicated elementarycatch(es) topiaid in data imported, check the query and query's parameters.\n",
                                              paste0("[elementarycath(es): ",
                                                     elementarycatch_id, collapse="];\n"),
                                              "].")
                                       } else if (nrow(x = activity_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No activity(ies) data imported, check the query and parameters associated.")
                                       } else if (nrow(x = activity_data) != length(unique(activity_data$activity_id))) {
                                         activity_id <- unique(activity_data$activity_id[duplicated(activity_data)])
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Duplicated activity(ies) topiaid in data imported, check the query and query's parameters.\n",
                                              paste0("[activity: ",
                                                     activity_id, collapse="];\n"),
                                              "].")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful activity(ies) and elementary catch(es) data importation from observe database(s).\n",
                                             sep="")
                                       }
                                     } else if (data_source == "avdth_database") {
                                       # 3 - Process for AVDTH database ----
                                       # specific argument verification
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "JDBCConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument. Class \"JDBCConnection\" expected.")
                                       }
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start activity(ies) and elementary catch(es) data importation from an AVDTH database.\n",
                                           sep="")
                                       activity_sql <- paste(readLines(con = system.file("sql",
                                                                                         "avdth",
                                                                                         "avdth_activities.sql",
                                                                                         package = "t3")),
                                                             collapse = "\n")
                                       activity_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                 sql = activity_sql,
                                                                                 begin_time_period  = DBI::SQL(paste0("#",
                                                                                                                      (dplyr::first(years_period,
                                                                                                                                    order_by = years_period) - 1),
                                                                                                                      "-10-01#")),
                                                                                 end_time_period = DBI::SQL(paste0("#",
                                                                                                                   (dplyr::last(years_period,
                                                                                                                                order_by = years_period) + 1),
                                                                                                                   "-03-31#")),
                                                                                 flag_codes = DBI::SQL(paste0("'",
                                                                                                              paste0(flag_codes,
                                                                                                                     collapse = "', '"),
                                                                                                              "'")),
                                                                                 ocean_codes = DBI::SQL(paste0(paste0(ocean_codes,
                                                                                                                      collapse = ", "))),
                                                                                 vessel_type_codes = DBI::SQL(paste0(paste0(vessel_type_codes,
                                                                                                                            collapse = ", "))))
                                       cat("[",
                                           activity_sql_final,
                                           "]\n")
                                       elementarycatch_sql <- DBI::SQL(x = paste(readLines(con = system.file("sql",
                                                                                                             "avdth",
                                                                                                             "avdth_elementarycatches.sql",
                                                                                                             package = "t3")),
                                                                                 collapse = "\n"))
                                       elementarycatch_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                        sql = elementarycatch_sql,
                                                                                        begin_time_period  = DBI::SQL(paste0("#",
                                                                                                                             (dplyr::first(years_period,
                                                                                                                                           order_by = years_period) - 1),
                                                                                                                             "-10-01#")),
                                                                                        end_time_period = DBI::SQL(paste0("#",
                                                                                                                          (dplyr::last(years_period,
                                                                                                                                       order_by = years_period) + 1),
                                                                                                                          "-03-31#")),
                                                                                        flag_codes = DBI::SQL(paste0("'",
                                                                                                                     paste0(flag_codes,
                                                                                                                            collapse = "', '"),
                                                                                                                     "'")),
                                                                                        ocean_codes = DBI::SQL(paste0(paste0(ocean_codes,
                                                                                                                             collapse = ", "))),
                                                                                        vessel_type_codes = DBI::SQL(paste0(paste0(vessel_type_codes,
                                                                                                                                   collapse = ", "))))
                                       cat("[",
                                           elementarycatch_sql_final,
                                           "]\n")
                                       elementarycatch_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_connection,
                                                                                             statement = elementarycatch_sql_final)) %>%
                                         dplyr::mutate(species_fate_code = dplyr::case_when(
                                           (species_code <= 709) | (species_code >= 859 & species_code <= 862) ~ "6",
                                           (species_code >= 801 & species_code <= 858) | (species_code >= 863 & species_code <= 899) ~ "11",
                                           TRUE ~ "error"
                                         ))
                                       if (any(unique(x = elementarycatch_data$species_fate_code) == "error")) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - At least one species code of AVDTH referential don't have any relation with the referential \"species fate code\".\n",
                                              "Check the following code(s): ",
                                              paste0(unique(x = dplyr::filter(.data = elementarycatch_data,
                                                                              species_fate_code == "error")$species_code),
                                                     collapse = ", "))
                                       }
                                       elementarycatch_data <- elementarycatch_data %>%
                                         dplyr::filter(species_fate_code %in% !!species_fate_codes) %>%
                                         dplyr::mutate(activity_id = as.character(x = activity_id),
                                                       elementarycatch_id = as.character(x = elementarycatch_id),
                                                       ocean_code = as.integer(x = ocean_code),
                                                       school_type_code = as.integer(x = school_type_code),
                                                       weight_category_code = as.character(x = weight_category_code),
                                                       weight_category_label = as.character(x = weight_category_label),
                                                       weight_category_min = NA_real_,
                                                       weight_category_max = NA_real_,
                                                       species_fao_code = as.character(x = species_fao_code),
                                                       species_fate_code = as.integer(x = species_fate_code),
                                                       catch_weight = as.numeric(x = catch_weight),
                                                       catch_count=NA_integer_)

                                       activity_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_connection,
                                                                                      statement = activity_sql_final)) %>%
                                         dplyr::mutate(trip_id = as.character(x = trip_id),
                                                       activity_id = as.character(x = activity_id),
                                                       ocean_code = as.integer(x = ocean_code),
                                                       activity_date = as.character(x = activity_date),
                                                       activity_number = as.integer(x = activity_number),
                                                       activity_longitude = as.numeric(x = activity_longitude),
                                                       activity_latitude = as.numeric(x = activity_latitude),
                                                       set_count = as.integer(x = set_count),
                                                       set_success_status_code = as.integer(x = dplyr::case_when(
                                                         activity_code == 0 ~ 0,
                                                         activity_code == 1 ~ 1,
                                                         activity_code == 2 ~ 2,
                                                         TRUE ~ NA_integer_
                                                       )),
                                                       set_success_status_label = dplyr::case_when(
                                                         activity_code == 0 ~ "Null",
                                                         activity_code == 1 ~ "Positive",
                                                         activity_code == 2 ~ "Unknown success status",
                                                         TRUE ~ NA_character_
                                                       ),
                                                       school_type_code = dplyr::case_when(
                                                         school_type_code == 1 ~  as.integer(1),
                                                         school_type_code == 2 ~  as.integer(2),
                                                         # Unknown code=0 in observe database
                                                         school_type_code == 3 ~  as.integer(0),
                                                         TRUE ~ NA_integer_
                                                       ),
                                                       activity_code = as.integer(x = activity_code),
                                                       activity_label = as.character(x = activity_label),
                                                       objectoperation_code = NA_character_,
                                                       objectoperation_label = NA_character_,
                                                       objectoperation_id = NA_character_,
                                                       time_at_sea = as.integer(x = time_at_sea))
                                       if (nrow(x = elementarycatch_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and query's parameters.")
                                       ##### Warning dupliacted elementarycatch topiaid in AVDTH
                                       # } else if (nrow(x = elementarycatch_data) != length(unique(elementarycatch_data$elementarycatch_id))) {
                                       #   elementarycatch_id <- unique(elementarycatch_data$elementarycatch_id[duplicated(elementarycatch_data)])
                                       #   stop(format(x = Sys.time(),
                                       #               format = "%Y-%m-%d %H:%M:%S"),
                                       #        " - Duplicated elementarycatch(es) topiaid in data imported, check the query and query's parameters.\n",
                                       #        paste0("[activity: ",
                                       #               elementarycatch_id, collapse="];\n"),
                                       #        "].")
                                       } else if (nrow(x = activity_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and query's parameters.")
                                       } else if (nrow(x = activity_data) != length(unique(activity_data$activity_id))) {
                                         activity_id <- unique(activity_data$activity_id[duplicated(activity_data)])
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Duplicated activity(ies) topiaid in data imported, check the query and query's parameters.\n",
                                              paste0("[activity: ",
                                                     activity_id, collapse="];\n"),
                                              "].")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful activity(ies) and elementary catch(es) data importation from an AVDTH database.\n",
                                             sep="")
                                       }
                                     } else if (data_source == "csv_file") {
                                       # 4 - Process for csv file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start activity(ies) and elementary catch(es) data importation from csv file.\n",
                                           sep="")
                                       activity_data <- dplyr::tibble(read.csv2(file = data_path,
                                                                                stringsAsFactors = FALSE))
                                       elementarycatch_data <- dplyr::tibble(read.csv2(file = data_path,
                                                                                       stringsAsFactors = FALSE))
                                       if (nrow(x = elementarycatch_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No elementary catch(es) data imported, check your csv file.")
                                       } else if (nrow(x = activity_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No activity(ies) data imported, check your csv file.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful activity(ies) and elementary catch(es) data importation from csv file.\n",
                                             sep="")
                                       }

                                     } else if (data_source == "rdata_file") {
                                       # 5 - Process for rdata file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start activity(ies) and elementary catch(es) data importation from RData file.\n",
                                           sep="")
                                       load(file = data_path,
                                            envir = tmp_envir <- new.env())
                                       if (exists(x = "elementarycatches",
                                                  envir = tmp_envir)) {
                                         elementarycatch_data <- dplyr::tibble(get(x = "elementarycatches",
                                                                                   envir = tmp_envir))
                                         if (paste0(class(x = elementarycatch_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = elementarycatch_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid RData, no R object named \"elementarycatches\" available in the R environment provided.")
                                       }
                                       if (exists(x = "activities",
                                                  envir = tmp_envir)) {
                                         activity_data <- dplyr::tibble(get(x = "activities",
                                                                            envir = tmp_envir))
                                         if (paste0(class(x = activity_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = activity_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         } else if (nrow(x = activity_data) != length(unique(activity_data$activity_id))) {
                                           activity_id <- unique(activity_data$activity_id[duplicated(activity_data)])
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - Duplicated activity(ies) topiaid in data imported, check the query and query's parameters.\n",
                                                paste0("[activity: ",
                                                       activity_id, collapse="];\n"),
                                                "].")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid RData, no R object named \"activities\" available in the R environment provided.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful activity(ies) and elementary catch(es) data importation from RData file.\n")
                                     } else if (data_source == "envir") {
                                       # 6 - R environment source ----
                                       # specific argument verification
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "activity")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       # process beginning
                                       if (exists(x = "activity",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start activity(ies) data importation from R environment.\n")
                                         activity_data <- dplyr::tibble(get(x = "activity",
                                                                            envir = environment_name))
                                         if (paste0(class(x = activity_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = activity_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         } else if (nrow(x = activity_data) != length(unique(activity_data$activity_id))) {
                                           activity_id <- unique(activity_data$activity_id[duplicated(activity_data)])
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - Duplicated activity(ies) topiaid in data imported, check the query and query's parameters.\n",
                                                paste0("[activity: ",
                                                       activity_id, collapse="];\n"),
                                                "].")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"activity\" available in the R environment.")
                                       }
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "elementarycatch")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       # process beginning
                                       if (exists(x = "elementarycatch",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary catch(es) data importation from R environment.\n", sep="")
                                         elementarycatch_data <- dplyr::tibble(get(x = "elementarycatch",
                                                                                   envir = environment_name))
                                         if (paste0(class(x = elementarycatch_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = elementarycatch_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"elementarycatch\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful activity(ies) and elementary catch(es) data importation from R environment.\n")
                                     }
                                     # 7 - Common data design ----
                                     # Check elementarycatches data column names and types done in activity.R
                                     activity_data <- unclass(x = activity_data)
                                     object_activities <- object_r6(class_name = "activities")
                                     T1 <- Sys.time()
                                     cli::cli_alert_info(paste0(format(x = Sys.time(),
                                                                       format = "%Y-%m-%d %H:%M:%S "),
                                     " - Importation of activity and elementary catch(es) element:"))
                                     object_activities$add(lapply(cli::cli_progress_along(seq_len(length.out = length(activity_data[[1]])),
                                                                                          clear = getOption("cli.progress_clear", FALSE),
                                                                                          format = paste0("                        ",
                                                                                            "[{cli::pb_current}/{cli::pb_total}], ",
                                                                                            "[{cli::pb_bar}{cli::pb_percent}",
                                                                                            "], Time remaining:{cli::pb_eta}"),,
                                                                                          total = length(activity_data[[1]])),
                                                                  FUN = function(activity_id) {
                                                                    #Sys.sleep(3/100)
                                                                    elementarycatches_data <- elementarycatch_data[elementarycatch_data$activity_id==activity_data[[2]][activity_id],]
                                                                    if(nrow(elementarycatches_data)==0){
                                                                      elementarycatches_data <- NULL
                                                                    }
                                                                    activity <- activity$new(trip_id = activity_data$trip_id[activity_id],
                                                                                             activity_id = activity_data$activity_id[activity_id],
                                                                                             ocean_code = activity_data$ocean_code[activity_id],
                                                                                             activity_date = activity_data$activity_date[activity_id],
                                                                                             activity_number = activity_data$activity_number[activity_id],
                                                                                             activity_longitude = activity_data$activity_longitude[activity_id],
                                                                                             activity_latitude = activity_data$activity_latitude[activity_id],
                                                                                             set_count = activity_data$set_count[activity_id],
                                                                                             set_success_status_code = activity_data$set_success_status_code[activity_id],
                                                                                             set_success_status_label = activity_data$set_success_status_label[activity_id],
                                                                                             school_type_code =  as.integer(activity_data$school_type_code[activity_id]),
                                                                                             activity_code = activity_data$activity_code[activity_id],
                                                                                             activity_label = activity_data$activity_label[activity_id],
                                                                                             objectoperation_code = activity_data$objectoperation_code[activity_id],
                                                                                             objectoperation_label = activity_data$objectoperation_label[activity_id],
                                                                                             objectoperation_id = activity_data$objectoperation_id[activity_id],
                                                                                             elementarycatches = elementarycatches_data,
                                                                                             time_at_sea = activity_data$time_at_sea[activity_id])
                                                                    return(activity)
                                                                  }))
                                     private$activities <- object_activities
                                     T2 <- Sys.time()
                                     elapsed_time <- format(round(T2-T1,2), units="secs")
                                     cli::cli_alert_info(paste0(format(x = Sys.time(),
                                                                       format = "%Y-%m-%d %H:%M:%S "),
                                                                cli::col_green(cli::symbol$tick)," Successful importation of ",
                                                                length(x = activity_data[[1]]),
                                                                " activities and elementary catche(s), in ", elapsed_time, "."))
                                     capture.output(gc(full=TRUE), file="NUL")
                                   },
                                   #' @description Creation of a R6 reference object class elementarylandings which contain one or more R6 reference object class elementarylanding
                                   #' @param data_source  Object of class {\link[base]{character}} expected. By default "observe_database". Identification of data source. You can switch between "observe_database", "avdth_database", "csv_file" (with separator ";" and decimal ","), "rdata_file" or "envir" (for an object in the R environment).
                                   #' @param database_connection Database connection, list of one or more R object(s) expected. By default NULL.
                                   #' Mandatory argument for data source "observe_database" ("PqConnection" R object), corresponding to the second element of the object returned by \href{https://ob7-ird.github.io/furdeb/reference/postgresql_dbconnection.html}{`furdeb::postgresql_dbconnection()`}.
                                   #' Or mandatory argument for data source"avdth_database" ("JDBCConnection" R object) corresponding to the second element of the object returned by \href{https://ob7-ird.github.io/furdeb/reference/access_dbconnection.html}{`furdeb::access_dbconnection()`}.
                                   #' For data source "observe_database", a list of "PqConnection" R objects can be specified to query data from different observe databases.
                                   #' For example, a list of two database connection arguments for "observe_main" and "observe_acquisition" can be specified to simultaneously import and process recent data from acquisition database, which has not yet been imported into the main database, and older data from the main database.
                                   #' @param years_period Object of class {\link[base]{integer}} expected. By default NULL. Year(s) of the reference time period coded on 4 digits. Mandatory for data source "observe_database" and "avdth_database".
                                   #' @param flag_codes Object of class {\link[base]{character}} expected. By default NULL. Country(ies) code related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param ocean_codes Object of class {\link[base]{integer}} expected. By default NULL. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param vessel_type_codes Object of class {\link[base]{integer}} expected. By default NULL. Vessel type(s) related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param trip_ids Object of class {\link[base]{character}} expected. By default NULL. Additional parameter only used with data source "observe_database". Use trip(s) identification(s) for selected trip(s) kept in the query. This argument overrides all others arguments like "years_period", "country" or "ocean".
                                   #' @param data_path Object of class {\link[base]{character}} expected. By default NULL. Path of the data csv/RData file.
                                   #' @param envir Object of class {\link[base]{character}} expected. By default the first environment where data are found will be used. Specify an environment to look in for data source "envir".
                                   elementarylandings_object_creation = function(data_source = "observe_database",
                                                                                 database_connection = NULL,
                                                                                 years_period = NULL,
                                                                                 flag_codes = NULL,
                                                                                 ocean_codes = NULL,
                                                                                 vessel_type_codes = NULL,
                                                                                 trip_ids = NULL,
                                                                                 data_path = NULL,
                                                                                 envir = NULL) {
                                     # 1 - Arguments verifications ----
                                     if (data_source %in% c("observe_database",
                                                            "avdth_database")) {
                                       if(is.null(years_period)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"years_period\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(flag_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"flag_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(ocean_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"ocean_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(vessel_type_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"vessel_type_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       codama::r_type_checking(r_object = years_period,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = flag_codes,
                                                               type = "character")
                                       codama::r_type_checking(r_object = ocean_codes,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = vessel_type_codes,
                                                               type = "integer")
                                     } else if (data_source %in% c("csv_file",
                                                                   "rdata_file")) {
                                       codama::r_type_checking(r_object = data_path,
                                                               type = "character",
                                                               length = 1L)
                                     } else if (data_source != "envir") {
                                       stop(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Invalid \"data_source\" argument. Check function documention through ?object_model_data for more details.")
                                     }
                                     # 2 - Process for observe database ----
                                     if (data_source == "observe_database") {
                                       # specific argument verification for multiple query
                                       if (length(x = database_connection) > 1) {
                                         if( any(unlist(lapply(database_connection, class)) !=  "PqConnection")) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - Invalid \"database_connection\" argument. List of objects of class \"PqConnection\" expected for multiple observe databases query.")


                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary landing(s) data importation from ",
                                             length(database_connection)  ," observe databases.\n")
                                       } else {
                                         # specific argument verification for simple query
                                         if (paste0(class(x = database_connection),
                                                    collapse = " ") != "PqConnection") {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - Invalid \"database_connection\" argument. Class \"PqConnection\" expected.")
                                         }
                                         # process beginning
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary landing(s) data importation from an observe database.\n")
                                       }
                                       for(i in  1:length(database_connection)){
                                         if(length(database_connection)>1){
                                           database_conn <- database_connection[[i]]
                                         } else {
                                           database_conn <- database_connection
                                         }
                                         if (! is.null(x = trip_ids)) {
                                           codama::r_type_checking(r_object = trip_ids,
                                                                   type = "character")
                                           elementarylanding_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                               "observe",
                                                                                                               "observe_elementarylandings_selected_trip.sql",
                                                                                                               package = "t3")),
                                                                                   collapse = "\n"))
                                           elementarylanding_sql_final <- DBI::sqlInterpolate(conn = database_conn,
                                                                                              sql = elementarylanding_sql,
                                                                                              trip_ids = DBI::SQL(paste0("'",
                                                                                                                         paste0(trip_ids,
                                                                                                                                collapse = "', '"),
                                                                                                                         "'")))
                                         } else {
                                           elementarylanding_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                               "observe",
                                                                                                               "observe_elementarylandings.sql",
                                                                                                               package = "t3")),
                                                                                   collapse = "\n"))
                                           elementarylanding_sql_final <- DBI::sqlInterpolate(conn = database_conn,
                                                                                              sql = elementarylanding_sql,
                                                                                              begin_time_period = paste0((dplyr::first(years_period,
                                                                                                                                       order_by = years_period) - 1),
                                                                                                                         "-10-01"),
                                                                                              end_time_period = paste0((dplyr::last(years_period,
                                                                                                                                    order_by = years_period) + 1),
                                                                                                                       "-03-31"),
                                                                                              flag_codes = DBI::SQL(paste0("'",
                                                                                                                           paste0(flag_codes,
                                                                                                                                  collapse = "', '"),
                                                                                                                           "'")),
                                                                                              ocean_codes = DBI::SQL(paste0("'",
                                                                                                                            paste0(ocean_codes,
                                                                                                                                   collapse = "', '"),
                                                                                                                            "'")),
                                                                                              vessel_type_codes = DBI::SQL(paste0("'",
                                                                                                                                  paste0(vessel_type_codes,
                                                                                                                                         collapse = "', '"),
                                                                                                                                  "'")))
                                         }
                                         cat("[",
                                             elementarylanding_sql_final,
                                             "]\n")
                                         if(i >1){
                                           elementarylanding_data <- dplyr::full_join(elementarylanding_data,
                                                                                      dplyr::tibble(DBI::dbGetQuery(conn = database_conn,
                                                                                                                    statement = elementarylanding_sql_final)))
                                         } else {
                                           elementarylanding_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_conn,
                                                                                                   statement = elementarylanding_sql_final))
                                         }
                                       }
                                       if (nrow(x = elementarylanding_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and parameters associated.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary landing(s) data importation from observe database(s).\n")
                                       }
                                     } else if (data_source == "avdth_database") {
                                       # 3 - Process for AVDTH database ----
                                       # specific argument verification
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "JDBCConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument. Class \"JDBCConnection\" expected.")
                                       }
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start elementary landing(s) data importation from an AVDTH database.\n")
                                       elementarylanding_sql <- paste(readLines(con = system.file("sql",
                                                                                                  "avdth",
                                                                                                  "avdth_elementarylandings.sql",
                                                                                                  package = "t3")),
                                                                      collapse = "\n")
                                       elementarylanding_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                          sql = elementarylanding_sql,
                                                                                          begin_time_period  = DBI::SQL(paste0("#",
                                                                                                                               (dplyr::first(years_period,
                                                                                                                                             order_by = years_period) - 1),
                                                                                                                               "-10-01#")),
                                                                                          end_time_period = DBI::SQL(paste0("#",
                                                                                                                            (dplyr::last(years_period,
                                                                                                                                         order_by = years_period) + 1),
                                                                                                                            "-03-31#")),
                                                                                          flag_codes = DBI::SQL(paste0("'",
                                                                                                                       paste0(flag_codes,
                                                                                                                              collapse = "', '"),
                                                                                                                       "'")),
                                                                                          ocean_codes = DBI::SQL(paste0(paste0(ocean_codes,
                                                                                                                               collapse = ", "))),
                                                                                          vessel_type_codes = DBI::SQL(paste0(paste0(vessel_type_codes,
                                                                                                                                     collapse = ", "))))
                                       cat("[",
                                           elementarylanding_sql_final,
                                           "]\n")
                                       elementarylanding_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_connection,
                                                                                               statement = elementarylanding_sql_final)) %>%
                                         dplyr::mutate(trip_id = as.character(trip_id),
                                                       elementarylanding_id = as.character(elementarylanding_id),
                                                       weight_category_code = as.character(weight_category_code),
                                                       weight_category_label = as.character(weight_category_label),
                                                       species_fao_code = as.character(species_fao_code),
                                                       landing_weight = as.numeric(landing_weight))
                                       if (nrow(x = elementarylanding_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and query's parameters.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary landing(s) data importation from avdht database.\n")
                                       }
                                     } else if (data_source == "csv_file") {
                                       # 4 - Process for csv file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start elementary landing(s) data importation from csv file.\n")
                                       elementarylanding_data <- read.csv2(file = data_path,
                                                                           stringsAsFactors = FALSE)
                                       if (nrow(x = elementarylanding_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary landing(s) data importation from csv file.\n")
                                       }
                                     } else if (data_source == "rdata_file") {
                                       # 5 - Process for rdata file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start elementary landing(s) data importation from RData file.\n")
                                       load(file = data_path,
                                            envir = tmp_envir <- new.env())
                                       if (exists(x = "elementarylandings",
                                                  envir = tmp_envir)) {
                                         elementarylanding_data <- dplyr::tibble(get(x = "elementarylandings",
                                                                                     envir = tmp_envir))
                                         if (paste0(class(x = elementarylanding_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = elementarylanding_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid RData, no R object named \"elementarylandings\" available in the R environment provided.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful elementary landing(s) data importation from RData file.\n")
                                     } else if (data_source == "envir") {
                                       # 6 - R environment source ----
                                       # specific argument verification
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "elementarylanding")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       # process beginning
                                       if (exists(x = "elementarylanding",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary landing(s) data importation from R environment.\n")
                                         elementarylanding_data <- dplyr::tibble(get(x = "elementarylanding",
                                                                                     envir = environment_name))
                                         if (paste0(class(x = elementarylanding_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = elementarylanding_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"elementarylanding\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           "- Successful elementary landing(s) data importation R environment.\n")
                                     }
                                     # 7 - Common data design ----
                                     elementarylanding_data <- unclass(x = elementarylanding_data)
                                     object_elementarylandings <- object_r6(class_name = "elementarylandings")
                                     T1 <- Sys.time()
                                     cli::cli_alert_info(paste0(format(x = Sys.time(),
                                                                       format = "%Y-%m-%d %H:%M:%S "),
                                                                "- Importation of elementary landing element:"))
                                     object_elementarylandings$add(lapply(cli::cli_progress_along(seq_len(length.out = length(x = elementarylanding_data[[1]])),
                                                                                                  clear = getOption("cli.progress_clear", FALSE),
                                                                                                  format = paste0(
                                                                                                    "                        ",
                                                                                                    "[{cli::pb_current}/{cli::pb_total}], ",
                                                                                                    "[{cli::pb_bar}{cli::pb_percent}",
                                                                                                    "], Time remaining:{cli::pb_eta}"),,
                                                                                                  total = length(x = elementarylanding_data[[1]])),
                                                                          FUN = function(elementarylanding_id) {
                                                                            Sys.sleep(1/100)
                                                                            elementarylanding <- elementarylanding$new(trip_id = elementarylanding_data$trip_id[elementarylanding_id],
                                                                                                                       elementarylanding_id = elementarylanding_data$elementarylanding_id[elementarylanding_id],
                                                                                                                       weight_category_code = elementarylanding_data$weight_category_code[elementarylanding_id],
                                                                                                                       weight_category_label = elementarylanding_data$weight_category_label[elementarylanding_id],
                                                                                                                       species_fao_code = elementarylanding_data$species_fao_code[elementarylanding_id],
                                                                                                                       landing_weight = elementarylanding_data$landing_weight[elementarylanding_id])
                                                                            return(elementarylanding)
                                                                          }))
                                     T2 <- Sys.time()
                                     elapsed_time <- format(round(T2-T1,2), units="secs")
                                     cli::cli_alert_info(paste0(format(x = Sys.time(),
                                                                       format = "%Y-%m-%d %H:%M:%S "),
                                                                cli::col_green(cli::symbol$tick)," Successful importation of ",
                                                                length(x = elementarylanding_data[[1]]),
                                                                " elementary landing(s), in ", elapsed_time, "."))
                                     private$elementarylandings <- object_elementarylandings
                                     capture.output(gc(full=TRUE), file="NUL")
                                   },
                                   #' @description Creation of a R6 reference object class wells which contain one or more R6 reference object class well, wellset, samples and elementarywellplan.
                                   #' @param data_source  Object of class {\link[base]{character}} expected. By default "observe_database". Identification of data source. You can switch between "observe_database", "avdth_database", "csv_file" (with separator ";" and decimal ","), "rdata_file" or "envir" (for an object in the R environment).
                                   #' @param database_connection Database connection, list of one or more R object(s) expected. By default NULL.
                                   #' Mandatory argument for data source "observe_database" ("PqConnection" R object), corresponding to the second element of the object returned by \href{https://ob7-ird.github.io/furdeb/reference/postgresql_dbconnection.html}{`furdeb::postgresql_dbconnection()`}.
                                   #' Or mandatory argument for data source"avdth_database" ("JDBCConnection" R object) corresponding to the second element of the object returned by \href{https://ob7-ird.github.io/furdeb/reference/access_dbconnection.html}{`furdeb::access_dbconnection()`}.
                                   #' For data source "observe_database", a list of "PqConnection" R objects can be specified to query data from different observe databases.
                                   #' For example, a list of two database connection arguments for "observe_main" and "observe_acquisition" can be specified to simultaneously import and process recent data from acquisition database, which has not yet been imported into the main database, and older data from the main database.
                                   #' @param years_period Object of class {\link[base]{integer}} expected. By default NULL. Year(s) of the reference time period coded on 4 digits. Mandatory for data source "observe_database" and "avdth_database".
                                   #' @param flag_codes Object of class {\link[base]{character}} expected. By default NULL. Country(ies) code related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param ocean_codes Object of class {\link[base]{integer}} expected. By default NULL. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param vessel_type_codes Object of class {\link[base]{integer}} expected. By default NULL. Vessel type(s) related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param sample_type_codes Object of class {\link[base]{integer}} expected. By default NULL. Sample type identification.
                                   #' @param trip_ids Object of class {\link[base]{character}} expected. By default NULL. Additional parameter only used with data source "observe_database". Use trip(s) identification(s) for selected trip(s) kept in the query. This argument overrides all others arguments like "years_period", "country" or "ocean".
                                   #' @param data_path_sample Object of class {\link[base]{character}} expected. By default NULL. Path of the data sql/csv file for samples.
                                   #' @param data_path_wellplan Object of class {\link[base]{character}} expected. By default NULL. Path of the data sql/csv file for well plans.
                                   #' @param envir Object of class {\link[base]{character}} expected. By default NULL. Specify an environment to look in for data source "envir".
                                   wells_object_creation = function(data_source = "observe_database",
                                                                    database_connection = NULL,
                                                                    years_period = NULL,
                                                                    flag_codes = NULL,
                                                                    ocean_codes = NULL,
                                                                    vessel_type_codes = NULL,
                                                                    sample_type_codes = NULL,
                                                                    trip_ids = NULL,
                                                                    data_path_sample = NULL,
                                                                    data_path_wellplan = NULL,
                                                                    envir = NULL) {
                                     # 1 - Arguments verifications ----
                                     if (data_source %in% c("observe_database",
                                                            "avdth_database")) {
                                       if(is.null(years_period)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"years_period\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(flag_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"flag_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(ocean_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"ocean_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(vessel_type_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"vessel_type_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(sample_type_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"sample_type_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       codama::r_type_checking(r_object = years_period,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = flag_codes,
                                                               type = "character")
                                       codama::r_type_checking(r_object = ocean_codes,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = vessel_type_codes,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = sample_type_codes,
                                                               type = "integer")
                                     } else if (data_source %in% c("csv_file",
                                                                   "rdata_file")) {
                                       codama::r_type_checking(r_object = data_path_sample,
                                                               type = "character",
                                                               length = 1L)
                                       codama::r_type_checking(r_object = data_path_wellplan,
                                                               type = "character",
                                                               length = 1L)
                                     } else if (data_source != "envir") {
                                       stop(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Invalid \"data_source\" argument. Check function documention through ?object_model_data for more details.")
                                     }
                                     # 2 - Process for observe database ----
                                     if (data_source == "observe_database") {
                                       # specific argument verification for multiple query
                                       if (length(x = database_connection) > 1) {
                                         if( any(unlist(lapply(database_connection, class)) !=  "PqConnection")) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - Invalid \"database_connection\" argument. List of objects of class \"PqConnection\" expected for multiple observe databases query.")


                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start sample(s) data importation from ", length(database_connection)  ," observe databases.\n", sep="")
                                       } else {
                                         # specific argument verification for simple query
                                         if (paste0(class(x = database_connection),
                                                    collapse = " ") != "PqConnection") {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - Invalid \"database_connection\" argument. Class \"PqConnection\" expected.")
                                         }
                                         # process beginning
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start sample(s) data importation from an observe database.\n", sep="")
                                       }
                                       for(i in  1:length(database_connection)){
                                         if(length(database_connection)>1){
                                           database_conn <- database_connection[[i]]
                                         } else {
                                           database_conn <- database_connection
                                         }
                                         if (! is.null(x = trip_ids)) {
                                           codama::r_type_checking(r_object = trip_ids,
                                                                   type = "character")
                                           sample_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                    "observe",
                                                                                                    "observe_samples_selected_trips.sql",
                                                                                                    package = "t3")),
                                                                        collapse = "\n"))
                                           sample_sql_final <- DBI::sqlInterpolate(conn = database_conn,
                                                                                   sql = sample_sql,
                                                                                   trip_ids = DBI::SQL(paste0("'",
                                                                                                              paste0(trip_ids,
                                                                                                                     collapse = "', '"),
                                                                                                              "'")))
                                         } else {
                                           sample_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                    "observe",
                                                                                                    "observe_samples.sql",
                                                                                                    package = "t3")),
                                                                        collapse = "\n"))
                                           sample_sql_final <- DBI::sqlInterpolate(conn = database_conn,
                                                                                   sql = sample_sql,
                                                                                   begin_time_period = paste0((dplyr::first(years_period,
                                                                                                                            order_by = years_period) - 1),
                                                                                                              "-10-01"),
                                                                                   end_time_period = paste0((dplyr::last(years_period,
                                                                                                                         order_by = years_period) + 1),
                                                                                                            "-03-31"),
                                                                                   flag_codes = DBI::SQL(paste0("'",
                                                                                                                paste0(flag_codes,
                                                                                                                       collapse = "', '"),
                                                                                                                "'")),
                                                                                   ocean_codes = DBI::SQL(paste0("'",
                                                                                                                 paste0(ocean_codes,
                                                                                                                        collapse = "', '"),
                                                                                                                 "'")),
                                                                                   vessel_type_codes = DBI::SQL(paste0("'",
                                                                                                                       paste0(vessel_type_codes,
                                                                                                                              collapse = "', '"),
                                                                                                                       "'")),
                                                                                   sample_type_codes = DBI::SQL(paste0("'",
                                                                                                                       paste0(sample_type_codes,
                                                                                                                              collapse = "', '"),
                                                                                                                       "'")))
                                         }
                                         cat("[",
                                             sample_sql_final,
                                             "]\n")
                                         if(i >1){
                                           sample_data  <- dplyr::full_join(sample_data, dplyr::tibble(DBI::dbGetQuery(conn = database_conn,
                                                                                                                       statement = sample_sql_final)))
                                         } else {
                                           sample_data  <- dplyr::tibble(DBI::dbGetQuery(conn = database_conn,
                                                                                         statement = sample_sql_final))
                                         }
                                       }
                                       if (nrow(x = sample_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and parameters associated.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful sample(s) data importation from observe database(s).\n", sep="")
                                       }
                                       # well plan(s) importation
                                       # process beginning for multiple query
                                       if (length(x = database_connection) > 1) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start well plan(s) importation from ", length(database_connection),
                                             " observe databases.\n", sep="")
                                       } else {
                                         # process beginning for simple query
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start well plan(s) data importation from an observe database.\n", sep="")
                                       }
                                       for(i in  1:length(database_connection)){
                                         if(length(database_connection)>1){
                                           database_conn <- database_connection[[i]]
                                         } else {
                                           database_conn <- database_connection
                                         }

                                         if (! is.null(x = trip_ids)) {
                                           codama::r_type_checking(r_object = trip_ids,
                                                                   type = "character")
                                           wellplan_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                      "observe",
                                                                                                      "observe_wellplans_selected_trips.sql",
                                                                                                      package = "t3")),
                                                                          collapse = "\n"))
                                           wellplan_sql_final <- DBI::sqlInterpolate(conn = database_conn,
                                                                                     sql = wellplan_sql,
                                                                                     trip_ids = DBI::SQL(paste0("'",
                                                                                                                paste0(trip_ids,
                                                                                                                       collapse = "', '"),
                                                                                                                "'")))
                                         } else {
                                           wellplan_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                      "observe",
                                                                                                      "observe_wellplans.sql",
                                                                                                      package = "t3")),
                                                                          collapse = "\n"))
                                           wellplan_sql_final <- DBI::sqlInterpolate(conn = database_conn,
                                                                                     sql = wellplan_sql,
                                                                                     begin_time_period = paste0((dplyr::first(years_period,
                                                                                                                              order_by = years_period) - 1),
                                                                                                                "-10-01"),
                                                                                     end_time_period = paste0((dplyr::last(years_period,
                                                                                                                           order_by = years_period) + 1),
                                                                                                              "-03-31"),
                                                                                     flag_codes = DBI::SQL(paste0("'",
                                                                                                                  paste0(flag_codes,
                                                                                                                         collapse = "', '"),
                                                                                                                  "'")),
                                                                                     ocean_codes = DBI::SQL(paste0("'",
                                                                                                                   paste0(ocean_codes,
                                                                                                                          collapse = "', '"),
                                                                                                                   "'")),
                                                                                     vessel_type_codes = DBI::SQL(paste0("'",
                                                                                                                         paste0(vessel_type_codes,
                                                                                                                                collapse = "', '"),
                                                                                                                         "'")))
                                         }
                                         cat("[",
                                             wellplan_sql_final,
                                             "]\n")
                                         if(i >1){
                                           wellplan_data <- dplyr::full_join(wellplan_data, dplyr::tibble(DBI::dbGetQuery(conn = database_conn,
                                                                                                                          statement = wellplan_sql_final)))
                                         } else {
                                           wellplan_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_conn,
                                                                                          statement = wellplan_sql_final))
                                         }
                                       }
                                       if (nrow(x = wellplan_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and parameters associated.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful well plan(s) data importation from observe database(s).\n", sep="")
                                       }
                                     } else if (data_source == "avdth_database") {
                                       # 3 - Process for AVDTH database ----
                                       # specific argument verification
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "JDBCConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument. Class \"JDBCConnection\" expected.")
                                       }
                                       # process beginning
                                       # sample(s) importation
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start sample(s) data importation from an AVDTH database.\n", sep="")
                                       sample_sql <- paste(readLines(con = system.file("sql",
                                                                                       "avdth",
                                                                                       "avdth_samples.sql",
                                                                                       package = "t3")),
                                                           collapse = "\n")
                                       sample_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                               sql = sample_sql,
                                                                               begin_time_period  = DBI::SQL(paste0("#",
                                                                                                                    (dplyr::first(years_period,
                                                                                                                                  order_by = years_period) - 1),
                                                                                                                    "-10-01#")),
                                                                               end_time_period = DBI::SQL(paste0("#",
                                                                                                                 (dplyr::last(years_period,
                                                                                                                              order_by = years_period) + 1),
                                                                                                                 "-03-31#")),
                                                                               flag_codes = DBI::SQL(paste0("'",
                                                                                                            paste0(flag_codes,
                                                                                                                   collapse = "', '"),
                                                                                                            "'")),
                                                                               ocean_codes = DBI::SQL(paste0(paste0(ocean_codes,
                                                                                                                    collapse = ", "))),
                                                                               vessel_type_codes = DBI::SQL(paste0(paste0(vessel_type_codes,
                                                                                                                          collapse = ", "))),
                                                                               sample_type_codes = DBI::SQL(paste0(sample_type_codes,
                                                                                                                   collapse = ", ")))
                                       cat("[",
                                           sample_sql_final,
                                           "]\n")
                                       sample_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_connection,
                                                                                    statement = sample_sql_final)) %>%
                                         dplyr::mutate(trip_id = as.character(trip_id),
                                                       well_id = as.character(well_id),
                                                       well_minus10_weight = as.numeric(well_minus10_weight),
                                                       well_plus10_weight = as.numeric(well_plus10_weight),
                                                       well_global_weight = as.numeric(well_global_weight),
                                                       sample_id = as.character(sample_id),
                                                       sub_sample_id = as.integer(sub_sample_id),
                                                       sub_sample_total_count_id = as.character(sub_sample_total_count_id),
                                                       elementarysampleraw_id = as.character(paste0(elementarysampleraw_id,
                                                                                                    ".",
                                                                                                    dplyr::row_number())),
                                                       sample_quality_code = as.integer(sample_quality_code),
                                                       sample_type_code = as.integer(x = sample_type_code),
                                                       species_fao_code = as.character(species_fao_code),
                                                       size_measure_type_code = as.character(size_measure_type_code),
                                                       sample_total_count = as.integer(sample_total_count),
                                                       sample_number_measured = as.integer(sample_number_measured),
                                                       sample_length_class = as.numeric(sample_length_class))
                                       if (nrow(x = sample_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and query's parameters.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful sample(s) data importation from avdht database.\n", sep="")
                                       }
                                       # well plan(s) importation
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start well plan(s) data importation from an AVDTH database.\n", sep="")
                                       wellplan_sql <- paste(readLines(con = system.file("sql",
                                                                                         "avdth",
                                                                                         "avdth_wellplans.sql",
                                                                                         package = "t3")),
                                                             collapse = "\n")
                                       wellplan_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                 sql = wellplan_sql,
                                                                                 begin_time_period  = DBI::SQL(paste0("#",
                                                                                                                      (dplyr::first(years_period,
                                                                                                                                    order_by = years_period) - 1),
                                                                                                                      "-10-01#")),
                                                                                 end_time_period = DBI::SQL(paste0("#",
                                                                                                                   (dplyr::last(years_period,
                                                                                                                                order_by = years_period) + 1),
                                                                                                                   "-03-31#")),
                                                                                 flag_codes = DBI::SQL(paste0("'",
                                                                                                              paste0(flag_codes,
                                                                                                                     collapse = "', '"),
                                                                                                              "'")),
                                                                                 ocean_codes = DBI::SQL(paste0(paste0(ocean_codes,
                                                                                                                      collapse = ", "))),
                                                                                 vessel_type_codes = DBI::SQL(paste0(paste0(vessel_type_codes,
                                                                                                                            collapse = ", "))))
                                       cat("[",
                                           wellplan_sql_final,
                                           "]\n")
                                       wellplan_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_connection,
                                                                                      statement = wellplan_sql_final)) %>%
                                         dplyr::mutate(wellplan_id = as.character(wellplan_id),
                                                       well_id = as.character(well_id),
                                                       activity_id = as.character(activity_id),
                                                       sample_id = as.character(sample_id),
                                                       species_fao_code = as.character(species_fao_code),
                                                       wellplan_weight = as.numeric(wellplan_weight),
                                                       weight_category_code = as.character(weight_category_code),
                                                       weight_category_label = as.character(weight_category_label))
                                       if (nrow(x = wellplan_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and query's parameters.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful well plan(s) data importation from avdht database.\n", sep="")
                                       }
                                     } else if (data_source == "csv_file") {
                                       # 4 - Process for csv file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start sample(s) data importation from csv file.\n", sep="")
                                       sample_data <- read.csv2(file = data_path_sample,
                                                                stringsAsFactors = FALSE)
                                       if (nrow(x = sample_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful samples(s) data importation from csv file.\n", sep="")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start well plan(s) data importation from csv file.\n", sep="")
                                       wellplan_data <- read.csv2(file = data_path_wellplan,
                                                                  stringsAsFactors = FALSE)
                                       if (nrow(x = wellplan_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful well plans(s) data importation from csv file.\n", sep="")
                                       }
                                     } else if (data_source == "rdata_file") {
                                       # 5 - Process for rdata file ----
                                       # process beginning
                                       # sample(s) importation
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start sample(s) data importation from RData file.\n", sep="")
                                       load(file = data_path_sample,
                                            envir = tmp_envir <- new.env())
                                       if (exists(x = "samples",
                                                  envir = tmp_envir)) {
                                         sample_data <- dplyr::tibble(get(x = "samples",
                                                                          envir = tmp_envir))
                                         if (paste0(class(x = sample_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = sample_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid RData, no R object named \"samples\" available in the R environment provided.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful sample(s) data importation from RData file.\n", sep="")
                                       # well plan(s) importation
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start well plan(s) data importation from RData file.\n", sep="")
                                       load(file = data_path_wellplan,
                                            envir = tmp_envir <- new.env())
                                       if (exists(x = "wellplans",
                                                  envir = tmp_envir)) {
                                         wellplan_data <- dplyr::tibble(get(x = "wellplans",
                                                                            envir = tmp_envir))
                                         if (paste0(class(x = wellplan_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = wellplan_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid RData, no R object named \"wellplans\" available in the R environment provided.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful well plan(s) data importation from RData file.\n", sep="")
                                     } else if (data_source == "envir") {
                                       # 6 - R environment source ----
                                       # specific argument verification
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "sample")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       # process beginning
                                       # sample(s) importation
                                       if (exists(x = "sample",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start sample(s) data importation from R environment.\n", sep="")
                                         sample_data <- dplyr::tibble(get(x = "sample",
                                                                          envir = environment_name))
                                         if (paste0(class(x = sample_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = sample_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"sample\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful sample(s) data importation R environment.\n", sep="")
                                       # well plan(s) importation
                                       if (exists(x = "wellplan",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start well plan(s) data importation from R environment.\n", sep="")
                                         wellplan_data <- dplyr::tibble(get(x = "wellplan",
                                                                            envir = environment_name))
                                         if (paste0(class(x = wellplan_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = wellplan_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"wellplan\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful well plan(s) data importation R environment.\n", sep="")
                                     }
                                     # 7 - Common data design ----
                                     object_wells <- object_r6(class_name = "wells")
                                     for (trip_id in unique(x = sample_data$trip_id)) {
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start importation of well(s) data for trip element ",
                                           which(x = unique(x = sample_data$trip_id) == trip_id),
                                           ".\n",
                                           "[trip: ",
                                           trip_id,
                                           "]\n")
                                       tmp_trip <- dplyr::filter(.data = sample_data,
                                                                 trip_id == !!trip_id)
                                       for (well_id in unique(x = tmp_trip$well_id)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start importation of well data item ",
                                             which(x = unique(tmp_trip$well_id) == well_id),
                                             ".\n",
                                             "[well: ",
                                             well_id,
                                             "]\n", sep="")
                                         if (is.na(x = well_id)) {
                                           warning(format(x = Sys.time(),
                                                          format = "%Y-%m-%d %H:%M:%S"),
                                                   " - Missing \"well_id\" argument in trip number: \"",
                                                   trip_id,
                                                   "\".")
                                           tmp_well <- dplyr::filter(.data = tmp_trip,
                                                                     is.na(well_id))
                                           if (length(x = unique(x = tmp_well$sample_id)) != 1) {
                                             warning(format(x = Sys.time(),
                                                            format = "%Y-%m-%d %H:%M:%S"),
                                                     " - Well unknown identify in trip number \"",
                                                     trip_id,
                                                     "\" have more than one sampling associated.\n",
                                                     "Data avoided for model incrementation.")
                                             next()
                                           }
                                         } else {
                                           tmp_well <- dplyr::filter(.data = tmp_trip,
                                                                     well_id == !!well_id)
                                         }
                                         if (length(unique(x = tmp_well$well_minus10_weight)) != 1
                                             | length(unique(x = tmp_well$well_plus10_weight)) != 1
                                             | length(unique(x = tmp_well$well_global_weight)) != 1) {
                                           warning(format(x = Sys.time(),
                                                          format = "%Y-%m-%d %H:%M:%S"),
                                                   " - At least one well data (\"well_minus10_weight\", \"well_plus10_weight\" and \"well_global_weight\") is different between well samples. Only the first element will use.\n",
                                                   "[trip: ",
                                                   trip_id,
                                                   ", well: ",
                                                   well_id,
                                                   "]")
                                         }
                                         object_well <- well$new(trip_id = trip_id,
                                                                 well_id = well_id,
                                                                 well_minus10_weight = unique(x = tmp_well$well_minus10_weight)[[1]],
                                                                 well_plus10_weight = unique(x = tmp_well$well_plus10_weight)[[1]],
                                                                 well_global_weight = unique(x = tmp_well$well_global_weight[[1]]))
                                         for (sample_id in unique(x = tmp_well$sample_id)) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Start importation of sample data item ",
                                               which(x = unique(tmp_well$sample_id) == sample_id),
                                               ".\n",
                                               "[sample: ",
                                               sample_id,
                                               "]\n", sep="")
                                           tmp_sample <- dplyr::filter(.data = tmp_well,
                                                                       sample_id == !!sample_id)
                                           tmp_sample <- unclass(x = tmp_sample)
                                           object_well$.__enclos_env__$private$elementarysampleraw <- append(object_well$.__enclos_env__$private$elementarysampleraw,
                                                                                                             list(lapply(X = seq_len(length.out = length(x = tmp_sample[[1]])),
                                                                                                                         FUN = function(i) {
                                                                                                                           elementarysampleraw$new(trip_id = trip_id,
                                                                                                                                                   well_id = well_id,
                                                                                                                                                   sample_id = sample_id,
                                                                                                                                                   sub_sample_id = tmp_sample$sub_sample_id[i],
                                                                                                                                                   sub_sample_total_count_id = tmp_sample$sub_sample_total_count_id[i],
                                                                                                                                                   elementarysampleraw_id = tmp_sample$elementarysampleraw_id[i],
                                                                                                                                                   sample_quality_code = tmp_sample$sample_quality_code[i],
                                                                                                                                                   sample_type_code = tmp_sample$sample_type_code[i],
                                                                                                                                                   species_fao_code = tmp_sample$species_fao_code[i],
                                                                                                                                                   size_measure_type_code = tmp_sample$size_measure_type_code[i],
                                                                                                                                                   sample_total_count = tmp_sample$sample_total_count[i],
                                                                                                                                                   sample_number_measured = tmp_sample$sample_number_measured[i],
                                                                                                                                                   sample_length_class = tmp_sample$sample_length_class[i])
                                                                                                                         })))
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful importation of sample data item ",
                                               which(x = unique(x = tmp_well$sample_id) == sample_id),
                                               ".\n",
                                               "[sample: ",
                                               sample_id,
                                               "]\n", sep="")
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start importation of well plan data item ",
                                             which(x = unique(x = wellplan_data$well_id) == well_id),
                                             ".\n",
                                             "[well: ",
                                             well_id,
                                             "]\n", sep="")
                                         tmp_wellplan <- dplyr::filter(.data = wellplan_data,
                                                                       well_id == !!well_id)
                                         tmp_wellplan <- unclass(x = tmp_wellplan)
                                         object_well$.__enclos_env__$private$wellplan <- lapply(X = seq_len(length.out = length(x = tmp_wellplan[[1]])),
                                                                                                FUN = function(j) {
                                                                                                  elementarywellplan$new(wellplan_id = tmp_wellplan$wellplan_id[j],
                                                                                                                         well_id = tmp_wellplan$well_id[j],
                                                                                                                         activity_id = tmp_wellplan$activity_id[j],
                                                                                                                         sample_id = tmp_wellplan$sample_id[j],
                                                                                                                         species_fao_code = tmp_wellplan$species_fao_code[j],
                                                                                                                         wellplan_weight = tmp_wellplan$wellplan_weight[j],
                                                                                                                         weight_category_code = tmp_wellplan$weight_category_code[j],
                                                                                                                         weight_category_label = tmp_wellplan$weight_category_label[j])
                                                                                                })
                                         object_wells$add(object_well)
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful importation of well data item ",
                                             which(x = unique(x = tmp_trip$well_id) == well_id),
                                             ".\n",
                                             "[well: ",
                                             well_id,
                                             "]\n", sep="")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful importation of well(s) data for trip element ",
                                           which(x = unique(x = sample_data$trip_id) == trip_id),
                                           ".\n",
                                           "[trip: ",
                                           trip_id,
                                           "]\n", sep="")
                                     }
                                     private$wells <- object_wells
                                     capture.output(gc(full=TRUE), file="NUL")
                                   },
                                   #' @description Creation of a data frame object with parameters of set duration algorithms.
                                   #' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "csv_file" (with separator character ";" and decimal ","). Identification of data source. You can switch to "rdata_file" or "envir" (for an object in the R environment).
                                   #' @param data_path Object of class {\link[base]{character}} expected. By default NULL. Mandatory argument for data source "csv_file", "rdata_file" or "envir". Path of the data file.
                                   #' @param envir Object of class {\link[base]{character}} expected. By default NULL. Specify an environment to look in for data source "envir".
                                   setdurationrefs_data = function(data_source = "csv_file",
                                                                   data_path = NULL,
                                                                   envir = NULL) {
                                     # 1 - Arguments verifications ----
                                     if (data_source %in% c("csv_file",
                                                            "rdata_file")) {
                                       codama::r_type_checking(r_object = data_path,
                                                               type = "character",
                                                               length = 1L)
                                     } else if (data_source != "envir") {
                                       stop(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Invalid \"data_source\" argument. Check function documention through ?object_model_data for more details.")
                                     }
                                     if (data_source == "csv_file") {
                                       # 2 - Process for csv file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start set duration(s) data importation from csv file.\n")
                                       set_duration_ref_data <- read.csv(file = data_path,
                                                                          stringsAsFactors = FALSE)
                                       if (nrow(x = set_duration_ref_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.")
                                       } else {
                                         set_duration_ref_data <- dplyr::mutate(.data = set_duration_ref_data,
                                                                                year = as.integer(year),
                                                                                flag_code = as.integer(flag_code),
                                                                                flag_code_iso_3 = as.character(flag_code_iso_3),
                                                                                ocean_code = as.integer(ocean_code),
                                                                                school_type_code_avdth = as.integer(school_type_code_avdth),
                                                                                school_type_code_observe = as.integer(school_type_code_observe),
                                                                                parameter_a = as.numeric(parameter_a),
                                                                                parameter_b = as.numeric(parameter_b),
                                                                                null_set_value = as.numeric(null_set_value))
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful set duration(s) data importation from csv file.\n")
                                       }
                                     } else if (data_source == "rdata_file") {
                                       # 3 - Process for rdata file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start set duration(s) data importation from RData.\n")
                                       load(file = data_path,
                                            envir = tmp_envir <- new.env())
                                       if (exists(x = "setdurationrefs",
                                                  envir = tmp_envir)) {
                                         set_duration_ref_data <- dplyr::tibble(get(x = "setdurationrefs",
                                                                                    envir = tmp_envir))
                                         if (paste0(class(x =  set_duration_ref_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x =  set_duration_ref_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid RData, no R object named \"setdurationrefs\" available in the R environment provided.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful set duration(s) data importation from RData.\n")
                                     } else if (data_source == "envir") {
                                       # 4 - R environment source ----
                                       # specific argument verification
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "setdurationref")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       # process beginning
                                       if (exists(x = "setdurationref",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start set duration(s) data importation from R environment.\n")
                                         set_duration_refs_data <- dplyr::tibble(get(x = "setdurationref",
                                                                                     envir = environment_name))
                                         if (paste0(class(x = set_duration_ref_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = set_duration_ref_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"setdurationref\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful set duration(s) data importation R environment.\n")
                                     }
                                     private$setdurationrefs <- set_duration_ref_data
                                     capture.output(gc(full=TRUE), file="NUL")
                                   },
                                   #' @description Creation of a data frame object with length ratio between ld1 and lf class.
                                   #' @param data_source Object of class {\link[base]{character}} expected. By default "csv_file" (with separator character ";" and decimal ","). Identification of data source. You can switch to "rdata_file" or "envir" (for an object in the R environment).
                                   #' @param data_path Object of class {\link[base]{character}} expected. By default NULL. Mandatory argument for data source "csv_file", "rdata_file" or "envir". Path of the data file.
                                   #' @param envir Object of class {\link[base]{character}} expected. By default NULL. Specify an environment to look in for data source "envir".
                                   lengthsteps_data = function(data_source = "csv_file",
                                                               data_path = NULL,
                                                               envir = NULL) {
                                     # 1 - Arguments verifications ----
                                     if (data_source %in% c("csv_file",
                                                            "rdata_file")) {
                                       codama::r_type_checking(r_object = data_path,
                                                               type = "character",
                                                               length = 1L)
                                     } else if (data_source != "envir") {
                                       stop(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Invalid \"data_source\" argument. Check function documention through ?object_model_data for more details.")
                                     }
                                     if (data_source == "csv_file") {
                                       # 2 - Process for csv file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start length step(s) data importation from csv file.\n")
                                       lengthstep_data <- read.csv(file = data_path,
                                                                    stringsAsFactors = FALSE)
                                       if (nrow(x = lengthstep_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.")
                                       } else {
                                         lengthstep_data <- dplyr::mutate(.data = lengthstep_data,
                                                                          ocean_code = as.integer(x = ocean_code),
                                                                          species_fao_code = as.character(x = species_fao_code),
                                                                          ld1_class = as.numeric(x = ld1_class),
                                                                          lf_class = as.integer(x = lf_class),
                                                                          ratio = as.numeric(x = ratio))
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length step(s) data importation from csv file.\n")
                                       }
                                     } else if (data_source == "rdata_file") {
                                       # 3 - Process for rdata file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start length step(s) data importation from RData.\n")
                                       load(file = data_path,
                                            envir = tmp_envir <- new.env())
                                       if (exists(x = "lengthsteps",
                                                  envir = tmp_envir)) {
                                         lengthstep_data <- dplyr::tibble(get(x = "lengthsteps",
                                                                              envir = tmp_envir))
                                         if (paste0(class(x = lengthstep_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = lengthstep_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid RData, no R object named \"lengthsteps\" available in the R environment provided.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful length step(s) data importation from RData.\n")
                                     } else if (data_source == "envir") {
                                       # 4 - R environment source ----
                                       # specific argument verification
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "lengthstep")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       # process beginning
                                       if (exists(x = "lengthstep",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start length step(s) data importation from R environment.\n")
                                         lengthstep_data <- dplyr::tibble(get(x = "lengthstep",
                                                                              envir = environment_name))
                                         if (paste0(class(x = lengthstep_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = lengthstep_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"lengthstep\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful length step(s) data importation R environment.\n")
                                     }
                                     private$lengthsteps <- lengthstep_data
                                     capture.output(gc(full=TRUE), file="NUL")
                                   },
                                   #' @description Creation of a data frame object with weighted weight of each set sampled.
                                   #' @param data_source  Object of class {\link[base]{character}} expected. By default "observe_database". Identification of data source. You can switch between "observe_database", "avdth_database", "csv_file" (with separator ";" and decimal ","), "rdata_file" or "envir" (for an object in the R environment).
                                   #' @param database_connection Database connection, list of one or more R object(s) expected. By default NULL.
                                   #' Mandatory argument for data source "observe_database" ("PqConnection" R object), corresponding to the second element of the object returned by \href{https://ob7-ird.github.io/furdeb/reference/postgresql_dbconnection.html}{`furdeb::postgresql_dbconnection()`}.
                                   #' Or mandatory argument for data source"avdth_database" ("JDBCConnection" R object) corresponding to the second element of the object returned by \href{https://ob7-ird.github.io/furdeb/reference/access_dbconnection.html}{`furdeb::access_dbconnection()`}.
                                   #' For data source "observe_database", a list of "PqConnection" R objects can be specified to query data from different observe databases.
                                   #' For example, a list of two database connection arguments for "observe_main" and "observe_acquisition" can be specified to simultaneously import and process recent data from acquisition database, which has not yet been imported into the main database, and older data from the main database.
                                   #' @param years_period Object of class {\link[base]{integer}} expected. By default NULL. Year(s) of the reference time period coded on 4 digits. Mandatory for data source "observe_database" and "avdth_database".
                                   #' @param flag_codes Object of class {\link[base]{character}} expected. By default NULL. Country(ies) code related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param ocean_codes Object of class {\link[base]{integer}} expected. By default NULL. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param vessel_type_codes Object of class {\link[base]{integer}} expected. By default NULL. Vessel type(s) related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param trip_ids Object of class {\link[base]{character}} expected. By default NULL. Additional parameter only used with data source "observe_database". Use trip(s) identification(s) for selected trip(s) kept in the query. This argument overrides all others arguments like "years_period", "country" or "ocean".
                                   #' @param data_path Object of class {\link[base]{character}} expected. By default NULL. Path of the data csv/RData file.
                                   #' @param envir Object of class {\link[base]{character}} expected. By default the first environment where data are found will be used. Specify an environment to look in for data source "envir".
                                   samplesets_data = function(data_source = "observe_database",
                                                              database_connection = NULL,
                                                              years_period = NULL,
                                                              flag_codes = NULL,
                                                              ocean_codes = NULL,
                                                              vessel_type_codes = NULL,
                                                              trip_ids = NULL,
                                                              data_path = NULL,
                                                              envir = NULL) {
                                     # 1 - Arguments verifications ----
                                     if (data_source %in% c("observe_database",
                                                            "avdth_database")) {
                                       if(is.null(years_period)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"years_period\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(flag_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"flag_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(ocean_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"ocean_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       if(is.null(vessel_type_codes)){
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Missing \"vessel_type_codes\" argument.
                                              Check function documention through ?object_model_data for more details.")
                                       }
                                       codama::r_type_checking(r_object = years_period,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = flag_codes,
                                                               type = "character")
                                       codama::r_type_checking(r_object = ocean_codes,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = vessel_type_codes,
                                                               type = "integer")
                                     } else if (data_source %in% c("csv_file",
                                                                   "rdata_file")) {
                                       codama::r_type_checking(r_object = data_path,
                                                               type = "character",
                                                               length = 1L)
                                     } else if (data_source != "envir") {
                                       stop(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Invalid \"data_source\" argument. Check function documention through ?object_model_data for more details.")
                                     }
                                     # 2 - Process for observe database ----
                                     if (data_source == "observe_database") {
                                       # specific argument verification for multiple query
                                       if (length(x = database_connection) > 1) {
                                         if( any(unlist(lapply(database_connection, class)) !=  "PqConnection")) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - Invalid \"database_connection\" argument. List of objects of class \"PqConnection\" expected for multiple observe databases query.")


                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start sample set(s) data importation from ",
                                             length(database_connection) ," observe databases.\n")
                                       } else {
                                         # specific argument verification for simple query
                                         if (paste0(class(x = database_connection),
                                                    collapse = " ") != "PqConnection") {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - Invalid \"database_connection\" argument. Class \"PqConnection\" expected.")
                                         }
                                         # process beginning
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start sample set(s) data importation from an observe database.\n")
                                       }
                                       for(i in  1:length(database_connection)){
                                         if(length(database_connection)>1){
                                           database_conn <- database_connection[[i]]
                                         } else {
                                           database_conn <- database_connection
                                         }
                                         if (! is.null(x = trip_ids)) {
                                           codama::r_type_checking(r_object = trip_ids,
                                                                   type = "character")
                                           sampleset_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                       "observe",
                                                                                                       "observe_samplesets_selected_trips.sql",
                                                                                                       package = "t3")),
                                                                           collapse = "\n"))
                                           sampleset_sql_final <- DBI::sqlInterpolate(conn = database_conn,
                                                                                      sql = sampleset_sql,
                                                                                      trip_ids = DBI::SQL(paste0("'",
                                                                                                                 paste0(trip_ids,
                                                                                                                        collapse = "', '"),
                                                                                                                 "'")))
                                         } else {
                                           sampleset_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                       "observe",
                                                                                                       "observe_samplesets.sql",
                                                                                                       package = "t3")),
                                                                           collapse = "\n"))
                                           sampleset_sql_final <- DBI::sqlInterpolate(conn = database_conn,
                                                                                      sql = sampleset_sql,
                                                                                      begin_time_period = paste0((dplyr::first(years_period,
                                                                                                                               order_by = years_period) - 1),
                                                                                                                 "-10-01"),
                                                                                      end_time_period = paste0((dplyr::last(years_period,
                                                                                                                            order_by = years_period) + 1),
                                                                                                               "-03-31"),
                                                                                      flag_codes = DBI::SQL(paste0("'",
                                                                                                                   paste0(flag_codes,
                                                                                                                          collapse = "', '"),
                                                                                                                   "'")),
                                                                                      ocean_codes = DBI::SQL(paste0("'",
                                                                                                                    paste0(ocean_codes,
                                                                                                                           collapse = "', '"),
                                                                                                                    "'")),
                                                                                      vessel_type_codes = DBI::SQL(paste0("'",
                                                                                                                          paste0(vessel_type_codes,
                                                                                                                                 collapse = "', '"),
                                                                                                                          "'")))
                                         }
                                         cat("[",
                                             sampleset_sql_final,
                                             "]\n")
                                         if(i >1){
                                           sampleset_data <- dplyr::full_join(sampleset_data, dplyr::tibble(DBI::dbGetQuery(conn = database_conn,
                                                                                                                            statement = sampleset_sql_final)))
                                         } else {
                                           sampleset_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_conn,
                                                                                           statement = sampleset_sql_final))
                                         }
                                       }
                                       if (nrow(x = sampleset_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and parameters associated.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful sample set(s) data importation from observe database(s).\n")
                                       }
                                     } else if (data_source == "avdth_database") {
                                       # 3 - Process for AVDTH database ----
                                       # specific argument verification
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "JDBCConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument. Class \"JDBCConnection\" expected.")
                                       }
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start sample set(s) data importation from an AVDTH database.\n")
                                       sampleset_sql <- paste(readLines(con = system.file("sql",
                                                                                          "avdth",
                                                                                          "avdth_samplesets.sql",
                                                                                          package = "t3")),
                                                              collapse = "\n")
                                       sampleset_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                  sql = sampleset_sql,
                                                                                  begin_time_period  = DBI::SQL(paste0("#",
                                                                                                                       (dplyr::first(years_period,
                                                                                                                                     order_by = years_period) - 1),
                                                                                                                       "-10-01#")),
                                                                                  end_time_period = DBI::SQL(paste0("#",
                                                                                                                    (dplyr::last(years_period,
                                                                                                                                 order_by = years_period) + 1),
                                                                                                                    "-03-31#")),
                                                                                  flag_codes = DBI::SQL(paste0("'",
                                                                                                               paste0(flag_codes,
                                                                                                                      collapse = "', '"),
                                                                                                               "'")),
                                                                                  ocean_codes = DBI::SQL(paste0(paste0(ocean_codes,
                                                                                                                       collapse = ", "))),
                                                                                  vessel_type_codes = DBI::SQL(paste0(paste0(vessel_type_codes,
                                                                                                                             collapse = ", "))))
                                       cat("[",
                                           sampleset_sql_final,
                                           "]\n")
                                       sampleset_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_connection,
                                                                                       statement = sampleset_sql_final)) %>%
                                         dplyr::mutate(trip_id = as.character(trip_id),
                                                       activity_id = as.character(activity_id),
                                                       well_id = as.character(well_id),
                                                       sample_id = as.character(sample_id),
                                                       well_set_weighted_weight = as.numeric(well_set_weighted_weight))
                                       if (nrow(x = sampleset_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and query's parameters.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful sample set(s) data importation from avdht database.\n")
                                       }
                                     } else if (data_source == "csv_file") {
                                       # 4 - Process for csv file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start sample set(s) data importation from csv file.\n")
                                       sampleset_data <- read.csv2(file = data_path,
                                                                   stringsAsFactors = FALSE)
                                       if (nrow(x = sampleset_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful sample set(s) data importation from csv file.\n")
                                       }
                                     } else if (data_source == "rdata_file") {
                                       # 5 - Process for rdata file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start sample set(s) data importation from RData.\n")
                                       load(file = data_path,
                                            envir = tmp_envir <- new.env())
                                       if (exists(x = "samplesets",
                                                  envir = tmp_envir)) {
                                         sampleset_data <- dplyr::tibble(get(x = "samplesets",
                                                                             envir = tmp_envir))
                                         if (paste0(class(x = sampleset_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = sampleset_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid RData, no R object named \"samplesets\" available in the R environment provided.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful sample set(s) data importation from RData.\n")
                                     } else if (data_source == "envir") {
                                       # 6 - R environment source ----
                                       # specific argument verification
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "sampleset")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       # process beginning
                                       if (exists(x = "sampleset",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start sample set(s) data importation from R environment.\n")
                                         sampleset_data <- dplyr::tibble(get(x = "sampleset",
                                                                             envir = environment_name))
                                         if (paste0(class(x = sampleset_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = sampleset_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"sampleset\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful sample set(s) data importation R environment.\n")
                                     }
                                     private$samplesets <- sampleset_data
                                     capture.output(gc(full=TRUE), file="NUL")
                                   },
                                   #' @description Creation of a data frame object with parameters for length weight relationship.
                                   #' @param data_source Object of class {\link[base]{character}} expected. By default "observe_database". Identification of data source. You can switch to "csv_file" (with separator character ";" and decimal ","), "rdata_file" or "envir" (for an object in the R environment).
                                   #' @param database_connection Database connection R object expected. By default NULL. Mandatory argument for data source "observe_database".
                                   #' @param data_path Object of class {\link[base]{character}} expected. By default NULL. Path of the data file.
                                   #' @param envir Object of class {\link[base]{character}} expected. By default NULL. Specify an environment to look in for data source "envir".
                                   lengthweightrelationships_data = function(data_source = "observe_database",
                                                                             database_connection = NULL,
                                                                             data_path = NULL,
                                                                             envir = NULL) {
                                     # 1 - Arguments verifications ----
                                     if (data_source == "observe_database") {
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "PqConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument. Class \"PqConnection\" expected.")
                                       }
                                     } else if (data_source %in% c("csv_file",
                                                                   "rdata_file")) {
                                       codama::r_type_checking(r_object = data_path,
                                                               type = "character",
                                                               length = 1L)
                                     } else if (data_source != "envir") {
                                       stop(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Invalid \"data_source\" argument. Check function documention through ?object_model_data for more details.")
                                     }
                                     # 2 - Process for observe database ----
                                     if (data_source == "observe_database") {
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start length weight relationship(s) data importation from an observe database.\n")
                                       lengthweightrelationship_sql <- paste(readLines(con = system.file("sql",
                                                                                                         "observe",
                                                                                                         "observe_lengthweightrelationships.sql",
                                                                                                         package = "t3")),
                                                                             collapse = "\n")
                                       cat("[",
                                           lengthweightrelationship_sql,
                                           "]\n")
                                       lengthweightrelationship_data <- DBI::dbGetQuery(conn = database_connection,
                                                                                        statement = lengthweightrelationship_sql)
                                       if (nrow(x = lengthweightrelationship_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and parameters associated.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length weight relationship(s) data importation from an observe database.\n")
                                       }
                                     } else if (data_source == "csv_file") {
                                       # 4 - Process for csv file ----
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start length weight relationship(s) data importation from csv file.\n")
                                       lengthweightrelationship_data <- read.csv(file = data_path,
                                                                                  stringsAsFactors = FALSE)
                                       if (nrow(x = lengthweightrelationship_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.")
                                       } else {
                                         lengthweightrelationship_data <- dplyr::mutate(.data = lengthweightrelationship_data,
                                                                                        ocean_code = as.integer(x = ocean_code),
                                                                                        ocean_label = as.character(x = ocean_label),
                                                                                        species_fao_code = as.character(x = species_fao_code),
                                                                                        length_weight_formula = as.character(x = length_weight_formula),
                                                                                        lwr_a = as.numeric(x = lwr_a),
                                                                                        lwr_b = as.numeric(x = lwr_b))
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length weight relationship(s) data importation from csv file.\n")
                                       }
                                     } else if (data_source == "rdata_file") {
                                       # 5 - Process for rdata file ----
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start length weight relationship(s) data importation from RData file.\n")
                                       load(file = data_path,
                                            envir = tmp_envir <- new.env())
                                       if (exists(x = "lengthweightrelationships",
                                                  envir = tmp_envir)) {
                                         lengthweightrelationship_data <- dplyr::tibble(get(x = "lengthweightrelationships",
                                                                                            envir = tmp_envir))
                                         if (paste0(class(x = lengthweightrelationship_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = lengthweightrelationship_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid RData, no R object named \"lengthweightrelationships\" available in the R environment provided.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful length weight relationship(s) data importation from RData file.\n")
                                     } else if (data_source == "envir") {
                                       # 6 - R environment source ----
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "lengthweightrelationship")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       # process beginning
                                       if (exists(x = "lengthweightrelationship",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start length weight relationship(s) data importation from R environment.\n")
                                         lengthweightrelationship_data <- dplyr::tibble(get(x = "lengthweightrelationship",
                                                                                            envir = environment_name))
                                         if (paste0(class(x = lengthweightrelationship_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = lengthweightrelationship_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"lengthweightrelationship\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful length weight relationship(s) data importation R environment.\n")
                                     }
                                     private$lengthweightrelationships <- lengthweightrelationship_data
                                     capture.output(gc(full=TRUE), file="NUL")
                                   },
                                   #' @description Creation of reference table with the activity codes to be taken into account for the allocation of sea and/or fishing time,
                                   #'  and/or searching time and/or set duration..
                                   #' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "csv_file" (with separator character ";" and decimal ","). Identification of data source. You can switch to "rdata_file" or "envir" (for an object in the R environment).
                                   #' @param data_path Object of class {\link[base]{character}} expected. By default NULL. Mandatory argument for data source "csv_file", "rdata_file" or "envir". Path of the data file.
                                   #' @param envir Object of class {\link[base]{character}} expected. By default NULL. Specify an environment to look in for data source "envir".
                                   activitycoderefs_data = function(data_source = "csv_file",
                                                                    data_path = NULL,
                                                                    envir = NULL) {
                                     # 1 - Arguments verifications ----
                                     if (data_source %in% c("csv_file",
                                                            "rdata_file")) {
                                       codama::r_type_checking(r_object = data_path,
                                                               type = "character",
                                                               length = 1L)
                                     } else if (data_source != "envir") {
                                       stop(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Invalid \"data_source\" argument. Check function documention through ?object_model_data for more details.")
                                     }
                                     if (data_source == "csv_file") {
                                       # 2 - Process for csv file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start activity codes referential data importation from csv file.\n")
                                       activity_code_ref_data <- read.csv2(file = data_path,
                                                                           stringsAsFactors = FALSE)
                                       if (nrow(x = activity_code_ref_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful activity code referential data importation from csv file.\n")
                                       }
                                     } else if (data_source == "rdata_file") {
                                       # 3 - Process for rdata file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start activity code referential data importation from RData.\n")
                                       load(file = data_path,
                                            envir = tmp_envir <- new.env())
                                       if (exists(x = "activitycoderefs",
                                                  envir = tmp_envir)) {
                                         activity_code_ref_data <- dplyr::tibble(get(x = "activitycoderefs",
                                                                                     envir = tmp_envir))
                                         if (paste0(class(x =  activity_code_ref_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x =  activity_code_ref_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid RData, no R object named \"activitycoderefs\" available in the R environment provided.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful activity code referential data importation from RData.\n")
                                     } else if (data_source == "envir") {
                                       # 4 - R environment source ----
                                       # specific argument verification
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "activitycoderefs")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       # process beginning
                                       if (exists(x = "activitycoderefs",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start activity code referential data importation from R environment.\n")
                                         activity_code_ref_data <- dplyr::tibble(get(x = "activitycoderefs",
                                                                                     envir = environment_name))
                                         if (paste0(class(x = activity_code_ref_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = activity_code_ref_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"activitycoderefs\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful activity code referential data importation R environment.\n")
                                     }
                                     private$activitycoderefs <- activity_code_ref_data
                                   }
                                 ),
                                 private = list(
                                   trips = NULL,
                                   activities = NULL,
                                   elementarylandings = NULL,
                                   wells = NULL,
                                   samplesets = NULL,
                                   setdurationrefs = NULL,
                                   lengthsteps = NULL,
                                   lengthweightrelationships = NULL,
                                   activitycoderefs = NULL
                                 ))
