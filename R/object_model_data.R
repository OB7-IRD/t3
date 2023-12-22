#' @title R6 class object_model_data
#' @name object_model_data
#' @description Create R6 reference object class object_model_data
object_model_data <- R6::R6Class(classname = "object_model_data",
                                 public = list(
                                   #' @description Creation of an R6 reference object class trips which contains one or more R6 reference object class trip.
                                   #' @param data_source  Object of class {\link[base]{character}} expected. By default "observe_database". Identification of data source. You can switch between "observe_database", "avdth_database", "csv_file" (with separator ";" and decimal ","), "rdata_file" or "envir" (for an object in the R environment).
                                   #' @param database_connection Database connection R object expected. By default NULL. Mandatory argument for data source "observe_database" and "avdth_database".
                                   #' @param time_period Object of class {\link[base]{integer}} expected. By default NULL. Year(s) of the reference time period coded on 4 digits. Mandatory for data source "observe_database" and "avdth_database".
                                   #' @param fleet_code Object of class {\link[base]{character}} expected. By default NULL. Country(ies) code related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param ocean_code Object of class {\link[base]{integer}} expected. By default NULL. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param vessel_type_code Object of class {\link[base]{integer}} expected. By default NULL. Vessel type(s) related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param trip_id Object of class {\link[base]{character}} expected. By default NULL. Additional parameter only used with data source "observe_database". Use trip(s) identification(s) for selected trip(s) kept in the query. This argument overrides all others arguments like "time_periode", "country" or "ocean".
                                   #' @param data_path Object of class {\link[base]{character}} expected. By default NULL. Path of the data csv/RData file.
                                   #' @param envir Object of class {\link[base]{character}} expected. By default the first environment where data are found will be used. Specify an environment to look in for data source "envir".
                                   trips_object_creation = function(data_source = "observe_database",
                                                                    database_connection = NULL,
                                                                    time_period = NULL,
                                                                    fleet_code = NULL,
                                                                    ocean_code = NULL,
                                                                    vessel_type_code = NULL,
                                                                    trip_id = NULL,
                                                                    data_path = NULL,
                                                                    envir = NULL) {
                                     # 1 - Arguments verifications ----
                                     if (data_source %in% c("observe_database",
                                                            "avdth_database")) {
                                       codama::r_type_checking(r_object = time_period,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = fleet_code,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = ocean_code,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = vessel_type_code,
                                                               type = "integer")
                                     } else if (data_source %in% c("csv_file",
                                                                   "rdata_file")) {
                                       codama::r_type_checking(r_object = data_path,
                                                               type = "character",
                                                               length = 1L)
                                     } else if (data_source != "envir") {
                                       stop(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Invalid \"data_source\" argument.",
                                            "\nCheck function documention through ?object_model_data for more details.")
                                     }
                                     # 2 - Process for observe database ----
                                     if (data_source == "observe_database") {
                                       # specific argument verification
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "PostgreSQLConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument.",
                                              "\nClass \"PostgreSQLConnection\" expected.")
                                       }
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start trip(s) data importation from an observe database.\n",
                                           sep = "")
                                       if (! is.null(x = trip_id)) {
                                         codama::r_type_checking(r_object = trip_id,
                                                                 type = "character")
                                         trip_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                "observe",
                                                                                                "observe_trip_selected_trip.sql",
                                                                                                package = "t3")),
                                                                    collapse = "\n"))
                                         trip_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                               sql = trip_sql,
                                                                               trip_id = DBI::SQL(paste0("'",
                                                                                                         paste0(trip_id,
                                                                                                                collapse = "', '"),
                                                                                                         "'")))
                                       } else {
                                         trip_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                "observe",
                                                                                                "observe_trip.sql",
                                                                                                package = "t3")),
                                                                    collapse = "\n"))
                                         trip_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                               sql = trip_sql,
                                                                               begin_time_period = paste0((dplyr::first(time_period,
                                                                                                                        order_by = time_period) - 1),
                                                                                                          "-10-01"),
                                                                               end_time_period = paste0((dplyr::last(time_period,
                                                                                                                     order_by = time_period) + 1),
                                                                                                        "-03-31"),
                                                                               fleet_code = DBI::SQL(paste0("'",
                                                                                                            paste0(fleet_code,
                                                                                                                   collapse = "', '"),
                                                                                                            "'")),
                                                                               ocean_code = DBI::SQL(paste0("'",
                                                                                                            paste0(ocean_code,
                                                                                                                   collapse = "', '"),
                                                                                                            "'")),
                                                                               vessel_type_code = DBI::SQL(paste0("'",
                                                                                                                  paste0(vessel_type_code,
                                                                                                                         collapse = "', '"),
                                                                                                                  "'")))
                                       }
                                       cat("[",
                                           trip_sql_final,
                                           "]\n",
                                           sep = "")
                                       trip_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_connection,
                                                                                  statement = trip_sql_final))
                                       if (nrow(x = trip_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and parameters associated.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful trip(s) data importation from an observe database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "avdth_database") {
                                       # 3 - Process for AVDTH database ----
                                       # specific argument verification
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "JDBCConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument.",
                                              "\nClass \"JDBCConnection\" expected.")
                                       }
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start trip(s) data importation from an AVDTH database.\n",
                                           sep = "")
                                       trip_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                              "avdth",
                                                                                              "avdth_trip.sql",
                                                                                              package = "t3")),
                                                                  collapse = "\n"))
                                       trip_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                             sql = trip_sql,
                                                                             begin_time_period  = DBI::SQL(paste0("#",
                                                                                                                  (dplyr::first(time_period,
                                                                                                                                order_by = time_period) - 1),
                                                                                                                  "-10-01#")),
                                                                             end_time_period = DBI::SQL(paste0("#",
                                                                                                               (dplyr::last(time_period,
                                                                                                                            order_by = time_period) + 1),
                                                                                                               "-03-31#")),
                                                                             fleet_code = DBI::SQL(paste0(paste0(fleet_code,
                                                                                                                 collapse = ", "))),
                                                                             ocean_code = DBI::SQL(paste0(paste0(ocean_code,
                                                                                                                 collapse = ", "))),
                                                                             vessel_type_code = DBI::SQL(paste0(paste0(vessel_type_code,
                                                                                                                       collapse = ", "))))

                                       cat("[",
                                           trip_sql_final,
                                           "]\n",
                                           sep = "")
                                       trip_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_connection,
                                                                                  statement = trip_sql_final)) %>%
                                         dplyr::mutate(trip_id = as.character(x = trip_id),
                                                       fleet_code = as.integer(x = fleet_code),
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
                                             " - Successful trip(s) data importation from avdht database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "csv_file") {
                                       # 4 - Process for csv file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start trip(s) data importation from csv file.\n",
                                           sep = "")
                                       trip_data <- dplyr::tibble(read.csv2(file = data_path,
                                                                            stringsAsFactors = FALSE))
                                       if (nrow(x = trip_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.\n")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful trip(s) data importation from csv file.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "rdata_file") {
                                       # 5 - Process for rdata file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start trip(s) data importation from RData file.\n",
                                           sep = "")
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
                                           " - Successful trip(s) data importation from RData file.\n",
                                           sep = "")
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
                                             " - Start trip(s) data importation from R environment.\n",
                                             sep = "")
                                         trip_data <- dplyr::tibble(get(x = "trip",
                                                                        envir = environment_name))
                                         if (paste0(class(x = trip_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = trip_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.\n")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"trip\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful trip(s) data importation R environment.\n",
                                           sep = "")
                                     }
                                     # 7 - Common data design ----
                                     trip_data <- unclass(x = trip_data)
                                     object_trips <- object_r6(class_name = "trips")
                                     object_trips$add(lapply(X = seq_len(length.out = length(x = trip_data[[1]])),
                                                             FUN = function(trip_id) {
                                                               cat(format(x = Sys.time(),
                                                                          "%Y-%m-%d %H:%M:%S"),
                                                                   " - Start importation of trip element ",
                                                                   trip_id,
                                                                   ".\n",
                                                                   "[trip: ",
                                                                   trip_data$trip_id[trip_id],
                                                                   "]\n",
                                                                   sep = "")
                                                               trip <- trip$new(trip_id = trip_data$trip_id[trip_id],
                                                                                fleet_code = trip_data$fleet_code[trip_id],
                                                                                departure_date = trip_data$departure_date[trip_id],
                                                                                trip_end_date = trip_data$trip_end_date[trip_id],
                                                                                logbook_availability_code = trip_data$logbook_availability_code[trip_id],
                                                                                landing_well_content_code = trip_data$landing_well_content_code[trip_id],
                                                                                vessel_code = trip_data$vessel_code[trip_id],
                                                                                vessel_type_code = trip_data$vessel_type_code[trip_id])
                                                               cat(format(x = Sys.time(),
                                                                          format = "%Y-%m-%d %H:%M:%S"),
                                                                   " - Successful importation of trip element ",
                                                                   trip_id,
                                                                   ".\n",
                                                                   sep = "")
                                                               return(trip)
                                                             }))
                                     private$trips <- object_trips
                                   },
                                   #' @description Creation of a R6 reference object class activities which contain one or more R6 reference object class activity.
                                   #' @param data_source  Object of class {\link[base]{character}} expected. By default "observe_database". Identification of data source. You can switch between "observe_database", "avdth_database", "csv_file" (with separator ";" and decimal ","), "rdata_file" or "envir" (for an object in the R environment).
                                   #' @param database_connection Database connection R object expected. By default NULL. Mandatory argument for data source "observe_database" and "avdth_database".
                                   #' @param time_period Object of class {\link[base]{integer}} expected. By default NULL. Year(s) of the reference time period coded on 4 digits. Mandatory for data source "observe_database" and "avdth_database".
                                   #' @param fleet_code Object of class {\link[base]{character}} expected. By default NULL. Country(ies) code related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param ocean_code Object of class {\link[base]{integer}} expected. By default NULL. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param vessel_type_code Object of class {\link[base]{integer}} expected. By default NULL. Vessel type(s) related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param trip_id Object of class {\link[base]{character}} expected. By default NULL. Additional parameter only used with data source "observe_database". Use trip(s) identification(s) for selected trip(s) kept in the query. This argument overrides all others arguments like "time_periode", "country" or "ocean".
                                   #' @param data_path Object of class {\link[base]{character}} expected. By default NULL. Path of the data csv/RData file.
                                   #' @param envir Object of class {\link[base]{character}} expected. By default the first environment where data are found will be used. Specify an environment to look in for data source "envir".
                                   activities_object_creation = function(data_source = "observe_database",
                                                                         database_connection = NULL,
                                                                         time_period = NULL,
                                                                         fleet_code = NULL,
                                                                         ocean_code = NULL,
                                                                         vessel_type_code = NULL,
                                                                         trip_id = NULL,
                                                                         data_path = NULL,
                                                                         envir = NULL) {
                                     # 1 - Arguments verifications ----
                                     if (data_source %in% c("observe_database",
                                                            "avdth_database")) {
                                       codama::r_type_checking(r_object = time_period,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = fleet_code,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = ocean_code,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = vessel_type_code,
                                                               type = "integer")
                                     } else if (data_source %in% c("csv_file",
                                                                   "rdata_file")) {
                                       codama::r_type_checking(r_object = data_path,
                                                               type = "character",
                                                               length = 1L)
                                     } else if (data_source != "envir") {
                                       stop(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Invalid \"data_source\" argument.",
                                            "\nCheck function documention through ?object_model_data for more details.")
                                     }
                                     # 2 - Process for observe database ----
                                     if (data_source == "observe_database") {
                                       # specific argument verification
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "PostgreSQLConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument.",
                                              "\nClass \"PostgreSQLConnection\" expected.")
                                       }
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start activities data importation from an observe database.\n",
                                           sep = "")
                                       if (! is.null(x = trip_id)) {
                                         codama::r_type_checking(r_object = trip_id,
                                                                 type = "character")
                                         activity_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                    "observe",
                                                                                                    "observe_activity_selected_trip.sql",
                                                                                                    package = "t3")),
                                                                        collapse = "\n"))
                                         activity_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                   sql = activity_sql,
                                                                                   trip_id = DBI::SQL(paste0("'",
                                                                                                             paste0(trip_id,
                                                                                                                    collapse = "', '"),
                                                                                                             "'")))
                                       } else {
                                         activity_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                    "observe",
                                                                                                    "observe_activity.sql",
                                                                                                    package = "t3")),
                                                                        collapse = "\n"))
                                         activity_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                   sql = activity_sql,
                                                                                   begin_time_period = paste0((dplyr::first(time_period,
                                                                                                                            order_by = time_period) - 1),
                                                                                                              "-10-01"),
                                                                                   end_time_period = paste0((dplyr::last(time_period,
                                                                                                                         order_by = time_period) + 1),
                                                                                                            "-03-31"),
                                                                                   fleet_code = DBI::SQL(paste0("'",
                                                                                                                paste0(fleet_code,
                                                                                                                       collapse = "', '"),
                                                                                                                "'")),
                                                                                   ocean_code = DBI::SQL(paste0("'",
                                                                                                                paste0(ocean_code,
                                                                                                                       collapse = "', '"),
                                                                                                                "'")),
                                                                                   vessel_type_code = DBI::SQL(paste0("'",
                                                                                                                      paste0(vessel_type_code,
                                                                                                                             collapse = "', '"),
                                                                                                                      "'")))
                                       }
                                       cat("[",
                                           activity_sql_final,
                                           "]\n",
                                           sep = "")
                                       activity_data <- DBI::dbGetQuery(conn = database_connection,
                                                                        statement = activity_sql_final)
                                       if (nrow(x = activity_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and parameters associated.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful activity(ies) data importation from an observe database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "avdth_database") {
                                       # 3 - Process for AVDTH database ----
                                       # specific argument verification
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "JDBCConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument.",
                                              "\nClass \"JDBCConnection\" expected.")
                                       }
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start activities data importation from an AVDTH database.\n",
                                           sep = "")
                                       activity_sql <- paste(readLines(con = system.file("sql",
                                                                                         "avdth",
                                                                                         "avdth_activity.sql",
                                                                                         package = "t3")),
                                                             collapse = "\n")
                                       activity_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                 sql = activity_sql,
                                                                                 begin_time_period  = DBI::SQL(paste0("#",
                                                                                                                      (dplyr::first(time_period,
                                                                                                                                    order_by = time_period) - 1),
                                                                                                                      "-10-01#")),
                                                                                 end_time_period = DBI::SQL(paste0("#",
                                                                                                                   (dplyr::last(time_period,
                                                                                                                                order_by = time_period) + 1),
                                                                                                                   "-03-31#")),
                                                                                 fleet_code = DBI::SQL(paste0(paste0(fleet_code,
                                                                                                                     collapse = ", "))),
                                                                                 ocean_code = DBI::SQL(paste0(paste0(ocean_code,
                                                                                                                     collapse = ", "))),
                                                                                 vessel_type_code = DBI::SQL(paste0(paste0(vessel_type_code,
                                                                                                                           collapse = ", "))))
                                       cat("[",
                                           activity_sql_final,
                                           "]\n",
                                           sep = "")
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
                                                       school_type_code = as.integer(x = school_type_code),
                                                       activity_code = as.integer(x = activity_code),
                                                       activity_label = as.character(x = activity_label),
                                                       time_at_sea = as.integer(x = time_at_sea))
                                       if (nrow(x = activity_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and query's parameters.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful activity(ies) data importation from an AVDTH database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "csv_file") {
                                       # 4 - Process for csv file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start activity(ies) data importation from csv file.\n",
                                           sep = "")
                                       activity_data <- dplyr::tibble(read.csv2(file = data_path,
                                                                                stringsAsFactors = FALSE))
                                       if (nrow(x = activity_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.\n")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful activity(ies) data importation from csv file.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "rdata_file") {
                                       # 5 - Process for rdata file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start activity(ies) data importation from RData file.\n",
                                           sep = "")
                                       load(file = data_path,
                                            envir = tmp_envir <- new.env())
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
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid RData, no R object named \"activities\" available in the R environment provided.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful activity(ies) data importation from RData file.\n",
                                           sep = "")
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
                                             " - Start activity(ies) data importation from R environment.\n",
                                             sep = "")
                                         activity_data <- dplyr::tibble(get(x = "activity",
                                                                            envir = environment_name))
                                         if (paste0(class(x = activity_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = activity_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.\n")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"activity\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful activity(ies) data importation R environment.\n",
                                           sep = "")
                                     }
                                     # 7 - Common data design ----
                                     activity_data <- unclass(x = activity_data)
                                     object_activities <- object_r6(class_name = "activities")
                                     object_activities$add(lapply(X = seq_len(length.out = length(activity_data[[1]])),
                                                                  FUN = function(activity_id) {
                                                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                        " - Start importation of activity element ",
                                                                        activity_id,
                                                                        ".\n",
                                                                        "[activity: ",
                                                                        activity_data[[2]][activity_id],
                                                                        "]\n",
                                                                        sep = "")
                                                                    activity <- activity$new(trip_id = activity_data$trip_id[activity_id],
                                                                                             activity_id = activity_data$activity_id[activity_id],
                                                                                             ocean_code = activity_data$ocean_code[activity_id],
                                                                                             activity_date = activity_data$activity_date[activity_id],
                                                                                             activity_number = activity_data$activity_number[activity_id],
                                                                                             activity_longitude = activity_data$activity_longitude[activity_id],
                                                                                             activity_latitude = activity_data$activity_latitude[activity_id],
                                                                                             set_count = activity_data$set_count[activity_id],
                                                                                             school_type_code = activity_data$school_type_code[activity_id],
                                                                                             activity_code = activity_data$activity_code[activity_id],
                                                                                             activity_label = activity_data$activity_label[activity_id],
                                                                                             time_at_sea = activity_data$time_at_sea[activity_id])
                                                                    cat(format(x = Sys.time(),
                                                                               format = "%Y-%m-%d %H:%M:%S"),
                                                                        " - Successful importation of activity element ",
                                                                        activity_id,
                                                                        ".\n",
                                                                        sep = "")
                                                                    return(activity)
                                                                  }))
                                     private$activities <- object_activities
                                   },
                                   #' @description Creation of a R6 reference object class elementarycatches which contain one or more R6 reference object class elementarycatch.
                                   #' @param data_source  Object of class {\link[base]{character}} expected. By default "observe_database". Identification of data source. You can switch between "observe_database", "avdth_database", "csv_file" (with separator ";" and decimal ","), "rdata_file" or "envir" (for an object in the R environment).
                                   #' @param database_connection Database connection R object expected. By default NULL. Mandatory argument for data source "observe_database" and "avdth_database".
                                   #' @param time_period Object of class {\link[base]{integer}} expected. By default NULL. Year(s) of the reference time period coded on 4 digits. Mandatory for data source "observe_database" and "avdth_database".
                                   #' @param fleet_code Object of class {\link[base]{character}} expected. By default NULL. Country(ies) code related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param ocean_code Object of class {\link[base]{integer}} expected. By default NULL. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param vessel_type_code Object of class {\link[base]{integer}} expected. By default NULL. Vessel type(s) related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param species_fate_code Object of class {\link[base]{integer}} expected. By default NULL. Specie fate(s) related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param trip_id Object of class {\link[base]{character}} expected. By default NULL. Additional parameter only used with data source "observe_database". Use trip(s) identification(s) for selected trip(s) kept in the query. This argument overrides all others arguments like "time_periode", "country" or "ocean".
                                   #' @param data_path Object of class {\link[base]{character}} expected. By default NULL. Path of the data csv/RData file.
                                   #' @param envir Object of class {\link[base]{character}} expected. By default the first environment where data are found will be used. Specify an environment to look in for data source "envir".
                                   elementarycatches_object_creation = function(data_source = "observe_database",
                                                                                database_connection = NULL,
                                                                                time_period = NULL,
                                                                                fleet_code = NULL,
                                                                                ocean_code = NULL,
                                                                                vessel_type_code = NULL,
                                                                                species_fate_code = NULL,
                                                                                trip_id = NULL,
                                                                                data_path = NULL,
                                                                                envir = NULL) {
                                     # 1 - Arguments verifications ----
                                     if (data_source %in% c("observe_database",
                                                            "avdth_database")) {
                                       codama::r_type_checking(r_object = time_period,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = fleet_code,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = ocean_code,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = vessel_type_code,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = species_fate_code,
                                                               type = "integer")
                                     } else if (data_source %in% c("csv_file",
                                                                   "rdata_file")) {
                                       codama::r_type_checking(r_object = data_path,
                                                               type = "character",
                                                               length = 1L)
                                     } else if (data_source != "envir") {
                                       stop(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Invalid \"data_source\" argument.",
                                            "\nCheck function documention through ?object_model_data for more details.")
                                     }
                                     # 2 - Process for observe database ----
                                     if (data_source == "observe_database") {
                                       # specific argument verification
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "PostgreSQLConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument.",
                                              "\nClass \"PostgreSQLConnection\" expected.")
                                       }
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start elementary catches data importation from an Observe database.\n",
                                           sep = "")
                                       if (! is.null(x = trip_id)) {
                                         codama::r_type_checking(r_object = trip_id,
                                                                 type = "character")
                                         elementarycatch_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                           "observe",
                                                                                                           "observe_elementarycatch_selected_trip.sql",
                                                                                                           package = "t3")),
                                                                               collapse = "\n"))
                                         elementarycatch_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                          sql = elementarycatch_sql,
                                                                                          trip_id = DBI::SQL(paste0("'",
                                                                                                                    paste0(trip_id,
                                                                                                                           collapse = "', '"),
                                                                                                                    "'")))
                                       } else {
                                         elementarycatch_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                           "observe",
                                                                                                           "observe_elementarycatch.sql",
                                                                                                           package = "t3")),
                                                                               collapse = "\n"))
                                         elementarycatch_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                          sql = elementarycatch_sql,
                                                                                          begin_time_period = paste0((dplyr::first(time_period,
                                                                                                                                   order_by = time_period) - 1),
                                                                                                                     "-10-01"),
                                                                                          end_time_period = paste0((dplyr::last(time_period,
                                                                                                                                order_by = time_period) + 1),
                                                                                                                   "-03-31"),
                                                                                          fleet_code = DBI::SQL(paste0("'",
                                                                                                                       paste0(fleet_code,
                                                                                                                              collapse = "', '"),
                                                                                                                       "'")),
                                                                                          ocean_code = DBI::SQL(paste0("'",
                                                                                                                       paste0(ocean_code,
                                                                                                                              collapse = "', '"),
                                                                                                                       "'")),
                                                                                          vessel_type_code = DBI::SQL(paste0("'",
                                                                                                                             paste0(vessel_type_code,
                                                                                                                                    collapse = "', '"),
                                                                                                                             "'")),
                                                                                          species_fate_code = DBI::SQL(paste0("'",
                                                                                                                              paste0(species_fate_code,
                                                                                                                                     collapse = "', '"),
                                                                                                                              "'")))
                                       }
                                       cat("[",
                                           elementarycatch_sql_final,
                                           "]\n",
                                           sep = "")
                                       elementarycatch_data <- DBI::dbGetQuery(conn = database_connection,
                                                                               statement = elementarycatch_sql_final)
                                       if (nrow(x = elementarycatch_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and parameters associated.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary catch(es) data importation from an observe database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "avdth_database") {
                                       # 3 - Process for AVDTH database ----
                                       # specific argument verification
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "JDBCConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument.",
                                              "\nClass \"JDBCConnection\" expected.")
                                       }
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start elementary catches data importation from an observe database.\n",
                                           sep = "")
                                       elementarycatch_sql <- DBI::SQL(x = paste(readLines(con = system.file("sql",
                                                                                                             "avdth",
                                                                                                             "avdth_elementarycatch.sql",
                                                                                                             package = "t3")),
                                                                                 collapse = "\n"))
                                       elementarycatch_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                        sql = elementarycatch_sql,
                                                                                        begin_time_period  = DBI::SQL(paste0("#",
                                                                                                                             (dplyr::first(time_period,
                                                                                                                                           order_by = time_period) - 1),
                                                                                                                             "-10-01#")),
                                                                                        end_time_period = DBI::SQL(paste0("#",
                                                                                                                          (dplyr::last(time_period,
                                                                                                                                       order_by = time_period) + 1),
                                                                                                                          "-03-31#")),
                                                                                        fleet_code = DBI::SQL(paste0(paste0(fleet_code,
                                                                                                                            collapse = ", "))),
                                                                                        ocean_code = DBI::SQL(paste0(paste0(ocean_code,
                                                                                                                            collapse = ", "))),
                                                                                        vessel_type_code = DBI::SQL(paste0(paste0(vessel_type_code,
                                                                                                                                  collapse = ", "))))
                                       cat("[",
                                           elementarycatch_sql_final,
                                           "]\n",
                                           sep = "")
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
                                         dplyr::filter(species_fate_code %in% !!species_fate_code) %>%
                                         dplyr::mutate(activity_id = as.character(x = activity_id),
                                                       elementarycatch_id = as.character(x = elementarycatch_id),
                                                       ocean_code = as.integer(x = ocean_code),
                                                       school_type_code = as.integer(x = school_type_code),
                                                       weight_category_code = as.character(x = weight_category_code),
                                                       weight_category_label = as.character(x = weight_category_label),
                                                       species_code = as.integer(x = species_code),
                                                       species_fao_code = as.character(x = species_fao_code),
                                                       species_fate_code = as.integer(x = species_fate_code),
                                                       catch_weight = as.numeric(x = catch_weight))
                                       if (nrow(x = elementarycatch_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and query's parameters.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary catch(es) data importation from an AVDTH database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "csv_file") {
                                       # 4 - Process for csv file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start elementary catch(es) data importation from csv file.\n",
                                           sep = "")
                                       elementarycatch_data <- dplyr::tibble(read.csv2(file = data_path,
                                                                                       stringsAsFactors = FALSE))
                                       if (nrow(x = elementarycatch_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.\n")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary catch(es) data importation from csv file.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "rdata_file") {
                                       # 5 - Process for rdata file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start elementary catch(es) data importation from RData file.\n",
                                           sep = "")
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
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful elementary catch(es) data importation from RData file.\n",
                                           sep = "")
                                     } else if (data_source == "envir") {
                                       # 6 - R environment source ----
                                       # specific argument verification
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
                                             " - Start elementary catch(es) data importation from R environment.\n",
                                             sep = "")
                                         elementarycatch_data <- dplyr::tibble(get(x = "elementarycatch",
                                                                                   envir = environment_name))
                                         if (paste0(class(x = elementarycatch_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = elementarycatch_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.\n")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"elementarycatch\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful elementary catch(es) data importation R environment.\n",
                                           sep = "")
                                     }
                                     # 7 - Common data design ----
                                     elementarycatch_data <- unclass(x = elementarycatch_data)
                                     object_elementarycatches <- object_r6(class_name = "elementarycatches")
                                     object_elementarycatches$add(lapply(X = seq_len(length.out = length(elementarycatch_data[[1]])),
                                                                         FUN = function(elementarycatch_id) {
                                                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                               " - Start importation of elementary catch element ",
                                                                               elementarycatch_id,
                                                                               ".\n",
                                                                               "[elementarycatch: ",
                                                                               elementarycatch_data[[2]][elementarycatch_id],
                                                                               "]\n",
                                                                               sep = "")
                                                                           elementarycatch <- elementarycatch$new(activity_id = elementarycatch_data$activity_id[elementarycatch_id],
                                                                                                                  elementarycatch_id = elementarycatch_data$elementarycatch_id[elementarycatch_id],
                                                                                                                  ocean_code = elementarycatch_data$ocean_code[elementarycatch_id],
                                                                                                                  school_type_code = elementarycatch_data$school_type_code[elementarycatch_id],
                                                                                                                  weight_category_code = elementarycatch_data$weight_category_code[elementarycatch_id],
                                                                                                                  weight_category_label = elementarycatch_data$weight_category_label[elementarycatch_id],
                                                                                                                  species_code = elementarycatch_data$species_code[elementarycatch_id],
                                                                                                                  species_fao_code = elementarycatch_data$species_fao_code[elementarycatch_id],
                                                                                                                  species_fate_code = elementarycatch_data$species_fate_code[elementarycatch_id],
                                                                                                                  catch_weight = elementarycatch_data$catch_weight[elementarycatch_id])
                                                                           cat(format(x = Sys.time(),
                                                                                      format = "%Y-%m-%d %H:%M:%S"),
                                                                               " - Successful importation of elementary catch element ",
                                                                               elementarycatch_id,
                                                                               ".\n",
                                                                               sep = "")
                                                                           return(elementarycatch)
                                                                         }))
                                     private$elementarycatches <- object_elementarycatches
                                   },
                                   #' @description Creation of a R6 reference object class elementarylandings which contain one or more R6 reference object class elementarylanding
                                   #' @param data_source  Object of class {\link[base]{character}} expected. By default "observe_database". Identification of data source. You can switch between "observe_database", "avdth_database", "csv_file" (with separator ";" and decimal ","), "rdata_file" or "envir" (for an object in the R environment).
                                   #' @param database_connection Database connection R object expected. By default NULL. Mandatory argument for data source "observe_database" and "avdth_database".
                                   #' @param time_period Object of class {\link[base]{integer}} expected. By default NULL. Year(s) of the reference time period coded on 4 digits. Mandatory for data source "observe_database" and "avdth_database".
                                   #' @param fleet_code Object of class {\link[base]{character}} expected. By default NULL. Country(ies) code related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param ocean_code Object of class {\link[base]{integer}} expected. By default NULL. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param vessel_type_code Object of class {\link[base]{integer}} expected. By default NULL. Vessel type(s) related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param trip_id Object of class {\link[base]{character}} expected. By default NULL. Additional parameter only used with data source "observe_database". Use trip(s) identification(s) for selected trip(s) kept in the query. This argument overrides all others arguments like "time_periode", "country" or "ocean".
                                   #' @param data_path Object of class {\link[base]{character}} expected. By default NULL. Path of the data csv/RData file.
                                   #' @param envir Object of class {\link[base]{character}} expected. By default the first environment where data are found will be used. Specify an environment to look in for data source "envir".
                                   elementarylandings_object_creation = function(data_source = "observe_database",
                                                                                 database_connection = NULL,
                                                                                 time_period = NULL,
                                                                                 fleet_code = NULL,
                                                                                 ocean_code = NULL,
                                                                                 vessel_type_code = NULL,
                                                                                 trip_id = NULL,
                                                                                 data_path = NULL,
                                                                                 envir = NULL) {
                                     # 1 - Arguments verifications ----
                                     if (data_source %in% c("observe_database",
                                                            "avdth_database")) {
                                       codama::r_type_checking(r_object = time_period,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = fleet_code,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = ocean_code,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = vessel_type_code,
                                                               type = "integer")
                                     } else if (data_source %in% c("csv_file",
                                                                   "rdata_file")) {
                                       codama::r_type_checking(r_object = data_path,
                                                               type = "character",
                                                               length = 1L)
                                     } else if (data_source != "envir") {
                                       stop(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Invalid \"data_source\" argument.",
                                            "\nCheck function documention through ?object_model_data for more details.")
                                     }
                                     # 2 - Process for observe database ----
                                     if (data_source == "observe_database") {
                                       # specific argument verification
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "PostgreSQLConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument.",
                                              "\nClass \"PostgreSQLConnection\" expected.")
                                       }
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start elementary landing(s) data importation from an observe database.\n",
                                           sep = "")
                                       if (! is.null(x = trip_id)) {
                                         codama::r_type_checking(r_object = trip_id,
                                                                 type = "character")
                                         elementarylanding_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                             "observe",
                                                                                                             "observe_elementarylanding_selected_trip.sql",
                                                                                                             package = "t3")),
                                                                                 collapse = "\n"))
                                         elementarylanding_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                            sql = elementarylanding_sql,
                                                                                            trip_id = DBI::SQL(paste0("'",
                                                                                                                      paste0(trip_id,
                                                                                                                             collapse = "', '"),
                                                                                                                      "'")))
                                       } else {
                                         elementarylanding_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                             "observe",
                                                                                                             "observe_elementarylanding.sql",
                                                                                                             package = "t3")),
                                                                                 collapse = "\n"))
                                         elementarylanding_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                            sql = elementarylanding_sql,
                                                                                            begin_time_period = paste0((dplyr::first(time_period,
                                                                                                                                     order_by = time_period) - 1),
                                                                                                                       "-10-01"),
                                                                                            end_time_period = paste0((dplyr::last(time_period,
                                                                                                                                  order_by = time_period) + 1),
                                                                                                                     "-03-31"),
                                                                                            fleet_code = DBI::SQL(paste0("'",
                                                                                                                         paste0(fleet_code,
                                                                                                                                collapse = "', '"),
                                                                                                                         "'")),
                                                                                            ocean_code = DBI::SQL(paste0("'",
                                                                                                                         paste0(ocean_code,
                                                                                                                                collapse = "', '"),
                                                                                                                         "'")),
                                                                                            vessel_type_code = DBI::SQL(paste0("'",
                                                                                                                               paste0(vessel_type_code,
                                                                                                                                      collapse = "', '"),
                                                                                                                               "'")))
                                       }
                                       cat("[",
                                           elementarylanding_sql_final,
                                           "]\n",
                                           sep = "")
                                       elementarylanding_data <- DBI::dbGetQuery(conn = database_connection,
                                                                                 statement = elementarylanding_sql_final)
                                       if (nrow(x = elementarylanding_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and parameters associated.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary landing(s) data importation from an observe database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "avdth_database") {
                                       # 3 - Process for AVDTH database ----
                                       # specific argument verification
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "JDBCConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument.",
                                              "\nClass \"JDBCConnection\" expected.")
                                       }
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start elementary landing(s) data importation from an AVDTH database.\n",
                                           sep = "")
                                       elementarylanding_sql <- paste(readLines(con = system.file("sql",
                                                                                                  "avdth",
                                                                                                  "avdth_elementarylanding.sql",
                                                                                                  package = "t3")),
                                                                      collapse = "\n")
                                       elementarylanding_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                          sql = elementarylanding_sql,
                                                                                          begin_time_period  = DBI::SQL(paste0("#",
                                                                                                                               (dplyr::first(time_period,
                                                                                                                                             order_by = time_period) - 1),
                                                                                                                               "-10-01#")),
                                                                                          end_time_period = DBI::SQL(paste0("#",
                                                                                                                            (dplyr::last(time_period,
                                                                                                                                         order_by = time_period) + 1),
                                                                                                                            "-03-31#")),
                                                                                          fleet_code = DBI::SQL(paste0(paste0(fleet_code,
                                                                                                                              collapse = ", "))),
                                                                                          ocean_code = DBI::SQL(paste0(paste0(ocean_code,
                                                                                                                              collapse = ", "))),
                                                                                          vessel_type_code = DBI::SQL(paste0(paste0(vessel_type_code,
                                                                                                                                    collapse = ", "))))
                                       cat("[",
                                           elementarylanding_sql_final,
                                           "]\n",
                                           sep = "")
                                       elementarylanding_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_connection,
                                                                                               statement = elementarylanding_sql_final)) %>%
                                         dplyr::mutate(trip_id = as.character(trip_id),
                                                       elementarylanding_id = as.character(elementarylanding_id),
                                                       weight_category_code = as.character(weight_category_code),
                                                       weight_category_label = as.character(weight_category_label),
                                                       species_code = as.integer(species_code),
                                                       species_fao_code = as.character(species_fao_code),
                                                       landing_weight = as.numeric(landing_weight))
                                       if (nrow(x = elementarylanding_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and query's parameters.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary landing(s) data importation from avdht database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "csv_file") {
                                       # 4 - Process for csv file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start elementary landing(s) data importation from csv file.\n",
                                           sep = "")
                                       elementarylanding_data <- read.csv2(file = data_path,
                                                                           stringsAsFactors = FALSE)
                                       if (nrow(x = elementarylanding_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.\n")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary landing(s) data importation from csv file.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "rdata_file") {
                                       # 5 - Process for rdata file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start elementary landing(s) data importation from RData file.\n",
                                           sep = "")
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
                                           " - Successful elementary landing(s) data importation from RData file.\n",
                                           sep = "")
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
                                             " - Start elementary landing(s) data importation from R environment.\n",
                                             sep = "")
                                         elementarylanding_data <- dplyr::tibble(get(x = "elementarylanding",
                                                                                     envir = environment_name))
                                         if (paste0(class(x = elementarylanding_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = elementarylanding_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.\n")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"elementarylanding\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful elementary landing(s) data importation R environment.\n",
                                           sep = "")
                                     }
                                     # 7 - Common data design ----
                                     elementarylanding_data <- unclass(x = elementarylanding_data)
                                     object_elementarylandings <- object_r6(class_name = "elementarylandings")
                                     object_elementarylandings$add(lapply(X = seq_len(length.out = length(x = elementarylanding_data[[1]])),
                                                                          FUN = function(elementarylanding_id) {
                                                                            cat(format(x = Sys.time(),
                                                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                                                " - Start importation of elementary landing element ",
                                                                                elementarylanding_id,
                                                                                ".\n",
                                                                                "[elementarylanding: ",
                                                                                elementarylanding_data[[2]][elementarylanding_id],
                                                                                "]\n",
                                                                                sep = "")
                                                                            elementarylanding <- elementarylanding$new(trip_id = elementarylanding_data$trip_id[elementarylanding_id],
                                                                                                                       elementarylanding_id = elementarylanding_data$elementarylanding_id[elementarylanding_id],
                                                                                                                       weight_category_code = elementarylanding_data$weight_category_code[elementarylanding_id],
                                                                                                                       weight_category_label = elementarylanding_data$weight_category_label[elementarylanding_id],
                                                                                                                       species_code = elementarylanding_data$species_code[elementarylanding_id],
                                                                                                                       species_fao_code = elementarylanding_data$species_fao_code[elementarylanding_id],
                                                                                                                       landing_weight = elementarylanding_data$landing_weight[elementarylanding_id])
                                                                            cat(format(x = Sys.time(),
                                                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                                                " - Successful importation of elementary landing(s) element ",
                                                                                elementarylanding_id,
                                                                                ".\n",
                                                                                sep = "")
                                                                            return(elementarylanding)
                                                                          }))
                                     private$elementarylandings <- object_elementarylandings
                                   },
                                   #' @description Creation of a R6 reference object class wells which contain one or more R6 reference object class well, wellset, samples and elementarywellplan.
                                   #' @param data_source  Object of class {\link[base]{character}} expected. By default "observe_database". Identification of data source. You can switch between "observe_database", "avdth_database", "csv_file" (with separator ";" and decimal ","), "rdata_file" or "envir" (for an object in the R environment).
                                   #' @param database_connection Database connection R object expected. By default NULL. Mandatory argument for data source "observe_database" and "avdth_database".
                                   #' @param time_period Object of class {\link[base]{integer}} expected. By default NULL. Year(s) of the reference time period coded on 4 digits. Mandatory for data source "observe_database" and "avdth_database".
                                   #' @param fleet_code Object of class {\link[base]{character}} expected. By default NULL. Country(ies) code related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param ocean_code Object of class {\link[base]{integer}} expected. By default NULL. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param vessel_type_code Object of class {\link[base]{integer}} expected. By default NULL. Vessel type(s) related to data extraction. Necessary argument for data source "observe_database" and "avdth_database".
                                   #' @param sample_type_code Object of class {\link[base]{integer}} expected. By default NULL. Sample type identification.
                                   #' @param trip_id Object of class {\link[base]{character}} expected. By default NULL. Additional parameter only used with data source "observe_database". Use trip(s) identification(s) for selected trip(s) kept in the query. This argument overrides all others arguments like "time_periode", "country" or "ocean".
                                   #' @param data_path_sample Object of class {\link[base]{character}} expected. By default NULL. Path of the data sql/csv file for samples.
                                   #' @param data_path_wellplan Object of class {\link[base]{character}} expected. By default NULL. Path of the data sql/csv file for well plans.
                                   #' @param envir Object of class {\link[base]{character}} expected. By default NULL. Specify an environment to look in for data source "envir".
                                   wells_object_creation = function(data_source = "observe_database",
                                                                    database_connection = NULL,
                                                                    time_period = NULL,
                                                                    fleet_code = NULL,
                                                                    ocean_code = NULL,
                                                                    vessel_type_code = NULL,
                                                                    sample_type_code = NULL,
                                                                    trip_id = NULL,
                                                                    data_path_sample = NULL,
                                                                    data_path_wellplan = NULL,
                                                                    envir = NULL) {
                                     # 1 - Arguments verifications ----
                                     if (data_source %in% c("observe_database",
                                                            "avdth_database")) {
                                       codama::r_type_checking(r_object = time_period,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = fleet_code,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = ocean_code,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = vessel_type_code,
                                                               type = "integer")
                                       codama::r_type_checking(r_object = sample_type_code,
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
                                            " - Invalid \"data_source\" argument.",
                                            "\nCheck function documention through ?object_model_data for more details.")
                                     }
                                     # 2 - Process for observe database ----
                                     if (data_source == "observe_database") {
                                       # specific argument verification
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "PostgreSQLConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument.",
                                              "\nClass \"PostgreSQLConnection\" expected.")
                                       }
                                       # process beginning
                                       # sample(s) importation
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start sample(s) data importation from an observe database.\n",
                                           sep = "")
                                       if (! is.null(x = trip_id)) {
                                         codama::r_type_checking(r_object = trip_id,
                                                                 type = "character")
                                         sample_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                  "observe",
                                                                                                  "observe_sample_selected_trip.sql",
                                                                                                  package = "t3")),
                                                                      collapse = "\n"))
                                         sample_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                 sql = sample_sql,
                                                                                 trip_id = DBI::SQL(paste0("'",
                                                                                                           paste0(trip_id,
                                                                                                                  collapse = "', '"),
                                                                                                           "'")))
                                       } else {
                                         sample_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                  "observe",
                                                                                                  "observe_sample.sql",
                                                                                                  package = "t3")),
                                                                      collapse = "\n"))
                                         sample_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                 sql = sample_sql,
                                                                                 begin_time_period = paste0((dplyr::first(time_period,
                                                                                                                          order_by = time_period) - 1),
                                                                                                            "-10-01"),
                                                                                 end_time_period = paste0((dplyr::last(time_period,
                                                                                                                       order_by = time_period) + 1),
                                                                                                          "-03-31"),
                                                                                 fleet_code = DBI::SQL(paste0("'",
                                                                                                              paste0(fleet_code,
                                                                                                                     collapse = "', '"),
                                                                                                              "'")),
                                                                                 ocean_code = DBI::SQL(paste0("'",
                                                                                                              paste0(ocean_code,
                                                                                                                     collapse = "', '"),
                                                                                                              "'")),
                                                                                 vessel_type_code = DBI::SQL(paste0("'",
                                                                                                                    paste0(vessel_type_code,
                                                                                                                           collapse = "', '"),
                                                                                                                    "'")),
                                                                                 sample_type_code = DBI::SQL(paste0("'",
                                                                                                                    paste0(sample_type_code,
                                                                                                                           collapse = "', '"),
                                                                                                                    "'")))
                                       }
                                       cat("[",
                                           sample_sql_final,
                                           "]\n",
                                           sep = "")
                                       sample_data <- DBI::dbGetQuery(conn = database_connection,
                                                                      statement = sample_sql_final)
                                       if (nrow(x = sample_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and parameters associated.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful sample(s) data importation from an observe database.\n",
                                             sep = "")
                                       }
                                       # well plan(s) importation
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start well plan(s) data importation from an observe database.\n",
                                           sep = "")
                                       if (! is.null(x = trip_id)) {
                                         codama::r_type_checking(r_object = trip_id,
                                                                 type = "character")
                                         wellplan_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                    "observe",
                                                                                                    "observe_wellplan_selected_trip.sql",
                                                                                                    package = "t3")),
                                                                        collapse = "\n"))
                                         wellplan_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                   sql = wellplan_sql,
                                                                                   trip_id = DBI::SQL(paste0("'",
                                                                                                             paste0(trip_id,
                                                                                                                    collapse = "', '"),
                                                                                                             "'")))
                                       } else {
                                         wellplan_sql <- DBI::SQL(paste(readLines(con = system.file("sql",
                                                                                                    "observe",
                                                                                                    "observe_wellplan.sql",
                                                                                                    package = "t3")),
                                                                        collapse = "\n"))
                                         wellplan_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                   sql = wellplan_sql,
                                                                                   begin_time_period = paste0((dplyr::first(time_period,
                                                                                                                            order_by = time_period) - 1),
                                                                                                              "-10-01"),
                                                                                   end_time_period = paste0((dplyr::last(time_period,
                                                                                                                         order_by = time_period) + 1),
                                                                                                            "-03-31"),
                                                                                   fleet_code = DBI::SQL(paste0("'",
                                                                                                                paste0(fleet_code,
                                                                                                                       collapse = "', '"),
                                                                                                                "'")),
                                                                                   ocean_code = DBI::SQL(paste0("'",
                                                                                                                paste0(ocean_code,
                                                                                                                       collapse = "', '"),
                                                                                                                "'")),
                                                                                   vessel_type_code = DBI::SQL(paste0("'",
                                                                                                                      paste0(vessel_type_code,
                                                                                                                             collapse = "', '"),
                                                                                                                      "'")))
                                       }
                                       cat("[",
                                           wellplan_sql_final,
                                           "]\n", sep = "")
                                       wellplan_data <- DBI::dbGetQuery(conn = database_connection,
                                                                        statement = wellplan_sql_final)
                                       if (nrow(x = wellplan_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and parameters associated.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful well plan(s) data importation from an observe database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "avdth_database") {
                                       # 3 - Process for AVDTH database ----
                                       # specific argument verification
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "JDBCConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument.",
                                              "\nClass \"JDBCConnection\" expected.")
                                       }
                                       # process beginning
                                       # sample(s) importation
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start sample(s) data importation from an AVDTH database.\n",
                                           sep = "")
                                       sample_sql <- paste(readLines(con = system.file("sql",
                                                                                       "avdth",
                                                                                       "avdth_sample.sql",
                                                                                       package = "t3")),
                                                           collapse = "\n")
                                       sample_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                               sql = sample_sql,
                                                                               begin_time_period  = DBI::SQL(paste0("#",
                                                                                                                    (dplyr::first(time_period,
                                                                                                                                  order_by = time_period) - 1),
                                                                                                                    "-10-01#")),
                                                                               end_time_period = DBI::SQL(paste0("#",
                                                                                                                 (dplyr::last(time_period,
                                                                                                                              order_by = time_period) + 1),
                                                                                                                 "-03-31#")),
                                                                               fleet_code = DBI::SQL(paste0(paste0(fleet_code,
                                                                                                                   collapse = ", "))),
                                                                               ocean_code = DBI::SQL(paste0(paste0(ocean_code,
                                                                                                                   collapse = ", "))),
                                                                               vessel_type_code = DBI::SQL(paste0(paste0(vessel_type_code,
                                                                                                                         collapse = ", "))),
                                                                               sample_type_code = DBI::SQL(paste0(sample_type_code,
                                                                                                                  collapse = ", ")))
                                       cat("[",
                                           sample_sql_final,
                                           "]\n",
                                           sep = "")
                                       sample_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_connection,
                                                                                    statement = sample_sql_final)) %>%
                                         dplyr::mutate(trip_id = as.character(trip_id),
                                                       well_id = as.character(well_id),
                                                       well_minus10_weigth = as.numeric(well_minus10_weigth),
                                                       well_plus10_weigth = as.numeric(well_plus10_weigth),
                                                       well_global_weigth = as.numeric(well_global_weigth),
                                                       sample_id = as.character(sample_id),
                                                       sub_sample_id = as.integer(sub_sample_id),
                                                       sub_sample_total_count_id = as.character(sub_sample_total_count_id),
                                                       elementarysampleraw_id = as.character(paste0(elementarysampleraw_id,
                                                                                                    ".",
                                                                                                    dplyr::row_number())),
                                                       sample_quality_code = as.integer(sample_quality_code),
                                                       sample_type_code = as.integer(x = sample_type_code),
                                                       species_code = as.integer(x = species_code),
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
                                             " - Successful sample(s) data importation from avdht database.\n",
                                             sep = "")
                                       }
                                       # well plan(s) importation
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start well plan(s) data importation from an AVDTH database.\n",
                                           sep = "")
                                       wellplan_sql <- paste(readLines(con = system.file("sql",
                                                                                         "avdth",
                                                                                         "avdth_wellplan.sql",
                                                                                         package = "t3")),
                                                             collapse = "\n")
                                       wellplan_sql_final <- DBI::sqlInterpolate(conn = database_connection,
                                                                                 sql = wellplan_sql,
                                                                                 begin_time_period  = DBI::SQL(paste0("#",
                                                                                                                      (dplyr::first(time_period,
                                                                                                                                    order_by = time_period) - 1),
                                                                                                                      "-10-01#")),
                                                                                 end_time_period = DBI::SQL(paste0("#",
                                                                                                                   (dplyr::last(time_period,
                                                                                                                                order_by = time_period) + 1),
                                                                                                                   "-03-31#")),
                                                                                 fleet_code = DBI::SQL(paste0(paste0(fleet_code,
                                                                                                                     collapse = ", "))),
                                                                                 ocean_code = DBI::SQL(paste0(paste0(ocean_code,
                                                                                                                     collapse = ", "))),
                                                                                 vessel_type_code = DBI::SQL(paste0(paste0(vessel_type_code,
                                                                                                                           collapse = ", "))))
                                       cat("[",
                                           wellplan_sql_final,
                                           "]\n",
                                           sep = "")
                                       wellplan_data <- dplyr::tibble(DBI::dbGetQuery(conn = database_connection,
                                                                                      statement = wellplan_sql_final)) %>%
                                         dplyr::mutate(wellplan_id = as.character(wellplan_id),
                                                       well_id = as.character(well_id),
                                                       activity_id = as.character(activity_id),
                                                       sample_id = as.character(sample_id),
                                                       species_code = as.integer(species_code),
                                                       species_fao_code = as.character(species_fao_code),
                                                       wellplan_weight = as.numeric(wellplan_weight),
                                                       wellplan_count = as.integer(wellplan_count),
                                                       weight_category_code = as.character(weight_category_code),
                                                       weight_category_label = as.character(weight_category_label))
                                       if (nrow(x = wellplan_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and query's parameters.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful well plan(s) data importation from avdht database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "csv_file") {
                                       # 4 - Process for csv file ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start sample(s) data importation from csv file.\n",
                                           sep = "")
                                       sample_data <- read.csv2(file = data_path_sample,
                                                                stringsAsFactors = FALSE)
                                       if (nrow(x = sample_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.\n")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful samples(s) data importation from csv file.\n",
                                             sep = "")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start well plan(s) data importation from csv file.\n",
                                           sep = "")
                                       wellplan_data <- read.csv2(file = data_path_wellplan,
                                                                  stringsAsFactors = FALSE)
                                       if (nrow(x = wellplan_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.\n")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful well plans(s) data importation from csv file.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "rdata_file") {
                                       # 5 - Process for rdata file ----
                                       # process beginning
                                       # sample(s) importation
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start sample(s) data importation from RData file.\n",
                                           sep = "")
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
                                           " - Successful sample(s) data importation from RData file.\n",
                                           sep = "")
                                       # well plan(s) importation
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start well plan(s) data importation from RData file.\n",
                                           sep = "")
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
                                           " - Successful well plan(s) data importation from RData file.\n",
                                           sep = "")
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
                                             " - Start sample(s) data importation from R environment.\n",
                                             sep = "")
                                         sample_data <- dplyr::tibble(get(x = "sample",
                                                                          envir = environment_name))
                                         if (paste0(class(x = sample_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = sample_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.\n")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"sample\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful sample(s) data importation R environment.\n",
                                           sep = "")
                                       # well plan(s) importation
                                       if (exists(x = "wellplan",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start well plan(s) data importation from R environment.\n",
                                             sep = "")
                                         wellplan_data <- dplyr::tibble(get(x = "wellplan",
                                                                            envir = environment_name))
                                         if (paste0(class(x = wellplan_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = wellplan_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.\n")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"wellplan\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful well plan(s) data importation R environment.\n",
                                           sep = "")
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
                                           "]\n",
                                           sep = "")
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
                                             "]\n",
                                             sep = "")
                                         if (is.na(x = well_id)) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Warning: missing \"well_id\" argument in trip number: \"",
                                               trip_id,
                                               "\".\n",
                                               sep = "")
                                           tmp_well <- dplyr::filter(.data = tmp_trip,
                                                                     is.na(well_id))
                                           if (length(x = unique(x = tmp_well$sample_id)) != 1) {
                                             cat(format(x = Sys.time(),
                                                        format = "%Y-%m-%d %H:%M:%S"),
                                                 " - Warning: well unknown identify in trip number \"",
                                                 trip_id,
                                                 "\" have more than one sampling associated.\n",
                                                 "Data avoided for model incrementation.\n",
                                                 sep = "")
                                             next()
                                           }
                                         } else {
                                           tmp_well <- dplyr::filter(.data = tmp_trip,
                                                                     well_id == !!well_id)
                                         }
                                         if (length(unique(x = tmp_well$well_minus10_weigth)) != 1
                                             | length(unique(x = tmp_well$well_plus10_weigth)) != 1
                                             | length(unique(x = tmp_well$well_global_weigth)) != 1) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Warning: at least one well data (\"well_minus10_weigth\", \"well_plus10_weigth\" and \"well_global_weigth\") is different between well samples. Only the first element will use.\n",
                                               "[trip: ",
                                               trip_id,
                                               ", well: ",
                                               well_id,
                                               "]\n",
                                               sep = "")
                                         }
                                         object_well <- well$new(trip_id = trip_id,
                                                                 well_id = well_id,
                                                                 well_minus10_weigth = unique(x = tmp_well$well_minus10_weigth)[[1]],
                                                                 well_plus10_weigth = unique(x = tmp_well$well_plus10_weigth)[[1]],
                                                                 well_global_weigth = unique(x = tmp_well$well_global_weigth[[1]]))
                                         for (sample_id in unique(x = tmp_well$sample_id)) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Start importation of sample data item ",
                                               which(x = unique(tmp_well$sample_id) == sample_id),
                                               ".\n",
                                               "[sample: ",
                                               sample_id,
                                               "]\n",
                                               sep = "")
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
                                                                                                                                                   species_code = tmp_sample$species_code[i],
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
                                               "]\n",
                                               sep = "")
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start importation of well plan data item ",
                                             which(x = unique(x = wellplan_data$well_id) == well_id),
                                             ".\n",
                                             "[well: ",
                                             well_id,
                                             "]\n",
                                             sep = "")
                                         tmp_wellplan <- dplyr::filter(.data = wellplan_data,
                                                                       well_id == !!well_id)
                                         tmp_wellplan <- unclass(x = tmp_wellplan)
                                         object_well$.__enclos_env__$private$wellplan <- lapply(X = seq_len(length.out = length(x = tmp_wellplan[[1]])),
                                                                                                FUN = function(j) {
                                                                                                  elementarywellplan$new(wellplan_id = tmp_wellplan$wellplan_id[j],
                                                                                                                         well_id = tmp_wellplan$well_id[j],
                                                                                                                         activity_id = tmp_wellplan$activity_id[j],
                                                                                                                         sample_id = tmp_wellplan$sample_id[j],
                                                                                                                         species_code = tmp_wellplan$species_code[j],
                                                                                                                         species_fao_code = tmp_wellplan$species_fao_code[j],
                                                                                                                         wellplan_weight = tmp_wellplan$wellplan_weight[j],
                                                                                                                         wellplan_count = tmp_wellplan$wellplan_count[j],
                                                                                                                         weight_category_code = tmp_wellplan$weight_category_code[j],
                                                                                                                         weight_category_label = tmp_wellplan$weight_category_label[j])
                                                                                                })
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Sucessful importation of well plan data item ",
                                             which(x = unique(x = wellplan_data$well_id) == well_id),
                                             ".\n",
                                             "[well: ",
                                             well_id,
                                             "]\n",
                                             sep = "")
                                         object_wells$add(object_well)
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful importation of well data item ",
                                             which(x = unique(x = tmp_trip$well_id) == well_id),
                                             ".\n",
                                             "[well: ",
                                             well_id,
                                             "]\n",
                                             sep = "")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful importation of well(s) data for trip element ",
                                           which(x = unique(x = sample_data$trip_id) == trip_id),
                                           ".\n",
                                           "[trip: ",
                                           trip_id,
                                           "]\n",
                                           sep = "")
                                     }
                                     private$wells <- object_wells
                                   },
                                   #' @description Creation of a data frame object with parameters of set duration algorithms.
                                   #' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "t3_db" but you can switch to "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R environment).
                                   #' @param db_con Database connection R object expected. Mandatory argument for data source "t3_db", "avdth_db" and "sql_query".
                                   #' @param data_path Object of class {\link[base]{character}} expected. Path of the data sql/csv/RData file. By default NULL.
                                   #' @param envir Object of class {\link[base]{character}} expected. Specify an environment to look in for data source "envir". By default the first environment where data are found will be used.
                                   setdurationrefs_data = function(database_connection = NULL,
                                                                   data_source = "t3_db",
                                                                   data_path = NULL,
                                                                   envir = NULL) {
                                     # 1 - Common arguments verification
                                     if (codama::r_type_checking(r_object = data_source,
                                                                 type = "character",
                                                                 length = 1L,
                                                                 allowed_value = c("t3_db",
                                                                                   "sql_query",
                                                                                   "csv",
                                                                                   "rdata",
                                                                                   "envir"),
                                                                 output = "logical") != TRUE) {
                                       codama::r_type_checking(r_object = data_source,
                                                               type = "character",
                                                               length = 1L,
                                                               allowed_value = c("t3_db",
                                                                                 "sql_query",
                                                                                 "csv",
                                                                                 "rdata",
                                                                                 "envir"),
                                                               output = "message")
                                       stop()
                                     }
                                     # 2 - Global process
                                     if (data_source == "t3_db") {
                                       # t3 db source ----
                                       # specific arguments verification
                                       if (! inherits(x = db_con,
                                                      what = "PostgreSQLConnection")) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                       }
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start set duration data importation from T3 database.\n",
                                           sep = "")
                                       setdurationrefs_sql <- paste(readLines(con = system.file("sql",
                                                                                                "t3_setdurationrefs.sql",
                                                                                                package = "t3")),
                                                                    collapse = "\n")
                                       setdurationrefs_sql_final <- DBI::SQL(setdurationrefs_sql)
                                       cat("[",
                                           setdurationrefs_sql_final,
                                           "]\n",
                                           sep = "")
                                       set_duration_refs_data <- DBI::dbGetQuery(conn = db_con,
                                                                                 statement = setdurationrefs_sql_final)
                                       if (nrow(x = set_duration_refs_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data imported, check the query and query's parameters.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful set duration data importation from T3 database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "sql_query") {
                                       # sql queries source ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start set duration data importation from the database.\n",
                                           sep = "")
                                       setdurationrefs_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                                 collapse = "\n"))
                                       cat("[", setdurationrefs_sql, "]\n", sep = "")
                                       set_duration_refs_data <- DBI::dbGetQuery(conn = db_con,
                                                                                 statement = setdurationrefs_sql)
                                       if (nrow(x = set_duration_refs_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data imported, check the query.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful set duration data importation from the database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "csv") {
                                       # csv source ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful set duration data importation from csv file.\n",
                                           sep = "")
                                       set_duration_refs_data <- read.csv2(file = data_path,
                                                                           stringsAsFactors = FALSE)
                                       if (nrow(x = set_duration_refs_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data imported, check the csv file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         set_duration_refs_data <- dplyr::mutate(.data = set_duration_refs_data,
                                                                                 year = as.integer(year),
                                                                                 country = as.character(country),
                                                                                 ocean = as.integer(ocean),
                                                                                 school_type = as.integer(school_type),
                                                                                 parameter_a = as.numeric(parameter_a),
                                                                                 parameter_b = as.numeric(parameter_b),
                                                                                 null_set_value = as.numeric(null_set_value))
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful set duration data importation from csv file.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "rdata") {
                                       # rdata source ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start set duration references data importation from RData.\n",
                                           sep = "")
                                       load(file = data_path,
                                            envir = tmp_envir <- new.env())
                                       if (exists(x = "setdurationrefs",
                                                  envir = tmp_envir)) {
                                         set_duration_refs_data <- get(x = "setdurationrefs",
                                                                       envir = tmp_envir)
                                         if (! inherits(x = set_duration_refs_data,
                                                        what = "data.frame")) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"set_duration_refs_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"setdurationrefs\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(x = set_duration_refs_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"setdurationrefs\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful set duration references data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       # R envrionment source ----
                                       # specific arguments verification
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "setdurationrefs")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       # process beginning
                                       if (exists(x = "setdurationrefs",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start set duration references data importation from R environment.\n",
                                             sep = "")
                                         set_duration_refs_data <- get(x = "setdurationrefs",
                                                                       envir = environment_name)
                                         if (! inherits(x = set_duration_refs_data,
                                                        what = "data.frame")) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"set_duration_refs_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(x = set_duration_refs_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"elementary landings\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful set duration references data importation R environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"setdurationrefs\" available in the R environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     }
                                     private$setdurationrefs <- set_duration_refs_data
                                   },
                                   #' @description Creation of a data frame object with length ratio between ld1 and lf class.
                                   #' @param data_source Object of class {\link[base]{character}} expected. By default "observe_database". Identification of data source. You can switch to "csv_file" (with separator character ";" and decimal ","), "rdata_file" or "envir" (for an object in the R environment).
                                   #' @param database_connection Database connection R object expected. Mandatory argument for data source "observe_database".
                                   #' @param data_path Object of class {\link[base]{character}} expected. By default NULL. Mandatory argument for data source "csv_file", "rdata_file" or "envir". Path of the data file.
                                   #' @param envir Object of class {\link[base]{character}} expected. By default NULL. Specify an environment to look in for data source "envir".
                                   lengthsteps_data = function(data_source = "observe_database",
                                                               database_connection = NULL,
                                                               data_path = NULL,
                                                               envir = NULL) {
                                     # 1 - Arguments verifications ----
                                     if (data_source == "observe_database") {
                                       if (paste0(class(x = database_connection),
                                                  collapse = " ") != "PostgreSQLConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument.",
                                              "\nClass \"PostgreSQLConnection\" expected.")
                                       }
                                     } else if (data_source %in% c("csv_file",
                                                                   "rdata_file")) {
                                       codama::r_type_checking(r_object = data_path,
                                                               type = "character",
                                                               length = 1L)
                                     } else if (data_source != "envir") {
                                       stop(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Invalid \"data_source\" argument.",
                                            "\nCheck function documention through ?object_model_data for more details.")
                                     }
                                     if (data_source == "t3_db") {
                                       # t3 db source ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start length steps data importation from T3 database.\n",
                                           sep = "")
                                       lengthstep_sql <- paste(readLines(con = system.file("sql",
                                                                                           "t3_lengthsteps.sql",
                                                                                           package = "t3")),
                                                               collapse = "\n")
                                       cat("[", lengthstep_sql, "]\n", sep = "")
                                       lengthsteps_data <- DBI::dbGetQuery(conn = db_con,
                                                                           statement = lengthstep_sql)
                                       if (nrow(x = lengthsteps_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data imported, check the query and query's parameters.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length steps data importation from T3 database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "sql_query") {
                                       # sql queries source ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start length steps data importation from the database.\n",
                                           sep = "")
                                       lengthstep_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                            collapse = "\n"))
                                       cat("[", lengthstep_sql, "]\n", sep = "")
                                       lengthsteps_data <- DBI::dbGetQuery(conn = db_con,
                                                                           statement = lengthstep_sql)
                                       if (nrow(x = lengthsteps_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data imported, check the query.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length steps data importation from the database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "sql_query") {
                                       # sql queries source ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start length steps data importation from the database.\n",
                                           sep = "")
                                       lengthstep_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                            collapse = "\n"))
                                       cat("[", lengthstep_sql, "]\n", sep = "")
                                       lengthsteps_data <- DBI::dbGetQuery(conn = db_con,
                                                                           statement = lengthstep_sql)
                                       if (nrow(x = lengthsteps_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data imported, check the query.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length steps data importation from the database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "csv") {
                                       # csv source ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start length steps data importation from csv file.\n",
                                           sep = "")
                                       lengthsteps_data <- read.csv2(file = data_path,
                                                                     stringsAsFactors = FALSE)
                                       if (nrow(x = lengthsteps_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data imported, check the csv file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         lengthsteps_data <- dplyr::mutate(.data = lengthsteps_data,
                                                                           ocean = as.integer(x = ocean),
                                                                           specie_code = as.integer(x = specie_code),
                                                                           specie_code3l = as.character(x = specie_code3l),
                                                                           ld1_class = as.numeric(x = ld1_class),
                                                                           lf_class = as.integer(x = lf_class),
                                                                           ratio = as.numeric(x = ratio))
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length steps data importation from csv file.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "rdata") {
                                       # rdata source ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start length step data importation from RData.\n",
                                           sep = "")
                                       load(file = data_path,
                                            envir = tmp_envir <- new.env())
                                       if (exists(x = "lengthsteps",
                                                  envir = tmp_envir)) {
                                         lengthsteps_data <- get(x = "lengthsteps",
                                                                 envir = tmp_envir)
                                         if (! inherits(x = lengthsteps_data,
                                                        what = "data.frame")) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"lengthsteps_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"lengthsteps\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(x = lengthsteps_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"lengthsteps\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length step data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       # R environment source ----
                                       # specific arguments verification
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "lengthsteps")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       # process beginning
                                       if (exists(x = "lengthsteps",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start length step data importation from R environment.\n",
                                             sep = "")
                                         lengthsteps_data <- get(x = "lengthsteps",
                                                                 envir = environment_name)
                                         if (! inherits(x = lengthsteps_data,
                                                        what = "data.frame")) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"lengthsteps_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(x = lengthsteps_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"length step\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length step data importation R environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"lengthsteps\" available in the R environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     }
                                     private$lengthsteps <- lengthsteps_data
                                   },
                                   #' @description Creation of a data frame object with weighted weigth of each set sampled.
                                   #' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "t3_db" but you can switch to "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R environment).
                                   #' @param db_con Database connection R object expected. Mandatory argument for data source "t3_db", "avdth_db" and "sql_query".
                                   #' @param periode_reference Object of class {\link[base]{integer}} expected. Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries Object of class {\link[base]{character}} expected. ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param oceans Object of class {\link[base]{integer}} expected. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path Object of class {\link[base]{character}} expected. Path of the data sql/csv/RData file. By default NULL.
                                   #' @param trips_selected Object of class {\link[base]{character}} expected. Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   #' @param envir Object of class {\link[base]{character}} expected. Specify an environment to look in for data source "envir". By default the first environment where data are found will be used.
                                   samplesets_data = function(data_source = "t3_db",
                                                              db_con = NULL,
                                                              periode_reference = NULL,
                                                              countries = NULL,
                                                              oceans = NULL,
                                                              data_path = NULL,
                                                              trips_selected = NULL,
                                                              envir = NULL) {
                                     # common arguments verification ----
                                     if (data_source %in% c("t3_db",
                                                            "avdth_db")) {
                                       if (paste0(class(x = periode_reference),
                                                  collapse = " ") != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument.\n",
                                             sep = "")
                                         stop()
                                       } else if (paste0(class(x = countries),
                                                         collapse = " ") != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument.\n",
                                             sep = "")
                                         stop()
                                       } else if (paste0(class(x = oceans),
                                                         collapse = " ") != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument.\n",
                                             sep = "")
                                       }
                                     } else if (data_source %in% c("sql_query",
                                                                   "csv",
                                                                   "rdata")) {
                                       if (paste0(class(x = data_path),
                                                  collapse = " ") != "character"
                                           || length(x = data_path) != 1
                                           || (! file.exists(data_path))) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else if (data_source != "envir") {
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument.\n",
                                           sep = "")
                                       stop()
                                     }
                                     if (data_source == "t3_db") {
                                       # t3 db source ----
                                       # specific arguments verification
                                       if (! inherits(x = db_con,
                                                      what = "PostgreSQLConnection")) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                       }
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start sample sets data importation from T3 database.\n",
                                           sep = "")
                                       samplesets_sql <- paste(readLines(con = system.file("sql",
                                                                                           "t3_samplesets.sql",
                                                                                           package = "t3")),
                                                               collapse = "\n")
                                       if (! is.null(x = trips_selected)) {
                                         if (! inherits(x = trips_selected,
                                                        what = "character")) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"trips_selected\" argument, ",
                                               "class \"character\" expected if not NULL.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           samplesets_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                       sql = samplesets_sql,
                                                                                       begin_period = paste0((dplyr::first(periode_reference,
                                                                                                                           order_by = periode_reference) - 1),
                                                                                                             "-10-01"),
                                                                                       end_period = paste0((dplyr::last(periode_reference,
                                                                                                                        order_by = periode_reference) + 1),
                                                                                                           "-03-31"),
                                                                                       countries = DBI::SQL(paste0("'",
                                                                                                                   paste0(countries,
                                                                                                                          collapse = "', '"),
                                                                                                                   "'")),
                                                                                       oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                       collapse = ", "))),
                                                                                       trips_selected = DBI::SQL(paste0("'",
                                                                                                                        paste0(trips_selected,
                                                                                                                               collapse = "', '"),
                                                                                                                        "'")))
                                         }
                                       } else {
                                         samplesets_sql <- sub(pattern = "\n\tAND t.topiaid IN (?trips_selected)",
                                                               replacement = "",
                                                               x = samplesets_sql,
                                                               fixed = TRUE)
                                         samplesets_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                     sql = samplesets_sql,
                                                                                     begin_period = paste0((dplyr::first(periode_reference,
                                                                                                                         order_by = periode_reference) - 1),
                                                                                                           "-10-01"),
                                                                                     end_period = paste0((dplyr::last(periode_reference,
                                                                                                                      order_by = periode_reference) + 1),
                                                                                                         "-03-31"),
                                                                                     countries = DBI::SQL(paste0("'",
                                                                                                                 paste0(countries,
                                                                                                                        collapse = "', '"),
                                                                                                                 "'")),
                                                                                     oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                     collapse = ", "))))
                                       }
                                       cat("[", samplesets_sql_final, "]\n", sep = "")
                                       samplesets_data <- DBI::dbGetQuery(conn = db_con,
                                                                          statement = samplesets_sql_final)
                                       if (nrow(x = samplesets_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data imported, check the query and query's parameters.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful sample sets data importation from T3 database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "avdth_db") {
                                       # avdth db source ----
                                       # specific arguments verification
                                       if (! inherits(x = db_con,
                                                      what = "JDBCConnection")) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"JDBCConnection\" expected.\n",
                                             sep = "")
                                       }
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start sample sets data importation from avdth database.\n",
                                           sep = "")
                                       samplesets_sql <- paste(readLines(con = system.file("sql",
                                                                                           "avdth",
                                                                                           "avdth_samplesets.sql",
                                                                                           package = "t3")),
                                                               collapse = "\n")
                                       samplesets_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                   sql = samplesets_sql,
                                                                                   begin_period = DBI::SQL(paste0("#",
                                                                                                                  (dplyr::first(periode_reference,
                                                                                                                                order_by = periode_reference) - 1),
                                                                                                                  "-10-01#")),
                                                                                   end_period = DBI::SQL(paste0("#",
                                                                                                                (dplyr::last(periode_reference,
                                                                                                                             order_by = periode_reference) + 1),
                                                                                                                "-03-31#")),
                                                                                   countries = DBI::SQL(paste0("'",
                                                                                                               paste0(countries,
                                                                                                                      collapse = "', '"),
                                                                                                               "'")),
                                                                                   oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                   collapse = ", "))))

                                       cat("[", samplesets_sql_final, "]\n", sep = "")
                                       samplesets_data <- dplyr::tibble(DBI::dbGetQuery(conn = db_con,
                                                                                        statement = samplesets_sql_final)) %>%
                                         dplyr::mutate(trip_id = as.character(trip_id),
                                                       activity_id = as.character(activity_id),
                                                       well_id = as.character(well_id),
                                                       sample_id = as.character(sample_id),
                                                       well_set_weighted_weight = as.numeric(well_set_weighted_weight))
                                       if (nrow(x = samplesets_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data imported, check the query and query's parameters.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful sample sets data importation from avdth database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "sql_query") {
                                       # sql queries source ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start sample sets data importation from the database.\n",
                                           sep = "")
                                       samplesets_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                            collapse = "\n"))
                                       cat("[", samplesets_sql, "]\n", sep = "")
                                       samplesets_data <- DBI::dbGetQuery(conn = db_con,
                                                                          statement = samplesets_sql)
                                       if (nrow(x = samplesets_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data imported, check the query.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful sample sets data importation from the database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "csv") {
                                       # csv source ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start sample sets data importation from csv file.\n",
                                           sep = "")
                                       samplesets_data <- read.csv2(file = data_path,
                                                                    stringsAsFactors = FALSE)
                                       if (nrow(x = samplesets_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data imported, check the csv file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful sample sets data importation from csv file.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "rdata") {
                                       # rdata source ----
                                       # process beginning
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start samples set data importation from RData.\n",
                                           sep = "")
                                       load(file = data_path,
                                            envir = tmp_envir <- new.env())
                                       if (exists(x = "samplesets",
                                                  envir = tmp_envir)) {
                                         samplesets_data <- get(x = "samplesets",
                                                                envir = tmp_envir)
                                         if (! inherits(x = samplesets_data,
                                                        what = "data.frame")) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"samplesets_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"samplesets\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(x = samplesets_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"samplesets\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful samples set data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       # specific arguments verification
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "samplesets")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       # process beginning
                                       if (exists(x = "samplesets",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples set data importation from R environment.\n",
                                             sep = "")
                                         samplesets_data <- get(x = "samplesets",
                                                                envir = environment_name)
                                         if (! inherits(x = samplesets_data,
                                                        what = "data.frame")) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"samplesets_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(x = samplesets_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"samples set\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful samples set data importation R environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"samplesets\" available in the R environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     }
                                     private$samplesets <- samplesets_data
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
                                                  collapse = " ") != "PostgreSQLConnection") {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"database_connection\" argument.",
                                              "\nClass \"PostgreSQLConnection\" expected.")
                                       }
                                     } else if (data_source %in% c("csv_file",
                                                                   "rdata_file")) {
                                       codama::r_type_checking(r_object = data_path,
                                                               type = "character",
                                                               length = 1L)
                                     } else if (data_source != "envir") {
                                       stop(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Invalid \"data_source\" argument.",
                                            "\nCheck function documention through ?object_model_data for more details.")
                                     }
                                     # 2 - Process for observe database ----
                                     if (data_source == "observe_database") {
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start length weight relationship(s) data importation from an observe database.\n",
                                           sep = "")
                                       lengthweightrelationship_sql <- paste(readLines(con = system.file("sql",
                                                                                                         "observe",
                                                                                                         "observe_lengthweightrelationship.sql",
                                                                                                         package = "t3")),
                                                                             collapse = "\n")
                                       cat("[",
                                           lengthweightrelationship_sql,
                                           "]\n",
                                           sep = "")
                                       lengthweightrelationship_data <- DBI::dbGetQuery(conn = database_connection,
                                                                                        statement = lengthweightrelationship_sql)
                                       if (nrow(x = lengthweightrelationship_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check the query and parameters associated.")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length weight relationship(s) data importation from an observe database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "csv_file") {
                                       # 4 - Process for csv file ----
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start length weight relationship(s) data importation from csv file.\n",
                                           sep = "")
                                       lengthweightrelationship_data <- read.csv2(file = data_path,
                                                                                  stringsAsFactors = FALSE)
                                       if (nrow(x = lengthweightrelationship_data) == 0) {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No data imported, check your csv file.\n")
                                       } else {
                                         lengthweightrelationship_data <- dplyr::mutate(.data = lengthweightrelationship_data,
                                                                                        ocean_code = as.integer(x = ocean_code),
                                                                                        ocean_label = as.character(x = ocean_label),
                                                                                        species_code = as.integer(x = species_code),
                                                                                        species_fao_code = as.character(x = species_fao_code),
                                                                                        length_weight_formula = as.character(x = length_weight_formula),
                                                                                        lwr_a = as.numeric(x = lwr_a),
                                                                                        lwr_b = as.numeric(x = lwr_b))
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length weight relationship(s) data importation from csv file.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "rdata_file") {
                                       # 5 - Process for rdata file ----
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start length weight relationship(s) data importation from RData file.\n",
                                           sep = "")
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
                                           " - Successful length weight relationship(s) data importation from RData file.\n",
                                           sep = "")
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
                                             " - Start length weight relationship(s) data importation from R environment.\n",
                                             sep = "")
                                         lengthweightrelationship_data <- dplyr::tibble(get(x = "lengthweightrelationship",
                                                                                            envir = environment_name))
                                         if (paste0(class(x = lengthweightrelationship_data),
                                                    collapse = " ") != "tbl_df tbl data.frame"
                                             || nrow(x = lengthweightrelationship_data) == 0) {
                                           stop(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - No data imported, check the class of your RData file or data inside.\n")
                                         }
                                       } else {
                                         stop(format(x = Sys.time(),
                                                     format = "%Y-%m-%d %H:%M:%S"),
                                              " - No R object named \"lengthweightrelationship\" available in the R environment.")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful length weight relationship(s) data importation R environment.\n",
                                           sep = "")
                                     }
                                     private$lengthweightrelationships <- lengthweightrelationship_data
                                   }
                                 ),
                                 private = list(
                                   trips = NULL,
                                   activities = NULL,
                                   elementarycatches = NULL,
                                   elementarylandings = NULL,
                                   wells = NULL,
                                   setdurationrefs = NULL,
                                   lengthsteps = NULL,
                                   lengthweightrelationships = NULL,
                                   samplesets = NULL
                                 ))
