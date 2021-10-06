#' @title R6 class object_model_data
#' @name object_model_data
#' @description Create R6 reference object class object_model_data
#' @importFrom R6 R6Class
#' @importFrom DBI sqlInterpolate SQL dbGetQuery
#' @importFrom dplyr first last filter
#' @importFrom tools file_ext
object_model_data <- R6::R6Class(classname = "object_model_data",
                                 public = list(
                                   #' @description Creation of a R6 reference object class trips which contain one or more R6 reference object class trip.
                                   #' @param data_source  Object of class {\link[base]{character}} expected. Identification of data source. By default "t3_db" but you can switch to "avdth_db", "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R environment).
                                   #' @param db_con Object of class "database" expected. Check {\link[dbConnect]{dbConnect}}. An R's object which contain connection identifiers for a database. Necessary argument for data source "t3_db", "avdth_db" and "sql_query".
                                   #' @param periode_reference Object of class {\link[base]{integer}} expected. Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db" and "avdth_db". By default NULL.
                                   #' @param countries Object of class {\link[base]{character}} expected. ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db" and "avdth_db". By default NULL.
                                   #' @param oceans Object of class {\link[base]{integer}} expected. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "t3_db" and "avdth_db". By default NULL.
                                   #' @param data_path Object of class {\link[base]{character}} expected. Path of the data sql/csv/RData file. By default NULL.
                                   #' @param trips_selected Object of class {\link[base]{character}} expected. Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   #' @param envir Object of class {\link[base]{character}} expected. Specify an environment to look in for data source "envir". By default the first environment where data are found will be used.
                                   trips_object_creation = function(data_source = "t3_db",
                                                                    db_con = NULL,
                                                                    periode_reference = NULL,
                                                                    countries = NULL,
                                                                    oceans = NULL,
                                                                    data_path = NULL,
                                                                    trips_selected = NULL,
                                                                    envir = NULL) {
                                     if (data_source == "t3_db") {
                                       # t3 db source ----
                                       if (length(x = class(x = periode_reference)) != 1
                                           || class(x = periode_reference) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (length(x = class(x = countries)) != 1
                                                  || class(x = countries) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (length(x = class(x = oceans)) != 1
                                                  || class(x = oceans) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(x = db_con) != "PostgreSQLConnection") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from T3 database.\n",
                                             sep = "")
                                         trips_sql <- paste(readLines(con = system.file("sql",
                                                                                        "t3_trips.sql",
                                                                                        package = "t3")),
                                                            collapse = "\n")
                                         if (! is.null(trips_selected)) {
                                           if (class(x = trips_selected) != "character") {
                                             cat(format(x = Sys.time(),
                                                        format = "%Y-%m-%d %H:%M:%S"),
                                                 " - Error: invalid \"trips_selected\" argument, ",
                                                 "class \"character\" expected if not NULL.\n",
                                                 sep = "")
                                             stop()
                                           } else {
                                             trips_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                    sql = trips_sql,
                                                                                    begin_period = paste0((dplyr::first(periode_reference,
                                                                                                                        order_by = periode_reference) - 1),
                                                                                                          "-10-01"),
                                                                                    end_period = paste0((dplyr::last(periode_reference,
                                                                                                                     order_by = periode_reference) + 1),
                                                                                                        "-03-01"),
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
                                           trips_sql <- sub(pattern = "\n\tAND t.topiaid IN (?trips_selected)",
                                                            replacement = "",
                                                            x = trips_sql,
                                                            fixed = TRUE)
                                           trips_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                  sql = trips_sql,
                                                                                  begin_period = paste0((dplyr::first(periode_reference,
                                                                                                                      order_by = periode_reference) - 1),
                                                                                                        "-10-01"),
                                                                                  end_period = paste0((dplyr::last(periode_reference,
                                                                                                                   order_by = periode_reference) + 1),
                                                                                                      "-03-01"),
                                                                                  countries = DBI::SQL(paste0("'",
                                                                                                              paste0(countries,
                                                                                                                     collapse = "', '"),
                                                                                                              "'")),
                                                                                  oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                  collapse = ", "))))
                                         }
                                         cat("[", trips_sql_final, "]\n", sep = "")
                                         trips_data <- DBI::dbGetQuery(conn = db_con,
                                                                       statement = trips_sql_final)
                                         if (nrow(x = trips_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful trip(s) data importation from T3 database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "avdth_db") {
                                       # avdth db source ----
                                       if (length(x = class(x = periode_reference)) != 1
                                           || class(x = periode_reference) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (length(x = class(x = countries)) != 1
                                                  || class(x = countries) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (length(x = class(x = oceans)) != 1
                                                  || class(x = oceans) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(x = db_con) != "JDBCConnection") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"JDBCConnection\" expected.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from avdth database.\n",
                                             sep = "")
                                         trips_sql <- paste(readLines(con = system.file("sql\\avdth",
                                                                                        "avdth_trip.sql",
                                                                                        package = "t3")),
                                                            collapse = "\n")
                                         trips_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                sql = trips_sql,
                                                                                begin_period = DBI::SQL(paste0("#",
                                                                                                               (dplyr::first(periode_reference,
                                                                                                                             order_by = periode_reference) - 1),
                                                                                                               "-10-01#")),
                                                                                end_period = DBI::SQL(paste0("#",
                                                                                                             (dplyr::last(periode_reference,
                                                                                                                          order_by = periode_reference) + 1),
                                                                                                             "-03-01#")),
                                                                                countries = DBI::SQL(paste0("'",
                                                                                                            paste0(countries,
                                                                                                                   collapse = "', '"),
                                                                                                            "'")),
                                                                                oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                collapse = ", "))))

                                         cat("[", trips_sql_final, "]\n", sep = "")
                                         trips_data <- DBI::dbGetQuery(conn = db_con,
                                                                       statement = trips_sql_final)
                                         trips_data$trip_id <- as.character(trips_data$trip_id)
                                         trips_data$fleet <- as.character(trips_data$fleet)
                                         trips_data$departure_date <- as.character(trips_data$departure_date)
                                         trips_data$landing_date <- as.character(trips_data$landing_date)
                                         trips_data$logbook_availability <- as.integer(trips_data$logbook_availability)
                                         trips_data$fish_hold_empty <- as.integer(trips_data$fish_hold_empty)
                                         trips_data$vessel_id <- as.integer(trips_data$vessel_id)
                                         trips_data$vessel_type <- as.character(trips_data$vessel_type)
                                         if (nrow(x = trips_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful trip(s) data importation from avdht database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "sql_query") {
                                       # sql queries source ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from database.\n",
                                             sep = "")
                                         trips_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                         collapse = "\n"))
                                         cat("[", trips_sql, "]\n", sep = "")
                                         trips_data <- DBI::dbGetQuery(conn = db_con,
                                                                       statement = trips_sql)
                                         if (nrow(x = trips_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful trip(s) data importation from database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "csv") {
                                       # csv source ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from csv file.\n",
                                             sep = "")
                                         trips_data <- read.csv2(file = data_path,
                                                                 stringsAsFactors = FALSE)
                                         if (nrow(x = trips_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful trip(s) data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       # rdata source ----
                                       if (class(x = data_path) != "character"
                                           || length(x = data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(x = data_path) != "RData") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "trips",
                                                  envir = tmp_envir)) {
                                         trips_data <- get(x = "trips",
                                                           envir = tmp_envir)
                                         if (class(x = trips_data) != "data.frame") {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"trips_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"trips\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(x = trips_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"trips\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful trip(s) data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       # R environment source ----
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "trips")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       if (exists(x = "trips",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from R environment.\n",
                                             sep = "")
                                         trips_data <- get(x = "trips",
                                                           envir = environment_name)
                                         if (class(x = trips_data) != "data.frame") {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"trips_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(x = trips_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"trips\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful trip(s) data importation R environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"trips\" available in the R environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else {
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     trips_data <- unclass(x = trips_data)
                                     object_trips <- t3::object_r6(class_name = "trips")
                                     object_trips$add(lapply(seq_len(length.out = length(x = trips_data[[1]])),
                                                             function(i) {
                                                               cat(format(x = Sys.time(),
                                                                          "%Y-%m-%d %H:%M:%S"),
                                                                   " - Start importation of trip element ",
                                                                   i,
                                                                   ".\n",
                                                                   "[trip: ",
                                                                   trips_data[[1]][i],
                                                                   "]\n",
                                                                   sep = "")
                                                               trip <- t3:::trip$new(trip_id = trips_data[[1]][i],
                                                                                     fleet = trips_data[[2]][i],
                                                                                     departure_date = trips_data[[3]][i],
                                                                                     landing_date = trips_data[[4]][i],
                                                                                     logbook_availability = trips_data[[5]][i],
                                                                                     fish_hold_empty = trips_data[[6]][i],
                                                                                     vessel_id = trips_data[[7]][i],
                                                                                     vessel_type = trips_data[[8]][i])
                                                               cat(format(x = Sys.time(),
                                                                          format = "%Y-%m-%d %H:%M:%S"),
                                                                   " - Successful importation of trip element ",
                                                                   i,
                                                                   ".\n",
                                                                   sep = "")
                                                               return(trip)
                                                             }))
                                     private$trips <- object_trips
                                   },
                                   #' @description Creation of a R6 reference object class activities which contain one or more R6 reference object class activity.
                                   #' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "t3_db" but you can switch to "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R environment).
                                   #' @param db_con Object of class "database" expected. Check {\link[dbConnect]{dbConnect}}. An R's object which contain connection identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param periode_reference Object of class {\link[base]{integer}} expected. Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries Object of class {\link[base]{character}} expected. ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param oceans Object of class {\link[base]{integer}} expected. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path Object of class {\link[base]{character}} expected. Path of the data sql/csv/RData file. By default NULL.
                                   #' @param trips_selected Object of class {\link[base]{character}} expected. Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   #' @param envir Object of class {\link[base]{character}} expected. Specify an environment to look in for data source "envir". By default the first environment where data are found will be used.
                                   activities_object_creation = function(data_source = "t3_db",
                                                                         db_con = NULL,
                                                                         periode_reference = NULL,
                                                                         countries = NULL,
                                                                         oceans = NULL,
                                                                         data_path = NULL,
                                                                         trips_selected = NULL,
                                                                         envir = NULL) {
                                     if (data_source == "t3_db") {
                                       # t3 db source ----
                                       if (length(x = class(x = periode_reference)) != 1
                                           || class(periode_reference) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = countries)) != 1
                                                  || class(x = countries) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = oceans)) != 1
                                                  || class(x = oceans) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(db_con) != "PostgreSQLConnection") {
                                         cat(format(x = Sys.time(),
                                                    format =  "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start activities data importation from T3 database.\n",
                                             sep = "")
                                         activities_sql <- paste(readLines(con = system.file("sql",
                                                                                             "t3_activities.sql",
                                                                                             package = "t3")),
                                                                 collapse = "\n")
                                         if (! is.null(trips_selected)) {
                                           if (class(x = trips_selected) != "character") {
                                             cat(format(x = Sys.time(),
                                                        format = "%Y-%m-%d %H:%M:%S"),
                                                 " - Error: invalid \"trips_selected\" argument, ",
                                                 "class \"character\" expected if not NULL.\n",
                                                 sep = "")
                                             stop()
                                           } else {
                                             activities_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                         sql = activities_sql,
                                                                                         begin_period = paste0((dplyr::first(periode_reference,
                                                                                                                             order_by = periode_reference) - 1),
                                                                                                               "-10-01"),
                                                                                         end_period = paste0((dplyr::last(periode_reference,
                                                                                                                          order_by = periode_reference) + 1),
                                                                                                             "-03-01"),
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
                                           activities_sql <- sub(pattern = "\n\tAND t.topiaid IN (?trips_selected)",
                                                                 replacement = "",
                                                                 x = activities_sql,
                                                                 fixed = TRUE)
                                           activities_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                       sql = activities_sql,
                                                                                       begin_period = paste0((dplyr::first(periode_reference,
                                                                                                                           order_by = periode_reference) - 1),
                                                                                                             "-10-01"),
                                                                                       end_period = paste0((dplyr::last(periode_reference,
                                                                                                                        order_by = periode_reference) + 1),
                                                                                                           "-03-01"),
                                                                                       countries = DBI::SQL(paste0("'",
                                                                                                                   paste0(countries,
                                                                                                                          collapse = "', '"),
                                                                                                                   "'")),
                                                                                       oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                       collapse = ", "))))
                                         }
                                         cat("[", activities_sql_final, "]\n", sep = "")
                                         activities_data <- DBI::dbGetQuery(conn = db_con,
                                                                            statement = activities_sql_final)
                                         if (nrow(x = activities_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful activities data importation from T3 database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "avdth_db") {
                                       # avdth db source ----
                                       if (length(x = class(x = periode_reference)) != 1
                                           || class(periode_reference) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = countries)) != 1
                                                  || class(x = countries) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = oceans)) != 1
                                                  || class(x = oceans) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(db_con) != "JDBCConnection") {
                                         cat(format(x = Sys.time(),
                                                    format =  "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"JDBCConnection\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start activities data importation from avdth database.\n",
                                             sep = "")
                                         activities_sql <- paste(readLines(con = system.file("sql\\avdth",
                                                                                             "avdth_activities.sql",
                                                                                             package = "t3")),
                                                                 collapse = "\n")
                                         activities_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                     sql = activities_sql,
                                                                                     begin_period = DBI::SQL(paste0("#",
                                                                                                                    (dplyr::first(periode_reference,
                                                                                                                                  order_by = periode_reference) - 1),
                                                                                                                    "-10-01#")),
                                                                                     end_period = DBI::SQL(paste0("#",
                                                                                                                  (dplyr::last(periode_reference,
                                                                                                                               order_by = periode_reference) + 1),
                                                                                                                  "-03-01#")),
                                                                                     countries = DBI::SQL(paste0("'",
                                                                                                                 paste0(countries,
                                                                                                                        collapse = "', '"),
                                                                                                                 "'")),
                                                                                     oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                     collapse = ", "))))

                                         cat("[", activities_sql_final, "]\n", sep = "")
                                         activities_data <- DBI::dbGetQuery(conn = db_con,
                                                                            statement = activities_sql_final)
                                         activities_data$trip_id <- as.character(activities_data$trip_id)
                                         activities_data$activity_id <- as.character(activities_data$activity_id)
                                         activities_data$ocean <- as.integer(activities_data$ocean)
                                         activities_data$activity_date <- as.Date(activities_data$activity_date)
                                         activities_data$activity_number <- as.integer(activities_data$activity_number)
                                         activities_data$activity_longitude <- as.numeric(activities_data$activity_longitude)
                                         activities_data$activity_latitude <- as.numeric(activities_data$activity_latitude)
                                         activities_data$set_count <- as.integer(activities_data$set_count)
                                         activities_data$school_type <- as.integer(activities_data$school_type)
                                         activities_data$activity_code <- as.integer(activities_data$activity_code)
                                         activities_data$activity_name <- as.character(activities_data$activity_name)
                                         activities_data$time_at_sea <- as.integer(activities_data$time_at_sea)
                                         if (nrow(x = activities_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful activities data importation from avdth database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "sql_query") {
                                       # sql queries source ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start activities data importation from the database.\n",
                                             sep = "")
                                         activities_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                              collapse = "\n"))
                                         cat("[", activities_sql, "]\n", sep = "")
                                         activities_data <- DBI::dbGetQuery(conn = db_con,
                                                                            statement = activities_sql)
                                         if (nrow(x = activities_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful activities data importation from the database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "csv") {
                                       # csv source ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start activities data importation from csv file.\n",
                                             sep = "")
                                         activities_data <- read.csv2(file = data_path,
                                                                      stringsAsFactors = FALSE)
                                         if (nrow(x = activities_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful activities data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       # rdata source ----
                                       if (class(x = data_path) != "character"
                                           || length(x = data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(x = data_path) != "RData") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start activities data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "activities",
                                                  envir = tmp_envir)) {
                                         activities_data <- get(x = "activities",
                                                                envir = tmp_envir)
                                         if (class(x = activities_data) != "data.frame") {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"activities_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"activities\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(x = activities_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"activities\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful activities data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       # R environment source ----
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "activities")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       if (exists(x = "activities",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start activities data importation from R environment.\n",
                                             sep = "")
                                         activities_data <- get(x = "activities",
                                                                envir = environment_name)
                                         if (class(x = activities_data) != "data.frame") {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"activities_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(x = activities_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"activities\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful activities data importation R environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no R object named \"activities\" available in the R environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else {
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     activities_data <- unclass(x = activities_data)
                                     object_activities <- t3::object_r6(class_name = "activities")
                                     object_activities$add(lapply(X = seq_len(length.out = length(activities_data[[1]])),
                                                                  FUN = function(i) {
                                                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                        " - Start importation of activity element ",
                                                                        i,
                                                                        ".\n",
                                                                        "[activity: ",
                                                                        activities_data[[2]][i],
                                                                        "]\n",
                                                                        sep = "")
                                                                    activity <- t3:::activity$new(trip_id = activities_data[[1]][i],
                                                                                                  activity_id = activities_data[[2]][i],
                                                                                                  ocean = activities_data[[3]][i],
                                                                                                  activity_date = activities_data[[4]][i],
                                                                                                  activity_number = activities_data[[5]][i],
                                                                                                  activity_longitude = activities_data[[6]][i],
                                                                                                  activity_latitude = activities_data[[7]][i],
                                                                                                  set_count = activities_data[[8]][i],
                                                                                                  school_type = activities_data[[9]][i],
                                                                                                  activity_code = activities_data[[10]][i],
                                                                                                  activity_name = activities_data[[11]][i],
                                                                                                  time_at_sea = activities_data[[12]][i])
                                                                    cat(format(x = Sys.time(),
                                                                               format = "%Y-%m-%d %H:%M:%S"),
                                                                        " - Successful importation of activity element ",
                                                                        i,
                                                                        ".\n",
                                                                        sep = "")
                                                                    return(activity)
                                                                  }))
                                     private$activities <- object_activities
                                   },
                                   #' @description Creation of a R6 reference object class elementarycatches which contain one or more R6 reference object class elementarycatch.
                                   #' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "t3_db" but you can switch to "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R environment).
                                   #' @param db_con Object of class "database" expected. Check {\link[dbConnect]{dbConnect}}. An R's object which contain connection identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param periode_reference Object of class {\link[base]{integer}} expected. Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries Object of class {\link[base]{character}} expected. ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param oceans Object of class {\link[base]{integer}} expected. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path Object of class {\link[base]{character}} expected. Path of the data sql/csv/RData file. By default NULL.
                                   #' @param trips_selected Object of class {\link[base]{character}} expected. Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   #' @param envir Object of class {\link[base]{character}} expected. Specify an environment to look in for data source "envir". By default the first environment where data are found will be used.
                                   elementarycatches_object_creation = function(data_source = "t3_db",
                                                                                db_con = NULL,
                                                                                periode_reference = NULL,
                                                                                countries = NULL,
                                                                                oceans = NULL,
                                                                                data_path = NULL,
                                                                                trips_selected = NULL,
                                                                                envir = NULL) {
                                     if (data_source == "t3_db") {
                                       # t3 db source ----
                                       if (length(x = class(periode_reference)) != 1
                                           || class(x = periode_reference) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = countries)) != 1
                                                  || class(x = countries) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = oceans)) != 1
                                                  || class(x = oceans) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(x = db_con) != "PostgreSQLConnection") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary catches data importation from T3 database.\n",
                                             sep = "")
                                         elementarycatches_sql <- paste(readLines(con = system.file("sql",
                                                                                                    "t3_elementarycatches.sql",
                                                                                                    package = "t3")),
                                                                        collapse = "\n")
                                         if (! is.null(x = trips_selected)) {
                                           if (class(x = trips_selected) != "character") {
                                             cat(format(x = Sys.time(),
                                                        format = "%Y-%m-%d %H:%M:%S"),
                                                 " - Error: invalid \"trips_selected\" argument, ",
                                                 "class \"character\" expected if not NULL.\n",
                                                 sep = "")
                                             stop()
                                           } else {
                                             elementarycatches_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                                sql = elementarycatches_sql,
                                                                                                begin_period = paste0((dplyr::first(periode_reference,
                                                                                                                                    order_by = periode_reference) - 1),
                                                                                                                      "-10-01"),
                                                                                                end_period = paste0((dplyr::last(periode_reference,
                                                                                                                                 order_by = periode_reference) + 1),
                                                                                                                    "-03-01"),
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
                                           elementarycatches_sql <- sub(pattern = "\n\tAND t.topiaid IN (?trips_selected)",
                                                                        replacement = "",
                                                                        x = elementarycatches_sql,
                                                                        fixed = TRUE)
                                           elementarycatches_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                              sql = elementarycatches_sql,
                                                                                              begin_period = paste0((dplyr::first(periode_reference,
                                                                                                                                  order_by = periode_reference) - 1),
                                                                                                                    "-10-01"),
                                                                                              end_period = paste0((dplyr::last(periode_reference,
                                                                                                                               order_by = periode_reference) + 1),
                                                                                                                  "-03-01"),
                                                                                              countries = DBI::SQL(paste0("'",
                                                                                                                          paste0(countries,
                                                                                                                                 collapse = "', '"),
                                                                                                                          "'")),
                                                                                              oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                              collapse = ", "))))
                                         }
                                         cat("[", elementarycatches_sql_final, "]\n", sep = "")
                                         elementarycatches_data <- DBI::dbGetQuery(conn = db_con,
                                                                                   statement = elementarycatches_sql_final)
                                         if (nrow(x = elementarycatches_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful elementary catches data importation from T3 database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "avdth_db") {
                                       # avdth db source ----
                                       if (length(x = class(periode_reference)) != 1
                                           || class(x = periode_reference) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = countries)) != 1
                                                  || class(x = countries) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = oceans)) != 1
                                                  || class(x = oceans) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(x = db_con) != "JDBCConnection") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"JDBCConnection\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary catches data importation from avdth database.\n",
                                             sep = "")
                                         elementarycatches_sql <- paste(readLines(con = system.file("sql\\avdth",
                                                                                                    "avdth_elementarycatches.sql",
                                                                                                    package = "t3")),
                                                                        collapse = "\n")
                                         elementarycatches_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                            sql = elementarycatches_sql,
                                                                                            begin_period = DBI::SQL(paste0("#",
                                                                                                                           (dplyr::first(periode_reference,
                                                                                                                                         order_by = periode_reference) - 1),
                                                                                                                           "-10-01#")),
                                                                                            end_period = DBI::SQL(paste0("#",
                                                                                                                         (dplyr::last(periode_reference,
                                                                                                                                      order_by = periode_reference) + 1),
                                                                                                                         "-03-01#")),
                                                                                            countries = DBI::SQL(paste0("'",
                                                                                                                        paste0(countries,
                                                                                                                               collapse = "', '"),
                                                                                                                        "'")),
                                                                                            oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                            collapse = ", "))))
                                         cat("[", elementarycatches_sql_final, "]\n", sep = "")
                                         elementarycatches_data <- DBI::dbGetQuery(conn = db_con,
                                                                                   statement = elementarycatches_sql_final)
                                         elementarycatches_data$activity_id <- as.character(elementarycatches_data$activity_id)
                                         elementarycatches_data$elementarycatch_id <- as.character(elementarycatches_data$elementarycatch_id)
                                         elementarycatches_data$ocean <- as.integer(elementarycatches_data$ocean)
                                         elementarycatches_data$school_type <- as.integer(elementarycatches_data$school_type)
                                         elementarycatches_data$logbook_category <- as.integer(elementarycatches_data$logbook_category)
                                         elementarycatches_data$logbook_category_name <- as.character(elementarycatches_data$logbook_category_name)
                                         elementarycatches_data$specie_code <- as.integer(elementarycatches_data$specie_code)
                                         elementarycatches_data$specie_code3l <- as.character(elementarycatches_data$specie_code3l)
                                         elementarycatches_data$catch_weight <- as.numeric(elementarycatches_data$catch_weight)
                                         if (nrow(x = elementarycatches_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful elementary catches data importation from avdth database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "sql_query") {
                                       # sql queries source ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary catches data importation from the database.\n",
                                             sep = "")
                                         elementarycatches_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                                     collapse = "\n"))
                                         cat("[", elementarycatches_sql, "]\n", sep = "")
                                         elementarycatches_data <- DBI::dbGetQuery(conn = db_con,
                                                                                   statement = elementarycatches_sql)
                                         if (nrow(x = elementarycatches_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful elementary catches data importation from the database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "csv") {
                                       # csv source ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary catches data importation from csv file.\n",
                                             sep = "")
                                         elementarycatches_data <- read.csv2(file = data_path,
                                                                             stringsAsFactors = FALSE)
                                         if (nrow(x = elementarycatches_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful elementary catches data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       # rdata source ----
                                       if (class(x = data_path) != "character"
                                           || length(x = data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(x = data_path) != "RData") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary catches data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "elementarycatches",
                                                  envir = tmp_envir)) {
                                         elementarycatches_data <- get(x = "elementarycatches",
                                                                       envir = tmp_envir)
                                         if (class(x = elementarycatches_data) != "data.frame") {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"elementarycatches_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"elementarycatches\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(x = elementarycatches_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"elementarycatches\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary catches data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       # R environment source ----
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "elementarycatches")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       if (exists(x = "elementarycatches",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary catches data importation from R environment.\n",
                                             sep = "")
                                         elementarycatches_data <- get(x = "elementarycatches",
                                                                       envir = environment_name)
                                         if (class(x = elementarycatches_data) != "data.frame") {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"elementarycatches_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(x = elementarycatches_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"elementary catches\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary catches data importation R environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"elementarycatches\" available in the R environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else {
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     elementarycatches_data <- unclass(x = elementarycatches_data)
                                     object_elementarycatches <- t3::object_r6(class_name = "elementarycatches")
                                     object_elementarycatches$add(lapply(X = seq_len(length.out = length(elementarycatches_data[[1]])),
                                                                         FUN = function(i) {
                                                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                               " - Start importation of elementary catch element ",
                                                                               i,
                                                                               ".\n",
                                                                               "[elementarycatch: ",
                                                                               elementarycatches_data[[2]][i],
                                                                               "]\n",
                                                                               sep = "")
                                                                           elementarycatch <- t3:::elementarycatch$new(activity_id = elementarycatches_data[[1]][i],
                                                                                                                       elementarycatch_id = elementarycatches_data[[2]][i],
                                                                                                                       ocean = elementarycatches_data[[3]][i],
                                                                                                                       school_type = elementarycatches_data[[4]][i],
                                                                                                                       logbook_category = elementarycatches_data[[5]][i],
                                                                                                                       logbook_category_name = elementarycatches_data[[6]][i],
                                                                                                                       specie_code = elementarycatches_data[[7]][i],
                                                                                                                       specie_code3l = elementarycatches_data[[8]][i],
                                                                                                                       catch_weight = elementarycatches_data[[9]][i])
                                                                           cat(format(x = Sys.time(),
                                                                                      format = "%Y-%m-%d %H:%M:%S"),
                                                                               " - Successful importation of elementary catch element ",
                                                                               i,
                                                                               ".\n",
                                                                               sep = "")
                                                                           return(elementarycatch)
                                                                         }))
                                     private$elementarycatches <- object_elementarycatches
                                   },
                                   #' @description Creation of a R6 reference object class elementarylandings which contain one or more R6 reference object class elementarylanding
                                   #' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "t3_db" but you can switch to "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R environment).
                                   #' @param db_con Object of class "database" expected. Check {\link[dbConnect]{dbConnect}}. An R's object which contain connection identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param periode_reference Object of class {\link[base]{integer}} expected. Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries Object of class {\link[base]{character}} expected. ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param oceans Object of class {\link[base]{integer}} expected. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path Object of class {\link[base]{character}} expected. Path of the data sql/csv/RData file. By default NULL.
                                   #' @param trips_selected Object of class {\link[base]{character}} expected. Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   #' @param envir Object of class {\link[base]{character}} expected. Specify an environment to look in for data source "envir". By default the first environment where data are found will be used.
                                   elementarylandings_object_creation = function(data_source = "t3_db",
                                                                                 db_con = NULL,
                                                                                 periode_reference = NULL,
                                                                                 countries = NULL,
                                                                                 oceans = NULL,
                                                                                 data_path = NULL,
                                                                                 trips_selected = NULL,
                                                                                 envir = NULL) {
                                     if (data_source == "t3_db") {
                                       # t3 db source ----
                                       if (length(x = class(x = periode_reference)) != 1
                                           || class(x = periode_reference) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = countries)) != 1
                                                  || class(x = countries) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = oceans)) != 1
                                                  || class(x = oceans) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(x = db_con) != "PostgreSQLConnection") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary landings data importation from T3 database.\n",
                                             sep = "")
                                         elementarylandings_sql <- paste(readLines(con = system.file("sql",
                                                                                                     "t3_elementarylandings.sql",
                                                                                                     package = "t3")),
                                                                         collapse = "\n")
                                         if (! is.null(x = trips_selected)) {
                                           if (class(x = trips_selected) != "character") {
                                             cat(format(x = Sys.time(),
                                                        format = "%Y-%m-%d %H:%M:%S"),
                                                 " - Error: invalid \"trips_selected\" argument, ",
                                                 "class \"character\" expected if not NULL.\n",
                                                 sep = "")
                                             stop()
                                           } else {
                                             elementarylandings_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                                 sql = elementarylandings_sql,
                                                                                                 begin_period = paste0((dplyr::first(periode_reference,
                                                                                                                                     order_by = periode_reference) - 1),
                                                                                                                       "-10-01"),
                                                                                                 end_period = paste0((dplyr::last(periode_reference,
                                                                                                                                  order_by = periode_reference) + 1),
                                                                                                                     "-03-01"),
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
                                           elementarylandings_sql <- sub(pattern = "\n\tAND t.topiaid IN (?trips_selected)",
                                                                         replacement = "",
                                                                         x = elementarylandings_sql,
                                                                         fixed = TRUE)
                                           elementarylandings_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                               sql = elementarylandings_sql,
                                                                                               begin_period = paste0((dplyr::first(periode_reference,
                                                                                                                                   order_by = periode_reference) - 1),
                                                                                                                     "-10-01"),
                                                                                               end_period = paste0((dplyr::last(periode_reference,
                                                                                                                                order_by = periode_reference) + 1),
                                                                                                                   "-03-01"),
                                                                                               countries = DBI::SQL(paste0("'",
                                                                                                                           paste0(countries,
                                                                                                                                  collapse = "', '"),
                                                                                                                           "'")),
                                                                                               oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                               collapse = ", "))))
                                         }
                                         cat("[", elementarylandings_sql_final, "]\n", sep = "")
                                         elementarylandings_data <- DBI::dbGetQuery(conn = db_con,
                                                                                    statement = elementarylandings_sql_final)
                                         if (nrow(x = elementarylandings_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful elementary landings data importation from T3 database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "avdth_db") {
                                       # avdth db source ----
                                       if (length(x = class(x = periode_reference)) != 1
                                           || class(x = periode_reference) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = countries)) != 1
                                                  || class(x = countries) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = oceans)) != 1
                                                  || class(x = oceans) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(x = db_con) != "JDBCConnection") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"JDBCConnection\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary landings data importation from avdth database.\n",
                                             sep = "")
                                         elementarylandings_sql <- paste(readLines(con = system.file("sql\\avdth",
                                                                                                     "avdth_elementarylandings.sql",
                                                                                                     package = "t3")),
                                                                         collapse = "\n")
                                         elementarylandings_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                             sql = elementarylandings_sql,
                                                                                             begin_period = DBI::SQL(paste0("#",
                                                                                                                            (dplyr::first(periode_reference,
                                                                                                                                          order_by = periode_reference) - 1),
                                                                                                                            "-10-01#")),
                                                                                             end_period = DBI::SQL(paste0("#",
                                                                                                                          (dplyr::last(periode_reference,
                                                                                                                                       order_by = periode_reference) + 1),
                                                                                                                          "-03-01#")),
                                                                                             countries = DBI::SQL(paste0("'",
                                                                                                                         paste0(countries,
                                                                                                                                collapse = "', '"),
                                                                                                                         "'")),
                                                                                             oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                             collapse = ", "))))
                                         cat("[", elementarylandings_sql_final, "]\n", sep = "")
                                         elementarylandings_data <- DBI::dbGetQuery(conn = db_con,
                                                                                    statement = elementarylandings_sql_final)
                                         elementarylandings_data$trip_id <- as.character(elementarylandings_data$trip_id)
                                         elementarylandings_data$elementarylanding_id <- as.character(elementarylandings_data$elementarylanding_id)
                                         elementarylandings_data$landing_category <- as.integer(elementarylandings_data$landing_category)
                                         elementarylandings_data$landing_category_name <- as.character(elementarylandings_data$landing_category_name)
                                         elementarylandings_data$specie_code <- as.integer(elementarylandings_data$specie_code)
                                         elementarylandings_data$specie_code3l <- as.character(elementarylandings_data$specie_code3l)
                                         elementarylandings_data$landing_weight <- as.numeric(elementarylandings_data$landing_weight)
                                         if (nrow(x = elementarylandings_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful elementary landings data importation from avdth database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "sql_query") {
                                       # sql queries sources ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary landings data importation from the database.\n",
                                             sep = "")
                                         elementarylandings_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                                      collapse = "\n"))
                                         cat("[", elementarylandings_sql, "]\n", sep = "")
                                         elementarylandings_data <- DBI::dbGetQuery(conn = db_con,
                                                                                    statement = elementarylandings_sql)
                                         if (nrow(x = elementarylandings_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful elementary landings data importation from the database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "csv") {
                                       # csv source ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary landings data importation from csv file.\n",
                                             sep = "")
                                         elementarylandings_data <- read.csv2(file = data_path,
                                                                              stringsAsFactors = FALSE)
                                         if (nrow(x = elementarylandings_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful elementary landings data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       # rdata source ----
                                       if (class(x = data_path) != "character"
                                           || length(x = data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(x = data_path) != "RData") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary landings data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "elementarylandings",
                                                  envir = tmp_envir)) {
                                         elementarylandings_data <- get(x = "elementarylandings",
                                                                        envir = tmp_envir)
                                         if (class(x = elementarylandings_data) != "data.frame") {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"elementarylandings_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"elementarylandings\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(x = elementarylandings_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"elementarylandings\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary landings data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       # R environment source ----
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "elementarylandings")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       if (exists(x = "elementarylandings",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary landings data importation from R environment.\n",
                                             sep = "")
                                         elementarylandings_data <- get(x = "elementarylandings",
                                                                        envir = environment_name)
                                         if (class(x = elementarylandings_data) != "data.frame") {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"elementarylandings_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(x = elementarylandings_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"elementary landings\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary landings data importation R environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"elementarylandings\" available in the R environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else {
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     elementarylandings_data <- unclass(x = elementarylandings_data)
                                     object_elementarylandings <- t3::object_r6(class_name = "elementarylandings")
                                     object_elementarylandings$add(lapply(X = seq_len(length.out = length(x = elementarylandings_data[[1]])),
                                                                          FUN = function(i) {
                                                                            cat(format(x = Sys.time(),
                                                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                                                " - Start importation of elementary landing element ",
                                                                                i,
                                                                                ".\n",
                                                                                "[elementarylanding: ",
                                                                                elementarylandings_data[[2]][i],
                                                                                "]\n",
                                                                                sep = "")
                                                                            elementarylanding <- t3:::elementarylanding$new(trip_id = elementarylandings_data[[1]][i],
                                                                                                                            elementarylanding_id = elementarylandings_data[[2]][i],
                                                                                                                            landing_category = elementarylandings_data[[3]][i],
                                                                                                                            landing_category_name = elementarylandings_data[[4]][i],
                                                                                                                            specie_code = elementarylandings_data[[5]][i],
                                                                                                                            specie_code3l = elementarylandings_data[[6]][i],
                                                                                                                            landing_weight = elementarylandings_data[[7]][i])
                                                                            cat(format(x = Sys.time(),
                                                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                                                " - Successful importation of elementary landing element ",
                                                                                i,
                                                                                ".\n",
                                                                                sep = "")
                                                                            return(elementarylanding)
                                                                          }))
                                     private$elementarylandings <- object_elementarylandings
                                   },
                                   #' @description Creation of a R6 reference object class wells which contain one or more R6 reference object class well, wellset, samples and elementarywellplan.
                                   #' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "t3_db" but you can switch to "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R environment).
                                   #' @param db_con Object of class "database" expected. Check {\link[dbConnect]{dbConnect}}. An R's object which contain connection identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param periode_reference Object of class {\link[base]{integer}} expected. Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries Object of class {\link[base]{character}} expected. ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param oceans Object of class {\link[base]{integer}} expected. Ocean(s) related to data coded on 1 digit. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param trips_selected Object of class {\link[base]{character}} expected. Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   #' @param envir Object of class {\link[base]{character}} expected. Specify an environment to look in for data source "envir". By default the first environment where data are found will be used.
                                   #' @param sample_type Object of class {\link[base]{integer}} expected. Sample type identification (landing, observer, ...). By default NULL.
                                   #' @param data_path_samples Object of class {\link[base]{character}} expected. Path of the data sql/csv file for samples. By default NULL.
                                   #' @param data_path_wellplans Object of class {\link[base]{character}} expected. Path of the data sql/csv file for well plans. By default NULL.
                                   wells_object_creation = function(data_source = "t3_db",
                                                                    db_con = NULL,
                                                                    periode_reference = NULL,
                                                                    countries = NULL,
                                                                    oceans = NULL,
                                                                    sample_type = NULL,
                                                                    trips_selected = NULL,
                                                                    data_path_samples = NULL,
                                                                    data_path_wellplans = NULL,
                                                                    envir = NULL) {
                                     if (data_source == "t3_db") {
                                       # t3 db source ----
                                       if (length(x = class(x = periode_reference)) != 1
                                           || class(x = periode_reference) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = countries)) != 1
                                                  || class(x = countries) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = oceans)) != 1
                                                  || class(x = oceans) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(x = db_con) != "PostgreSQLConnection") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                       } else if (class(x = sample_type) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"sample_type\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples data importation from T3 database.\n",
                                             sep = "")
                                         samples_sql <- paste(readLines(con = system.file("sql",
                                                                                          "t3_samples.sql",
                                                                                          package = "t3")),
                                                              collapse = "\n")
                                         if (! is.null(x = trips_selected)) {
                                           if (class(x = trips_selected) != "character") {
                                             cat(format(x = Sys.time(),
                                                        format = "%Y-%m-%d %H:%M:%S"),
                                                 " - Error: invalid \"trips_selected\" argument, ",
                                                 "class \"character\" expected if not NULL.\n",
                                                 sep = "")
                                             stop()
                                           } else {
                                             samples_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                      sql = samples_sql,
                                                                                      begin_period = paste0((dplyr::first(periode_reference,
                                                                                                                          order_by = periode_reference) - 1),
                                                                                                            "-10-01"),
                                                                                      end_period = paste0((dplyr::last(periode_reference,
                                                                                                                       order_by = periode_reference) + 1),
                                                                                                          "-03-01"),
                                                                                      countries = DBI::SQL(paste0("'",
                                                                                                                  paste0(countries,
                                                                                                                         collapse = "', '"),
                                                                                                                  "'")),
                                                                                      oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                      collapse = ", "))),
                                                                                      sample_type = DBI::SQL(paste0(sample_type,
                                                                                                                    collapse = ", ")),
                                                                                      trips_selected = DBI::SQL(paste0("'",
                                                                                                                       paste0(trips_selected,
                                                                                                                              collapse = "', '"),
                                                                                                                       "'")))
                                           }
                                         } else {
                                           samples_sql <- sub(pattern = "\n\tAND t.topiaid IN (?trips_selected)",
                                                              replacement = "",
                                                              x = samples_sql,
                                                              fixed = TRUE)
                                           samples_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                    sql = samples_sql,
                                                                                    begin_period = paste0((dplyr::first(periode_reference,
                                                                                                                        order_by = periode_reference) - 1),
                                                                                                          "-10-01"),
                                                                                    end_period = paste0((dplyr::last(periode_reference,
                                                                                                                     order_by = periode_reference) + 1),
                                                                                                        "-03-01"),
                                                                                    countries = DBI::SQL(paste0("'",
                                                                                                                paste0(countries,
                                                                                                                       collapse = "', '"),

                                                                                                                "'")),
                                                                                    oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                    collapse = ", "))),
                                                                                    sample_type = DBI::SQL(paste0(sample_type,
                                                                                                                  collapse = ", ")))
                                         }
                                         cat("[", samples_sql_final, "]\n", sep = "")
                                         samples_data <- DBI::dbGetQuery(conn = db_con,
                                                                         statement = samples_sql_final)
                                         if (nrow(x = samples_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful samples data importation from T3 database.\n",
                                               sep = "")
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start well plans data importation from T3 database.\n",
                                             sep = "")
                                         wellplan_sql <- paste(readLines(con = system.file("sql",
                                                                                           "t3_wellplans.sql",
                                                                                           package = "t3")),
                                                               collapse = "\n")
                                         if (! is.null(x = trips_selected)) {
                                           if (class(x = trips_selected) != "character") {
                                             cat(format(x = Sys.time(),
                                                        format = "%Y-%m-%d %H:%M:%S"),
                                                 " - Error: invalid \"trips_selected\" argument, ",
                                                 "class \"character\" expected if not NULL.\n",
                                                 sep = "")
                                             stop()
                                           } else {
                                             wellplan_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                       sql = wellplan_sql,
                                                                                       begin_period = paste0((dplyr::first(periode_reference,
                                                                                                                           order_by = periode_reference) - 1),
                                                                                                             "-10-01"),
                                                                                       end_period = paste0((dplyr::last(periode_reference,
                                                                                                                        order_by = periode_reference) + 1),
                                                                                                           "-03-01"),
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
                                           wellplan_sql <- sub(pattern = "\n\tAND t.topiaid IN (?trips_selected)",
                                                               replacement = "",
                                                               x = wellplan_sql,
                                                               fixed = TRUE)
                                           wellplan_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                     sql = wellplan_sql,
                                                                                     begin_period = paste0((dplyr::first(periode_reference,
                                                                                                                         order_by = periode_reference) - 1),
                                                                                                           "-10-01"),
                                                                                     end_period = paste0((dplyr::last(periode_reference,
                                                                                                                      order_by = periode_reference) + 1),
                                                                                                         "-03-01"),
                                                                                     countries = DBI::SQL(paste0("'",
                                                                                                                 paste0(countries,
                                                                                                                        collapse = "', '"),
                                                                                                                 "'")),
                                                                                     oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                     collapse = ", "))))
                                         }
                                         cat("[", wellplan_sql_final, "]\n", sep = "")
                                         wellplan_data <- DBI::dbGetQuery(conn = db_con,
                                                                          statement = wellplan_sql_final)
                                         if (nrow(x = wellplan_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful well plans data importation from T3 database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "avdth_db") {
                                       # avdth db source ----
                                       if (length(x = class(x = periode_reference)) != 1
                                           || class(x = periode_reference) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = countries)) != 1
                                                  || class(x = countries) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = oceans)) != 1
                                                  || class(x = oceans) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(x = db_con) != "JDBCConnection") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"JDBCConnection\" expected.\n",
                                             sep = "")
                                       } else if (class(x = sample_type) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"sample_type\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples data importation from avdth database.\n",
                                             sep = "")
                                         samples_sql <- paste(readLines(con = system.file("sql\\avdth",
                                                                                          "avdth_samples.sql",
                                                                                          package = "t3")),
                                                              collapse = "\n")
                                         samples_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                  sql = samples_sql,
                                                                                  begin_period = DBI::SQL(paste0("#",
                                                                                                                 (dplyr::first(periode_reference,
                                                                                                                               order_by = periode_reference) - 1),
                                                                                                                 "-10-01#")),
                                                                                  end_period = DBI::SQL(paste0("#",
                                                                                                               (dplyr::last(periode_reference,
                                                                                                                            order_by = periode_reference) + 1),
                                                                                                               "-03-01#")),
                                                                                  countries = DBI::SQL(paste0("'",
                                                                                                              paste0(countries,
                                                                                                                     collapse = "', '"),

                                                                                                              "'")),
                                                                                  oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                  collapse = ", "))),
                                                                                  sample_type = DBI::SQL(paste0(sample_type,
                                                                                                                collapse = ", ")))
                                         cat("[", samples_sql_final, "]\n", sep = "")
                                         samples_data <- DBI::dbGetQuery(conn = db_con,
                                                                         statement = samples_sql_final)
                                         samples_data <- samples_data %>%
                                           dplyr::mutate(trip_id = as.character(trip_id),
                                                         well_id = as.character(well_id),
                                                         well_minus10_weigth = as.integer(well_minus10_weigth),
                                                         well_plus10_weigth = as.integer(well_plus10_weigth),
                                                         well_global_weigth = as.integer(well_global_weigth),
                                                         sample_id = as.character(sample_id),
                                                         sub_sample_id = as.integer(sub_sample_id),
                                                         sub_sample_id_total_count = as.character(sub_sample_id_total_count),
                                                         elementarysampleraw_id = as.character(paste0(elementarysampleraw_id,
                                                                                                      ".",
                                                                                                      dplyr::row_number())),
                                                         specie_code = as.integer(specie_code),
                                                         specie_code3l = as.character(specie_code3l),
                                                         length_type = as.integer(length_type),
                                                         sample_total_count = as.integer(sample_total_count),
                                                         sample_number_measured = as.integer(sample_number_measured),
                                                         sample_length_class = as.integer(sample_length_class))
                                         if (nrow(x = samples_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful samples data importation from avdth database.\n",
                                               sep = "")
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start well plans data importation from avdth database.\n",
                                             sep = "")
                                         wellplan_sql <- paste(readLines(con = system.file("sql\\avdth",
                                                                                           "avdth_wellplans.sql",
                                                                                           package = "t3")),
                                                               collapse = "\n")
                                         wellplan_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                   sql = wellplan_sql,
                                                                                   begin_period = DBI::SQL(paste0("#",
                                                                                                                  (dplyr::first(periode_reference,
                                                                                                                                order_by = periode_reference) - 1),
                                                                                                                  "-10-01#")),
                                                                                   end_period = DBI::SQL(paste0("#",
                                                                                                                (dplyr::last(periode_reference,
                                                                                                                             order_by = periode_reference) + 1),
                                                                                                                "-03-01#")),
                                                                                   countries = DBI::SQL(paste0("'",
                                                                                                               paste0(countries,
                                                                                                                      collapse = "', '"),
                                                                                                               "'")),
                                                                                   oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                   collapse = ", "))))

                                         cat("[", wellplan_sql_final, "]\n", sep = "")
                                         wellplan_data <- DBI::dbGetQuery(conn = db_con,
                                                                          statement = wellplan_sql_final)
                                         wellplan_data$wellplan_id <- as.character(wellplan_data$wellplan_id)
                                         wellplan_data$well_id <- as.character(wellplan_data$well_id)
                                         wellplan_data$activity_id <- as.character(wellplan_data$activity_id)
                                         wellplan_data$sample_id <- as.character(wellplan_data$sample_id)
                                         wellplan_data$specie_code <- as.integer(wellplan_data$specie_code)
                                         wellplan_data$specie_code3l <- as.character(wellplan_data$specie_code3l)
                                         wellplan_data$wellplan_weight <- as.numeric(wellplan_data$wellplan_weight)
                                         wellplan_data$wellplan_number <- as.integer(wellplan_data$wellplan_number)
                                         wellplan_data$wellplan_weigth_category_code <- as.integer(wellplan_data$wellplan_weigth_category_code)
                                         wellplan_data$wellplan_weigth_category_label <- as.character(wellplan_data$wellplan_weigth_category_label)
                                         if (nrow(x = wellplan_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful well plans data importation from avdth database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "sql_query") {
                                       # sql queries source ----
                                       if (class(x = data_path_samples) != "character"
                                           || class(x = data_path_wellplans) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path_samples\" argument and or \"data_path_wellplans\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path_samples)
                                                  || ! file.exists(data_path_wellplans)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path_samples\" argument and or \"data_path_wellplans\" argument, ",
                                             "file(s) doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples data importation from the database.\n",
                                             sep = "")
                                         samples_sql <- DBI::SQL(x = paste(readLines(con = data_path_samples),
                                                                           collapse = "\n"))
                                         cat("[", samples_sql, "]\n", sep = "")
                                         samples_data <- DBI::dbGetQuery(conn = db_con,
                                                                         statement = samples_sql)
                                         if (nrow(x = samples_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful samples data importation from the database.\n",
                                               sep = "")
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start well plans data importation from the database.\n",
                                             sep = "")
                                         wellplan_sql <- DBI::SQL(x = paste(readLines(con = data_path_wellplans),
                                                                            collapse = "\n"))
                                         cat("[", wellplan_sql, "]\n", sep = "")
                                         wellplan_data <- DBI::dbGetQuery(conn = db_con,
                                                                          statement = wellplan_sql)
                                         if (nrow(x = wellplan_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful well plans data importation from the database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "csv") {
                                       # csv source ----
                                       if (class(x = data_path_samples) != "character"
                                           || class(x = data_path_wellplans) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path_samples\" argument and or \"data_path_wellplans\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path_samples)
                                                  || ! file.exists(data_path_wellplans)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path_samples\" argument and or \"data_path_wellplans\" argument, ",
                                             "file(s) doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples data importation from csv file.\n",
                                             sep = "")
                                         samples_data <- read.csv2(file = data_path_samples,
                                                                   stringsAsFactors = FALSE)
                                         if (nrow(x = samples_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful samples data importation from csv file.\n",
                                               sep = "")
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start well plans data importation from csv file.\n",
                                             sep = "")
                                         wellplan_data <- read.csv2(file = data_path_wellplans,
                                                                    stringsAsFactors = FALSE)
                                         if (nrow(x = wellplan_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful well plans data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       # rdata source ----
                                       if (class(x = data_path_samples) != "character"
                                           || length(x = data_path_samples) != 1
                                           || ! file.exists(data_path_samples)
                                           || tools::file_ext(x = data_path_samples) != "RData") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path_samples\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path_samples,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "samples",
                                                  envir = tmp_envir)) {
                                         samples_data <- get(x = "samples",
                                                             envir = tmp_envir)
                                         if (class(x = samples_data) != "data.frame") {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"samples_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"samples\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(x = samples_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"samples\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful samples data importation from RData.\n",
                                             sep = "")
                                       }
                                       if (class(x = data_path_wellplans) != "character"
                                           || length(x = data_path_wellplans) != 1
                                           || ! file.exists(data_path_wellplans)
                                           || tools::file_ext(x = data_path_wellplans) != "RData") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path_wellplans\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start well plans data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path_wellplans,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "wellplans",
                                                  envir = tmp_envir)) {
                                         wellplan_data <- get(x = "wellplans",
                                                              envir = tmp_envir)
                                         if (class(x = wellplan_data) != "data.frame") {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"wellplan_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"wellplans\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(x = wellplan_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"wellplans\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful well plans data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       # R environment source ----
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "samples")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       if (exists(x = "samples",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples data importation from R environment.\n",
                                             sep = "")
                                         samples_data <- get(x = "samples",
                                                             envir = environment_name)
                                         if (class(x = samples_data) != "data.frame") {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"samples_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(x = samples_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"samples\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful samples data importation R environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"samples\" available in the R environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (exists(x = "wellplans",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start well plans data importation from R environment.\n",
                                             sep = "")
                                         wellplan_data <- get(x = "wellplans",
                                                              envir = environment_name)
                                         if (class(x = wellplan_data) != "data.frame") {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"wellplan_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(x = wellplan_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"wellplans\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful well plans data importation R environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"wellplans\" available in the R environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else {
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     # global manipulations ----
                                     object_wells <- t3::object_r6(class_name = "wells")
                                     for (trip in unique(x = samples_data$trip_id)) {
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start importation of well(s) data for trip element ",
                                           which(x = unique(x = samples_data$trip_id) == trip),
                                           ".\n",
                                           "[trip: ",
                                           trip,
                                           "]\n",
                                           sep = "")
                                       tmp_trip <- dplyr::filter(.data = samples_data, trip_id == trip)
                                       for (well in unique(x = tmp_trip$well_id)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start importation of well data item ",
                                             which(x = unique(tmp_trip$well_id) == well),
                                             ".\n",
                                             "[well: ",
                                             well,
                                             "]\n",
                                             sep = "")
                                         if (is.na(x = well)) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Warning: missing \"well_id\" argument in trip number: \"",
                                               trip,
                                               "\".\n",
                                               sep = "")
                                           tmp_well <- dplyr::filter(.data = tmp_trip, is.na(well_id))
                                           if (length(x = unique(x = tmp_well$sample_id)) != 1) {
                                             cat(format(x = Sys.time(),
                                                        format = "%Y-%m-%d %H:%M:%S"),
                                                 " - Warning: well unknown identify in trip number \"",
                                                 trip,
                                                 "\" have more than one sampling associated.\n",
                                                 "Data avoided for model incrementation.\n",
                                                 sep = "")
                                             next()
                                           }
                                         } else {
                                           tmp_well <- dplyr::filter(.data = tmp_trip, well_id == well)
                                         }
                                         if (length(unique(x = tmp_well$well_minus10_weigth)) != 1
                                             | length(unique(x = tmp_well$well_plus10_weigth)) != 1
                                             | length(unique(x = tmp_well$well_global_weigth)) != 1) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Warning: at least one well data (\"well_minus10_weigth\", \"well_plus10_weigth\" and \"well_global_weigth\") is different between well samples. Only the first element will use.\n",
                                               "[trip: ",
                                               trip,
                                               ", well: ",
                                               well,
                                               "]\n",
                                               sep = "")
                                         }
                                         object_well <- t3:::well$new(trip_id = trip,
                                                                      well_id = well,
                                                                      well_minus10_weigth = unique(x = tmp_well$well_minus10_weigth)[[1]],
                                                                      well_plus10_weigth = unique(x = tmp_well$well_plus10_weigth)[[1]],
                                                                      well_global_weigth = unique(x = tmp_well$well_global_weigth[[1]]))
                                         for (sample in unique(x = tmp_well$sample_id)) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Start importation of sample data item ",
                                               which(x = unique(tmp_well$sample_id) == sample),
                                               ".\n",
                                               "[sample: ",
                                               sample,
                                               "]\n",
                                               sep = "")
                                           tmp_sample <- dplyr::filter(.data = tmp_well, sample_id == sample)
                                           tmp_sample <- unclass(x = tmp_sample)
                                           object_well$.__enclos_env__$private$elementarysampleraw <- append(object_well$.__enclos_env__$private$elementarysampleraw,
                                                                                                             list(lapply(X = seq_len(length.out = length(x = tmp_sample[[1]])),
                                                                                                                         FUN = function(i) {
                                                                                                                           t3:::elementarysampleraw$new(trip_id = trip,
                                                                                                                                                        well_id = well,
                                                                                                                                                        sample_id = sample,
                                                                                                                                                        sub_sample_id = tmp_sample[[7]][i],
                                                                                                                                                        sub_sample_id_total_count = tmp_sample[[8]][i],
                                                                                                                                                        elementarysampleraw_id = tmp_sample[[9]][i],
                                                                                                                                                        sample_quality = tmp_sample[[10]][i],
                                                                                                                                                        sample_type = tmp_sample[[11]][i],
                                                                                                                                                        specie_code = tmp_sample[[12]][i],
                                                                                                                                                        specie_code3l = tmp_sample[[13]][i],
                                                                                                                                                        length_type = tmp_sample[[14]][i],
                                                                                                                                                        sample_total_count = tmp_sample[[15]][i],
                                                                                                                                                        sample_number_measured = tmp_sample[[16]][i],
                                                                                                                                                        sample_length_class = tmp_sample[[17]][i])
                                                                                                                         })))
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful importation of sample data item ",
                                               which(x = unique(x = tmp_well$sample_id) == sample),
                                               ".\n",
                                               "[sample: ",
                                               sample,
                                               "]\n",
                                               sep = "")
                                         }
                                         if (well %in% unique(x = wellplan_data$well_id)) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Start importation of well plan data item ",
                                               which(x = unique(x = wellplan_data$well_id) == well),
                                               ".\n",
                                               "[well: ",
                                               well,
                                               "]\n",
                                               sep = "")
                                           tmp_wellplan <- dplyr::filter(.data = wellplan_data, well_id == well)
                                           tmp_wellplan <- unclass(x = tmp_wellplan)
                                           object_well$.__enclos_env__$private$wellplan <- lapply(X = seq_len(length.out = length(x = tmp_wellplan[[1]])),
                                                                                                  FUN = function(j) {
                                                                                                    t3:::elementarywellplan$new(wellplan_id = tmp_wellplan[[1]][j],
                                                                                                                                well_id = tmp_wellplan[[2]][j],
                                                                                                                                activity_id = tmp_wellplan[[3]][j],
                                                                                                                                sample_id = tmp_wellplan[[4]][j],
                                                                                                                                specie_code = tmp_wellplan[[5]][j],
                                                                                                                                specie_code3l = tmp_wellplan[[6]][j],
                                                                                                                                wellplan_weight = tmp_wellplan[[7]][j],
                                                                                                                                wellplan_number = tmp_wellplan[[8]][j],
                                                                                                                                wellplan_weigth_category_code = tmp_wellplan[[9]][j],
                                                                                                                                wellplan_weigth_category_label = tmp_wellplan[[10]][j])
                                                                                                  })
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Sucessful importation of well plan data item ",
                                               which(x = unique(x = wellplan_data$well_id) == well),
                                               ".\n",
                                               "[well: ",
                                               well,
                                               "]\n",
                                               sep = "")
                                         }
                                         object_wells$add(object_well)
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful importation of well data item ",
                                             which(x = unique(x = tmp_trip$well_id) == well),
                                             ".\n",
                                             "[well: ",
                                             well,
                                             "]\n",
                                             sep = "")
                                       }
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Successful importation of well(s) data for trip element ",
                                           which(x = unique(x = samples_data$trip_id) == trip),
                                           ".\n",
                                           "[trip: ",
                                           trip,
                                           "]\n",
                                           sep = "")
                                     }
                                     private$wells <- object_wells
                                   },
                                   #' @description Creation of a data frame object with parameters of set duration algorithms.
                                   #' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "t3_db" but you can switch to "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R environment).
                                   #' @param db_con Object of class "database" expected. Check {\link[dbConnect]{dbConnect}}. An R's object which contain connection identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param periode_reference Object of class {\link[base]{integer}} expected. Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries Object of class {\link[base]{character}} expected. ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path Object of class {\link[base]{character}} expected. Path of the data sql/csv/RData file. By default NULL.
                                   #' @param envir Object of class {\link[base]{character}} expected. Specify an environment to look in for data source "envir". By default the first environment where data are found will be used.
                                   setdurationrefs_data = function(db_con = NULL,
                                                                   data_source = "t3_db",
                                                                   periode_reference = NULL,
                                                                   countries = NULL,
                                                                   data_path = NULL,
                                                                   envir = NULL) {
                                     if (data_source == "t3_db") {
                                       # t3 db source ----
                                       if (length(x = class(x = periode_reference)) != 1
                                           || class(x = periode_reference) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = countries)) != 1
                                                  || class(x = countries) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (class(x = db_con) != "PostgreSQLConnection") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start set duration data importation from T3 database.\n",
                                             sep = "")
                                         setdurationrefs_sql <- paste(readLines(con = system.file("sql",
                                                                                                  "t3_setdurationrefs.sql",
                                                                                                  package = "t3")),
                                                                      collapse = "\n")
                                         setdurationrefs_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                          sql = setdurationrefs_sql,
                                                                                          period = DBI::SQL(paste0(c((dplyr::first(periode_reference,
                                                                                                                                   order_by = periode_reference) - 1)
                                                                                                                     : (dplyr::last(periode_reference,
                                                                                                                                    order_by = periode_reference) + 1)),
                                                                                                                   collapse = ", ")),
                                                                                          countries = DBI::SQL(paste0("'",
                                                                                                                      paste0(countries,
                                                                                                                             collapse = "', '"),
                                                                                                                      "'")))
                                         cat("[", setdurationrefs_sql_final, "]\n", sep = "")
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
                                       }
                                     } else if (data_source == "sql_query") {
                                       # sql queries source ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
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
                                       }
                                     } else if (data_source == "csv") {
                                       # csv source ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
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
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful set duration data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       # rdata source ----
                                       if (class(x = data_path) != "character"
                                           || length(x = data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(data_path) != "RData") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start set duration references data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "setdurationrefs",
                                                  envir = tmp_envir)) {
                                         set_duration_refs_data <- get(x = "setdurationrefs",
                                                                       envir = tmp_envir)
                                         if (class(x = set_duration_refs_data) != "data.frame") {
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
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "setdurationrefs")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       if (exists(x = "setdurationrefs",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start set duration references data importation from R environment.\n",
                                             sep = "")
                                         set_duration_refs_data <- get(x = "setdurationrefs",
                                                                       envir = environment_name)
                                         if (class(x = set_duration_refs_data) != "data.frame") {
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
                                     } else {
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     # manual typage ----
                                     if (data_source %in% c("csv")) {
                                       set_duration_refs_data$year <- as.integer(set_duration_refs_data$year)
                                       set_duration_refs_data$country <- as.character(set_duration_refs_data$country)
                                       set_duration_refs_data$ocean <- as.integer(set_duration_refs_data$ocean)
                                       set_duration_refs_data$school_type <- as.integer(set_duration_refs_data$school_type)
                                       set_duration_refs_data$parameter_a <- as.numeric(set_duration_refs_data$parameter_a)
                                       set_duration_refs_data$parameter_b <- as.numeric(set_duration_refs_data$parameter_b)
                                       set_duration_refs_data$null_set_value <- as.numeric(set_duration_refs_data$null_set_value)
                                     }
                                     private$setdurationrefs <- set_duration_refs_data
                                   },
                                   #' @description Creation of a data frame object with length ratio between ld1 and lf class.
                                   #' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "t3_db" but you can switch to "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R environment).
                                   #' @param db_con Object of class "database" expected. Check {\link[dbConnect]{dbConnect}}. An R's object which contain connection identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_path Object of class {\link[base]{character}} expected. Path of the data sql/csv/RData file. By default NULL.
                                   #' @param envir Object of class {\link[base]{character}} expected. Specify an environment to look in for data source "envir". By default the first environment where data are found will be used.
                                   lengthsteps_data = function(db_con = NULL,
                                                               data_source = "t3_db",
                                                               data_path = NULL,
                                                               envir = NULL) {
                                     if (data_source == "t3_db") {
                                       # t3 db source ----
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
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
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
                                       }
                                     } else if (data_source == "avdth_db") {
                                       # avdth db source ----
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start length steps data importation from avdth database.\n",
                                           sep = "")
                                       lengthstep_sql <- paste(readLines(con = system.file("sql\\avdth",
                                                                                           "avdth_lengthsteps.sql",
                                                                                           package = "t3")),
                                                               collapse = "\n")
                                       cat("[", lengthstep_sql, "]\n", sep = "")
                                       lengthsteps_data <- DBI::dbGetQuery(conn = db_con,
                                                                           statement = lengthstep_sql)
                                       lengthsteps_data$ocean <- as.integer(lengthsteps_data$ocean)
                                       lengthsteps_data$specie_code <- as.integer(lengthsteps_data$specie_code)
                                       lengthsteps_data$specie_code3l <- as.character(lengthsteps_data$specie_code3l)
                                       lengthsteps_data$ld1_class <- as.integer(lengthsteps_data$ld1_class)
                                       lengthsteps_data$lf_class <- as.integer(lengthsteps_data$lf_class)
                                       lengthsteps_data$ratio <- as.numeric(lengthsteps_data$ratio)
                                       if (nrow(x = lengthsteps_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data imported, check the query and query's parameters.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length steps data importation from avdth database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "sql_query") {
                                       # sql queries source ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
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
                                       }
                                     } else if (data_source == "csv") {
                                       # csv source ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
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
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful length steps data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       # rdata source ----
                                       if (class(x = data_path) != "character"
                                           || length(x = data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(x = data_path) != "RData") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start length step data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "lengthsteps",
                                                  envir = tmp_envir)) {
                                         lengthsteps_data <- get(x = "lengthsteps",
                                                                 envir = tmp_envir)
                                         if (class(x = lengthsteps_data) != "data.frame") {
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
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "lengthsteps")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       if (exists(x = "lengthsteps",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start length step data importation from R environment.\n",
                                             sep = "")
                                         lengthsteps_data <- get(x = "lengthsteps",
                                                                 envir = environment_name)
                                         if (class(x = lengthsteps_data) != "data.frame") {
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
                                     } else {
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     cat("Be careful! Manual values for BET of LD1 of 55 cm added (related to data in 2018), don't forget to remove it after updating reference table in the database.\n")
                                     lengthsteps_data_tmp <- data.frame(ocean = 2,
                                                                        specie_code = 3,
                                                                        specie_code3l = "BET",
                                                                        ld1_class = 55,
                                                                        lf_class = ((55 + 21.45108)^2 / (5.28756^2)),
                                                                        ratio = 100)
                                     lengthsteps_data <- rbind(lengthsteps_data_tmp,
                                                               lengthsteps_data)
                                     private$lengthsteps <- lengthsteps_data
                                   },
                                   #' @description Creation of a data frame object with weighted weigth of each set sampled.
                                   #' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "t3_db" but you can switch to "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R environment).
                                   #' @param db_con Object of class "database" expected. Check {\link[dbConnect]{dbConnect}}. An R's object which contain connection identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
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
                                     if (data_source == "t3_db") {
                                       # t3 db source ----
                                       if (length(x = class(x = periode_reference)) != 1
                                           || class(x = periode_reference) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = countries)) != 1
                                                  || class(x = countries) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = oceans)) != 1
                                                  || class(x = oceans) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(x = db_con) != "PostgreSQLConnection") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start sample sets data importation from T3 database.\n",
                                             sep = "")
                                         samplesets_sql <- paste(readLines(con = system.file("sql",
                                                                                             "t3_samplesets.sql",
                                                                                             package = "t3")),
                                                                 collapse = "\n")
                                         if (! is.null(x = trips_selected)) {
                                           if (class(x = trips_selected) != "character") {
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
                                                                                                             "-03-01"),
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
                                                                                                           "-03-01"),
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
                                       }
                                     } else if (data_source == "avdth_db") {
                                       # avdth db source ----
                                       if (length(x = class(x = periode_reference)) != 1
                                           || class(x = periode_reference) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = countries)) != 1
                                                  || class(x = countries) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (length(x = class(x = oceans)) != 1
                                                  || class(x = oceans) != "integer") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(x = db_con) != "JDBCConnection") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"JDBCConnection\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start sample sets data importation from avdth database.\n",
                                             sep = "")
                                         samplesets_sql <- paste(readLines(con = system.file("sql\\avdth",
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
                                                                                                           "-03-01#")),
                                                                                       countries = DBI::SQL(paste0("'",
                                                                                                                   paste0(countries,
                                                                                                                          collapse = "', '"),
                                                                                                                   "'")),
                                                                                       oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                                                       collapse = ", "))))

                                         cat("[", samplesets_sql_final, "]\n", sep = "")
                                         samplesets_data <- DBI::dbGetQuery(conn = db_con,
                                                                            statement = samplesets_sql_final)
                                         samplesets_data$trip_id <- as.character(samplesets_data$trip_id)
                                         samplesets_data$activity_id <- as.character(samplesets_data$activity_id)
                                         samplesets_data$well_id <- as.character(samplesets_data$well_id)
                                         samplesets_data$sample_id <- as.character(samplesets_data$sample_id)
                                         samplesets_data$well_set_weighted_weight <- as.numeric(samplesets_data$well_set_weighted_weight)
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
                                       }
                                     } else if (data_source == "sql_query") {
                                       # sql queries source ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
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
                                       }
                                     } else if (data_source == "csv") {
                                       # csv source ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
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
                                       }
                                     } else if (data_source == "rdata") {
                                       # rdata source ----
                                       if (class(x = data_path) != "character"
                                           || length(x = data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(x = data_path) != "RData") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples set data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "samplesets",
                                                  envir = tmp_envir)) {
                                         samplesets_data <- get(x = "samplesets",
                                                                envir = tmp_envir)
                                         if (class(x = samplesets_data) != "data.frame") {
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
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "samplesets")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       if (exists(x = "samplesets",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples set data importation from R environment.\n",
                                             sep = "")
                                         samplesets_data <- get(x = "samplesets",
                                                                envir = environment_name)
                                         if (class(x = samplesets_data) != "data.frame") {
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
                                     } else {
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     private$samplesets <- samplesets_data
                                   },
                                   #' @description Creation of a data frame object with parameters for length weight relationship.
                                   #' @param data_source Object of class {\link[base]{character}} expected. Identification of data source. By default "t3_db" but you can switch to "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R environment).
                                   #' @param db_con Object of class "database" expected. Check {\link[dbConnect]{dbConnect}}. An R's object which contain connection identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_path Object of class {\link[base]{character}} expected. Path of the data sql/csv/RData file. By default NULL.
                                   #' @param envir Object of class {\link[base]{character}} expected. Specify an environment to look in for data source "envir". By default the first environment where data are found will be used.
                                   lengthweightrelationships_data = function(data_source = "t3_db",
                                                                             db_con = NULL,
                                                                             data_path = NULL,
                                                                             envir = NULL) {
                                     if (data_source == "t3_db") {
                                       # t3 db source ----
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Start length weight relationship data importation from T3 database.\n",
                                           sep = "")
                                       lengthweightrelationships_sql <- paste(readLines(con = system.file("sql",
                                                                                                          "t3_lengthweightrelationships.sql",
                                                                                                          package = "t3")),
                                                                              collapse = "\n")
                                       cat("[", lengthweightrelationships_sql, "]\n", sep = "")
                                       lengthweightrelationships_data <- DBI::dbGetQuery(conn = db_con,
                                                                                         statement = lengthweightrelationships_sql)
                                       if (nrow(x = lengthweightrelationships_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data imported, check the query and query's parameters.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length weight relationship data importation from T3 database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "sql_query") {
                                       # sql queries ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start length weight relationship data importation from the database.\n",
                                             sep = "")
                                         lengthweightrelationships_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                                             collapse = "\n"))
                                         cat("[", lengthweightrelationships_sql, "]\n", sep = "")
                                         lengthweightrelationships_data <- DBI::dbGetQuery(conn = db_con,
                                                                                           statement = lengthweightrelationships_sql)
                                         if (nrow(x = lengthweightrelationships_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful length weight relationship data importation from the database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "csv") {
                                       # csv source ----
                                       if (class(x = data_path) != "character") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start length weight relationship data importation from csv file.\n",
                                             sep = "")
                                         lengthweightrelationships_data <- read.csv2(file = data_path,
                                                                                     stringsAsFactors = FALSE)
                                         if (nrow(lengthweightrelationships_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Successful length weight relationship data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       # rdata source ----
                                       if (class(x = data_path) != "character"
                                           || length(x = data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(x = data_path) != "RData") {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "lengthweightrelationships",
                                                  envir = tmp_envir)) {
                                         lengthweightrelationships_data <- get(x = "lengthweightrelationships",
                                                                               envir = tmp_envir)
                                         if (class(lengthweightrelationships_data) != "data.frame") {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"lengthweightrelationships_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"lengthweightrelationships\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(x = lengthweightrelationships_data) == 0) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"lengthweightrelationships\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length weight relationships data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       # R environment source ----
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "lengthweightrelationships")[1])
                                       } else {
                                         environment_name <- as.environment(envir)
                                       }
                                       if (exists(x = "lengthweightrelationships",
                                                  envir = environment_name)) {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Start length weight relationships data importation from R environment.\n",
                                             sep = "")
                                         lengthweightrelationships_data <- get(x = "lengthweightrelationships",
                                                                               envir = environment_name)
                                         if (class(x = lengthweightrelationships_data) != "data.frame") {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"lengthweightrelationships\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(x = lengthweightrelationships_data) == 0) {
                                           cat(format(x = Sys.time(),
                                                      format = "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"length weight relationships\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length weight relationships data importation R environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(x = Sys.time(),
                                                    format = "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"lengthweightrelationships\" available in the R environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else {
                                       cat(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     # manual typage ----
                                     if (data_source %in% c("csv")) {
                                       lengthweightrelationships_data$ocean <- as.integer(lengthweightrelationships_data$ocean)
                                       lengthweightrelationships_data$specie_code <- as.integer(lengthweightrelationships_data$specie_code)
                                       lengthweightrelationships_data$specie_code3l <- as.character(lengthweightrelationships_data$specie_code3l)
                                       lengthweightrelationships_data$lwr_a <- as.numeric(lengthweightrelationships_data$lwr_a)
                                       lengthweightrelationships_data$lwr_b <- as.numeric(lengthweightrelationships_data$lwr_b)
                                     }
                                     private$lengthweightrelationships <- lengthweightrelationships_data
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
