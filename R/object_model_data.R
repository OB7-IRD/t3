#' @title R6 class object_model_data creation
#' @name object_model_data
#' @description Create R6 reference object class object_model_data
#' @importFrom R6 R6Class
#' @importFrom DBI sqlInterpolate SQL dbGetQuery
#' @importFrom dplyr first last filter
#' @importFrom rlang global_env current_env
object_model_data <- R6::R6Class(classname = "object_model_data",
                                 public = list(
                                   #' @description Creation of a R6 reference object class trips which contain one or more R6 reference object class trip
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R environment).
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param oceans (integer) Ocean(s) related to data coded on 1 digit. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path (character) Path of the data sql/csv/RData file. By default NULL.
                                   #' @param trips_selected (character) Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   #' @param envir (character) Specify an environment to look in for data source "envir". You can choose between "global" and "current". By default "global.
                                   trips_object_creation = function(db_con = NULL,
                                                                    data_source = "t3_db",
                                                                    periode_reference = NULL,
                                                                    countries = NULL,
                                                                    oceans = NULL,
                                                                    data_path = NULL,
                                                                    trips_selected = NULL,
                                                                    envir = "global") {
                                     if (data_source == "t3_db") {
                                       if (length(class(periode_reference)) != 1 || class(periode_reference) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (length(class(countries)) != 1 || class(countries) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (length(class(oceans)) != 1 || class(oceans) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(db_con) != "PostgreSQLConnection") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from T3 database.\n",
                                             sep = "")
                                         trip_sql <- paste(readLines(con = system.file("sql",
                                                                                       "t3_trip.sql",
                                                                                       package = "t3")),
                                                           collapse = "\n")
                                         if (! is.null(trips_selected)) {
                                           if (class(trips_selected) != "character") {
                                             cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                 " - Error: invalid \"trips_selected\" argument, ",
                                                 "class \"character\" expected if not NULL.\n",
                                                 sep = "")
                                             stop()
                                           } else {
                                             trip_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                   sql = trip_sql,
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
                                           trip_sql <- sub(pattern = "\n\tAND t.topiaid IN (?trips_selected)",
                                                           replacement = "",
                                                           x = trip_sql,
                                                           fixed = TRUE)
                                           trip_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                 sql = trip_sql,
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
                                         cat("[", trip_sql_final, "]\n", sep = "")
                                         trip_data <- DBI::dbGetQuery(db_con, trip_sql_final)
                                         if (nrow(trip_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful trip(s) data importation from T3 database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "sql_query") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from database.\n",
                                             sep = "")
                                         trip_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                        collapse = "\n"))
                                         cat("[", trip_sql, "]\n", sep = "")
                                         trip_data <- DBI::dbGetQuery(db_con, trip_sql)
                                         if (nrow(trip_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful trip(s) data importation from database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "csv") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from csv file.\n",
                                             sep = "")
                                         trip_data <- read.csv2(file = data_path,
                                                                stringsAsFactors = FALSE)
                                         if (nrow(trip_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful trip(s) data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       if (class(data_path) != "character"
                                           || length(data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(data_path) != "RData") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "trips",
                                                  envir = tmp_envir)) {
                                         trip_data <- get(x = "trips",
                                                          envir = tmp_envir)
                                         if (class(trip_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"trip_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"trips\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(trip_data) == 0) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"trips\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful trip(s) data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "trips")[1])
                                       } else {
                                         if ((class(x = envir) != "character"
                                              || length(x = envir) != 1)) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"envir\" argument, one value of class \"character\" expected.\n",
                                               "(\"global\" or \"current\").\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           environment_name <- as.environment(envir)
                                         }
                                       }
                                       if (exists(x = "trips",
                                                  envir = environment_name)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from R global environment.\n",
                                             sep = "")
                                         trip_data <- get(x = "trips",
                                                          envir = environment_name)
                                         if (class(trip_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"trip_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(trip_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"trips\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful trip(s) data importation R global environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"trips\" available in the R global environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     trip_data <- unclass(trip_data)
                                     object_trips <- t3::object_r6(class_name = "trips")
                                     object_trips$add(lapply(seq_len(length.out = length(trip_data[[1]])),
                                                             function(i) {
                                                               cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                   " - Start importation of trip element ",
                                                                   i,
                                                                   ".\n",
                                                                   "[trip: ",
                                                                   trip_data[[1]][i],
                                                                   "]\n",
                                                                   sep = "")
                                                               trip <- t3:::trip$new(trip_id = trip_data[[1]][i],
                                                                                     fleet = trip_data[[2]][i],
                                                                                     departure_date = trip_data[[3]][i],
                                                                                     landing_date = trip_data[[4]][i],
                                                                                     logbook_availability = trip_data[[5]][i],
                                                                                     fish_hold_empty = trip_data[[6]][i],
                                                                                     vessel_id = trip_data[[7]][i],
                                                                                     vessel_type = trip_data[[8]][i])
                                                               cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                   " - Successful importation of trip element ",
                                                                   i,
                                                                   ".\n",
                                                                   sep = "")
                                                               return(trip)
                                                             }))
                                     private$trips <- object_trips
                                   },
                                   #' @description Creation of a R6 reference object class activities which contain one or more R6 reference object class activity.
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R global environment).
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param oceans (integer) Ocean(s) related to data coded on 1 digit. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   #' @param trips_selected (character) Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   #' @param envir (character) Specify an environment to look in for data source "envir". You can choose between "global" and "current". By default "global.
                                   activities_object_creation = function(db_con = NULL,
                                                                         data_source = "t3_db",
                                                                         periode_reference = NULL,
                                                                         countries = NULL,
                                                                         oceans = NULL,
                                                                         data_path = NULL,
                                                                         trips_selected = NULL,
                                                                         envir = NULL) {
                                     if (data_source == "t3_db") {
                                       if (length(class(periode_reference)) != 1 || class(periode_reference) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(class(countries)) != 1 || class(countries) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (length(class(oceans)) != 1 || class(oceans) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(db_con) != "PostgreSQLConnection") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start activities data importation from T3 database.\n",
                                             sep = "")
                                         activities_sql <- paste(readLines(con = system.file("sql",
                                                                                             "t3_activities.sql",
                                                                                             package = "t3")),
                                                                 collapse = "\n")
                                         if (! is.null(trips_selected)) {
                                           if (class(trips_selected) != "character") {
                                             cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
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
                                         activities_data <- DBI::dbGetQuery(db_con, activities_sql_final)
                                         if (nrow(activities_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful activities data importation from T3 database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "sql_query") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start activities data importation from the database.\n",
                                             sep = "")
                                         activities_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                              collapse = "\n"))
                                         cat("[", activities_sql, "]\n", sep = "")
                                         activities_data <- DBI::dbGetQuery(db_con, activities_sql)
                                         if (nrow(activities_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful activities data importation from the database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "csv") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start activities data importation from csv file.\n",
                                             sep = "")
                                         activities_data <- read.csv2(file = data_path,
                                                                      stringsAsFactors = FALSE)
                                         if (nrow(activities_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful activities data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       if (class(data_path) != "character"
                                           || length(data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(data_path) != "RData") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start activities data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "activities",
                                                  envir = tmp_envir)) {
                                         activities_data <- get(x = "activities",
                                                          envir = tmp_envir)
                                         if (class(activities_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"activities_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"activities\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(activities_data) == 0) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"activities\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful activities data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "activities")[1])
                                       } else {
                                         if ((class(x = envir) != "character"
                                              || length(x = envir) != 1)) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"envir\" argument, one value of class \"character\" expected.\n",
                                               "(\"global\" or \"current\").\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           environment_name <- as.environment(envir)
                                         }
                                       }
                                       if (exists(x = "activities",
                                                  envir = environment_name)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start activities data importation from R global environment.\n",
                                             sep = "")
                                         activities_data <- get(x = "activities",
                                                                envir = environment_name)
                                         if (class(activities_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"activities_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(activities_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"activities\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful activities data importation R global environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no R object named \"activities\" available in the R global environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     activities_data <- unclass(activities_data)
                                     object_activities <- t3::object_r6(class_name = "activities")
                                     object_activities$add(lapply(seq_len(length.out = length(activities_data[[1]])),
                                                                  function(i) {
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
                                                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                        " - Successful importation of activity element ",
                                                                        i,
                                                                        ".\n",
                                                                        sep = "")
                                                                    return(activity)
                                                                  }))
                                     private$activities <- object_activities
                                   },
                                   #' @description Creation of a R6 reference object class elementarycatches which contain one or more R6 reference object class elementarycatch
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R global environment).
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param oceans (integer) Ocean(s) related to data coded on 1 digit. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   #' @param trips_selected (character) Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   #' @param envir (character) Specify an environment to look in for data source "envir". You can choose between "global" and "current". By default "global.
                                   elementarycatches_object_creation = function(db_con = NULL,
                                                                                data_source = "t3_db",
                                                                                periode_reference = NULL,
                                                                                countries = NULL,
                                                                                oceans = NULL,
                                                                                data_path = NULL,
                                                                                trips_selected = NULL,
                                                                                envir = "global") {
                                     if (data_source == "t3_db") {
                                       if (length(class(periode_reference)) != 1 || class(periode_reference) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(class(countries)) != 1 || class(countries) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (length(class(oceans)) != 1 || class(oceans) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(db_con) != "PostgreSQLConnection") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary catches data importation from T3 database.\n",
                                             sep = "")
                                         elementarycatch_sql <- paste(readLines(con = system.file("sql",
                                                                                                  "t3_elementarycatch.sql",
                                                                                                  package = "t3")),
                                                                      collapse = "\n")
                                         if (! is.null(trips_selected)) {
                                           if (class(trips_selected) != "character") {
                                             cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                 " - Error: invalid \"trips_selected\" argument, ",
                                                 "class \"character\" expected if not NULL.\n",
                                                 sep = "")
                                             stop()
                                           } else {
                                             elementarycatch_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                              sql = elementarycatch_sql,
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
                                           elementarycatch_sql <- sub(pattern = "\n\tAND t.topiaid IN (?trips_selected)",
                                                                      replacement = "",
                                                                      x = elementarycatch_sql,
                                                                      fixed = TRUE)
                                           elementarycatch_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                            sql = elementarycatch_sql,
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
                                         cat("[", elementarycatch_sql_final, "]\n", sep = "")
                                         elementarycatch_data <- DBI::dbGetQuery(db_con, elementarycatch_sql_final)
                                         if (nrow(elementarycatch_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Start elementary catches data importation from T3 database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "sql_query") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary catches data importation from the database.\n",
                                             sep = "")
                                         elementarycatch_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                                   collapse = "\n"))
                                         cat("[", elementarycatch_sql, "]\n", sep = "")
                                         elementarycatch_data <- DBI::dbGetQuery(db_con, elementarycatch_sql)
                                         if (nrow(elementarycatch_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful elementary catches data importation from the database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "csv") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary catches data importation from csv file.\n",
                                             sep = "")
                                         elementarycatch_data <- read.csv2(file = data_path,
                                                                           stringsAsFactors = FALSE)
                                         if (nrow(elementarycatch_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful elementary catches data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       if (class(data_path) != "character"
                                           || length(data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(data_path) != "RData") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary catches data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "elementary_catches",
                                                  envir = tmp_envir)) {
                                         elementarycatch_data <- get(x = "elementary_catches",
                                                          envir = tmp_envir)
                                         if (class(elementarycatch_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"elementarycatch_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"elementary_catches\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(elementarycatch_data) == 0) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"elementary_catches\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary catches data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "elementary_catches")[1])
                                       } else {
                                         if ((class(x = envir) != "character"
                                              || length(x = envir) != 1)) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"envir\" argument, one value of class \"character\" expected.\n",
                                               "(\"global\" or \"current\").\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           environment_name <- as.environment(envir)
                                         }
                                       }
                                       if (exists(x = "elementary_catches",
                                                  envir = environment_name)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary catches data importation from R global environment.\n",
                                             sep = "")
                                         elementarycatch_data <- get(x = "elementary_catches",
                                                                     envir = environment_name)
                                         if (class(elementarycatch_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"elementarycatch_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(elementarycatch_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"elementary catches\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary catches data importation R global environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"elementary_catches\" available in the R global environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     elementarycatch_data <- unclass(elementarycatch_data)
                                     object_elementarycatches <- t3::object_r6(class_name = "elementarycatches")
                                     object_elementarycatches$add(lapply(seq_len(length.out = length(elementarycatch_data[[1]])),
                                                                         function(i) {
                                                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                               " - Start importation of elementary catch element ",
                                                                               i,
                                                                               ".\n",
                                                                               "[elementarycatch: ",
                                                                               elementarycatch_data[[2]][i],
                                                                               "]\n",
                                                                               sep = "")
                                                                           elementarycatch <- t3:::elementarycatch$new(activity_id = elementarycatch_data[[1]][i],
                                                                                                                       elementarycatch_id = elementarycatch_data[[2]][i],
                                                                                                                       ocean = elementarycatch_data[[3]][i],
                                                                                                                       school_type = elementarycatch_data[[4]][i],
                                                                                                                       logbook_category = elementarycatch_data[[5]][i],
                                                                                                                       logbook_category_name = elementarycatch_data[[6]][i],
                                                                                                                       specie_code = elementarycatch_data[[7]][i],
                                                                                                                       specie_code3l = elementarycatch_data[[8]][i],
                                                                                                                       catch_weight = elementarycatch_data[[9]][i])
                                                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                               " - Successful importation of elementary catch element ",
                                                                               i,
                                                                               ".\n",
                                                                               sep = "")
                                                                           return(elementarycatch)
                                                                         }))
                                     private$elementarycatches <- object_elementarycatches
                                   },
                                   #' @description Creation of a R6 reference object class elementarylandings which contain one or more R6 reference object class elementarylanding
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R global environment).
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param oceans (integer) Ocean(s) related to data coded on 1 digit. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   #' @param trips_selected (character) Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   #' @param envir (character) Specify an environment to look in for data source "envir". You can choose between "global" and "current". By default "global.
                                   elementarylandings_object_creation = function(db_con = NULL,
                                                                                 data_source = "t3_db",
                                                                                 periode_reference = NULL,
                                                                                 countries = NULL,
                                                                                 oceans = NULL,
                                                                                 data_path = NULL,
                                                                                 trips_selected = NULL,
                                                                                 envir = "global") {
                                     if (data_source == "t3_db") {
                                       if (length(class(periode_reference)) != 1 || class(periode_reference) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(class(countries)) != 1 || class(countries) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (length(class(oceans)) != 1 || class(oceans) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(db_con) != "PostgreSQLConnection") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary landings data importation from T3 database.\n",
                                             sep = "")
                                         elementarylanding_sql <- paste(readLines(con = system.file("sql",
                                                                                                    "t3_elementarylanding.sql",
                                                                                                    package = "t3")),
                                                                        collapse = "\n")
                                         if (! is.null(trips_selected)) {
                                           if (class(trips_selected) != "character") {
                                             cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                 " - Error: invalid \"trips_selected\" argument, ",
                                                 "class \"character\" expected if not NULL.\n",
                                                 sep = "")
                                             stop()
                                           } else {
                                             elementarylanding_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                                sql = elementarylanding_sql,
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
                                           elementarylanding_sql <- sub(pattern = "\n\tAND t.topiaid IN (?trips_selected)",
                                                                        replacement = "",
                                                                        x = elementarylanding_sql,
                                                                        fixed = TRUE)
                                           elementarylanding_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                              sql = elementarylanding_sql,
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
                                         cat("[", elementarylanding_sql_final, "]\n", sep = "")
                                         elementarylanding_data <- DBI::dbGetQuery(db_con, elementarylanding_sql_final)
                                         if (nrow(elementarylanding_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful elementary landings data importation from T3 database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "sql_query") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary landings data importation from the database.\n",
                                             sep = "")
                                         elementarylanding_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                                     collapse = "\n"))
                                         cat("[", elementarylanding_sql, "]\n", sep = "")
                                         elementarylanding_data <- DBI::dbGetQuery(db_con, elementarylanding_sql)
                                         if (nrow(elementarylanding_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful elementary landings data importation from the database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "csv") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary landings data importation from csv file.\n",
                                             sep = "")
                                         elementarylanding_data <- read.csv2(file = data_path,
                                                                             stringsAsFactors = FALSE)
                                         if (nrow(elementarylanding_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful elementary landings data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       if (class(data_path) != "character"
                                           || length(data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(data_path) != "RData") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary landings data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "elementary_landings",
                                                  envir = tmp_envir)) {
                                         elementarylanding_data <- get(x = "elementary_landings",
                                                          envir = tmp_envir)
                                         if (class(elementarylanding_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"elementarylanding_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"elementary_landings\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(elementarylanding_data) == 0) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"elementary_landings\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary landings data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "elementary_landings")[1])
                                       } else {
                                         if ((class(x = envir) != "character"
                                              || length(x = envir) != 1)) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"envir\" argument, one value of class \"character\" expected.\n",
                                               "(\"global\" or \"current\").\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           environment_name <- as.environment(envir)
                                         }
                                       }
                                       if (exists(x = "elementary_landings",
                                                  envir = environment_name)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary landings data importation from R global environment.\n",
                                             sep = "")
                                         elementarylanding_data <- get(x = "elementary_landings",
                                                                       envir = environment_name)
                                         if (class(elementarylanding_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"elementarylanding_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(elementarylanding_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"elementary landings\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary landings data importation R global environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"elementary_landings\" available in the R global environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     elementarylanding_data <- unclass(elementarylanding_data)
                                     object_elementarylandings <- t3::object_r6(class_name = "elementarylandings")
                                     object_elementarylandings$add(lapply(seq_len(length.out = length(elementarylanding_data[[1]])),
                                                                          function(i) {
                                                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                                " - Start importation of elementary landing element ",
                                                                                i,
                                                                                ".\n",
                                                                                "[elementarylanding: ",
                                                                                elementarylanding_data[[2]][i],
                                                                                "]\n",
                                                                                sep = "")
                                                                            elementarylanding <- t3:::elementarylanding$new(trip_id = elementarylanding_data[[1]][i],
                                                                                                                            elementarylanding_id = elementarylanding_data[[2]][i],
                                                                                                                            landing_category = elementarylanding_data[[3]][i],
                                                                                                                            landing_category_name = elementarylanding_data[[4]][i],
                                                                                                                            specie_code = elementarylanding_data[[5]][i],
                                                                                                                            specie_code3l = elementarylanding_data[[6]][i],
                                                                                                                            landing_weight = elementarylanding_data[[7]][i])
                                                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                                " - Successful importation of elementary landing element ",
                                                                                i,
                                                                                ".\n",
                                                                                sep = "")
                                                                            return(elementarylanding)
                                                                          }))
                                     private$elementarylandings <- object_elementarylandings
                                   },
                                   #' @description Creation of a R6 reference object class wells which contain one or more R6 reference object class well, wellset and samples
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R global environment).
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param oceans (integer) Ocean(s) related to data coded on 1 digit. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param sample_type (integer) Sample type identification (landing, observer, ...). By default NULL.
                                   #' @param trips_selected (character) Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference, countries and sample types). By default NULL.
                                   #' @param data_path_samples (character) Path of the data sql/csv file for samples. By default NULL.
                                   #' @param data_path_wellplans (character) Path of the data sql/csv file for well plans. By default NULL.
                                   #' @param envir (character) Specify an environment to look in for data source "envir". You can choose between "global" and "current". By default "global.
                                   wells_object_creation = function(db_con = NULL,
                                                                    data_source = "t3_db",
                                                                    periode_reference = NULL,
                                                                    countries = NULL,
                                                                    oceans = NULL,
                                                                    sample_type = NULL,
                                                                    trips_selected = NULL,
                                                                    data_path_samples = NULL,
                                                                    data_path_wellplans = NULL,
                                                                    envir = "global") {
                                     if (data_source == "t3_db") {
                                       if (length(class(periode_reference)) != 1 || class(periode_reference) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(class(countries)) != 1 || class(countries) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (length(class(oceans)) != 1 || class(oceans) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(db_con) != "PostgreSQLConnection") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                       } else if (class(sample_type) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"sample_type\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples data importation from T3 database.\n",
                                             sep = "")
                                         samples_sql <- paste(readLines(con = system.file("sql",
                                                                                          "t3_samples.sql",
                                                                                          package = "t3")),
                                                              collapse = "\n")
                                         if (! is.null(trips_selected)) {
                                           if (class(trips_selected) != "character") {
                                             cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
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
                                         samples_data <- DBI::dbGetQuery(db_con, samples_sql_final)
                                         if (nrow(samples_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful samples data importation from T3 database.\n",
                                               sep = "")
                                         }
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start well plans data importation from T3 database.\n",
                                             sep = "")
                                         wellplan_sql <- paste(readLines(con = system.file("sql",
                                                                                           "t3_wellplan.sql",
                                                                                           package = "t3")),
                                                               collapse = "\n")
                                         if (! is.null(trips_selected)) {
                                           if (class(trips_selected) != "character") {
                                             cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
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
                                         wellplan_data <- DBI::dbGetQuery(db_con, wellplan_sql_final)
                                         if (nrow(wellplan_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful well plans data importation from T3 database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "sql_query") {
                                       if (class(data_path_samples) != "character" || class(data_path_wellplans) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path_samples\" argument and or \"data_path_wellplans\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path_samples) || ! file.exists(data_path_wellplans)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path_samples\" argument and or \"data_path_wellplans\" argument, ",
                                             "file(s) doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples data importation from the database.\n",
                                             sep = "")
                                         samples_sql <- DBI::SQL(x = paste(readLines(con = data_path_samples),
                                                                           collapse = "\n"))
                                         cat("[", samples_sql, "]\n", sep = "")
                                         samples_data <- DBI::dbGetQuery(db_con, samples_sql)
                                         if (nrow(samples_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful samples data importation from the database.\n",
                                               sep = "")
                                         }
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start well plans data importation from the database.\n",
                                             sep = "")
                                         wellplan_sql <- DBI::SQL(x = paste(readLines(con = data_path_wellplans),
                                                                            collapse = "\n"))
                                         cat("[", wellplan_sql, "]\n", sep = "")
                                         wellplan_data <- DBI::dbGetQuery(db_con, wellplan_sql)
                                         if (nrow(wellplan_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful well plans data importation from the database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "csv") {
                                       if (class(data_path_samples) != "character" || class(data_path_wellplans) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path_samples\" argument and or \"data_path_wellplans\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path_samples) || ! file.exists(data_path_wellplans)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path_samples\" argument and or \"data_path_wellplans\" argument, ",
                                             "file(s) doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples data importation from csv file.\n",
                                             sep = "")
                                         samples_data <- read.csv2(file = data_path_samples,
                                                                   stringsAsFactors = FALSE)
                                         if (nrow(samples_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful samples data importation from csv file.\n",
                                               sep = "")
                                         }
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start well plans data importation from csv file.\n",
                                             sep = "")
                                         wellplan_data <- read.csv2(file = data_path_wellplans,
                                                                    stringsAsFactors = FALSE)
                                         if (nrow(wellplan_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful well plans data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       if (class(data_path_samples) != "character"
                                           || length(data_path_samples) != 1
                                           || ! file.exists(data_path_samples)
                                           || tools::file_ext(data_path_samples) != "RData") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path_samples\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path_samples,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "samples",
                                                  envir = tmp_envir)) {
                                         samples_data <- get(x = "samples",
                                                          envir = tmp_envir)
                                         if (class(samples_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"samples_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"samples\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(samples_data) == 0) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"samples\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful samples data importation from RData.\n",
                                             sep = "")
                                       }
                                       if (class(data_path_wellplans) != "character"
                                           || length(data_path_wellplans) != 1
                                           || ! file.exists(data_path_wellplans)
                                           || tools::file_ext(data_path_wellplans) != "RData") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path_wellplans\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start well plans data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path_wellplans,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "well_plans",
                                                  envir = tmp_envir)) {
                                         wellplan_data <- get(x = "well_plans",
                                                          envir = tmp_envir)
                                         if (class(wellplan_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"wellplan_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"well_plans\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(wellplan_data) == 0) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"well_plans\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful well plans data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "samples")[1])
                                       } else {
                                         if ((class(x = envir) != "character"
                                              || length(x = envir) != 1)) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"envir\" argument, one value of class \"character\" expected.\n",
                                               "(\"global\" or \"current\").\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           environment_name <- as.environment(envir)
                                         }
                                       }
                                       if (exists(x = "samples",
                                                  envir = environment_name)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples data importation from R global environment.\n",
                                             sep = "")
                                         samples_data <- get(x = "samples",
                                                             envir = environment_name)
                                         if (class(samples_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"samples_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(samples_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"samples\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful samples data importation R global environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"samples\" available in the R global environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (exists(x = "well_plans",
                                                  envir = if ( envir == "global") {
                                                    rlang::global_env()
                                                  } else {
                                                    rlang::current_env()
                                                  })) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start well plans data importation from R global environment.\n",
                                             sep = "")
                                         wellplan_data <- get(x = "well_plans",
                                                              envir = if (envir == "global") {
                                                                rlang::global_env()
                                                              } else {
                                                                rlang::current_env()
                                                              })
                                         if (class(wellplan_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"wellplan_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(wellplan_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"well plans\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful well plans data importation R global environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"well_plans\" available in the R global environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     object_wells <- t3::object_r6(class_name = "wells")
                                     for (trip in unique(samples_data$trip_id)) {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Start importation of well(s) data for trip element ",
                                           which(unique(samples_data$trip_id) == trip),
                                           ".\n",
                                           "[trip: ",
                                           trip,
                                           "]\n",
                                           sep = "")
                                       tmp_trip <- dplyr::filter(.data = samples_data, trip_id == trip)
                                       for (well in unique(tmp_trip$well_id)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start importation of well data item ",
                                             which(unique(tmp_trip$well_id) == well),
                                             ".\n",
                                             "[well: ",
                                             well,
                                             "]\n",
                                             sep = "")
                                         if (is.na(well)) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Warning: missing \"well_id\" argument in trip number: \"",
                                               trip,
                                               "\".\n",
                                               sep = "")
                                           tmp_well <- dplyr::filter(.data = tmp_trip, is.na(well_id))
                                           if (length(unique(tmp_well$sample_id)) != 1) {
                                             cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
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
                                         if (length(unique(tmp_well$well_minus10_weigth)) != 1) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Warning: invalid \"well_minus10_weigth\" argument. Well data avoided and not imported.\n",
                                               "[trip: ",
                                               trip,
                                               ", well: ",
                                               well,
                                               "]\n",
                                               sep = "")
                                           next()
                                         } else if (length(unique(tmp_well$well_plus10_weigth)) != 1) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Warning: invalid \"well_plus10_weigth\" argument. Well data avoided and not imported.\n",
                                               "[trip: ",
                                               trip,
                                               ", well: ",
                                               well,
                                               "]\n",
                                               sep = "")
                                           next()
                                         } else if (length(unique(tmp_well$well_global_weigth)) != 1) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Warning: invalid \"well_global_weigth\" argument. Well data avoided and not imported.\n",
                                               "[trip: ",
                                               trip,
                                               ", well: ",
                                               well,
                                               "]\n",
                                               sep = "")
                                           next()
                                         } else {
                                           object_well <- t3:::well$new(trip_id = trip,
                                                                        well_id = well,
                                                                        well_minus10_weigth = unique(tmp_well$well_minus10_weigth),
                                                                        well_plus10_weigth = unique(tmp_well$well_plus10_weigth),
                                                                        well_global_weigth = unique(tmp_well$well_global_weigth))
                                         }
                                         for (sample in unique(tmp_well$sample_id)) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Start importation of sample data item ",
                                               which(unique(tmp_well$sample_id) == sample),
                                               ".\n",
                                               "[sample: ",
                                               sample,
                                               "]\n",
                                               sep = "")
                                           tmp_sample <- dplyr::filter(.data = tmp_well, sample_id == sample)
                                           tmp_sample <- unclass(tmp_sample)
                                           object_well$.__enclos_env__$private$elementarysampleraw <- append(object_well$.__enclos_env__$private$elementarysampleraw,
                                                                                                             list(lapply(X = seq_len(length.out = length(tmp_sample[[1]])),
                                                                                                                         FUN = function(i) {
                                                                                                                           t3:::elementarysampleraw$new(trip_id = trip,
                                                                                                                                                        well_id = well,
                                                                                                                                                        sample_id = sample,
                                                                                                                                                        sub_sample_id = tmp_sample[[7]][i],
                                                                                                                                                        sample_quality = tmp_sample[[8]][i],
                                                                                                                                                        sample_type = tmp_sample[[9]][i],
                                                                                                                                                        specie_code = tmp_sample[[10]][i],
                                                                                                                                                        specie_code3l = tmp_sample[[11]][i],
                                                                                                                                                        length_type = tmp_sample[[12]][i],
                                                                                                                                                        sample_total_count = tmp_sample[[13]][i],
                                                                                                                                                        sample_number_measured = tmp_sample[[14]][i],
                                                                                                                                                        sample_length_class = tmp_sample[[15]][i])
                                                                                                                         })))
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful importation of sample data item ",
                                               which(unique(tmp_well$sample_id) == sample),
                                               ".\n",
                                               "[sample: ",
                                               sample,
                                               "]\n",
                                               sep = "")
                                         }
                                         if (well %in% unique(wellplan_data$well_id)) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Start importation of well plan data item ",
                                               which(unique(wellplan_data$well_id) == well),
                                               ".\n",
                                               "[well: ",
                                               well,
                                               "]\n",
                                               sep = "")
                                           if (well == "fr.ird.t3.entities.data.Well#1584558866921#0.7612530116341976") {
                                             browser()
                                           }
                                           tmp_wellplan <- dplyr::filter(.data = wellplan_data, well_id == well)
                                           tmp_wellplan <- unclass(tmp_wellplan)
                                           object_well$.__enclos_env__$private$wellplan <- lapply(X = seq_len(length.out = length(tmp_wellplan[[1]])),
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
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Sucessful importation of well plan data item ",
                                               which(unique(wellplan_data$well_id) == well),
                                               ".\n",
                                               "[well: ",
                                               well,
                                               "]\n",
                                               sep = "")
                                         }
                                         object_wells$add(object_well)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful importation of well data item ",
                                             which(unique(tmp_trip$well_id) == well),
                                             ".\n",
                                             "[well: ",
                                             well,
                                             "]\n",
                                             sep = "")
                                       }
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Successful importation of well(s) data for trip element ",
                                           which(unique(samples_data$trip_id) == trip),
                                           ".\n",
                                           "[trip: ",
                                           trip,
                                           "]\n",
                                           sep = "")
                                     }
                                     private$wells <- object_wells
                                   },
                                   #' @description Creation of a data frame object with parameters of set duration algorithms
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R global environment).
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   #' @param envir (character) Specify an environment to look in for data source "envir". You can choose between "global" and "current". By default "global.
                                   setduration_data = function(db_con = NULL,
                                                               data_source = "t3_db",
                                                               periode_reference = NULL,
                                                               countries = NULL,
                                                               data_path = NULL,
                                                               envir = "global") {
                                     if (data_source == "t3_db") {
                                       if (length(class(periode_reference)) != 1 || class(periode_reference) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(class(countries)) != 1 || class(countries) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (class(db_con) != "PostgreSQLConnection") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start set duration data importation from T3 database.\n",
                                             sep = "")
                                         setdurationref_sql <- paste(readLines(con = system.file("sql",
                                                                                                 "t3_setdurationref.sql",
                                                                                                 package = "t3")),
                                                                     collapse = "\n")
                                         setdurationref_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                         sql = setdurationref_sql,
                                                                                         period = DBI::SQL(paste0(c((dplyr::first(periode_reference,
                                                                                                                                  order_by = periode_reference) - 1)
                                                                                                                    : (dplyr::last(periode_reference,
                                                                                                                                   order_by = periode_reference) + 1)),
                                                                                                                  collapse = ", ")),
                                                                                         countries = DBI::SQL(paste0("'",
                                                                                                                     paste0(countries,
                                                                                                                            collapse = "', '"),
                                                                                                                     "'")))
                                         cat("[", setdurationref_sql_final, "]\n", sep = "")
                                         setdurationref_data <- DBI::dbGetQuery(db_con, setdurationref_sql_final)
                                         if (nrow(setdurationref_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful set duration data importation from T3 database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "sql_query") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start set duration data importation from the database.\n",
                                             sep = "")
                                         setdurationref_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                                  collapse = "\n"))
                                         cat("[", setdurationref_sql, "]\n", sep = "")
                                         setdurationref_data <- DBI::dbGetQuery(db_con, setdurationref_sql)
                                         if (nrow(setdurationref_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful set duration data importation from the database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "csv") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful set duration data importation from csv file.\n",
                                             sep = "")
                                         setdurationref_data <- read.csv2(file = data_path,
                                                                          stringsAsFactors = FALSE)
                                         if (nrow(setdurationref_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful set duration data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       if (class(data_path) != "character"
                                           || length(data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(data_path) != "RData") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start set duration references data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "set_duration_ref",
                                                  envir = tmp_envir)) {
                                         setdurationref_data <- get(x = "set_duration_ref",
                                                          envir = tmp_envir)
                                         if (class(setdurationref_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"setdurationref_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"set_duration_ref\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(setdurationref_data) == 0) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"set_duration_ref\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful set duration references data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "set_duration_ref")[1])
                                       } else {
                                         if ((class(x = envir) != "character"
                                              || length(x = envir) != 1)) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"envir\" argument, one value of class \"character\" expected.\n",
                                               "(\"global\" or \"current\").\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           environment_name <- as.environment(envir)
                                         }
                                       }
                                       if (exists(x = "set_duration_ref",
                                                  envir = environment_name)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start set duration references data importation from R global environment.\n",
                                             sep = "")
                                         setdurationref_data <- get(x = "set_duration_ref",
                                                                    envir = environment_name)
                                         if (class(setdurationref_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"setdurationref_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(setdurationref_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"elementary landings\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful set duration references data importation R global environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"set_duration_ref\" available in the R global environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     cat("Be careful! Manual added set duration parameters for 2020, don't forget to remove it after updating reference table in the database.\n")
                                     set_duration_ref_tmp <- data.frame(year = c(2020, 2020, 2020, 2020, 2020, 2020),
                                                                        country = c("FRA", "FRA", "FRA", "FRA", "FRA", "FRA"),
                                                                        ocean = c(1, 1, 1, 2, 2, 2),
                                                                        school_type = c(2, 1, 3, 2, 1, 3),
                                                                        parameter_a = c(0.68568, 0.60206, 0.64752, 0.56406, 0.56406, 0.56406),
                                                                        parameter_b = c(128.525, 138.829, 133.745, 141.223, 141.223, 141.223),
                                                                        null_set_value = c(111.2, 142.7, 123.0, 125.7, 140.2, 133.1))
                                     setdurationref_data <- rbind(setdurationref_data, set_duration_ref_tmp)
                                     private$setdurationref <- setdurationref_data
                                   },
                                   #' @description Creation of a data frame object with length ratio between ld1 and lf class.
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R global environment).
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   #' @param envir (character) Specify an environment to look in for data source "envir". You can choose between "global" and "current". By default "global.
                                   lengthstep_data = function(db_con = NULL,
                                                              data_source = "t3_db",
                                                              data_path = NULL,
                                                              envir = "global") {
                                     if (data_source == "t3_db") {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Start length steps data importation from T3 database.\n",
                                           sep = "")
                                       lengthstep_sql <- paste(readLines(con = system.file("sql",
                                                                                           "t3_lengthstep.sql",
                                                                                           package = "t3")),
                                                               collapse = "\n")
                                       cat("[", lengthstep_sql, "]\n", sep = "")
                                       lengthstep_data <- DBI::dbGetQuery(db_con,
                                                                          lengthstep_sql)
                                       if (nrow(lengthstep_data) == 0) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data imported, check the query and query's parameters.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length steps data importation from T3 database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "sql_query") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start length steps data importation from the database.\n",
                                             sep = "")
                                         lengthstep_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                              collapse = "\n"))
                                         cat("[", lengthstep_sql, "]\n", sep = "")
                                         lengthstep_data <- DBI::dbGetQuery(db_con,
                                                                            lengthstep_sql)
                                         if (nrow(lengthstep_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful length steps data importation from the database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "csv") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start length steps data importation from csv file.\n",
                                             sep = "")
                                         lengthstep_data <- read.csv2(file = data_path,
                                                                      stringsAsFactors = FALSE)
                                         if (nrow(lengthstep_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful length steps data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       if (class(data_path) != "character"
                                           || length(data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(data_path) != "RData") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start length step data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "length_step",
                                                  envir = tmp_envir)) {
                                         lengthstep_data <- get(x = "length_step",
                                                          envir = tmp_envir)
                                         if (class(lengthstep_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"lengthstep_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"length_step\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(lengthstep_data) == 0) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"length_step\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length step data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "length_step")[1])
                                       } else {
                                         if ((class(x = envir) != "character"
                                              || length(x = envir) != 1)) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"envir\" argument, one value of class \"character\" expected.\n",
                                               "(\"global\" or \"current\").\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           environment_name <- as.environment(envir)
                                         }
                                       }
                                       if (exists(x = "length_step",
                                                  envir = environment_name)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start length step data importation from R global environment.\n",
                                             sep = "")
                                         lengthstep_data <- get(x = "length_step",
                                                                envir = environment_name)
                                         if (class(lengthstep_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"lengthstep_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(lengthstep_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"length step\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length step data importation R global environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"length_step\" available in the R global environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     cat("Be careful! Manual values for BET of LD1 of 55 cm added (related to data in 2018), don't forget to remove it after updating reference table in the database.\n")
                                     lengthstep_data_tmp <- data.frame(ocean = 2,
                                                                       specie_code = 3,
                                                                       specie_code3l = "BET",
                                                                       ld1_class = 55,
                                                                       lf_class = ((55 + 21.45108)^2 / (5.28756^2)),
                                                                       ratio = 100)
                                     lengthstep_data <- rbind(lengthstep_data_tmp,
                                                              lengthstep_data)
                                     private$lengthstep <- lengthstep_data
                                   },
                                   #' @description Creation of a data frame object with weighted weigth of each set sampled.
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R global environment).
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param oceans (integer) Ocean(s) related to data coded on 1 digit. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   #' @param trips_selected (character) Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   #' @param envir (character) Specify an environment to look in for data source "envir". You can choose between "global" and "current". By default "global.
                                   sampleset_data = function(db_con = NULL,
                                                             data_source = "t3_db",
                                                             periode_reference = NULL,
                                                             countries = NULL,
                                                             oceans = NULL,
                                                             data_path = NULL,
                                                             trips_selected = NULL,
                                                             envir = "global") {
                                     if (data_source == "t3_db") {
                                       if (length(class(periode_reference)) != 1 || class(periode_reference) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (length(class(countries)) != 1 || class(countries) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument, ",
                                             "class \"character\" expected.\n",
                                             sep = "")
                                       } else if (length(class(oceans)) != 1 || class(oceans) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"oceans\" argument, ",
                                             "class \"integer\" expected.\n",
                                             sep = "")
                                       } else if (class(db_con) != "PostgreSQLConnection") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument, ",
                                             "class \"PostgreSQLConnection\" expected.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start sample sets data importation from T3 database.\n",
                                             sep = "")
                                         sampleset_sql <- paste(readLines(con = system.file("sql",
                                                                                            "t3_sampleset.sql",
                                                                                            package = "t3")),
                                                                collapse = "\n")
                                         if (! is.null(trips_selected)) {
                                           if (class(trips_selected) != "character") {
                                             cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                 " - Error: invalid \"trips_selected\" argument, ",
                                                 "class \"character\" expected if not NULL.\n",
                                                 sep = "")
                                             stop()
                                           } else {
                                             sampleset_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                        sql = sampleset_sql,
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
                                           sampleset_sql <- sub(pattern = "\n\tAND t.topiaid IN (?trips_selected)",
                                                                replacement = "",
                                                                x = sampleset_sql,
                                                                fixed = TRUE)
                                           sampleset_sql_final <- DBI::sqlInterpolate(conn = db_con,
                                                                                      sql = sampleset_sql,
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
                                         cat("[", sampleset_sql_final, "]\n", sep = "")
                                         sampleset_data <- DBI::dbGetQuery(db_con, sampleset_sql_final)
                                         if (nrow(sampleset_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query and query's parameters.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful sample sets data importation from T3 database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "sql_query") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start sample sets data importation from the database.\n",
                                             sep = "")
                                         sampleset_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                             collapse = "\n"))
                                         cat("[", sampleset_sql, "]\n", sep = "")
                                         sampleset_data <- DBI::dbGetQuery(db_con, sampleset_sql)
                                         if (nrow(sampleset_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful sample sets data importation from the database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "csv") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start sample sets data importation from csv file.\n",
                                             sep = "")
                                         sampleset_data <- read.csv2(file = data_path,
                                                                     stringsAsFactors = FALSE)
                                         if (nrow(sampleset_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful sample sets data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       if (class(data_path) != "character"
                                           || length(data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(data_path) != "RData") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples set data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "samples_set",
                                                  envir = tmp_envir)) {
                                         sampleset_data <- get(x = "samples_set",
                                                          envir = tmp_envir)
                                         if (class(sampleset_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"sampleset_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"samples_set\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(sampleset_data) == 0) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"samples_set\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful samples set data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "samples_set")[1])
                                       } else {
                                         if ((class(x = envir) != "character"
                                              || length(x = envir) != 1)) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"envir\" argument, one value of class \"character\" expected.\n",
                                               "(\"global\" or \"current\").\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           environment_name <- as.environment(envir)
                                         }
                                       }
                                       if (exists(x = "samples_set",
                                                  envir = environment_name)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start samples set data importation from R global environment.\n",
                                             sep = "")
                                         sampleset_data <- get(x = "samples_set",
                                                               envir = environment_name)
                                         if (class(sampleset_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"sampleset_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(sampleset_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"samples set\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful samples set data importation R global environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"samples_set\" available in the R global environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     private$sampleset <- sampleset_data
                                   },
                                   #' @description Creation of a data frame object with parameters for length weight relationship.
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query", "csv" (with separator character ";" and decimal ","), "rdata" or "envir" (for an object in the R global environment).
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   #' @param envir (character) Specify an environment to look in for data source "envir". You can choose between "global" and "current". By default "global.
                                   lengthweightrelationship_data = function(db_con = NULL,
                                                                            data_source = "t3_db",
                                                                            data_path = NULL,
                                                                            envir = "global") {
                                     if (data_source == "t3_db") {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Start length weight relationship data importation from T3 database.\n",
                                           sep = "")
                                       lengthweightrelationship_sql <- paste(readLines(con = system.file("sql",
                                                                                                         "t3_lengthweightrelationship.sql",
                                                                                                         package = "t3")),
                                                                             collapse = "\n")
                                       cat("[", lengthweightrelationship_sql, "]\n", sep = "")
                                       lengthweightrelationship_data <- DBI::dbGetQuery(db_con, lengthweightrelationship_sql)
                                       if (nrow(lengthweightrelationship_data) == 0) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data imported, check the query and query's parameters.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length weight relationship data importation from T3 database.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "sql_query") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start length weight relationship data importation from the database.\n",
                                             sep = "")
                                         lengthweightrelationship_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                                            collapse = "\n"))
                                         cat("[", lengthweightrelationship_sql, "]\n", sep = "")
                                         lengthweightrelationship_data <- DBI::dbGetQuery(db_con, lengthweightrelationship_sql)
                                         if (nrow(lengthweightrelationship_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the query.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful length weight relationship data importation from the database.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "csv") {
                                       if (class(data_path) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "class character expected.\n",
                                             sep = "")
                                         stop()
                                       } else if (! file.exists(data_path)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"data_path\" argument, ",
                                             "file doesn't exist.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start length weight relationship data importation from csv file.\n",
                                             sep = "")
                                         lengthweightrelationship_data <- read.csv2(file = data_path,
                                                                                    stringsAsFactors = FALSE)
                                         if (nrow(lengthweightrelationship_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data imported, check the csv file.\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Successful length weight relationship data importation from csv file.\n",
                                               sep = "")
                                         }
                                       }
                                     } else if (data_source == "rdata") {
                                       if (class(data_path) != "character"
                                           || length(data_path) != 1
                                           || ! file.exists(data_path)
                                           || tools::file_ext(data_path) != "RData") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid \"data_path\" argument, class character with one value inside linked to a \"RData\" file.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from RData.\n",
                                             sep = "")
                                         load(file = data_path,
                                              envir = tmp_envir <- new.env())
                                       }
                                       if (exists(x = "length_weight_relationships",
                                                  envir = tmp_envir)) {
                                         lengthweightrelationship_data <- get(x = "length_weight_relationships",
                                                          envir = tmp_envir)
                                         if (class(lengthweightrelationship_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"lengthweightrelationship_data\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "invalid RData, no R object named \"length_weight_relationships\" available in the R environment provided.\n",
                                             sep = "")
                                         stop()
                                       }
                                       if (nrow(lengthweightrelationship_data) == 0) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: no data in \"length_weight_relationships\" data frame, check the RData.\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length weight relationships data importation from RData.\n",
                                             sep = "")
                                       }
                                     } else if (data_source == "envir") {
                                       if (is.null(x = envir)) {
                                         environment_name <- as.environment(find(what = "length_weight_relationships")[1])
                                       } else {
                                         if ((class(x = envir) != "character"
                                              || length(x = envir) != 1)) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"envir\" argument, one value of class \"character\" expected.\n",
                                               "(\"global\" or \"current\").\n",
                                               sep = "")
                                           stop()
                                         } else {
                                           environment_name <- as.environment(envir)
                                         }
                                       }
                                       if (exists(x = "length_weight_relationships",
                                                  envir = environment_name)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start length weight relationships data importation from R global environment.\n",
                                             sep = "")
                                         lengthweightrelationship_data <- get(x = "length_weight_relationships",
                                                                              envir = environment_name)
                                         if (class(lengthweightrelationship_data) != "data.frame") {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               "invalid \"length_weight_relationships\" argument, class \"data.frame\" expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                         if (nrow(lengthweightrelationship_data) == 0) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: no data in \"length weight relationships\" data frame.\n",
                                               sep = "")
                                           stop()
                                         }
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length weight relationships data importation R global environment.\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             "no R object named \"length_weight_relationships\" available in the R global environment.\n",
                                             sep = "")
                                         stop()
                                       }
                                     } else {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\", \"csv\", \"rdata\" or \"envir\" expected).\n",
                                           sep = "")
                                       stop()
                                     }
                                     private$lengthweightrelationship <- lengthweightrelationship_data
                                   }
                                 ),
                                 private = list(
                                   trips = NULL,
                                   activities = NULL,
                                   elementarycatches = NULL,
                                   elementarylandings = NULL,
                                   wells = NULL,
                                   setdurationref = NULL,
                                   lengthstep = NULL,
                                   lengthweightrelationship = NULL,
                                   sampleset = NULL
                                 ))
