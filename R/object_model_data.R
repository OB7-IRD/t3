#' @title R6 class object_model_data creation
#' @name object_model_data
#' @description Create R6 reference object class object_model_data
#' @importFrom R6 R6Class
#' @importFrom DBI sqlInterpolate SQL dbGetQuery
#' @importFrom dplyr first last filter
object_model_data <- R6::R6Class(classname = "object_model_data",
                                 public = list(
                                   #' @description Creation of a R6 reference object class trips which contain one or more R6 reference object class trip
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   #' @param trips_selected (character) Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   trips_object_creation = function(db_con,
                                                                    data_source = "t3_db",
                                                                    periode_reference = NULL,
                                                                    countries = NULL,
                                                                    data_path = NULL,
                                                                    trips_selected = NULL) {
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
                                                                                                             "'")))
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
                                     } else if (data_source == "csv_file") {
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
                                         trip_data <- read.csv2(file = data_path)
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
                                     } else {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\" or \"csv_file\" expected).\n",
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
                                                                                     vessel_id = trip_data[[7]][i])
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
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   #' @param trips_selected (character) Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   activities_object_creation = function(db_con,
                                                                         data_source = "t3_db",
                                                                         periode_reference = NULL,
                                                                         countries = NULL,
                                                                         data_path = NULL,
                                                                         trips_selected = NULL) {
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
                                                                                                                   "'")))
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
                                     } else if (data_source == "csv_file") {
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
                                         activities_data <- read.csv2(file = data_path)
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
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   #' @param trips_selected (character) Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   elementarycatches_object_creation = function(db_con,
                                                                                data_source = "t3_db",
                                                                                periode_reference = NULL,
                                                                                countries = NULL,
                                                                                data_path = NULL,
                                                                                trips_selected = NULL) {
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
                                                                                                                        "'")))
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
                                     } else if (data_source == "csv_file") {
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
                                         elementarycatch_data <- read.csv2(file = data_path)
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
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   #' @param trips_selected (character) Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   elementarylandings_object_creation = function(db_con,
                                                                                 data_source = "t3_db",
                                                                                 periode_reference = NULL,
                                                                                 countries = NULL,
                                                                                 data_path = NULL,
                                                                                 trips_selected = NULL) {
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
                                                                                                                          "'")))
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
                                     } else if (data_source == "csv_file") {
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
                                         elementarylanding_data <- read.csv2(file = data_path)
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
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ","). By default NULL.
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param sample_type (integer) Sample type identification (landing, observer, ...). By default NULL.
                                   #' @param trips_selected (character) Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference, countries and sample types). By default NULL.
                                   #' @param data_path_samples (character) Path of the data sql/csv file for samples. By default NULL.
                                   #' @param data_path_wellplans (character) Path of the data sql/csv file for well plans. By default NULL.
                                   wells_object_creation = function(db_con,
                                                                    data_source = "t3_db",
                                                                    periode_reference = NULL,
                                                                    countries = NULL,
                                                                    sample_type = NULL,
                                                                    trips_selected = NULL,
                                                                    data_path_samples = NULL,
                                                                    data_path_wellplans = NULL) {
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
                                                                                                                 "'")))
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
                                     } else if (data_source == "csv_file") {
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
                                         samples_data <- read.csv2(file = data_path_samples)
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
                                         wellplan_data <- read.csv2(file = data_path_wellplans)
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
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   setduration_data = function(db_con,
                                                               data_source = "t3_db",
                                                               periode_reference = NULL,
                                                               countries = NULL,
                                                               data_path = NULL) {
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
                                     } else if (data_source == "csv_file") {
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
                                         setdurationref_data <- read.csv2(file = data_path)
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
                                     }
                                     private$setdurationref <- setdurationref_data
                                   },
                                   #' @description Creation of a data frame object with length ratio between ld1 and lf class.
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   lengthstep_data = function(db_con,
                                                              data_source = "t3_db",
                                                              data_path = NULL) {
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
                                     } else if (data_source == "csv_file") {
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
                                         lengthstep_sql <- read.csv2(file = data_path)
                                         if (nrow(lengthstep_sql) == 0) {
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
                                     }
                                     private$lengthstep <- lengthstep_data
                                   },
                                   #' @description Creation of a data frame object with weighted weigth of each set sampled.
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db". By default NULL.
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   #' @param trips_selected (character) Additional parameter only used with data source "t3_db". Use trip(s) identification(s) for selected trip(s) kept in the query (by periode of reference and countries). By default NULL.
                                   sampleset_data = function(db_con,
                                                             data_source = "t3_db",
                                                             periode_reference = NULL,
                                                             countries = NULL,
                                                             data_path = NULL,
                                                             trips_selected = NULL) {
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
                                                                                                                  "'")))
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
                                     } else if (data_source == "csv_file") {
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
                                         sampleset_data <- read.csv2(file = data_path)
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
                                     }
                                     private$sampleset <- sampleset_data
                                   },
                                   #' @description Creation of a data frame object with parameters for length weight relationship.
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   lengthweightrelationship_data = function(db_con,
                                                                            data_source = "t3_db",
                                                                            data_path = NULL) {
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
                                     } else if (data_source == "csv_file") {
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
                                         lengthweightrelationship_data <- read.csv2(file = data_path)
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
