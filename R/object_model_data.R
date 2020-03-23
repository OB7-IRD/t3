#' @title R6 class object_model_data creation
#' @name object_model_data
#' @description Create R6 reference object class object_model_data
#' @importFrom R6 R6Class
#' @importFrom DBI sqlInterpolate SQL dbGetQuery
#' @importFrom dplyr first last filter
object_model_data <- R6::R6Class(classname = "object_model_data",
                                 public = list(
                                   #' @description Creation of a R6 reference object class trips which contain one or more R6 reference object class trip
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db".
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db".
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   trips_object_creation = function(periode_reference,
                                                                    countries,
                                                                    db_con,
                                                                    data_source = "t3_db",
                                                                    data_path = NULL) {
                                     if (data_source == "t3_db") {
                                       if (length(class(periode_reference)) != 1 || class(periode_reference) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument\n",
                                             "class \"integer\" expected\n",
                                             sep = "")
                                         stop()
                                       } else if (length(class(countries)) != 1 || class(countries) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument\n",
                                             "class \"character\" expected\n",
                                             sep = "")
                                         stop()
                                       } else if (class(db_con) != "PostgreSQLConnection") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument\n",
                                             "class \"PostgreSQLConnection\" expected\n",
                                             sep = "")
                                         stop()
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start trip(s) data importation from T3 database\n",
                                             sep = "")
                                         trip_sql <- paste(readLines(con = system.file("sql",
                                                                                       "t3_trip.sql",
                                                                                       package = "t3")),
                                                           collapse = "\n")
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
                                         trip_data <- DBI::dbGetQuery(db_con, trip_sql_final)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful trip(s) data importation from T3 database\n",
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
                                             " - Start activities data importation from the database\n",
                                             sep = "")
                                         trip_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                        collapse = "\n"))
                                         trip_data <- DBI::dbGetQuery(db_con, trip_sql)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful activities data importation from the database\n",
                                             sep = "")
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
                                         trip_data <- read.csv2(file = data_path)
                                       }
                                     } else {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data_source\" argument ",
                                           "(\"t3_db\", \"sql_query\" or \"csv_file\" expected)\n",
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
                                                                   "\n[trip: ",
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
                                                                   "\n",
                                                                   sep = "")
                                                               return(trip)
                                                             }))
                                     private$trips <- object_trips
                                   },
                                   #' @description Creation of a R6 reference object class activities which contain one or more R6 reference object class activity.
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db".
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db".
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   activities_object_creation = function(periode_reference,
                                                                         countries,
                                                                         db_con,
                                                                         data_source = "t3_db",
                                                                         data_path = NULL) {
                                     if (data_source == "t3_db") {
                                       if (length(class(periode_reference)) != 1 || class(periode_reference) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument\n",
                                             "class \"integer\" expected\n",
                                             sep = "")
                                       } else if (length(class(countries)) != 1 || class(countries) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument\n",
                                             "class \"character\" expected\n",
                                             sep = "")
                                       } else if (class(db_con) != "PostgreSQLConnection") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument\n",
                                             "class \"PostgreSQLConnection\" expected\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start activities data importation from T3 database\n",
                                             sep = "")
                                         activities_sql <- paste(readLines(con = system.file("sql",
                                                                                             "t3_activities.sql",
                                                                                             package = "t3")),
                                                                 collapse = "\n")
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
                                         activities_data <- DBI::dbGetQuery(db_con, activities_sql_final)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful activities data importation from T3 database\n",
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
                                             " - Start activities data importation from the database\n",
                                             sep = "")
                                         activities_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                              collapse = "\n"))
                                         activities_data <- DBI::dbGetQuery(db_con, activities_sql)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful activities data importation from the database\n",
                                             sep = "")
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
                                         activities_data <- read.csv2(file = data_path)
                                       }
                                     }
                                     activities_data <- unclass(activities_data)
                                     object_activities <- t3::object_r6(class_name = "activities")
                                     object_activities$add(lapply(seq_len(length.out = length(activities_data[[1]])),
                                                                  function(i) {
                                                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                        " - Start importation of activity element ",
                                                                        i,
                                                                        "\n[activity: ",
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
                                                                        "\n",
                                                                        sep = "")
                                                                    return(activity)
                                                                  }))
                                     private$activities <- object_activities
                                   },
                                   #' @description Creation of a R6 reference object class elementarycatches which contain one or more R6 reference object class elementarycatch
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db".
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db".
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   elementarycatches_object_creation = function(periode_reference,
                                                                                countries,
                                                                                db_con,
                                                                                data_source = "t3_db",
                                                                                data_path = NULL) {
                                     if (data_source == "t3_db") {
                                       if (length(class(periode_reference)) != 1 || class(periode_reference) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument\n",
                                             "class \"integer\" expected\n",
                                             sep = "")
                                       } else if (length(class(countries)) != 1 || class(countries) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument\n",
                                             "class \"character\" expected\n",
                                             sep = "")
                                       } else if (class(db_con) != "PostgreSQLConnection") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument\n",
                                             "class \"PostgreSQLConnection\" expected\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary catches data importation from T3 database\n",
                                             sep = "")
                                         elementarycatch_sql <- paste(readLines(con = system.file("sql",
                                                                                                  "t3_elementarycatch.sql",
                                                                                                  package = "t3")),
                                                                      collapse = "\n")
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
                                         elementarycatch_data <- DBI::dbGetQuery(db_con, elementarycatch_sql_final)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary catches data importation from T3 database\n",
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
                                             " - Start elementary catches data importation from the database\n",
                                             sep = "")
                                         elementarycatch_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                                   collapse = "\n"))
                                         elementarycatch_data <- DBI::dbGetQuery(db_con, elementarycatch_sql)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary catches data importation from the database\n",
                                             sep = "")
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
                                         elementarycatch_data <- read.csv2(file = data_path)
                                       }
                                     }
                                     elementarycatch_data <- unclass(elementarycatch_data)
                                     object_elementarycatches <- t3::object_r6(class_name = "elementarycatches")
                                     object_elementarycatches$add(lapply(seq_len(length.out = length(elementarycatch_data[[1]])),
                                                                         function(i) {
                                                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                               " - Start importation of elementary catch element ",
                                                                               i,
                                                                               "\n[elementarycatch: ",
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
                                                                               "\n",
                                                                               sep = "")
                                                                           return(elementarycatch)
                                                                         }))
                                     private$elementarycatches <- object_elementarycatches
                                   },
                                   #' @description Creation of a R6 reference object class elementarylandings which contain one or more R6 reference object class elementarylanding
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db".
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db".
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   elementarylandings_object_creation = function(periode_reference,
                                                                                 countries,
                                                                                 db_con,
                                                                                 data_source = "t3_db",
                                                                                 data_path = NULL) {
                                     if (data_source == "t3_db") {
                                       if (length(class(periode_reference)) != 1 || class(periode_reference) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument\n",
                                             "class \"integer\" expected\n",
                                             sep = "")
                                       } else if (length(class(countries)) != 1 || class(countries) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument\n",
                                             "class \"character\" expected\n",
                                             sep = "")
                                       } else if (class(db_con) != "PostgreSQLConnection") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument\n",
                                             "class \"PostgreSQLConnection\" expected\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start elementary landings data importation from T3 database\n",
                                             sep = "")
                                         elementarylanding_sql <- paste(readLines(con = system.file("sql",
                                                                                                    "t3_elementarylanding.sql",
                                                                                                    package = "t3")),
                                                                        collapse = "\n")
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
                                         elementarylanding_data <- DBI::dbGetQuery(db_con, elementarylanding_sql_final)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary landings data importation from T3 database\n",
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
                                             " - Start elementary landings data importation from the database\n",
                                             sep = "")
                                         elementarylanding_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                                     collapse = "\n"))
                                         elementarylanding_data <- DBI::dbGetQuery(db_con, elementarylanding_sql)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful elementary landings data importation from the database\n",
                                             sep = "")
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
                                         elementarylanding_data <- read.csv2(file = data_path)
                                       }
                                     }
                                     elementarylanding_data <- unclass(elementarylanding_data)
                                     object_elementarylandings <- t3::object_r6(class_name = "elementarylandings")
                                     object_elementarylandings$add(lapply(seq_len(length.out = length(elementarylanding_data[[1]])),
                                                                          function(i) {
                                                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                                " - Start importation of elementary landing element ",
                                                                                i,
                                                                                "\n[elementarylanding: ",
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
                                                                                "\n",
                                                                                sep = "")
                                                                            return(elementarylanding)
                                                                          }))
                                     private$elementarylandings <- object_elementarylandings
                                   },
                                   #' @description Creation of a R6 reference object class wells which contain one or more R6 reference object class well, wellset and samples
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db".
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db".
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param sample_type (integer) Sample type identification (landing, observer, ...).
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param data_path_samples (character) Path of the data sql/csv file for samples. By default NULL.
                                   #' @param data_path_wellplans (character) Path of the data sql/csv file for well plans. By default NULL.
                                   wells_object_creation = function(periode_reference,
                                                                    countries,
                                                                    db_con,
                                                                    sample_type,
                                                                    data_source = "t3_db",
                                                                    data_path_samples = NULL,
                                                                    data_path_wellplans = NULL) {
                                     if (data_source == "t3_db") {
                                       if (length(class(periode_reference)) != 1 || class(periode_reference) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument\n",
                                             "class \"integer\" expected\n",
                                             sep = "")
                                       } else if (length(class(countries)) != 1 || class(countries) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument\n",
                                             "class \"character\" expected\n",
                                             sep = "")
                                       } else if (class(db_con) != "PostgreSQLConnection") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument\n",
                                             "class \"PostgreSQLConnection\" expected\n",
                                             sep = "")
                                       } else if (class(sample_type) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"sample_type\" argument\n",
                                             "class \"integer\" expected\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start wells data (samples and well plans) importation from T3 database\n",
                                             sep = "")
                                         samples_sql <- paste(readLines(con = system.file("sql",
                                                                                          "t3_samples.sql",
                                                                                          package = "t3")),
                                                              collapse = "\n")
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
                                         samples_data <- DBI::dbGetQuery(db_con, samples_sql_final)
                                         wellplan_sql <- paste(readLines(con = system.file("sql",
                                                                                           "t3_wellplan.sql",
                                                                                           package = "t3")),
                                                               collapse = "\n")
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
                                         wellplan_data <- DBI::dbGetQuery(db_con, wellplan_sql_final)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful wells data (samples and well plans) importation from T3 database\n",
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
                                             " - Start wells data (samples and well plans) importation from the database\n",
                                             sep = "")
                                         samples_sql <- DBI::SQL(x = paste(readLines(con = data_path_samples),
                                                                           collapse = "\n"))
                                         samples_data <- DBI::dbGetQuery(db_con, samples_sql)
                                         wellplan_sql <- DBI::SQL(x = paste(readLines(con = data_path_wellplans),
                                                                            collapse = "\n"))
                                         wellplan_data <- DBI::dbGetQuery(db_con, wellplan_sql)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful wells data (samples and well plans) importation from the database\n",
                                             sep = "")
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
                                         samples_data <- read.csv2(file = data_path_samples)
                                         wellplan_data <- read.csv2(file = data_path_wellplans)
                                       }
                                     }
                                     object_wells <- t3::object_r6(class_name = "wells")
                                     for (trip in unique(samples_data$trip_id)) {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Start importation of well(s) data for trip element ",
                                           which(unique(samples_data$trip_id) == trip),
                                           "\n[trip: ",
                                           trip,
                                           "]\n",
                                           sep = "")
                                       tmp_trip <- dplyr::filter(.data = samples_data, trip_id == trip)
                                       for (well in unique(tmp_trip$well_id)) {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start importation of well data item ",
                                             which(unique(tmp_trip$well_id) == well),
                                             "\n[well: ",
                                             well,
                                             "]\n",
                                             sep = "")
                                         if (is.na(well)) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Warning: missing \"well_id\" argument in trip number: ", trip, "\n",
                                               sep = "")
                                           tmp_well <- dplyr::filter(.data = tmp_trip, is.na(well_id))
                                           if (length(unique(tmp_well$sample_id)) != 1) {
                                             cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                 " - Warning: well unknown identify in trip number \" ",
                                                 trip,
                                                 "\" have more than one sampling associated. Data avoided for model incrementation\n",
                                                 sep = "")
                                             next()
                                           }
                                         } else {
                                           tmp_well <- dplyr::filter(.data = tmp_trip, well_id == well)
                                         }
                                         if (length(unique(tmp_well$well_minus10_weigth)) != 1) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"well_minus10_weigth\" argument for trip id \"",
                                               trip,
                                               "\" and well id \"",
                                               well,
                                               "\"\n",
                                               sep = "")
                                           stop()
                                         } else if (length(unique(tmp_well$well_plus10_weigth)) != 1) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"well_plus10_weigth\" argument for trip id \"",
                                               trip,
                                               "\" and well id \"",
                                               well,
                                               "\"\n",
                                               sep = "")
                                           stop()
                                         } else if (length(unique(tmp_well$well_global_weigth)) != 1) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"well_global_weigth\" argument for trip id \"",
                                               trip,
                                               "\" and well id \"",
                                               well,
                                               "\"\n",
                                               sep = "")
                                           stop()
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
                                               "\n[sample: ",
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
                                               "\n[sample: ",
                                               sample,
                                               "]\n",
                                               sep = "")
                                         }
                                         if (well %in% unique(wellplan_data$well_id)) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Start importation of well plan data item ",
                                               which(unique(wellplan_data$well_id) == well),
                                               "\n[well: ",
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
                                               "\n[well: ",
                                               well,
                                               "]\n",
                                               sep = "")
                                         }
                                         object_wells$add(object_well)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful importation of well data item ",
                                             which(unique(tmp_trip$well_id) == well),
                                             "\n[well: ",
                                             well,
                                             "]\n",
                                             sep = "")
                                       }
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Successful importation of well(s) data for trip element ",
                                           which(unique(samples_data$trip_id) == trip),
                                           "\n[trip: ",
                                           trip,
                                           "]\n",
                                           sep = "")
                                     }
                                     private$wells <- object_wells
                                   },
                                   #' @description Creation of a data frame object with parameters of set duration algorithms
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db".
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db".
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   setduration_data = function(periode_reference,
                                                               countries,
                                                               db_con,
                                                               data_source = "t3_db",
                                                               data_path = NULL) {
                                     if (data_source == "t3_db") {
                                       if (length(class(periode_reference)) != 1 || class(periode_reference) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument\n",
                                             "class \"integer\" expected\n",
                                             sep = "")
                                       } else if (length(class(countries)) != 1 || class(countries) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument\n",
                                             "class \"character\" expected\n",
                                             sep = "")
                                       } else if (class(db_con) != "PostgreSQLConnection") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument\n",
                                             "class \"PostgreSQLConnection\" expected\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start set duration data importation from T3 database\n",
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
                                         setdurationref_data <- DBI::dbGetQuery(db_con, setdurationref_sql_final)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful set duration data importation from T3 database\n",
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
                                             " - Start set duration data importation from the database\n",
                                             sep = "")
                                         setdurationref_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                                  collapse = "\n"))
                                         setdurationref_data <- DBI::dbGetQuery(db_con, setdurationref_sql)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful set duration data importation from the database\n",
                                             sep = "")
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
                                         setdurationref_data <- read.csv2(file = data_path)
                                       }
                                     }
                                     private$setdurationref <- setdurationref_data
                                   },
                                   #' @description Creation of a data frame object with length ratio between ld1 and lf class.
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   lengthstep_data = function(data_source = "t3_db",
                                                              data_path = NULL) {
                                     if (data_source == "t3_db") {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Start length steps data importation from T3 database\n",
                                           sep = "")
                                       lengthstep_sql <- paste(readLines(con = system.file("sql",
                                                                                           "t3_lengthstep.sql",
                                                                                           package = "t3")),
                                                               collapse = "\n")
                                       lengthstep_data <- DBI::dbGetQuery(db_con, lengthstep_sql)
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Successful length steps data importation from T3 database\n",
                                           sep = "")
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
                                             " - Start length steps data importation from the database\n",
                                             sep = "")
                                         lengthstep_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                              collapse = "\n"))
                                         lengthstep_data <- DBI::dbGetQuery(db_con, lengthstep_sql)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length steps data importation from the database\n",
                                             sep = "")
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
                                         lengthstep_sql <- read.csv2(file = data_path)
                                       }
                                     }
                                     private$lengthstep <- lengthstep_data
                                   },
                                   #' @description Creation of a data frame object with weighted weigth of each set sampled.
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits. Necessary argument for data source "t3_db".
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries. Necessary argument for data source "t3_db".
                                   #' @param db_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a database. Necessary argument for data source "t3_db" and "sql_query".
                                   #' @param data_source (character) Identification of data source. By default "t3_db" but you can switch with "sql_query" or "csv_file" (with separator character ";" and decimal ",").
                                   #' @param data_path (character) Path of the data sql/csv file. By default NULL.
                                   sampleset_data = function(periode_reference,
                                                             countries,
                                                             db_con,
                                                             data_source = "t3_db",
                                                             data_path = NULL) {
                                     if (data_source == "t3_db") {
                                       if (length(class(periode_reference)) != 1 || class(periode_reference) != "integer") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"periode_reference\" argument\n",
                                             "class \"integer\" expected\n",
                                             sep = "")
                                       } else if (length(class(countries)) != 1 || class(countries) != "character") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"countries\" argument\n",
                                             "class \"character\" expected\n",
                                             sep = "")
                                       } else if (class(db_con) != "PostgreSQLConnection") {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Error: invalid \"db_con\" argument\n",
                                             "class \"PostgreSQLConnection\" expected\n",
                                             sep = "")
                                       } else {
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Start sample sets data importation from T3 database\n",
                                             sep = "")
                                         sampleset_sql <- paste(readLines(con = system.file("sql",
                                                                                            "t3_sampleset.sql",
                                                                                            package = "t3")),
                                                                collapse = "\n")
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
                                         sampleset_data <- DBI::dbGetQuery(db_con, sampleset_sql_final)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful sample sets data importation from T3 database\n",
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
                                             " - Start sample sets data importation from the database\n",
                                             sep = "")
                                         sampleset_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                             collapse = "\n"))
                                         sampleset_data <- DBI::dbGetQuery(db_con, sampleset_sql)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful sample sets data importation from the database\n",
                                             sep = "")
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
                                         sampleset_data <- read.csv2(file = data_path)
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
                                           " - Start length weight relationship data importation from T3 database\n",
                                           sep = "")
                                       lengthweightrelationship_sql <- paste(readLines(con = system.file("sql",
                                                                                                         "t3_lengthweightrelationship.sql",
                                                                                                         package = "t3")),
                                                                             collapse = "\n")
                                       lengthweightrelationship_data <- DBI::dbGetQuery(db_con, lengthweightrelationship_sql)
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Successful length weight relationship data importation from T3 database\n",
                                           sep = "")
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
                                             " - Start length weight relationship data importation from the database\n",
                                             sep = "")
                                         lengthweightrelationship_sql <- DBI::SQL(x = paste(readLines(con = data_path),
                                                                                            collapse = "\n"))
                                         lengthweightrelationship_data <- DBI::dbGetQuery(db_con, lengthweightrelationship_sql)
                                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                             " - Successful length weight relationship data importation from the database\n",
                                             sep = "")
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
                                         lengthweightrelationship_data <- read.csv2(file = data_path)
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
