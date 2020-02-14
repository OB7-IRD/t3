#' @title R6 class object_model_data creation
#' @name object_model_data
#' @description Create R6 reference object class object_model_data
#' @importFrom R6 R6Class
#' @importFrom DBI sqlInterpolate SQL dbGetQuery
#' @importFrom dplyr first last filter
object_model_data <- R6::R6Class(classname = "object_model_data",
                                 public = list(
                                   #' @description Creation of a R6 reference object class trips which contain one or more R6 reference object class trip
                                   #' @seealso \code{\link{trip}} \code{\link{trips}}
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries.
                                   #' @param t3_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a T3 database.
                                   #' @examples
                                   #' tmp <- t3:::object_model_data$new()
                                   #' tmp$trips_object_creation(periode_reference = c(2016, 2017),
                                   #'                           countries = ("FRA"),
                                   #'                           t3_con = t3_connection)
                                   #' # call trips object
                                   #' object_model_data$.__enclos_env__$private$trips
                                   #' # call the first trip
                                   #' object_model_data$.__enclos_env__$private$trips$.__enclos_env__$private$data[[1]]
                                   trips_object_creation = function(periode_reference,
                                                                    countries,
                                                                    t3_con) {
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Start trip(s) data importation from T3 database\n",
                                         sep = "")
                                     trip_sql <- paste(readLines(con = system.file("sql",
                                                                                   "t3_trip.sql",
                                                                                   package = "t3")),
                                                       collapse = "\n")
                                     trip_sql_final <- DBI::sqlInterpolate(conn = t3_con,
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
                                     trip_data <- DBI::dbGetQuery(t3_con, trip_sql_final)
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Successful trip(s) data importation from T3 database\n",
                                         sep = "")
                                     trip_data <- unclass(trip_data)
                                     object_trips <- t3::object_r6(class_name = "trips")
                                     object_trips$add(lapply(1:length(trip_data[[1]]),
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
                                   #' @title Creation of object activities
                                   #' @name activities_object_creation
                                   #' @description Creation of a R6 reference object class activities which contain one or more R6 reference object class activity
                                   #' @seealso \code{\link{activity}} \code{\link{activities}}
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries.
                                   #' @param t3_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a T3 database.
                                   #' @examples
                                   #' tmp <- t3:::object_model_data$new()
                                   #' tmp$activities_object_creation(periode_reference = c(2016, 2017),
                                   #'                                countries = ("FRA"),
                                   #'                                t3_con = t3_connection)
                                   #' # call activities object
                                   #' object_model_data$.__enclos_env__$private$activities
                                   #' # call the first activity
                                   #' object_model_data$.__enclos_env__$private$activities$.__enclos_env__$private$data[[1]]
                                   activities_object_creation = function(periode_reference,
                                                                         countries,
                                                                         t3_con) {
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Start activities data importation from T3 database\n",
                                         sep = "")
                                     activities_sql <- paste(readLines(con = system.file("sql",
                                                                                         "t3_activities.sql",
                                                                                         package = "t3")),
                                                             collapse = "\n")
                                     activities_sql_final <- DBI::sqlInterpolate(conn = t3_con,
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
                                     activities_data <- DBI::dbGetQuery(t3_con, activities_sql_final)
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Successful activities data importation from T3 database\n",
                                         sep = "")
                                     activities_data <- unclass(activities_data)
                                     object_activities <- t3::object_r6(class_name = "activities")
                                     object_activities$add(lapply(1:length(activities_data[[1]]),
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
                                   #' @title Creation of object elementarycatches
                                   #' @name elementarycatches_object_creation
                                   #' @description Creation of a R6 reference object class elementarycatches which contain one or more R6 reference object class elementarycatch
                                   #' @seealso \code{\link{elementarycatches}} \code{\link{elementarycatch}}
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries.
                                   #' @param t3_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a T3 database.
                                   #' @examples
                                   #' tmp <- t3:::object_model_data$new()
                                   #' tmp$elementarycatches_object_creation(periode_reference = c(2016, 2017),
                                   #'                                       countries = ("FRA"),
                                   #'                                       t3_con = t3_connection)
                                   #' # call elementarycatches object
                                   #' object_model_data$.__enclos_env__$private$elementarycatches
                                   #' # call the first elementarycatch
                                   #' object_model_data$.__enclos_env__$private$elementarycatches$.__enclos_env__$private$data[[1]]
                                   elementarycatches_object_creation = function(periode_reference,
                                                                                countries,
                                                                                t3_con) {
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Start elementary catches data importation from T3 database\n",
                                         sep = "")
                                     elementarycatch_sql <- paste(readLines(con = system.file("sql",
                                                                                              "t3_elementarycatch.sql",
                                                                                              package = "t3")),
                                                                  collapse = "\n")
                                     elementarycatch_sql_final <- DBI::sqlInterpolate(conn = t3_con,
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
                                     elementarycatch_data <- DBI::dbGetQuery(t3_con, elementarycatch_sql_final)
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Successful elementary catches data importation from T3 database\n",
                                         sep = "")
                                     elementarycatch_data <- unclass(elementarycatch_data)
                                     object_elementarycatches <- t3::object_r6(class_name = "elementarycatches")
                                     object_elementarycatches$add(lapply(1:length(elementarycatch_data[[1]]),
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
                                                                                                                       specie_code3l = elementarycatch_data[[7]][i],
                                                                                                                       catch_weight = elementarycatch_data[[8]][i])
                                                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                               " - Successful importation of elementary catch element ",
                                                                               i,
                                                                               "\n",
                                                                               sep = "")
                                                                           return(elementarycatch)
                                                                         }))
                                     private$elementarycatches <- object_elementarycatches
                                   },
                                   #' @title Creation of object elementarylandings
                                   #' @name elementarylandings_object_creation
                                   #' @description Creation of a R6 reference object class elementarylandings which contain one or more R6 reference object class elementarylanding
                                   #' @seealso \code{\link{elementarylandings}} \code{\link{elementarylanding}}
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries.
                                   #' @param t3_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a T3 database.
                                   #' @examples
                                   #' tmp <- t3:::object_model_data$new()
                                   #' tmp$elementarylandings_object_creation(periode_reference = c(2016, 2017),
                                   #'                                        countries = ("FRA"),
                                   #'                                        t3_con = t3_connection)
                                   #' # call elementarylandings object
                                   #' object_model_data$.__enclos_env__$private$elementarylandings
                                   #' # call the first elementarylanding
                                   #' object_model_data$.__enclos_env__$private$elementarylandings$.__enclos_env__$private$data[[1]]
                                   elementarylandings_object_creation = function(periode_reference,
                                                                                 countries,
                                                                                 t3_con) {
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Start elementary landings data importation from T3 database\n",
                                         sep = "")
                                     elementarylanding_sql <- paste(readLines(con = system.file("sql",
                                                                                                "t3_elementarylanding.sql",
                                                                                                package = "t3")),
                                                                    collapse = "\n")
                                     elementarylanding_sql_final <- DBI::sqlInterpolate(conn = t3_con,
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
                                     elementarylanding_data <- DBI::dbGetQuery(t3_con, elementarylanding_sql_final)
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Successful elementary landings data importation from T3 database\n",
                                         sep = "")
                                     elementarylanding_data <- unclass(elementarylanding_data)
                                     object_elementarylandings <- t3::object_r6(class_name = "elementarylandings")
                                     object_elementarylandings$add(lapply(1:length(elementarylanding_data[[1]]),
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
                                                                                                                            specie_code3l = elementarylanding_data[[5]][i],
                                                                                                                            landing_weight = elementarylanding_data[[6]][i])
                                                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                                " - Successful importation of elementary landing element ",
                                                                                i,
                                                                                "\n",
                                                                                sep = "")
                                                                            return(elementarylanding)
                                                                          }))
                                     private$elementarylandings <- object_elementarylandings
                                   },
                                   #' @title Creation of object wells
                                   #' @name wells_object_creation
                                   #' @description Creation of a R6 reference object class wells which contain one or more R6 reference object class well, wellset and samples
                                   #' @seealso \code{\link{wells}} \code{\link{well}} \code{\link{wellset}} \code{\link{samples}}
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries.
                                   #' @param t3_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a T3 database.
                                   #' @examples
                                   #' tmp <- t3:::object_model_data$new()
                                   #' tmp$wells_object_creation(periode_reference = c(2016, 2017),
                                   #'                           countries = ("FRA"),
                                   #'                           t3_con = t3_connection)
                                   #' # call wells object
                                   #' object_model_data$.__enclos_env__$private$wells
                                   #' # call the first well
                                   #' object_model_data$.__enclos_env__$private$wells$.__enclos_env__$private$data[[1]]
                                   wells_object_creation = function(periode_reference,
                                                                    countries,
                                                                    t3_con) {
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Start wells data (samples and well plans) importation from T3 database\n",
                                         sep = "")
                                     samples_sql <- paste(readLines(con = system.file("sql",
                                                                                      "t3_samples.sql",
                                                                                      package = "t3")),
                                                          collapse = "\n")
                                     samples_sql_final <- DBI::sqlInterpolate(conn = t3_con,
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
                                     samples_data <- DBI::dbGetQuery(t3_con, samples_sql_final)
                                     wellplan_sql <- paste(readLines(con = system.file("sql",
                                                                                       "t3_wellplan.sql",
                                                                                       package = "t3")),
                                                           collapse = "\n")
                                     wellplan_sql_final <- DBI::sqlInterpolate(conn = t3_con,
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
                                     wellplan_data <- DBI::dbGetQuery(t3_con, wellplan_sql_final)
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Successful wells data (samples and well plans) importation from T3 database\n",
                                         sep = "")
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
                                               " - Warning: missing \"well_id\" argument in trip number: ", trip,"\n",
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
                                                                                                             list(lapply(X = 1:length(tmp_sample[[1]]),
                                                                                                                         FUN = function(i) {
                                                                                                                           t3:::elementarysampleraw$new(trip_id = trip,
                                                                                                                                                        well_id = well,
                                                                                                                                                        sample_id = sample,
                                                                                                                                                        sub_sample_id = tmp_sample[[7]][i],
                                                                                                                                                        sample_quality = tmp_sample[[8]][i],
                                                                                                                                                        sample_type = tmp_sample[[9]][i],
                                                                                                                                                        specie_code3l = tmp_sample[[10]][i],
                                                                                                                                                        length_type = tmp_sample[[11]][i],
                                                                                                                                                        sample_total_count = tmp_sample[[12]][i],
                                                                                                                                                        sample_number_measured = tmp_sample[[13]][i],
                                                                                                                                                        sample_length_class = tmp_sample[[14]][i])
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
                                           object_well$.__enclos_env__$private$wellplan <- lapply(X = 1:length(tmp_wellplan[[1]]),
                                                                                                  FUN = function(j) {
                                                                                                    t3:::elementarywellplan$new(wellplan_id = tmp_wellplan[[1]][j],
                                                                                                                                well_id = tmp_wellplan[[2]][j],
                                                                                                                                activity_id = tmp_wellplan[[3]][j],
                                                                                                                                sample_id = tmp_wellplan[[4]][j],
                                                                                                                                specie_code3l = tmp_wellplan[[5]][j],
                                                                                                                                wellplan_weight = tmp_wellplan[[6]][j],
                                                                                                                                wellplan_number = tmp_wellplan[[7]][j],
                                                                                                                                wellplan_weigth_category_code = tmp_wellplan[[8]][j],
                                                                                                                                wellplan_weigth_category_label = tmp_wellplan[[9]][j])
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
                                   #' @title Creation of object set duration
                                   #' @name setduration_data
                                   #' @description Creation of a data frame object with parameters of set duration algorithms
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries.
                                   #' @param t3_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a T3 database.
                                   #' @examples
                                   #' tmp <- t3:::object_model_data$new()
                                   #' tmp$setduration_data(periode_reference = c(2016, 2017),
                                   #'                      countries = ("FRA"),
                                   #'                      t3_con = t3_connection)
                                   #' # call setduration object
                                   #' object_model_data$.__enclos_env__$private$setduration
                                   setduration_data = function(periode_reference,
                                                               countries,
                                                               t3_con) {
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Start set duration data importation from T3 database\n",
                                         sep = "")
                                     setdurationref_sql <- paste(readLines(con = system.file("sql",
                                                                                             "t3_setdurationref.sql",
                                                                                             package = "t3")),
                                                                 collapse = "\n")
                                     setdurationref_sql_final <- DBI::sqlInterpolate(conn = t3_con,
                                                                              sql = setdurationref_sql,
                                                                              period = DBI::SQL(paste0(periode_reference,
                                                                                                       collapse = ", ")),
                                                                              countries = DBI::SQL(paste0("'",
                                                                                                          paste0(countries,
                                                                                                                 collapse = "', '"),
                                                                                                          "'")))
                                     setdurationref_data <- DBI::dbGetQuery(t3_con, setdurationref_sql_final)
                                     private$setdurationref <- setdurationref_data
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Successful set duration data importation from T3 database\n",
                                         sep = "")
                                   },
                                   #' @title Creation of object length step
                                   #' @name lengthstep_data
                                   #' @description Creation of a data frame object with length ratio between ld1 and lf class
                                   #' @examples
                                   #' tmp <- t3:::object_model_data$new()
                                   #' tmp$lengthstep_data()
                                   #' # call lengthstep object
                                   #' object_model_data$.__enclos_env__$private$lengthstep
                                   lengthstep_data = function() {
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Start length steps data importation from T3 database\n",
                                         sep = "")
                                     lengthstep_sql <- paste(readLines(con = system.file("sql",
                                                                                         "t3_lengthstep.sql",
                                                                                         package = "t3")),
                                                             collapse = "\n")
                                     lengthstep_data <- DBI::dbGetQuery(t3_con, lengthstep_sql)
                                     private$lengthstep <- lengthstep_data
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Successful length steps data importation from T3 database\n",
                                         sep = "")
                                   },
                                   #' @title Creation of object sample set
                                   #' @name sampleset_data
                                   #' @description Creation of a data frame object with weighted weigth of each set sampled
                                   #' @param periode_reference (integer) Year(s) of the reference period coded on 4 digits.
                                   #' @param countries (character) ISO code on 3 letters related to one or more countries.
                                   #' @param t3_con (PostgreSQLConnection) An R's object which contain connexion identifiers for a T3 database.
                                   #' @examples
                                   #' tmp <- t3:::object_model_data$new()
                                   #' tmp$sampleset_data(periode_reference = c(2016, 2017),
                                   #'                    countries = ("FRA"),
                                   #'                    t3_con = t3_connection)
                                   #' # call sampleset object
                                   #' object_model_data$.__enclos_env__$private$sampleset
                                   sampleset_data = function(periode_reference,
                                                             countries,
                                                             t3_con) {
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Start sample sets data importation from T3 database\n",
                                         sep = "")
                                     sampleset_sql <- paste(readLines(con = system.file("sql",
                                                                                        "t3_sampleset.sql",
                                                                                        package = "t3")),
                                                            collapse = "\n")
                                     sampleset_sql_final <- DBI::sqlInterpolate(conn = t3_con,
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
                                     sampleset_data <- DBI::dbGetQuery(t3_con, sampleset_sql_final)
                                     private$sampleset <- sampleset_data
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Successful sample sets data importation from T3 database\n",
                                         sep = "")
                                   },
                                   #' @title Creation of object length weight relationship
                                   #' @name lengthweightrelationship_data
                                   #' @description Creation of a data frame object with parameters for length weight relationship
                                   #' @examples
                                   #' tmp <- t3:::object_model_data$new()
                                   #' tmp$lengthweightrelationship_data()
                                   #' # call lengthweightrelationship object
                                   #' object_model_data$.__enclos_env__$private$lengthweightrelationship
                                   lengthweightrelationship_data = function() {
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Start length weight relationship data importation from T3 database\n",
                                         sep = "")
                                     lengthweightrelationship_sql <- paste(readLines(con = system.file("sql",
                                                                                                       "t3_lengthweightrelationship.sql",
                                                                                                       package = "t3")),
                                                                           collapse = "\n")
                                     lengthweightrelationship_data <- DBI::dbGetQuery(t3_con, lengthweightrelationship_sql)
                                     private$lengthweightrelationship <- lengthweightrelationship_data
                                     cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         " - Successful length weight relationship data importation from T3 database\n",
                                         sep = "")
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
