#' @title R6 class full_trips
#' @name full_trips
#' @description Create R6 reference object class full_trips
#' @importFrom R6 R6Class
#' @importFrom dplyr tibble add_row mutate relocate group_by summarise ungroup pull arrange full_join left_join inner_join distinct select
#' @importFrom stringr str_c str_extract
#' @importFrom tidyr crossing tibble spread gather
#' @importFrom lubridate year month days dhours dminutes dseconds hms parse_date_time int_length interval
full_trips <- R6::R6Class(classname = "full_trips",
                          inherit = list_t3,
                          public = list(
                            # 1 - Full trips creation ----
                            #' @description Creation of full trip item from trips.
                            #' @param object_trips Object of type R6-trips expected. A R6 reference object of class trips.
                            create_full_trips = function(object_trips) {
                              # 1.1 - Arguments verifications ----
                              if (paste(class(x = object_trips),
                                        collapse = " ") != "trips list_t3 R6") {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Invalid \"object_trips\" argument.")
                              }
                              # 1.2 - Global process ----
                              # By default, trips are listed by vessel id and landing date
                              full_trips <- list()
                              full_trips_tmp <- list()
                              full_trip_warning <- 0
                              trip_id <- 1
                              while (trip_id <= object_trips$count()) {
                                if (trip_id == 1) {
                                  cat(format(Sys.time(),
                                             "%Y-%m-%d %H:%M:%S"),
                                      " - Start full trips creation.\n", sep="")
                                }
                                if (object_trips$view(trip_id)[[1]]$.__enclos_env__$private$landing_well_content_code == 1) {
                                  full_trips <- append(full_trips,
                                                       list(list(object_trips$view(trip_id)[[1]]$clone())))
                                  trip_id <- trip_id + 1
                                } else {
                                  for (sub_trip_id in trip_id:object_trips$count()) {
                                    if (sub_trip_id == object_trips$count()) {
                                      full_trips_tmp <- append(full_trips_tmp,
                                                               object_trips$view(sub_trip_id)[[1]]$clone())
                                      full_trip_warning <- 1
                                      trip_id <- trip_id + 1
                                    } else {
                                      if (object_trips$view(sub_trip_id)[[1]]$.__enclos_env__$private$vessel_code == object_trips$view(sub_trip_id + 1)[[1]]$.__enclos_env__$private$vessel_code) {
                                        full_trips_tmp <- append(full_trips_tmp,
                                                                 object_trips$view(sub_trip_id)[[1]]$clone())
                                        if (object_trips$view(sub_trip_id + 1)[[1]]$.__enclos_env__$private$landing_well_content_code == 1) {
                                          full_trips_tmp <- append(full_trips_tmp,
                                                                   object_trips$view(sub_trip_id + 1)[[1]]$clone())
                                          trip_id <- sub_trip_id + 2
                                          break ()
                                        }
                                      } else {
                                        full_trip_warning <- 1
                                        full_trips_tmp <- append(full_trips_tmp,
                                                                 object_trips$view(sub_trip_id)[[1]]$clone())
                                        trip_id <- sub_trip_id + 1
                                        break ()
                                      }
                                    }
                                  }
                                  if (full_trip_warning == 1) {
                                    full_trip_warning <- 0
                                    private$id_not_full_trip <- append(private$id_not_full_trip,
                                                                       length(full_trips) + 1)
                                    warning(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Missing trip(s) in item ",
                                            length(full_trips) + 1,
                                            ".\n[trip: ",
                                            object_trips$view(sub_trip_id)[[1]]$.__enclos_env__$private$trip_id,
                                            "]")
                                  }
                                  full_trips <- append(full_trips,
                                                       list(full_trips_tmp))
                                  full_trips_tmp <- list()
                                }
                              }
                              names(full_trips) <- seq_len(length.out = length(full_trips))
                              private$data <- full_trips
                              # 1.3 - Log summary annotation ----
                              private$log_summary <- dplyr::tibble(step = "create_full_trips",
                                                                   input_trips = object_trips$count(),
                                                                   output_trips = length(x = unlist(x = full_trips)),
                                                                   output_full_trips = length(x = full_trips))
                              cat(format(Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - End of full trips creation.\n")
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 2 - Add activities ----
                            #' @description Function for add activities in full trips object.
                            #' @param object_activities Object of type R6-activities expected. A R6 reference object of class activities.
                            add_activities = function(object_activities) {
                              # 2.1 - Arguments verifications ----
                              if (object_activities$count() == 0) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - No activity is available for the process.")
                              }
                              if (! any(class(x = object_activities) == "activities")) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Invalid \"object_activities\" argument.")
                              }
                              # 2.2 - Global process ----
                              for (full_trip_id in seq_len(length.out = length(private$data))) {
                                if (full_trip_id == 1) {
                                  cat(format(Sys.time(),
                                             "%Y-%m-%d %H:%M:%S"),
                                      " - Start of add activities and elementarycatches.\n")
                                }
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Ongoing process of adding activities elementarycatches on full trip \"",
                                    names(x = private$data)[[full_trip_id]],
                                    "\".\n", sep="")
                                capture.output(current_trips <- object_r6(class_name = "trips"),
                                               file = "NUL")
                                capture.output(current_trips$add(new_item = private$data[[full_trip_id]]),
                                               file = "NUL")
                                full_trips_activities <- lapply(X = seq_len(length.out = current_trips$count()),
                                                                FUN = function(trip_id) {
                                                                  current_trip_id <- current_trips$extract(attribut_l1 = "data",
                                                                                                           attribut_l2 = "trip_id",
                                                                                                           id = trip_id)
                                                                  object_activities$filter_l1(filter = paste0("$path$trip_id == \"",
                                                                                                              current_trip_id,
                                                                                                              "\""),
                                                                                              clone = TRUE)
                                                                })
                                invisible(x = lapply(X = seq_len(length.out = current_trips$count()),
                                                     FUN = function(trip_id) {
                                                       current_trips$.__enclos_env__$private$data[[trip_id]]$.__enclos_env__$private$activities <- full_trips_activities[[trip_id]]
                                                     }))
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Successful process of adding activities and elementarycatches on full trip \"",
                                    names(private$data)[[full_trip_id]],
                                    "\".\n", sep="")
                              }
                              # 2.3 - Log summary annotation ----
                              capture.output(current_trips <- object_r6(class_name = "trips"),
                                             file = "NUL")
                              capture.output(current_trips$add(new_item = unlist(x = private$data)),
                                             file = "NUL")
                              private$log_summary <- private$log_summary %>%
                                dplyr::mutate(input_full_trips = NA_integer_,
                                              input_activities = NA_integer_,
                                              output_activities = NA_integer_) %>%
                                dplyr::add_row(step = "add_activities",
                                               input_trips = length(x = unlist(x = private$data)),
                                               input_full_trips = length(x = private$data),
                                               output_full_trips = input_full_trips,
                                               output_trips = input_trips,
                                               input_activities = object_activities$count(),
                                               output_activities = length(x = unlist(x = current_trips$extract_l1_element_value(element = "activities")))) %>%
                                dplyr::relocate(input_full_trips,
                                                input_activities,
                                                .before = output_trips)
                              capture.output(current_activities <- object_r6(class_name = "activities"),
                                             file = "NUL")
                              capture.output(current_activities$add(new_item = unlist(x = current_trips$extract_l1_element_value(element = "activities"))),
                                             file = "NUL")
                              capture.output(current_elementarycatches <- current_activities$extract_l1_element_value(element="elementarycatches"),
                                             file = "NUL")
                              current_elementarycatches <- current_elementarycatches[-which(sapply(current_elementarycatches, is.null))]
                              private$log_summary <- private$log_summary %>%
                                dplyr::mutate(input_elementary_catches = NA_integer_,
                                              output_elementary_catches = NA_integer_,
                                              output_catch_weight_elementary_catches = NA_real_) %>%
                                dplyr::add_row(step = "add_elementarycatches",
                                               input_trips = length(x = unlist(x = private$data_selected)),
                                               input_full_trips = length(x = private$data_selected),
                                               input_activities = current_activities$count(),
                                               input_elementary_catches = nrow(dplyr::bind_rows(object_activities$extract_l1_element_value(element="elementarycatches"))),
                                               output_trips = input_trips,
                                               output_full_trips = input_full_trips,
                                               output_activities = input_activities,
                                               output_elementary_catches = sum(sapply(current_elementarycatches, nrow)),
                                               output_catch_weight_elementary_catches = current_elementarycatches %>%
                                                 sapply(dplyr::summarize, catch_weight=sum(catch_weight, na.rm=TRUE)) %>%
                                                 unlist() %>%
                                                 sum()) %>%
                                dplyr::relocate(input_elementary_catches,
                                                .before = output_trips)
                              cat(format(Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - End of add activities and elementarycatches.\n", sep="")
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 3 - Filter full trips by year(s) period ----
                            #' @description Function for filter full trips by a year(s) period.
                            #' @param years_period Object of class {\link[base]{integer}} expected. Year(s) in 4 digits format.
                            filter_by_years_period = function(years_period) {
                              # 3.1 - Arguments verifications ----
                              codama::r_type_checking(r_object = years_period,
                                                      type = "integer")
                              # 3.2 - Global process ----
                              for (full_trip_id in seq_len(length.out = length(x = private$data))) {
                                if (full_trip_id == 1) {
                                  cat(format(Sys.time(),
                                             "%Y-%m-%d %H:%M:%S"),
                                      " - Start of full trips filtering by reference periode.\n", sep="")
                                }
                                capture.output(current_trips <- object_r6(class_name = "trips"),
                                               file = "NUL")
                                capture.output(current_trips$add(new_item = private$data[[full_trip_id]]),
                                               file = "NUL")
                                if (length(x = unlist(x = current_trips$extract_l1_element_value(element = "activities"))) != 0) {
                                  capture.output(current_activities <- object_r6(class_name = "activities"),
                                                 file = "NUL")
                                  capture.output(current_activities$add(new_item = unlist(x = current_trips$extract_l1_element_value(element = "activities"))),
                                                 file = "NUL")
                                  activities_date <- do.call("c",
                                                             current_activities$extract_l1_element_value(element = "activity_date"))
                                  activities_years <- unique(x = lubridate::year(x = activities_date))
                                  if (any(activities_years %in% years_period)) {
                                    private$data_selected <- append(private$data_selected,
                                                                    list(lapply(X = seq_len(length.out = current_trips$count()),
                                                                                FUN = function(list_id) {
                                                                                  private$data[[full_trip_id]][[list_id]]$clone()
                                                                                })))
                                    names(private$data_selected)[length(x = private$data_selected)] <- names(private$data[full_trip_id])
                                  }
                                }
                              }
                              if (any(private$id_not_full_trip %in% names(private$data_selected))) {
                                warning(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Missing trip(s) in at least one full trip item selected.\n",
                                        "[name(s)/id(s) in data selected of element(s): ",
                                        paste0(private$id_not_full_trip,
                                               "/",
                                               which(x = names(private$data_selected) %in% private$id_not_full_trip),
                                               collapse = ", "),
                                        "]")
                                private$id_not_full_trip_retained <- names(private$data_selected)[which(x = names(private$data_selected) %in% private$id_not_full_trip)]
                              }
                              # 3.3 - Log summary annotation ----
                              capture.output(current_trips <- object_r6(class_name = "trips"),
                                             file = "NUL")
                              capture.output(current_trips$add(new_item = unlist(x = private$data)),
                                             file = "NUL")
                              capture.output(current_activities <- object_r6(class_name = "activities"),
                                             file = "NUL")
                              capture.output(current_activities$add(new_item = unlist(x = current_trips$extract_l1_element_value(element = "activities"))),
                                             file = "NUL")
                              capture.output(current_trips_selected <- object_r6(class_name = "trips"),
                                             file = "NUL")
                              if(is.null(private$data_selected)){
                                stop(format(x = Sys.time(),
                                            format = "%Y-%m-%d %H:%M:%S"),
                                     " - No full trips retained when filtering by years period : ",
                                     paste0(years_period, collapse=", "),
                                     ". Check if the imported data includes these years.")
                              }
                              capture.output(current_trips_selected$add(new_item = unlist(x = private$data_selected)),
                                             file = "NUL")
                              capture.output(current_activities_selected <- object_r6(class_name = "activities"),
                                             file = "NUL")
                              capture.output(current_activities_selected$add(new_item = unlist(x = current_trips_selected$extract_l1_element_value(element = "activities"))),
                                             file = "NUL")
                              private$log_summary <- private$log_summary %>%
                                dplyr::add_row(step = "filter_by_years_period",
                                               input_trips = length(x = unlist(x = private$data)),
                                               input_activities = current_activities$count(),
                                               input_full_trips = length(x = private$data),
                                               output_full_trips = length(x = private$data_selected),
                                               output_trips = length(x = unlist(x = private$data_selected)),
                                               output_activities = current_activities_selected$count()) %>%
                                dplyr::relocate(input_full_trips,
                                                .before = output_trips)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End of full trips filtering.\n", sep="")
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 5 - Add elementary landings ----
                            #' @description Function for add elementary landings in full trips object.
                            #' @param object_elementarylandings Object of type R6-elementarylandings expected. A R6 reference object of class elementarylandings.
                            add_elementarylandings = function(object_elementarylandings) {
                              # 5.1 - Arguments verifications ----
                              if (length(private$data_selected) == 0) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Argument \"data_selected\" empty, ",
                                     "launch selection data before.")
                              } else if (! any(class(object_elementarylandings) == "elementarylandings")) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Invalid \"object_elementarylandings\" argument, ",
                                     "class elementarylandings expected.")
                              }
                              # 5.2 - Global process ----
                              for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                if (full_trip_id == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start of add elementary landings.\n", sep="")
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Ongoing process of adding elementary landings on full trip \"",
                                    names(private$data_selected)[[full_trip_id]],
                                    "\".\n", sep="")
                                capture.output(current_trips <- object_r6(class_name = "trips"),
                                               file = "NUL")
                                capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                               file = "NUL")
                                full_trips_elementarylandings <- lapply(X = seq_len(length.out = current_trips$count()),
                                                                        FUN = function(trip_id) {
                                                                          current_trip_id <- current_trips$extract(attribut_l1 = "data",
                                                                                                                   attribut_l2 = "trip_id",
                                                                                                                   id = trip_id)
                                                                          object_elementarylandings$filter_l1(filter = paste0("$path$trip_id == \"",
                                                                                                                              current_trip_id,
                                                                                                                              "\""),
                                                                                                              clone = TRUE)
                                                                        })
                                invisible(x = lapply(X = seq_len(length.out = current_trips$count()),
                                                     FUN = function(trip_id) {
                                                       current_trips$.__enclos_env__$private$data[[trip_id]]$.__enclos_env__$private$elementarylandings <- full_trips_elementarylandings[[trip_id]]
                                                     }))
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Successful process of adding elementary landings on full trip \"",
                                    names(private$data_selected)[[full_trip_id]],
                                    "\".\n", sep="")
                              }
                              # 5.3 - Log summary annotation ----
                              capture.output(current_trips <- object_r6(class_name = "trips"),
                                             file = "NUL")
                              capture.output(current_trips$add(new_item = unlist(x = private$data_selected)),
                                             file = "NUL")
                              capture.output(current_activities <- object_r6(class_name = "activities"),
                                             file = "NUL")
                              capture.output(current_activities$add(new_item = unlist(x = current_trips$extract_l1_element_value(element = "activities"))),
                                             file = "NUL")
                              capture.output(current_elementarycatches <- current_activities$extract_l1_element_value(element="elementarycatches"),
                                             file = "NUL")
                              current_elementarycatches <- current_elementarycatches[-which(sapply(current_elementarycatches, is.null))]
                              capture.output(current_elementarylandings <- object_r6(class_name = "elementarylandings"),
                                             file = "NUL")
                              capture.output(current_elementarylandings$add(new_item = unlist(x = current_trips$extract_l1_element_value(element = "elementarylandings"))),
                                             file = "NUL")
                              private$log_summary <- private$log_summary %>%
                                dplyr::mutate(input_elementary_landings = NA_integer_,
                                              output_elementary_landings = NA_integer_,
                                              output_landing_weight_elementary_landings = NA_real_) %>%
                                dplyr::add_row(step = "add_elementarylandings",
                                               input_trips = length(x = unlist(x = private$data_selected)),
                                               input_full_trips = length(x = private$data_selected),
                                               input_activities = current_activities$count(),
                                               input_elementary_catches = sum(sapply(current_elementarycatches, nrow)),
                                               input_elementary_landings = object_elementarylandings$count(),
                                               output_trips = input_trips,
                                               output_full_trips = input_full_trips,
                                               output_activities = input_activities,
                                               output_elementary_catches = input_elementary_catches,
                                               output_catch_weight_elementary_catches = sum(unlist(sapply(current_elementarycatches,
                                                                                                          dplyr::summarize,
                                                                                                          catch_weight=sum(catch_weight,
                                                                                                                           na.rm=TRUE))))
                                               ,
                                               output_elementary_landings = current_elementarylandings$count(),
                                               output_landing_weight_elementary_landings = sum(unlist(x = current_elementarylandings$extract_l1_element_value(element = "landing_weight")))) %>%
                                dplyr::relocate(input_elementary_landings,
                                                .before = output_trips)
                              cat(format(Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - End of add elementary landings.\n", sep="")
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 6 - Add wells and samples ----
                            #' @description Function for add wells and samples caracteristics in full trips object.
                            #' @param object_wells Object of type R6-wells expected. A R6 reference object of class wells.
                            add_wells_samples = function(object_wells) {
                              # 6.1 - Arguments verifications ----
                              if (length(private$data_selected) == 0) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Argument \"data_selected\" empty.")
                              } else if (! any(class(object_wells) == "wells")) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Invalid \"object_wells\" argument.")
                              }
                              # 6.2 - Global process ----
                              for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                if (full_trip_id == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start of add well(s) - sample(s). \n", sep="")
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Ongoing process of adding well(s) - sample(s) on full trip \"",
                                    names(private$data_selected)[[full_trip_id]],
                                    "\".\n", sep="")
                                capture.output(current_trips <- object_r6(class_name = "trips"),
                                               file = "NUL")
                                capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                               file = "NUL")
                                full_trips_wells <- lapply(X = seq_len(length.out = current_trips$count()),
                                                           FUN = function(trip_id) {
                                                             current_trip_id <- current_trips$extract(attribut_l1 = "data",
                                                                                                      attribut_l2 = "trip_id",
                                                                                                      id = trip_id)
                                                             object_wells$filter_l1(filter = paste0("$path$trip_id == \"",
                                                                                                    current_trip_id,
                                                                                                    "\""),
                                                                                    clone = TRUE)
                                                           })
                                invisible(x = lapply(X = seq_len(length.out = current_trips$count()),
                                                     FUN = function(trip_id) {
                                                       current_trips$.__enclos_env__$private$data[[trip_id]]$.__enclos_env__$private$wells <- full_trips_wells[[trip_id]]
                                                     }))
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Successful process of adding well(s) - sample(s) on full trip \"",
                                    names(private$data_selected)[[full_trip_id]],
                                    "\".\n", sep="")
                              }
                              # 6.3 - Log summary annotation ----
                              capture.output(current_trips <- object_r6(class_name = "trips"),
                                             file = "NUL")
                              capture.output(current_trips$add(new_item = unlist(x = private$data_selected)),
                                             file = "NUL")
                              capture.output(current_activities <- object_r6(class_name = "activities"),
                                             file = "NUL")
                              capture.output(current_activities$add(new_item = unlist(x = current_trips$extract_l1_element_value(element = "activities"))),
                                             file = "NUL")
                              capture.output(current_elementarycatches <- current_activities$extract_l1_element_value(element="elementarycatches"),
                                             file = "NUL")
                              current_elementarycatches <- current_elementarycatches[-which(sapply(current_elementarycatches, is.null))]
                              capture.output(current_elementarylandings <- object_r6(class_name = "elementarylandings"),
                                             file = "NUL")
                              capture.output(current_elementarylandings$add(new_item = unlist(x = current_trips$extract_l1_element_value(element = "elementarylandings"))),
                                             file = "NUL")
                              capture.output(current_wells <- object_r6(class_name = "wells"),
                                             file = "NUL")
                              capture.output(current_wells$add(new_item = unlist(x = current_trips$extract_l1_element_value(element = "wells"))),
                                             file = "NUL")
                              capture.output(current_elementarysamplesraw <- object_r6(class_name = "elementarysamplesraw"),
                                             file = "NUL")
                              capture.output(current_elementarysamplesraw$add(new_item = unlist(x = current_wells$extract_l1_element_value(element = "elementarysampleraw"))),
                                             file = "NUL")
                              private$log_summary <- private$log_summary %>%
                                dplyr::mutate(input_wells = NA_integer_,
                                              input_elementarysamplesraw = NA_integer_,
                                              output_wells = NA_integer_,
                                              output_elementarysamplesraw = NA_integer_) %>%
                                dplyr::add_row(step = "add_wells_samples",
                                               input_trips = length(x = unlist(x = private$data_selected)),
                                               input_full_trips = length(x = private$data_selected),
                                               input_activities = current_activities$count(),
                                               input_elementary_catches = sum(sapply(current_elementarycatches, nrow)),
                                               input_elementary_landings = current_elementarylandings$count(),
                                               input_wells = object_wells$count(),
                                               input_elementarysamplesraw = length(x = unlist(x = object_wells$extract_l1_element_value(element = "elementarysampleraw"))),
                                               output_trips = input_trips,
                                               output_full_trips = input_full_trips,
                                               output_activities = input_activities,
                                               output_elementary_catches = input_elementary_catches,
                                               output_catch_weight_elementary_catches = sum(unlist(sapply(current_elementarycatches,
                                                                                                          dplyr::summarize,
                                                                                                          catch_weight=sum(catch_weight,
                                                                                                                           na.rm=TRUE)))),
                                               output_elementary_landings = input_elementary_landings,
                                               output_landing_weight_elementary_landings = sum(unlist(x = current_elementarylandings$extract_l1_element_value(element = "landing_weight"))),
                                               output_wells = current_wells$count(),
                                               output_elementarysamplesraw = current_elementarysamplesraw$count()) %>%
                                dplyr::relocate(input_wells,
                                                input_elementarysamplesraw,
                                                .before = output_trips)
                              cat(format(Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - End of add well(s) - sample(s).\n", sep="")
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 7 - Process 1.1: rf1 ----
                            #' @description Process of Raising Factor level 1 (RF1).
                            #' @param species_fao_codes_rf1 Object of type \code{\link[base]{character}} expected. By default YFT, SKJ, BET, ALB, MIX and LOT. Specie(s) FAO code(s) used for the RF1 process.
                            #' @param species_fate_codes_rf1 Object of type \code{\link[base]{integer}} expected. By default 6 ("Retained, presumably destined for the cannery"). Specie(s) fate code(s) used for the RF1 process.
                            #' @param vessel_type_codes_rf1 Object of type \code{\link[base]{integer}} expected. By default 4, 5 and 6. Vessel type(s).
                            #' @param rf1_lowest_limit Object of type \code{\link[base]{numeric}} expected. Verification value for the lowest limit of the RF1. By default 0.8.
                            #' @param rf1_highest_limit Object of type \code{\link[base]{numeric}} expected. Verification value for the highest limit of the RF1. By default 1.2.
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction egual TRUE. Path of the global outputs directory. The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            #' @importFrom codama r_type_checking
                            rf1 = function(species_fao_codes_rf1 = c("YFT", "SKJ", "BET", "ALB", "MIX", "LOT"),
                                           species_fate_codes_rf1 = as.integer(c(6, 11)),
                                           vessel_type_codes_rf1 = as.integer(c(4, 5, 6)),
                                           rf1_lowest_limit = 0.8,
                                           rf1_highest_limit = 1.2,
                                           global_output_path = NULL,
                                           output_format = "eu") {
                              # 7.1 - Arguments verification ----
                              codama::r_type_checking(r_object = species_fao_codes_rf1,
                                                      type = "character")
                              codama::r_type_checking(r_object = species_fate_codes_rf1,
                                                      type = "integer")
                              codama::r_type_checking(r_object = vessel_type_codes_rf1,
                                                      type = "integer")
                              codama::r_type_checking(r_object = rf1_lowest_limit,
                                                      type = "numeric",
                                                      length = 1L)
                              codama::r_type_checking(r_object = rf1_highest_limit,
                                                      type = "numeric",
                                                      length = 1L)
                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              # 7.2 - Global process ----
                              if (is.null(x = private$data_selected)) {
                                stop(format(x = Sys.time(),
                                            format = "%Y-%m-%d %H:%M:%S"),
                                     " - Empty data selected in the R6 object. Process 1.1 (Raising Factor level 1) cancelled.")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(x = private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(x = Sys.time(),
                                               format = "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 1.1: Raising Factor level 1.\n", sep="")
                                  }
                                  if (names(x = private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    warning(format(x = Sys.time(),
                                                   format = "%Y-%m-%d %H:%M:%S"),
                                            " - Warning: missing trip(s) in full trip element \"",
                                            names(x = private$data_selected)[full_trip_id],
                                            "\".")
                                    stop <- 0
                                    for (trip_id in seq_len(length.out = length(x = private$data_selected[[full_trip_id]]))) {
                                      # Case 1.1 ----
                                      # at least one logbook is missing in not complete full trip item
                                      if (trip_id == 1) {
                                        logbook_availability <- vector(mode = "integer")
                                      }
                                      current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                      logbook_availability <- append(logbook_availability,
                                                                     current_trip$.__enclos_env__$private$logbook_availability_code)
                                      if (trip_id == length(x = private$data_selected[[full_trip_id]])) {
                                        if (any(logbook_availability) == 0) {
                                          warning(format(x = Sys.time(),
                                                         format = "%Y-%m-%d %H:%M:%S"),
                                                  " - Missing logbook in trip element \"",
                                                  names(x = private$data_selected)[full_trip_id],
                                                  "\".\n",
                                                  "[trip: ",
                                                  current_trip$.__enclos_env__$private$trip_id,
                                                  "]")
                                          capture.output(current_trips <- object_r6(class_name = "trips"),
                                                         file = "NUL")
                                          capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                         file = "NUL")
                                          current_trips$modification_l1(modification = "$path$rf1 <- NA_real_")
                                          current_trips$modification_l1(modification = "$path$statut_rf1 <- 1.1")
                                          stop <- 1
                                        }
                                      }
                                    }
                                    if (stop != 1) {
                                      for (trip_id in seq_len(length.out = length(x = private$data_selected[[full_trip_id]]))) {
                                        if (trip_id == 1) {
                                          current_elementarycatches <- NULL
                                        }
                                        current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                        if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                          for (activity_id in seq_len(length.out = length(x = current_trip$.__enclos_env__$private$activities))) {
                                            current_elementarycatches <- rbind(current_elementarycatches,
                                                                               current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches)
                                          }
                                        }
                                      }
                                      if (is.null(x = current_elementarycatches)) {
                                        # Case 1.2 ----
                                        # trips with no catches (for example route or support) in not complete full trip item
                                        capture.output(current_trips <- object_r6(class_name = "trips"),
                                                       file = "NUL")
                                        capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                       file = "NUL")
                                        current_trips$modification_l1(modification = "$path$rf1 <- NA_real_")
                                        current_trips$modification_l1(modification = "$path$statut_rf1 <- 1.2")
                                      } else {
                                        for (trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                          if (trip_id == 1) {
                                            current_elementarylandings <- NULL
                                            stop_bis <- 0
                                          }
                                          current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                          if (trip_id == length(x = private$data_selected[[full_trip_id]])) {
                                            if (! is.null(x = unlist(x = current_trip$.__enclos_env__$private$elementarylandings))) {
                                              current_elementarylandings <- append(current_elementarylandings,
                                                                                   unlist(current_trip$.__enclos_env__$private$elementarylandings))
                                            } else {
                                              stop_bis <- 1
                                            }
                                          } else {
                                            current_elementarylandings <- append(current_elementarylandings,
                                                                                 unlist(current_trip$.__enclos_env__$private$elementarylandings))
                                          }
                                          if (stop_bis == 1) {
                                            # Case 1.3 ----
                                            # at least one elementary landing is missing in not complete full trip item
                                            warning(format(x = Sys.time(),
                                                           format = "%Y-%m-%d %H:%M:%S"),
                                                    " - Missing elementary landing in trip element \"",
                                                    names(x = private$data_selected)[full_trip_id],
                                                    "\".\n",
                                                    "[trip: ",
                                                    current_trip$.__enclos_env__$private$trip_id,
                                                    "]")
                                            capture.output(current_trips <- object_r6(class_name = "trips"),
                                                           file = "NUL")
                                            capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                           file = "NUL")
                                            current_trips$modification_l1(modification = "$path$rf1 <- NA_real_")
                                            current_trips$modification_l1(modification = "$path$statut_rf1 <- 1.3")
                                          } else {
                                            # Case 1.4 ----
                                            # almost rocks dude ! (not complete full trip item)
                                            capture.output(current_trips <- object_r6(class_name = "trips"),
                                                           file = "NUL")
                                            capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                           file = "NUL")
                                            current_trips$modification_l1(modification = "$path$rf1 <- NA_real_")
                                            current_trips$modification_l1(modification = "$path$statut_rf1 <- 1.4")
                                          }
                                        }
                                      }
                                    }
                                  } else {
                                    capture.output(current_trips <- object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (unique(x = unlist(x = current_trips$extract_l1_element_value(element = "vessel_type_code"))) %in% vessel_type_codes_rf1) {
                                      stop <- 0
                                      for (trip_id in seq_len(length.out = length(x = private$data_selected[[full_trip_id]]))) {
                                        # Case 2.1 ----
                                        # at least one logbook is missing in complete full trip item
                                        if (trip_id == 1) {
                                          logbook_availability <- vector(mode = "integer")
                                        }
                                        current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                        logbook_availability <- append(logbook_availability,
                                                                       current_trip$.__enclos_env__$private$logbook_availability_code)
                                        if (trip_id == length(x = private$data_selected[[full_trip_id]])) {
                                          if (any(logbook_availability) == 0) {
                                            warning(format(x = Sys.time(),
                                                           format = "%Y-%m-%d %H:%M:%S"),
                                                    " - Missing logbook in trip element \"",
                                                    names(x = private$data_selected)[full_trip_id],
                                                    "\".\n",
                                                    "[trip: ",
                                                    current_trip$.__enclos_env__$private$trip_id,
                                                    "]")
                                            capture.output(current_trips <- object_r6(class_name = "trips"),
                                                           file = "NUL")
                                            capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                           file = "NUL")
                                            current_trips$modification_l1(modification = "$path$rf1 <- 1")
                                            current_trips$modification_l1(modification = "$path$statut_rf1 <- 2.1")
                                            stop <- 1
                                          }
                                        }
                                      }
                                      if (stop != 1) {
                                        capture.output(current_elementarycatches <- NULL,
                                                       file = "NUL")
                                        for (trip_id in seq_len(length.out = length(x = private$data_selected[[full_trip_id]]))) {
                                          current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                          if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                            for (activity_id in seq_len(length.out = length(x = current_trip$.__enclos_env__$private$activities))) {
                                              capture.output(current_elementarycatches <- rbind(current_elementarycatches,
                                                                                                current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches),
                                                             file = "NUL")
                                            }
                                          }
                                        }
                                        if (nrow(current_elementarycatches) == 0
                                            || (! any((tidyr::crossing(species_fao_codes_rf1,
                                                                       species_fate_codes_rf1) %>%
                                                       dplyr::mutate(species_fao_fate_codes_rf1 = stringr::str_c(species_fao_codes_rf1,
                                                                                                                 species_fate_codes_rf1,
                                                                                                                 sep = "_")) %>%
                                                       dplyr::pull(species_fao_fate_codes_rf1)) %in% (unique(x = paste(current_elementarycatches$species_fao_code,
                                                                                                                       current_elementarycatches$species_fate_code,
                                                                                                                       sep = "_")))))) {
                                          # Case 2.2 ----
                                          # trips with no catches (at all or related to the arguments species_fao_codes_rf1 and species_fate_codes_rf1) in complete full trip item
                                          capture.output(current_trips <- object_r6(class_name = "trips"),
                                                         file = "NUL")
                                          capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                         file = "NUL")
                                          current_trips$modification_l1(modification = "$path$rf1 <- 1")
                                          current_trips$modification_l1(modification = "$path$statut_rf1 <- 2.2")
                                        } else {
                                          current_elementarycatches_weight <- vector(mode = "numeric")
                                          capture.output(current_elementarycatches_final <- current_elementarycatches %>%  dplyr::filter(species_fao_code %in% species_fao_codes_rf1,
                                                                                                                                         species_fate_code %in% species_fate_codes_rf1),
                                                         file = "NUL")
                                          current_elementarycatches_weight <- current_elementarycatches_final %>% dplyr::pull(catch_weight)
                                          capture.output(current_elementarylandings <- object_r6(class_name = "elementarylandings"),
                                                         file = "NUL")
                                          for (trip_id in seq_len(length.out = length(x = private$data_selected[[full_trip_id]]))) {
                                            current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                            if (length(x = current_trip$.__enclos_env__$private$elementarylandings) != 0)
                                              capture.output(current_elementarylandings$add(new_item = current_trip$.__enclos_env__$private$elementarylandings),
                                                             file = "NUL")
                                          }
                                          if (current_elementarylandings$count() == 0
                                              || (! any(unique(x = unlist(x = current_elementarylandings$extract_l1_element_value(element = "species_fao_code"))) %in% species_fao_codes_rf1))) {
                                            # Case 2.3 ----
                                            # no elementary landing (at all or related to the arguments species_fao_codes_rf1 and species_fate_codes_rf1) in complete full trip item
                                            warning(format(x = Sys.time(),
                                                           format = "%Y-%m-%d %H:%M:%S"),
                                                    " - Warning: missing elementary landing in trip element \"",
                                                    names(x = private$data_selected)[full_trip_id],
                                                    "\".\n",
                                                    "[trip: ",
                                                    current_trip$.__enclos_env__$private$trip_id,
                                                    "]")
                                            capture.output(current_trips <- object_r6(class_name = "trips"),
                                                           file = "NUL")
                                            capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                           file = "NUL")
                                            current_trips$modification_l1(modification = "$path$rf1 <- 1")
                                            current_trips$modification_l1(modification = "$path$statut_rf1 <- 2.3")
                                          } else {
                                            # Case 2.4 ----
                                            # everything rocks dude !
                                            capture.output(current_elementarylandings_rf1_species <- object_r6(class_name = "elementarylandings"),
                                                           file = "NUL")
                                            capture.output(current_elementarylandings_rf1_species$add(new_item = current_elementarylandings$filter_l1(filter = paste0("$path$species_fao_code %in% c(\"",
                                                                                                                                                                      paste(species_fao_codes_rf1,
                                                                                                                                                                            collapse = "\", \""),
                                                                                                                                                                      "\")"))),
                                                           file = "NUL")
                                            current_elementarylandings_weight <- unlist(x = current_elementarylandings_rf1_species$extract_l1_element_value(element = "landing_weight"))
                                            current_rf1 <- sum(current_elementarylandings_weight) / sum(current_elementarycatches_weight)
                                            if (current_rf1 < rf1_lowest_limit
                                                | current_rf1 > rf1_highest_limit) {
                                              warning(format(x = Sys.time(),
                                                             format = "%Y-%m-%d %H:%M:%S"),
                                                      " - Rf1 value of full trip element \"",
                                                      names(x = private$data_selected)[full_trip_id],
                                                      "\" out of theorical boundaries: ",
                                                      round(x = current_rf1,
                                                            digits = 3),
                                                      ".\n",
                                                      "[trip: ",
                                                      current_trip$.__enclos_env__$private$trip_id,
                                                      "]")
                                            }
                                            capture.output(current_trips <- object_r6(class_name = "trips"),
                                                           file = "NUL")
                                            capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                           file = "NUL")
                                            current_trips$modification_l1(modification = paste0("$path$rf1 <- ",
                                                                                                current_rf1))
                                            current_trips$modification_l1(modification = "$path$statut_rf1 <- 2.4")
                                          }
                                        }
                                      }
                                    } else {
                                      # Case 3.1 ----d
                                      current_trips$modification_l1(modification = "$path$rf1 <- NA_real_")
                                      current_trips$modification_l1(modification = "$path$statut_rf1 <- 3.1")
                                    }
                                  }
                                  # Assign rf1 to elementary catches ----
                                  for (trip_id in seq_len(length.out = length(x = private$data_selected[[full_trip_id]]))) {
                                    current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                    current_rf1 <- current_trip$.__enclos_env__$private$rf1
                                    current_vessel_type_code <- current_trip$.__enclos_env__$private$vessel_type_code
                                    if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                      for (activity_id in seq_len(length.out = length(x = current_trip$.__enclos_env__$private$activities))) {
                                        current_elementarycatches <- current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches
                                        if (! is.null(x = current_elementarycatches)) {
                                          if(current_vessel_type_code %in% vessel_type_codes_rf1){
                                            current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches <- current_elementarycatches %>%
                                              dplyr::mutate(catch_weight_rf1=catch_weight * current_rf1)
                                          } else{
                                            current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches <- current_elementarycatches %>%
                                              dplyr::mutate(catch_weight_rf1=NA_real_)
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                # 7.3 - Outputs extraction ----
                                # outputs manipulation
                                if (! is.null(x = global_output_path)) {
                                  full_trips_selected <- private$data_selected
                                  capture.output(trips_selected <- object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                                 file = "NUL")
                                  total_landings_catches_species_activities <- lapply(X = seq_len(length.out = trips_selected$count()),
                                                                                      FUN = function(trip_id) {
                                                                                        current_trip <- trips_selected$extract(id = trip_id)[[1]]
                                                                                        if (length(x = current_trip$.__enclos_env__$private$elementarylandings) != 0) {
                                                                                          capture.output(current_elementarylandings <- object_r6(class_name = "elementarylandings"),
                                                                                                         file = "NUL")
                                                                                          capture.output(current_elementarylandings$add(current_trip$.__enclos_env__$private$elementarylandings),
                                                                                                         file = "NUL")
                                                                                          current_total_landings_species <- data.frame(species = unlist(x = current_elementarylandings$extract_l1_element_value(element = "species_fao_code")),
                                                                                                                                       landing_weight = unlist(x = current_elementarylandings$extract_l1_element_value(element = "landing_weight"))) %>%
                                                                                            dplyr::group_by(species) %>%
                                                                                            dplyr::summarise(landing_weight = sum(landing_weight),
                                                                                                             .groups = "drop") %>%
                                                                                            dplyr::mutate(trip_id = current_trip$.__enclos_env__$private$trip_id)
                                                                                          elementarylandings <- TRUE
                                                                                        } else {
                                                                                          elementarylandings <- FALSE
                                                                                        }
                                                                                        if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                                                                          capture.output(current_activities <- object_r6(class_name = "activities"),
                                                                                                         file = "NUL")
                                                                                          capture.output(current_activities$add(current_trip$.__enclos_env__$private$activities),
                                                                                                         file = "NUL")
                                                                                          current_activities_latitude_longitude <- dplyr::tibble(activity_id = unlist(x = current_activities$extract_l1_element_value(element = "activity_id")),
                                                                                                                                                 activity_latitude = unlist(x = current_activities$extract_l1_element_value(element = "activity_latitude")),
                                                                                                                                                 activity_longitude = unlist(x = current_activities$extract_l1_element_value(element = "activity_longitude")))
                                                                                          capture.output(current_elementarycatches <- do.call(rbind,
                                                                                                                                              current_activities$extract_l1_element_value(element = "elementarycatches")),
                                                                                                         file = "NUL")
                                                                                          if (length(current_elementarycatches) == 0) {
                                                                                            elementarycatches <- FALSE
                                                                                          } else {
                                                                                            current_total_catches_species_activities <- current_elementarycatches %>%
                                                                                              dplyr::select(activity_id,
                                                                                                            species_fao_code,
                                                                                                            species_fate_code,
                                                                                                            catch_weight,
                                                                                                            catch_count,
                                                                                                            catch_weight_rf1) %>%
                                                                                              dplyr::rename(species = species_fao_code) %>%
                                                                                              dplyr::mutate(trip_id = current_trip$.__enclos_env__$private$trip_id) %>%
                                                                                              dplyr::relocate(trip_id,
                                                                                                              .before = activity_id) %>%
                                                                                              dplyr::arrange(activity_id)
                                                                                            elementarycatches <- TRUE
                                                                                          }
                                                                                        } else {
                                                                                          elementarycatches <- FALSE
                                                                                        }
                                                                                        if (elementarylandings == TRUE) {
                                                                                          if (elementarycatches == TRUE) {
                                                                                            current_total_landings_catches_species_activities <- current_total_landings_species %>%
                                                                                              dplyr::full_join(current_total_catches_species_activities,
                                                                                                               by = c("species",
                                                                                                                      "trip_id")) %>%
                                                                                              dplyr::left_join(current_activities_latitude_longitude,
                                                                                                               by = "activity_id") %>%
                                                                                              dplyr::relocate(activity_latitude,
                                                                                                              activity_longitude,
                                                                                                              .after = activity_id)
                                                                                          } else {
                                                                                            current_total_landings_catches_species_activities <- dplyr::mutate(.data = current_total_landings_species,
                                                                                                                                                               activity_id = NA_character_,
                                                                                                                                                               activity_latitude = NA_real_,
                                                                                                                                                               activity_longitude = NA_real_,
                                                                                                                                                               species_fate_code = NA_integer_,
                                                                                                                                                               catch_weight = NA_real_,
                                                                                                                                                               catch_count = NA_real_,
                                                                                                                                                               catch_weight_rf1 = NA_real_)
                                                                                          }
                                                                                        } else {
                                                                                          if (elementarycatches == TRUE) {
                                                                                            current_total_landings_catches_species_activities <- dplyr::mutate(.data = current_total_catches_species_activities,
                                                                                                                                                               landing_weight = NA_real_) %>%
                                                                                              dplyr::left_join(current_activities_latitude_longitude,
                                                                                                               by = "activity_id") %>%
                                                                                              dplyr::relocate(activity_latitude,
                                                                                                              activity_longitude,
                                                                                                              .after = activity_id)

                                                                                          } else {
                                                                                            current_total_landings_catches_species_activities <- NULL
                                                                                          }
                                                                                        }
                                                                                        return(current_total_landings_catches_species_activities)
                                                                                      })
                                  total_landings_catches_species_activities <- tidyr::tibble(do.call(what = rbind,
                                                                                                     args = total_landings_catches_species_activities))
                                  total_landings_catches <- dplyr::distinct(.data = total_landings_catches_species_activities,
                                                                            trip_id,
                                                                            species,
                                                                            landing_weight) %>%
                                    dplyr::group_by(trip_id) %>%
                                    dplyr::summarise(landing_weight = sum(landing_weight,
                                                                          na.rm = TRUE),
                                                     .groups = "drop") %>%
                                    dplyr::inner_join(dplyr::select(.data = total_landings_catches_species_activities,
                                                                    trip_id,
                                                                    catch_weight,
                                                                    catch_count,
                                                                    catch_weight_rf1) %>%
                                                        dplyr::group_by(trip_id) %>%
                                                        dplyr::summarise(catch_weight = sum(catch_weight,
                                                                                            na.rm = TRUE),
                                                                         catch_count = sum(catch_count,
                                                                                           na.rm=TRUE),
                                                                         catch_weight_rf1 = sum(catch_weight_rf1,
                                                                                                na.rm = TRUE),
                                                                         .groups = "drop"),
                                                      by = "trip_id")
                                  outputs_process_1_1 <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                   FUN = function(full_trip_id) {
                                                                                                     if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                       return(rep(x = full_trip_id,
                                                                                                                  length(x = full_trips_selected[[full_trip_id]])))
                                                                                                     } else {
                                                                                                       return(full_trip_id)
                                                                                                     }
                                                                                                   })),
                                                                    "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                     FUN = function(full_trip_id) {
                                                                                                       if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                         return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                    length(x = full_trips_selected[[full_trip_id]])))
                                                                                                       } else {
                                                                                                         return(names(x = full_trips_selected[full_trip_id]))
                                                                                                       }
                                                                                                     })),
                                                                    "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                    "trip_end_date" = do.call("c",
                                                                                              trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                    "year_trip_end_date" = sapply(do.call("c",
                                                                                                          trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                  lubridate::year),
                                                                    "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                    "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))),
                                                                    "rf1" = unlist(x = (trips_selected$extract_l1_element_value(element = "rf1"))),
                                                                    "statut_rf1" = unlist(x = (trips_selected$extract_l1_element_value(element = "statut_rf1"))))
                                  global_outputs_process_1_1 <- dplyr::left_join(x = outputs_process_1_1,
                                                                                 y = total_landings_catches,
                                                                                 by = "trip_id")
                                  detail_outputs_process_1_1 <- outputs_process_1_1 %>%
                                    dplyr::full_join(x = outputs_process_1_1,
                                                     y = total_landings_catches_species_activities,
                                                     by = "trip_id") %>%
                                    dplyr::relocate(activity_id,
                                                    activity_latitude,
                                                    activity_longitude,
                                                    .after = trip_id)
                                  # extraction
                                  if (output_format == "us") {
                                    outputs_dec <- "."
                                    outputs_sep <- ","
                                  } else if (output_format == "eu") {
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  }
                                  write.table(x = global_outputs_process_1_1,
                                              file = file.path(global_output_path,
                                                               "level1",
                                                               "data",
                                                               "process_1_1_global.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  write.table(x = detail_outputs_process_1_1,
                                              file = file.path(global_output_path,
                                                               "level1",
                                                               "data",
                                                               "process_1_1_detail.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Outputs extracted in the following directory:\n",
                                      file.path(global_output_path,
                                                "level1",
                                                "data"), "\n",
                                      sep = "")
                                }
                                cat(format(x = Sys.time(),
                                           format = "%Y-%m-%d %H:%M:%S"),
                                    " - Successful process 1.1: Raising Factor level 1.\n", sep="")
                              }
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 8 - Process 1.2: rf2 ----
                            #' @description Process of Raising Factor level 2 (rf2).
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction egual TRUE. Path of the global outputs directory. The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            #' @importFrom lubridate year
                            rf2 = function(global_output_path = NULL,
                                           output_format = "eu") {
                              # 8.1 - Arguments verification ----
                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              # 8.2 - Global process ----
                              if (is.null(x = private$data_selected)) {
                                stop(format(x = Sys.time(),
                                            format = "%Y-%m-%d %H:%M:%S"),
                                     " - Empty data selected in the R6 object.\n",
                                     " - Process 1.2 (raising factor level 2) cancelled.")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(x = private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(x = Sys.time(),
                                               format = "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 1.2: raising factor level 2.\n", sep="")
                                  }
                                  if (is.null(x = private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$statut_rf1)) {
                                    stop(format(Sys.time(),
                                                "%Y-%m-%d %H:%M:%S"),
                                         " - RF1 is null for the item \"",
                                         names(private$data_selected)[full_trip_id],
                                         "\".\n",
                                         "Check if the process 1.1 (raising factor level 1) was successfully applied.\n",
                                         "[trip: ",
                                         private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                         "]")
                                  } else {
                                    if (private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$statut_rf1 == 2.1) {
                                      # Case 1 ----
                                      # rf2 calculated
                                      stop(format(x = Sys.time(),
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                           " - RF2 not developped yet.")
                                    } else {
                                      if (private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$statut_rf1 %in% c(2.2, 2.3, 2.4)) {
                                        # Case 2 ----
                                        # rf2 not need to be calculated
                                        for (trip_id in seq_len(length.out = length(x = private$data_selected[[full_trip_id]]))) {
                                          current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                          current_rf2 <- 1
                                          current_trip$.__enclos_env__$private$rf2 <- current_rf2
                                          current_trip$.__enclos_env__$private$statut_rf2 <- 2
                                          if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                            for (activity_id in seq_len(length.out = length(x = current_trip$.__enclos_env__$private$activities))) {
                                              current_elementarycatches <- current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches
                                              if (length(x = current_elementarycatches) != 0) {
                                                current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches <- current_elementarycatches %>%
                                                  dplyr::mutate(catch_weight_rf2 = catch_weight_rf1)
                                              }
                                            }
                                          }
                                        }
                                      } else {
                                        # Case 3 ----
                                        # full trip not complete or vessel type code not validated
                                        for (trip_id in seq_len(length.out = length(x = private$data_selected[[full_trip_id]]))) {
                                          current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                          current_trip$.__enclos_env__$private$rf2 <- NA_real_
                                          current_trip$.__enclos_env__$private$statut_rf2 <- 3
                                          if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                            for (activity_id in seq_len(length.out = length(x = current_trip$.__enclos_env__$private$activities))) {
                                              current_elementarycatches <- current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches
                                              if (length(x = current_elementarycatches) != 0) {
                                                current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches <- current_elementarycatches %>%
                                                  dplyr::mutate(catch_weight_rf2 = catch_weight_rf1)
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                # 8.3 - Outputs extraction ----
                                # outputs manipulation
                                if (! is.null(x = global_output_path)) {
                                  full_trips_selected <- private$data_selected
                                  capture.output(trips_selected <- object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                                 file = "NUL")
                                  total_landings_catches_species <- lapply(X = seq_len(length.out = trips_selected$count()),
                                                                           FUN = function(trip_id) {
                                                                             current_trip <- trips_selected$extract(id = trip_id)[[1]]
                                                                             if (length(x = current_trip$.__enclos_env__$private$elementarylandings) != 0) {
                                                                               capture.output(current_elementarylandings <- object_r6(class_name = "elementarylandings"),
                                                                                              file = "NUL")
                                                                               capture.output(current_elementarylandings$add(current_trip$.__enclos_env__$private$elementarylandings),
                                                                                              file = "NUL")
                                                                               current_total_landings_species <- data.frame(species = unlist(x = current_elementarylandings$extract_l1_element_value(element = "species_fao_code")),
                                                                                                                            landing_weight = unlist(x = current_elementarylandings$extract_l1_element_value(element = "landing_weight"))) %>%
                                                                                 dplyr::group_by(species) %>%
                                                                                 dplyr::summarise(landing_weight = sum(landing_weight),
                                                                                                  .groups = "drop") %>%
                                                                                 dplyr::mutate(trip_id = current_trip$.__enclos_env__$private$trip_id)
                                                                               elementarylandings <- TRUE
                                                                             } else {
                                                                               elementarylandings <- FALSE
                                                                             }
                                                                             if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                                                               capture.output(current_activities <- object_r6(class_name = "activities"),
                                                                                              file = "NUL")
                                                                               capture.output(current_activities$add(current_trip$.__enclos_env__$private$activities),
                                                                                              file = "NUL")
                                                                               current_activities_latitude_longitude <- dplyr::tibble(activity_id = unlist(x = current_activities$extract_l1_element_value(element = "activity_id")),
                                                                                                                                      activity_latitude = unlist(x = current_activities$extract_l1_element_value(element = "activity_latitude")),
                                                                                                                                      activity_longitude = unlist(x = current_activities$extract_l1_element_value(element = "activity_longitude")))
                                                                               capture.output(current_elementarycatches <- do.call(rbind,
                                                                                                                                   current_activities$extract_l1_element_value(element = "elementarycatches")),
                                                                                              file = "NUL")
                                                                               if (length(current_elementarycatches) == 0) {
                                                                                 elementarycatches <- FALSE
                                                                               } else {
                                                                                 current_total_catches_species_activities <- current_elementarycatches %>%
                                                                                   dplyr::select(activity_id,
                                                                                                 species_fao_code,
                                                                                                 species_fate_code,
                                                                                                 catch_weight,
                                                                                                 catch_count,
                                                                                                 catch_weight_rf2) %>%
                                                                                   dplyr::rename(species = species_fao_code) %>%
                                                                                   dplyr::mutate(trip_id = current_trip$.__enclos_env__$private$trip_id) %>%
                                                                                   dplyr::arrange(activity_id)
                                                                                 elementarycatches <- TRUE
                                                                               }
                                                                             } else {
                                                                               elementarycatches <- FALSE
                                                                             }
                                                                             if (elementarylandings == TRUE) {
                                                                               if (elementarycatches == TRUE) {
                                                                                 current_total_landings_catches_species_activities <- current_total_landings_species %>%
                                                                                   dplyr::full_join(current_total_catches_species_activities,
                                                                                                    by = c("species",
                                                                                                           "trip_id")) %>%
                                                                                   dplyr::left_join(current_activities_latitude_longitude,
                                                                                                    by = "activity_id") %>%
                                                                                   dplyr::relocate(activity_latitude,
                                                                                                   activity_longitude,
                                                                                                   .after = activity_id)
                                                                               } else {
                                                                                 current_total_landings_catches_species_activities <- dplyr::mutate(.data = current_total_landings_species,
                                                                                                                                                    activity_id = NA_character_,
                                                                                                                                                    activity_latitude = NA_real_,
                                                                                                                                                    activity_longitude = NA_real_,
                                                                                                                                                    species_fate_code = NA_integer_,
                                                                                                                                                    catch_weight = NA_real_,
                                                                                                                                                    catch_count=NA_real_,
                                                                                                                                                    catch_weight_rf2 = NA_real_)
                                                                               }
                                                                             } else {
                                                                               if (elementarycatches == TRUE) {
                                                                                 current_total_landings_catches_species_activities <- dplyr::mutate(.data = current_total_catches_species_activities,
                                                                                                                                                    landing_weight = NA_real_) %>%
                                                                                   dplyr::left_join(current_activities_latitude_longitude,
                                                                                                    by = "activity_id") %>%
                                                                                   dplyr::relocate(activity_latitude,
                                                                                                   activity_longitude,
                                                                                                   .after = activity_id)
                                                                               } else {
                                                                                 current_total_landings_catches_species_activities <- NULL
                                                                               }
                                                                             }
                                                                             return(current_total_landings_catches_species_activities)
                                                                           })
                                  total_landings_catches_species <- as.data.frame(do.call(what = rbind,
                                                                                          args = total_landings_catches_species))
                                  total_landings_catches <- total_landings_catches_species %>%
                                    dplyr::group_by(trip_id) %>%
                                    dplyr::summarise(landing_weight = sum(landing_weight,
                                                                          na.rm = TRUE),
                                                     catch_weight = sum(catch_weight,
                                                                        na.rm = TRUE),
                                                     catch_count = sum(catch_count,
                                                                       na.rm=TRUE),
                                                     catch_weight_rf2 = sum(catch_weight_rf2,
                                                                            na.rm = TRUE),
                                                     .groups = "drop")
                                  outputs_process_1_2 <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                   FUN = function(full_trip_id) {
                                                                                                     if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                       return(rep(x = full_trip_id,
                                                                                                                  length(x = full_trips_selected[[full_trip_id]])))
                                                                                                     } else {
                                                                                                       return(full_trip_id)
                                                                                                     }
                                                                                                   })),
                                                                    "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                     FUN = function(full_trip_id) {
                                                                                                       if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                         return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                    length(x = full_trips_selected[[full_trip_id]])))
                                                                                                       } else {
                                                                                                         return(names(x = full_trips_selected[full_trip_id]))
                                                                                                       }
                                                                                                     })),
                                                                    "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                    "trip_end_date" = do.call("c",
                                                                                              trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                    "year_trip_end_date" = sapply(do.call("c",
                                                                                                          trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                  lubridate::year),
                                                                    "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                    "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))),
                                                                    "rf2" = unlist(x = (trips_selected$extract_l1_element_value(element = "rf2"))),
                                                                    "statut_rf2" = unlist(x = (trips_selected$extract_l1_element_value(element = "statut_rf2"))))
                                  global_outputs_process_1_2 <- dplyr::left_join(x = outputs_process_1_2,
                                                                                 y = total_landings_catches,
                                                                                 by = "trip_id")
                                  detail_outputs_process_1_2 <- outputs_process_1_2 %>%
                                    dplyr::full_join(x = outputs_process_1_2,
                                                     y = total_landings_catches_species,
                                                     by = "trip_id") %>%
                                    dplyr::relocate(activity_id,
                                                    activity_latitude,
                                                    activity_longitude,
                                                    .after = trip_id)
                                  # extraction
                                  if (output_format == "us") {
                                    outputs_dec <- "."
                                    outputs_sep <- ","
                                    output_fileEncoding = ""
                                  } else if (output_format == "eu") {
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                    output_fileEncoding = "UTF-16LE"
                                  } else {
                                    warning(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Wrong outputs format define, European format will be applied.")
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  }
                                  write.table(x = global_outputs_process_1_2,
                                              file = file.path(global_output_path,
                                                               "level1",
                                                               "data",
                                                               "process_1_2_global.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  write.table(x = detail_outputs_process_1_2,
                                              file = file.path(global_output_path,
                                                               "level1",
                                                               "data",
                                                               "process_1_2_detail.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Outputs extracted in the following directory:\n",
                                      file.path(global_output_path,
                                                "level1",
                                                "data"),"\n",
                                      sep = "")
                                }
                                cat(format(x = Sys.time(),
                                           format = "%Y-%m-%d %H:%M:%S"),
                                    " - End of raising factor process 2.\n")
                              }
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 9 - Process 1.3: conversion_weight_category ----
                            #' @description Process of logbook weight categories conversion.
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction egual TRUE. Path of the global outputs directory. The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            #' @param referential_template Object of class \code{\link[base]{character}} expected. By default "observe". Referential template selected (for example regarding the activity_code). You can switch to "avdth".
                            conversion_weigth_category = function(global_output_path = NULL,
                                                                  output_format = "eu",
                                                                  referential_template = "observe") {
                              # 9.1 - Arguments verification ----
                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              codama::r_type_checking(r_object = referential_template,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("observe",
                                                                        "avdth"))
                              # 9.2 - Global process ----
                              category_1 <- "<10kg"
                              category_2 <- "10-30kg"
                              category_3 <- ">30kg"
                              category_4 <- ">10kg"
                              category_5 <- "unknown"
                              if (is.null(x = private$data_selected)) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Empty data selected in the R6 object. Process 1.3 (logbook weight categories) cancelled.")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 1.3: logbook weight categories conversion.\n")
                                  }
                                  if (names(x = private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    message(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Warning on item \"",
                                            names(x = private$data_selected)[full_trip_id],
                                            "\": full trip avoided because a least one trip inside is missing.\n",
                                            "[trip: ",
                                            private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                            "]")
                                    capture.output(current_trips <- object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = current_trips$extract_l1_element_value(element = "activities")) != 0) {
                                      capture.output(current_activities <- object_r6(class_name = "activities"),
                                                     file = "NUL")
                                      capture.output(current_activities$add(new_item = unlist(current_trips$extract_l1_element_value(element = "activities"))),
                                                     file = "NUL")
                                      capture.output(current_elementarycatches <- do.call(rbind,
                                                                                          current_activities$extract_l1_element_value(element = "elementarycatches")),
                                                     file = "NUL")
                                      if (length(current_elementarycatches) != 0) {
                                        for (trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                          current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                          if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                            for (activity_id in seq_len(length.out = length(current_trip$.__enclos_env__$private$activities))) {
                                              current_elementarycatches <- current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches
                                              if (length(current_elementarycatches) != 0) {
                                                current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches <- current_elementarycatches %>%
                                                  dplyr::mutate(weight_category_code_corrected=NA_character_,
                                                                catch_weight_category_code_corrected=NA_real_)
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  } else {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.3 on item \"",
                                        names(x = private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                    if (is.null(x = private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$rf2)) {
                                      stop(format(Sys.time(),
                                                  "%Y-%m-%d %H:%M:%S"),
                                           " - RF2 is null for the item \"",
                                           names(private$data_selected)[full_trip_id],
                                           "\".\n",
                                           "Check if the process 1.2 (raising factor level 2) was successfully applied.\n",
                                           "[trip: ",
                                           private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                           "]")
                                    } else {
                                      # first stage: conversion of all categories except for unknown (category 9) ----
                                      for (trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                        current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                        if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                          for (activity_id in seq_len(length.out = length(current_trip$.__enclos_env__$private$activities))) {
                                            current_elementarycatches_corrected <- NULL
                                            if (current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$activity_code %in% (if (referential_template == "observe") c(6,32) else c(0, 1, 2, 14))) {
                                              current_elementarycatches <- current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches
                                              if (length(current_elementarycatches) != 0) {
                                                current_elementarycatches <- current_elementarycatches %>%
                                                  dplyr::mutate(weight_category_code_corrected=NA_character_,
                                                                catch_weight_category_code_corrected=NA_real_)
                                                current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches <- current_elementarycatches
                                                ocean_activity <- current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$ocean_code
                                                school_type_activity <- current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$school_type_code

                                                for (elementarycatch_id in seq_len(length.out = length(current_elementarycatches$elementarycatch_id))) {
                                                  current_elementarycatch <- current_elementarycatches[elementarycatch_id,]
                                                  current_weight_category_code <- as.integer(x = ifelse(test = referential_template == "observe",
                                                                                                        yes = stringr::str_extract(string = current_elementarycatch$weight_category_code,
                                                                                                                                   pattern = "[:digit:]+$"),
                                                                                                        no = current_elementarycatch$weight_category_code))
                                                  if ( !is.na(x = current_weight_category_code)){
                                                    if (ocean_activity == 1) {
                                                      # for atlantic ocean
                                                      if (school_type_activity %in% (if (referential_template == "observe") c(2, 0) else c(2, 3))) {
                                                        # for free school and undetermined school
                                                        if (current_elementarycatch$species_fao_code %in% c("YFT",
                                                                                                            "BET",
                                                                                                            "ALB")) {
                                                          # for YFT, BET and ALB
                                                          if (current_weight_category_code %in% c(1, 2, 10)) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_1
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else if (current_weight_category_code == 4) {
                                                            current_elementarycatch[2,] <- current_elementarycatch[1,]
                                                            current_elementarycatch$weight_category_code_corrected[1] <- category_1
                                                            current_elementarycatch$catch_weight_category_code_corrected[1] <- current_elementarycatch$catch_weight_rf2[1] * 0.2
                                                            current_elementarycatch$weight_category_code_corrected[2] <- category_2
                                                            current_elementarycatch$catch_weight_category_code_corrected[2] <- current_elementarycatch$catch_weight_rf2[2] * 0.8
                                                          } else if (current_weight_category_code %in% c(3, 12)) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_2
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else if (current_weight_category_code == 6) {
                                                            current_elementarycatch[2,] <- current_elementarycatch[1,]
                                                            current_elementarycatch$weight_category_code_corrected[1] <- category_2
                                                            current_elementarycatch$catch_weight_category_code_corrected[1] <- current_elementarycatch$catch_weight_rf2[1] * 0.5
                                                            current_elementarycatch$weight_category_code_corrected[2] <- category_3
                                                            current_elementarycatch$catch_weight_category_code_corrected[2] <- current_elementarycatch$catch_weight_rf2[2] * 0.5
                                                          } else if (current_weight_category_code %in% c(5, 7, 8, 13, 14)) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_3
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else if (current_weight_category_code == 11) {
                                                            current_elementarycatch[2,] <- current_elementarycatch[1,]
                                                            current_elementarycatch$weight_category_code_corrected[1] <- category_2
                                                            current_elementarycatch$catch_weight_category_code_corrected[1]  <- current_elementarycatch$catch_weight_rf2[1]  * 0.1
                                                            current_elementarycatch$weight_category_code_corrected[2]  <- category_3
                                                            current_elementarycatch$catch_weight_category_code_corrected[2]  <- current_elementarycatch$catch_weight_rf2[2]  * 0.9
                                                          } else if (current_weight_category_code == 9) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_5
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else {
                                                            stop(format(Sys.time(),
                                                                        "%Y-%m-%d %H:%M:%S"),
                                                                 " - Logbook category ",
                                                                 current_weight_category_code,
                                                                 " not set in the algorithm.\n",
                                                                 "[trip: ",
                                                                 current_trip$.__enclos_env__$private$trip_id,
                                                                 ", activity: ",
                                                                 current_elementarycatch$activity_id,
                                                                 ", elementarycatch: ",
                                                                 current_elementarycatch$elementarycatch_id,
                                                                 "]")
                                                          }
                                                        } else if (current_elementarycatch$species_fao_code == "SKJ") {
                                                          if (current_weight_category_code != 9) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_1
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else {
                                                            current_elementarycatch$weight_category_code_corrected <- category_5
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          }
                                                        } else {
                                                          current_elementarycatch$weight_category_code_corrected <- category_5
                                                          current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                        }
                                                      } else {
                                                        # for floating object school
                                                        if (current_elementarycatch$species_fao_code %in% c("YFT", "BET", "ALB")) {
                                                          if (current_weight_category_code %in% c(1, 2, 10)) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_1
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else if (current_weight_category_code == 4) {
                                                            current_elementarycatch[2,] <- current_elementarycatch[1,]
                                                            current_elementarycatch$weight_category_code_corrected[1] <- category_1
                                                            current_elementarycatch$catch_weight_category_code_corrected[1] <- current_elementarycatch$catch_weight_rf2[1] * 0.2
                                                            current_elementarycatch$weight_category_code_corrected[2] <- category_4
                                                            current_elementarycatch$catch_weight_category_code_corrected[2] <- current_elementarycatch$catch_weight_rf2[2] * 0.8
                                                          } else if (current_weight_category_code %in% c(3, 12, 5, 7, 8, 13, 14, 6, 11)) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_4
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else if (current_weight_category_code == 9) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_5
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else {
                                                            stop(format(Sys.time(),
                                                                        "%Y-%m-%d %H:%M:%S"),
                                                                 " - Logbook category ",
                                                                 current_weight_category_code,
                                                                 " not set in the algorithm.\n",
                                                                 "[trip: ",
                                                                 current_trip$.__enclos_env__$private$trip_id,
                                                                 ", activity: ",
                                                                 current_elementarycatch$activity_id,
                                                                 ", elementarycatch: ",
                                                                 current_elementarycatch$elementarycatch_id,
                                                                 "]")
                                                          }
                                                        } else if (current_elementarycatch$species_fao_code == "SKJ") {
                                                          if (current_weight_category_code != 9) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_1
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else {
                                                            current_elementarycatch$weight_category_code_corrected <- category_5
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          }
                                                        } else {
                                                          current_elementarycatch$weight_category_code_corrected <- category_5
                                                          current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                        }
                                                      }
                                                    } else if (ocean_activity == 2) {
                                                      # for indian ocean
                                                      if (school_type_activity %in% (if (referential_template == "observe") c(2, 0) else c(2, 3))) {
                                                        # for free school and undetermined school
                                                        if (current_elementarycatch$species_fao_code %in% c("YFT", "BET", "ALB")) {
                                                          if (current_weight_category_code %in% c(1, 2, 10)) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_1
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else if (current_weight_category_code == 4) {
                                                            current_elementarycatch[2,] <- current_elementarycatch[1,]
                                                            current_elementarycatch$weight_category_code_corrected[1] <- category_1
                                                            current_elementarycatch$catch_weight_category_code_corrected[1] <- current_elementarycatch$catch_weight_rf2[1] * 0.2
                                                            current_elementarycatch$weight_category_code_corrected[2] <- category_4
                                                            current_elementarycatch$catch_weight_category_code_corrected[2] <- current_elementarycatch$catch_weight_rf2[2] * 0.8
                                                          } else if (current_weight_category_code %in% c(3, 12, 5, 7, 8, 13, 6, 11)) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_4
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else if (current_weight_category_code == 9) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_5
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else {
                                                            stop(format(Sys.time(),
                                                                        "%Y-%m-%d %H:%M:%S"),
                                                                 " - Logbook category ",
                                                                 current_weight_category_code,
                                                                 " not set in the algorithm.\n",
                                                                 "[trip: ",
                                                                 current_trip$.__enclos_env__$private$trip_id,
                                                                 ", activity: ",
                                                                 current_elementarycatch$activity_id,
                                                                 ", elementarycatch: ",
                                                                 current_elementarycatch$elementarycatch_id,
                                                                 "]")
                                                          }
                                                        } else if (current_elementarycatch$species_fao_code == "SKJ") {
                                                          if (current_weight_category_code != 9) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_1
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else {
                                                            current_elementarycatch$weight_category_code_corrected <- category_5
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          }
                                                        } else {
                                                          current_elementarycatch$weight_category_code_corrected <- category_5
                                                          current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                        }
                                                      } else {
                                                        # for floating object school
                                                        if (current_elementarycatch$species_fao_code %in% c("YFT", "BET", "ALB")) {
                                                          if (current_weight_category_code %in% c(1, 2, 10)) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_1
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else if (current_weight_category_code == 4) {
                                                            current_elementarycatch[2,] <- current_elementarycatch[1,]
                                                            current_elementarycatch$weight_category_code_corrected[1] <- category_1
                                                            current_elementarycatch$catch_weight_category_code_corrected[1] <- current_elementarycatch$catch_weight_rf2[1] * 0.2
                                                            current_elementarycatch$weight_category_code_corrected[2] <- category_4
                                                            current_elementarycatch$catch_weight_category_code_corrected[2] <- current_elementarycatch$catch_weight_rf2[2] * 0.8
                                                          } else if (current_weight_category_code %in% c(3, 12, 5, 7, 8, 13, 14, 6, 11)) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_4
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else if (current_weight_category_code == 9) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_5
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else {
                                                            stop(format(Sys.time(),
                                                                        "%Y-%m-%d %H:%M:%S"),
                                                                 " - Logbook category ",
                                                                 current_weight_category_code,
                                                                 " not set in the algorithm.\n",
                                                                 "[trip: ",
                                                                 current_trip$.__enclos_env__$private$trip_id,
                                                                 ", activity: ",
                                                                 current_elementarycatch$activity_id,
                                                                 ", elementarycatch: ",
                                                                 current_elementarycatch$elementarycatch_id,
                                                                 "]")
                                                          }
                                                        } else if (current_elementarycatch$species_fao_code == "SKJ") {
                                                          if (current_weight_category_code != 9) {
                                                            current_elementarycatch$weight_category_code_corrected <- category_1
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          } else {
                                                            current_elementarycatch$weight_category_code_corrected <- category_5
                                                            current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                          }
                                                        } else {
                                                          current_elementarycatch$weight_category_code_corrected <- category_5
                                                          current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                        }
                                                      }
                                                    } else {
                                                      stop(format(Sys.time(),
                                                                  "%Y-%m-%d %H:%M:%S"),
                                                           " - Algorithm not developed yet for the ocean number ",
                                                           ocean_activity,
                                                           ".\n",
                                                           "[trip: ",
                                                           current_trip$.__enclos_env__$private$trip_id,
                                                           ", activity: ",
                                                           current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$activity_id,
                                                           "]")
                                                    }
                                                  } else {
                                                    current_elementarycatch$weight_category_code_corrected <- category_5
                                                    current_elementarycatch$catch_weight_category_code_corrected <- current_elementarycatch$catch_weight_rf2
                                                  }
                                                  current_elementarycatches_corrected <- rbind(current_elementarycatches_corrected ,
                                                                                               current_elementarycatch)
                                                }
                                                private$data_selected[[full_trip_id]][[trip_id]]$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches <- current_elementarycatches_corrected
                                              }
                                            }
                                          }
                                        }
                                      }
                                      # second stage: conversion of category unknown (category 9) if possible ----
                                      for (trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                        capture.output(current_trip <- object_r6(class_name = "trips"),
                                                       file = "NUL")
                                        capture.output(current_trip$add(new_item = private$data_selected[[full_trip_id]][[trip_id]]),
                                                       file = "NUL")
                                        if (length(x=unlist(current_trip$extract_l1_element_value(element = "activities"))) != 0) {
                                          capture.output(current_activities <- object_r6(class_name = "activities"),
                                                         file = "NUL")
                                          capture.output(current_activities$add(new_item = unlist(current_trip$extract_l1_element_value(element = "activities"))),
                                                         file = "NUL")
                                          current_elementarycatches <- NULL
                                          if(length(current_activities$extract_l1_element_value(element = "elementarycatches")) !=0){
                                            current_elementarycatches <- do.call(rbind, current_activities$extract_l1_element_value(element = "elementarycatches"))
                                          }
                                        }
                                        if (length(current_elementarycatches) != 0) {
                                          category_9 <- FALSE
                                          names(x = category_9) <- 0
                                          other_category <- FALSE
                                          names(x = other_category) <- 0
                                          for (elementarycatch_id in seq_len(length.out = length(current_elementarycatches$elementarycatch_id))) {
                                            #find NA weight_category_code
                                            current_weight_category_code <- as.integer(x = ifelse(test = referential_template == "observe",
                                                                                                  yes = stringr::str_extract(string = current_elementarycatches$weight_category_code[elementarycatch_id],
                                                                                                                             pattern = "[:digit:]+$"),
                                                                                                  no = current_elementarycatches$weight_category_code[elementarycatch_id]))
                                            if ((is.na(x = current_weight_category_code)
                                                 | current_weight_category_code == 9)
                                                & current_elementarycatches$species_fao_code[elementarycatch_id] %in% c("YFT", "BET", "ALB", "SKJ")) {
                                              category_9 <- append(category_9,
                                                                   TRUE)
                                              names(x = category_9)[length(category_9)] <- elementarycatch_id
                                            } else {
                                              other_category <- append(other_category, TRUE)
                                              names(other_category)[length(other_category)] <- elementarycatch_id
                                            }
                                          }
                                          if (any(category_9 == TRUE)) {
                                            if (any(other_category == TRUE)) {
                                              category_9 <- category_9[-1]
                                              strate_category_9 <- vector(mode = "character")
                                              for (names_category_9_id in as.numeric(x = names(x = category_9))) {
                                                strate_category_9 <- append(strate_category_9,
                                                                            paste(current_elementarycatches$school_type_code[names_category_9_id],
                                                                                  current_elementarycatches$ocean_code[names_category_9_id],
                                                                                  current_elementarycatches$species_fao_code[names_category_9_id],
                                                                                  sep = "_"))
                                              }
                                              other_category <- other_category[-1]
                                              for (strate_category_9_id in unique(strate_category_9)) {
                                                school_type <- unlist(strsplit(x = strate_category_9_id,
                                                                               split = "_"))[1]
                                                ocean <- unlist(strsplit(x = strate_category_9_id,
                                                                         split = "_"))[2]
                                                species <- unlist(strsplit(x = strate_category_9_id,
                                                                           split = "_"))[3]
                                                current_other_category <- current_elementarycatches[as.numeric(x = names(x = other_category)),] %>%
                                                  dplyr::filter(school_type_code == school_type,
                                                                ocean_code == ocean,
                                                                species_fao_code == species)
                                                if (nrow(current_other_category) != 0) {
                                                  current_category_9 <- current_elementarycatches[as.numeric(x = names(x = category_9)),] %>%
                                                    dplyr::filter(school_type_code == school_type,
                                                                  ocean_code == ocean,
                                                                  species_fao_code == species)
                                                  total_catch_weight_category_code_corrected <- sum(current_other_category$catch_weight_category_code_corrected)
                                                  other_category_names <- unique(current_other_category$weight_category_code_corrected)
                                                  proportion <- vector(mode = "numeric")
                                                  for (other_category_names_id in other_category_names) {
                                                    weight_category_corrected <- sum(sapply(X = seq_len(length.out = nrow(x = current_other_category)),
                                                                                            FUN = function(i) {
                                                                                              if (current_other_category$weight_category_code_corrected[i] == other_category_names_id) {
                                                                                                current_other_category$catch_weight_category_code_corrected[i]
                                                                                              } else {
                                                                                                0
                                                                                              }
                                                                                            }))
                                                    proportion <- append(proportion,
                                                                         weight_category_corrected / total_catch_weight_category_code_corrected)
                                                    names(x = proportion)[length(x = proportion)] <- other_category_names_id
                                                  }
                                                  for (category_9_id in seq_len(length.out=nrow(x = current_category_9))) {
                                                    for (proportion_id in seq_len(length.out = length(x = proportion))) {
                                                      if (proportion_id == length(x = proportion)) {
                                                        current_category_9$weight_category_code_corrected[category_9_id] <- names(x = proportion)[proportion_id]
                                                        current_category_9$catch_weight_category_code_corrected[category_9_id] <- current_category_9$catch_weight_rf2[category_9_id] * as.numeric(x = proportion[proportion_id])
                                                      } else {
                                                        current_category_9 <- dplyr::add_row(.data=current_category_9,
                                                                                             current_category_9[category_9_id,])

                                                        current_category_9$weight_category_code_corrected[category_9_id+1] <- names(x = proportion)[proportion_id]
                                                        current_category_9$catch_weight_category_code_corrected[category_9_id+1] <- current_category_9$catch_weight_rf2[category_9_id+1] * as.numeric(x = proportion[proportion_id])
                                                      }
                                                      for (activity_id in seq_len(length.out = length(private$data_selected[[full_trip_id]][[trip_id]]$.__enclos_env__$private$activities))) {
                                                        if (private$data_selected[[full_trip_id]][[trip_id]]$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$activity_id %in% current_category_9$activity_id[category_9_id]) {
                                                          private$data_selected[[full_trip_id]][[trip_id]]$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches <- rbind(private$data_selected[[full_trip_id]][[trip_id]]$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$elementarycatches %>%
                                                                                                                                                                                                                  dplyr::filter(!(elementarycatch_id %in% current_category_9$elementarycatch_id[category_9_id])),
                                                                                                                                                                                                                current_category_9[category_9_id,])
                                                          # %>%
                                                          #   dplyr::filter(activity_id==private$data_selected[[full_trip_id]][[trip_id]]$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$activity_id))
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Process 1.3 successfull on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                  }
                                }
                                # 9.3 - Outputs extraction ----
                                # outputs manipulation
                                if (! is.null(x = global_output_path)) {
                                  full_trips_selected <- private$data_selected
                                  capture.output(trips_selected <- object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                                 file = "NUL")
                                  capture.output(activities_selected <- object_r6(class_name = "activities"),
                                                 file = "NUL")
                                  capture.output(activities_selected$add(new_item = unlist(x = trips_selected$extract_l1_element_value(element = "activities"))),
                                                 file = "NUL")
                                  capture.output(elementarycatches_selected <- do.call(rbind, activities_selected$extract_l1_element_value(element = "elementarycatches")),
                                                 file = "NUL")
                                  outputs_process_1_3_trips <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                         FUN = function(full_trip_id) {
                                                                                                           if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                             return(rep(x = full_trip_id,
                                                                                                                        length(x = full_trips_selected[[full_trip_id]])))
                                                                                                           } else {
                                                                                                             return(full_trip_id)
                                                                                                           }
                                                                                                         })),
                                                                          "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                           FUN = function(full_trip_id) {
                                                                                                             if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                               return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                          length(x = full_trips_selected[[full_trip_id]])))
                                                                                                             } else {
                                                                                                               return(names(x = full_trips_selected[full_trip_id]))
                                                                                                             }
                                                                                                           })),
                                                                          "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                          "trip_end_date" = do.call("c",
                                                                                                    trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                          "year_trip_end_date" = sapply(do.call("c",
                                                                                                                trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                        lubridate::year),
                                                                          "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                          "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))))
                                  outputs_process_1_3_activities <- data.frame("trip_id" = unlist(x = activities_selected$extract_l1_element_value(element = "trip_id")),
                                                                               "activity_id" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_id")),
                                                                               "activity_latitude" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_latitude")),
                                                                               "activity_longitude" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_longitude")),
                                                                               "activity_date" = do.call("c",
                                                                                                         activities_selected$extract_l1_element_value(element = "activity_date")),
                                                                               "ocean_code" = unlist(x = activities_selected$extract_l1_element_value(element = "ocean_code")),
                                                                               "school_type_code" = unlist(x = activities_selected$extract_l1_element_value(element = "school_type_code")))
                                  outputs_process_1_3_elementarycatches <- data.frame("activity_id" = elementarycatches_selected$activity_id,
                                                                                      "elementarycatch_id" = elementarycatches_selected$elementarycatch_id,
                                                                                      "species_fao_code" = elementarycatches_selected$species_fao_code,
                                                                                      "weight_category_code" = elementarycatches_selected$weight_category_code,
                                                                                      "weight_category_label" = elementarycatches_selected$weight_category_label,
                                                                                      "catch_weight_rf2" = elementarycatches_selected$catch_weight_rf2,
                                                                                      "weight_category_code_corrected" = elementarycatches_selected$weight_category_code_corrected,
                                                                                      "catch_weight_category_code_corrected" = elementarycatches_selected$catch_weight_category_code_corrected,
                                                                                      "catch_count" = elementarycatches_selected$catch_count)
                                  outputs_process_1_3 <- outputs_process_1_3_elementarycatches %>%
                                    dplyr::left_join(outputs_process_1_3_activities,
                                                     by = "activity_id") %>%
                                    dplyr::left_join(outputs_process_1_3_trips,
                                                     by = "trip_id") %>%
                                    dplyr::relocate(full_trip_id,
                                                    full_trip_name,
                                                    trip_id,
                                                    trip_end_date,
                                                    year_trip_end_date,
                                                    vessel_code,
                                                    vessel_type_code,
                                                    activity_id,
                                                    activity_latitude,
                                                    activity_longitude,
                                                    activity_date,
                                                    ocean_code,
                                                    school_type_code,
                                                    elementarycatch_id)
                                  # extraction
                                  if (output_format == "us") {
                                    outputs_dec <- "."
                                    outputs_sep <- ","
                                  } else if (output_format == "eu") {
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  } else {
                                    warning(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Wrong outputs format define, European format will be applied.")
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  }
                                  write.table(x = outputs_process_1_3,
                                              file = file.path(global_output_path,
                                                               "level1",
                                                               "data",
                                                               "process_1_3.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Outputs extracted in the following directory:\n",
                                      file.path(global_output_path,
                                                "level1",
                                                "data"), "\n")
                                }
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - End process 1.3: logbook weight categories conversion.\n")
                              }
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 10 - Process 1.4: set_count ----
                            #' @description Process for positive sets count.
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction egual TRUE. Path of the global outputs directory. The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            #' @param referential_template Object of class \code{\link[base]{character}} expected. By default "observe". Referential template selected (for example regarding the activity_code). You can switch to "avdth".
                            set_count = function(global_output_path = NULL,
                                                 output_format = "eu",
                                                 referential_template = "observe") {
                              # 10.1 - Arguments verification ----
                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              codama::r_type_checking(r_object = referential_template,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("observe",
                                                                        "avdth"))
                              # 10.2 - Global process ----
                              if (is.null(x = private$data_selected)) {
                                message(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Empty data selected in the R6 object.\n",
                                        " - Process 1.4 (set count) cancelled.")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 1.4: set count.\n")
                                  }
                                  if (names(x = private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    message(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Warning on item \"",
                                            names(x = private$data_selected)[full_trip_id],
                                            "\": full trip avoided because a least one trip inside is missing.\n",
                                            "[trip: ",
                                            private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                            "]")
                                    capture.output(current_trips <- object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = current_trips$extract_l1_element_value(element = "activities")) != 0) {
                                      capture.output(current_activities <- object_r6(class_name = "activities"),
                                                     file = "NUL")
                                      capture.output(current_activities$add(new_item = unlist(current_trips$extract_l1_element_value(element = "activities"))),
                                                     file = "NUL")
                                      current_activities$modification_l1(modification = "$path$positive_set_count <- NA_real_")
                                    }
                                  } else {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.4 on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                    for (trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        for (activity_id in seq_len(length.out = length(current_trip$.__enclos_env__$private$activities))) {
                                          current_activity <- current_trip$.__enclos_env__$private$activities[[activity_id]]
                                          if (current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$activity_code %in%
                                              (if (referential_template == "observe") c(6,32) else c(0, 1, 2, 14))) {
                                            capture.output(current_elementarycatches <- current_activity$.__enclos_env__$private$elementarycatches,
                                                           file = "NUL")
                                            if (length(current_elementarycatches) != 0) {
                                              if (any(is.null(x = current_elementarycatches$catch_weight_category_code_corrected))){
                                                stop(format(Sys.time(),
                                                            "%Y-%m-%d %H:%M:%S"),
                                                     " - Error: argument \"catch_weight_category_code_corrected\" is null.\n",
                                                     "Check if the process 1.3 (logbook weight categories conversion) has already been launched.",
                                                     "\n[trip: ",
                                                     current_activity$.__enclos_env__$private$trip_id,
                                                     ", activity: ",
                                                     current_activity$.__enclos_env__$private$activity_id,
                                                     "]")
                                              }
                                              else{
                                                catch_weight_category_corrected <- sum(current_elementarycatches$catch_weight_category_code_corrected,
                                                                                       na.rm=TRUE)
                                              }

                                              if (catch_weight_category_corrected == 0) {
                                                if (any(is.na(x = current_elementarycatches$catch_weight_category_code_corrected)
                                                        && is.na(x = current_elementarycatches$catch_count))) {
                                                  stop(format(Sys.time(),
                                                              "%Y-%m-%d %H:%M:%S"),
                                                       " - Error: arguments \"catch_weight_category_code_corrected\" and \"catch_count\" are equal to \"NA\".\n",
                                                       "Check the data.",
                                                       "\n[trip: ",
                                                       current_activity$.__enclos_env__$private$trip_id,
                                                       ", activity: ",
                                                       current_activity$.__enclos_env__$private$activity_id,
                                                       "]")
                                                } else {
                                                  catch_count <- sum(current_elementarycatches$catch_count, na.rm=TRUE)
                                                }
                                                current_activity$.__enclos_env__$private$positive_set_count <- ifelse(catch_count==0, 0, current_activity$.__enclos_env__$private$set_count)

                                              } else {
                                                current_activity$.__enclos_env__$private$positive_set_count <- current_activity$.__enclos_env__$private$set_count
                                              }

                                            } else {
                                              current_activity$.__enclos_env__$private$positive_set_count <- 0
                                            }
                                          } else {
                                            current_activity$.__enclos_env__$private$positive_set_count <- NA_real_
                                          }
                                        }
                                      }
                                    }
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Process 1.4 successfull on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                  }
                                }
                                # 10.3 - Outputs extraction ----
                                # outputs manipulation
                                if (! is.null(x = global_output_path)) {
                                  full_trips_selected <- private$data_selected
                                  capture.output(trips_selected <- object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                                 file = "NUL")
                                  capture.output(activities_selected <- object_r6(class_name = "activities"),
                                                 file = "NUL")
                                  capture.output(activities_selected$add(new_item = unlist(x = trips_selected$extract_l1_element_value(element = "activities"))),
                                                 file = "NUL")
                                  outputs_process_1_4_trips <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                         FUN = function(full_trip_id) {
                                                                                                           if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                             return(rep(x = full_trip_id,
                                                                                                                        length(x = full_trips_selected[[full_trip_id]])))
                                                                                                           } else {
                                                                                                             return(full_trip_id)
                                                                                                           }
                                                                                                         })),
                                                                          "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                           FUN = function(full_trip_id) {
                                                                                                             if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                               return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                          length(x = full_trips_selected[[full_trip_id]])))
                                                                                                             } else {
                                                                                                               return(names(x = full_trips_selected[full_trip_id]))
                                                                                                             }
                                                                                                           })),
                                                                          "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                          "trip_end_date" = do.call("c",
                                                                                                    trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                          "year_trip_end_date" = sapply(do.call("c",
                                                                                                                trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                        lubridate::year),
                                                                          "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                          "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))))
                                  outputs_process_1_4_activities <- data.frame("trip_id" = unlist(x = activities_selected$extract_l1_element_value(element = "trip_id")),
                                                                               "activity_id" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_id")),
                                                                               "activity_latitude" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_latitude")),
                                                                               "activity_longitude" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_longitude")),
                                                                               "activity_date" = do.call("c",
                                                                                                         activities_selected$extract_l1_element_value(element = "activity_date")),
                                                                               "activity_code" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_code")),
                                                                               "ocean_code" = unlist(x = activities_selected$extract_l1_element_value(element = "ocean_code")),
                                                                               "school_type_code" = unlist(x = activities_selected$extract_l1_element_value(element = "school_type_code")),
                                                                               "positive_set_count" = unlist(x = activities_selected$extract_l1_element_value(element = "positive_set_count")))
                                  outputs_process_1_4 <- outputs_process_1_4_activities %>%
                                    dplyr::left_join(outputs_process_1_4_trips,
                                                     by = "trip_id") %>%
                                    dplyr::relocate(full_trip_id,
                                                    full_trip_name,
                                                    trip_id,
                                                    trip_end_date,
                                                    year_trip_end_date,
                                                    vessel_code,
                                                    vessel_type_code,
                                                    activity_id,
                                                    activity_latitude,
                                                    activity_longitude,
                                                    activity_date,
                                                    activity_code,
                                                    ocean_code,
                                                    school_type_code,
                                                    positive_set_count)
                                  # extraction
                                  if (output_format == "us") {
                                    outputs_dec <- "."
                                    outputs_sep <- ","
                                  } else if (output_format == "eu") {
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  } else {
                                    warning(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Wrong outputs format define, European format will be applied.")
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  }
                                  write.table(x = outputs_process_1_4,
                                              file = file.path(global_output_path,
                                                               "level1",
                                                               "data",
                                                               "process_1_4.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Outputs extracted in the following directory:\n",
                                      file.path(global_output_path,
                                                "level1",
                                                "data"), "\n")
                                }
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - End process 1.4: set count.\n")
                              }
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 11 - Process 1.5: set_duration ----
                            #' @description Process for set duration calculation (in hours).
                            #' @param set_duration_ref Object of type \code{\link[base]{data.frame}} or \code{\link[tibble]{tbl_df}} expected. Data and parameters for set duration calculation (by year, country, ocean and school type).
                            #' Duration in minutes in the reference table, converted into hours in output for subsequent processing).
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction equal TRUE. Path of the global outputs directory. The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            #' @param referential_template Object of class \code{\link[base]{character}} expected. By default "observe". Referential template selected (for example regarding the activity_code). You can switch to "avdth".
                            #' @param activity_code_ref Object of type \code{\link[base]{data.frame}} or \code{\link[tibble]{tbl_df}} expected. Reference table with the activity codes to be taken into account for the allocation of sea and/or fishing time,
                            #'  and/or searching time and/or set duration.
                            set_duration = function(set_duration_ref,
                                                    activity_code_ref,
                                                    global_output_path = NULL,
                                                    output_format = "eu",
                                                    referential_template = "observe") {
                              # 11.1 - Arguments verification ----
                              if (! paste0(class(x = set_duration_ref),
                                           collapse = "_") %in% c("data.frame",
                                                                  "tbl_df_tbl_data.frame")
                                  || ncol(x = set_duration_ref) != 9
                                  || nrow(x = set_duration_ref) <1) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Invalid \"set_duration_ref\" argument, class \"data.frame\" or \"tibble\" with 9 columns and at least 1 row expected.")
                              }
                              if (! paste0(class(x = activity_code_ref),
                                           collapse = "_") %in% c("data.frame",
                                                                  "tbl_df_tbl_data.frame",
                                                                  "spec_tbl_df_tbl_df_tbl_data.frame")
                                  || ncol(x = activity_code_ref) !=14
                                  || nrow(x = activity_code_ref) <1) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Invalid \"activity_code_ref\" argument, class \"data.frame\" or \"tibble\" with 14 columns and at least 1 row expected.")
                              }
                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              codama::r_type_checking(r_object = referential_template,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("observe",
                                                                        "avdth"))
                              # 11.2 - Global process ----
                              if (is.null(x = private$data_selected)) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Empty data selected in the R6 object. Process 1.5 (set duration calculation) cancelled.")
                              } else {
                                if (referential_template == "observe") {
                                  set_duration_ref <- dplyr::mutate(.data = set_duration_ref,
                                                                    school_type_code = school_type_code_observe)
                                  activity_code_ref <- dplyr::mutate(.data = activity_code_ref,
                                                                     activity_code = activity_code_observe,
                                                                     activity_label = activity_label_observe,
                                                                     set_success_status = set_success_status_code_observe,
                                                                     objectoperation_code = objectoperation_code_observe,
                                                                     objectoperation_label = objectoperation_label_observe)

                                } else {
                                  set_duration_ref <- dplyr::mutate(.data = set_duration_ref,
                                                                    # Conversion from school_type_code_avdth to school_type_code_observe
                                                                    # Done by object_model_data$activities_object_creation()
                                                                    school_type_code = school_type_code_observe)
                                  activity_code_ref <- dplyr::mutate(.data = activity_code_ref,
                                                                     activity_code = activity_code_avdth,
                                                                     activity_label = activity_label_avdth)
                                }
                                set_duration_ref <- dplyr::select(.data = set_duration_ref,
                                                                  -school_type_code_avdth,
                                                                  -school_type_code_observe)
                                activity_code_ref <- dplyr::select(.data = activity_code_ref,
                                                                   -activity_code_avdth,
                                                                   -activity_label_avdth,
                                                                   -activity_code_observe,
                                                                   -activity_label_observe,
                                                                   -set_success_status_code_observe,
                                                                   -objectoperation_code_observe,
                                                                   -objectoperation_label_observe,
                                                                   -schooltype_code_observe,
                                                                   -schooltype_label_observe,
                                                                   -status_active_observe,
                                                                   -comment)
                                catch_activity_codes <- unique(activity_code_ref %>%
                                                                 dplyr::filter(set_duration==1) %>%
                                                                 dplyr::pull(activity_code))

                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 1.5: set duration calculation.\n")
                                  }
                                  if (names(private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    message(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Warning on item \"",
                                            names(x = private$data_selected)[full_trip_id],
                                            "\": full trip avoided because a least one trip inside is missing.\n",
                                            "[trip: ",
                                            private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                            "]")
                                    capture.output(current_trips <- object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = current_trips$extract_l1_element_value(element = "activities")) != 0) {
                                      capture.output(current_activities <- object_r6(class_name = "activities"),
                                                     file = "NUL")
                                      capture.output(current_activities$add(new_item = unlist(current_trips$extract_l1_element_value(element = "activities"))),
                                                     file = "NUL")
                                      current_activities$modification_l1(modification = "$path$set_duration <- NA_integer_")
                                    }
                                  } else {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.5 on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                    for (trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        capture.output(current_activities <- object_r6(class_name = "activities"),
                                                       file = "NUL")
                                        capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                                                       file = "NUL")
                                        for (activity_id in seq_len(length.out = current_activities$count())) {
                                          current_activity <- current_activities$extract(id = activity_id)[[1]]
                                          # for activity declared as Fishing (6, 32) in observe or
                                          # null set (0), positive set (1), unknown set (2) or pocket capsizing (14) in AVDTH
                                          if (current_trip$.__enclos_env__$private$activities[[activity_id]]$.__enclos_env__$private$activity_code %in% catch_activity_codes){
                                            current_set_duration_ref <- set_duration_ref %>%
                                              dplyr::filter(year == lubridate::year(current_activity$.__enclos_env__$private$activity_date),
                                                            ocean_code == current_activity$.__enclos_env__$private$ocean_code,
                                                            school_type_code == current_activity$.__enclos_env__$private$school_type_code,
                                                            flag_code_iso_3 == current_trip$.__enclos_env__$private$flag_code)
                                            if (dim(current_set_duration_ref)[1] != 1) {
                                              stop(format(Sys.time(),
                                                          "%Y-%m-%d %H:%M:%S"),
                                                   " - Error: invalid \"set_duration_ref\" argument.\n",
                                                   "No correspondance with activity parameters (ocean and/or school type) number \"",
                                                   activity_id,
                                                   "\".\n",
                                                   "[trip: ",
                                                   current_trip$.__enclos_env__$private$trip_id,
                                                   ", activity: ",
                                                   current_activity$.__enclos_env__$private$activity_id,
                                                   "]")
                                            } else {
                                              capture.output(current_elementarycatches <- current_activity$.__enclos_env__$private$elementarycatches,
                                                             file = "NUL")
                                              if (length(current_elementarycatches) != 0) {
                                                if (any(is.null(x = current_elementarycatches$catch_weight_category_code_corrected))){
                                                  stop(format(Sys.time(),
                                                              "%Y-%m-%d %H:%M:%S"),
                                                       " - Error: argument \"catch_weight_category_code_corrected\" is null.\n",
                                                       "Check if the process 1.3 (logbook weight categories conversion) has already been launched.",
                                                       "\n[trip: ",
                                                       current_activity$.__enclos_env__$private$trip_id,
                                                       ", activity: ",
                                                       current_activity$.__enclos_env__$private$activity_id,
                                                       "]")
                                                }
                                                else{
                                                  catch_weight_category_corrected <- sum(current_elementarycatches$catch_weight_category_code_corrected,
                                                                                         na.rm=TRUE)
                                                }

                                                if (catch_weight_category_corrected == 0) {
                                                  if (any(is.na(x = current_elementarycatches$catch_weight_category_code_corrected)
                                                          && is.na(x = current_elementarycatches$catch_count))) {
                                                    stop(format(Sys.time(),
                                                                "%Y-%m-%d %H:%M:%S"),
                                                         " - Error: arguments \"catch_weight_category_code_corrected\" and \"catch_count\" are equal to \"NA\".\n",
                                                         "Check the data.",
                                                         "\n[trip: ",
                                                         current_activity$.__enclos_env__$private$trip_id,
                                                         ", activity: ",
                                                         current_activity$.__enclos_env__$private$activity_id,
                                                         "]")
                                                  } else {
                                                    catch_count <- sum(current_elementarycatches$catch_count, na.rm=TRUE)
                                                  }
                                                  if(catch_count == 0){
                                                    current_activity$.__enclos_env__$private$set_duration  <- 0
                                                  } else {
                                                    current_activity$.__enclos_env__$private$set_duration <-  round((1/60)*current_set_duration_ref$null_set_value,
                                                                                                                    digits=4)
                                                  }
                                                } else{
                                                  parameter_a <- current_set_duration_ref$parameter_a
                                                  parameter_b <- current_set_duration_ref$parameter_b
                                                  current_activity$.__enclos_env__$private$set_duration <- round((1/60)*(parameter_a * catch_weight_category_corrected + parameter_b),
                                                                                                                 digits=4)
                                                }

                                              } else {
                                                if ((referential_template == "observe"
                                                     && (current_activity$.__enclos_env__$private$activity_code == 6
                                                         & current_activity$.__enclos_env__$private$set_success_status_code == 1))
                                                    | (referential_template == "avdth"
                                                       && current_activity$.__enclos_env__$private$activity_code == 1)) {
                                                  warning(format(Sys.time(),
                                                                 "%Y-%m-%d %H:%M:%S"),
                                                          " - Set declared as successful fishing operation but without elementary catch associated.",
                                                          " Set duration define as NA.",
                                                          "\n[trip: ",
                                                          current_trip$.__enclos_env__$private$trip_id,
                                                          ", activity: ",
                                                          current_activity$.__enclos_env__$private$activity_id,
                                                          "]")
                                                  current_activity$.__enclos_env__$private$set_duration <- NA_integer_
                                                } else {
                                                  current_activity$.__enclos_env__$private$set_duration <- round((1/60)*current_set_duration_ref$null_set_value,
                                                                                                                 digits=4)
                                                }
                                              }
                                            }
                                          } else {
                                            # current_activity$.__enclos_env__$private$set_duration <- NA_integer_
                                            current_activity$.__enclos_env__$private$set_duration <- 0.0
                                          }
                                        }
                                      }
                                    }
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Process 1.5 successfull on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                  }
                                }
                                # 11.3 - Outputs extraction ----
                                # outputs manipulation
                                if (! is.null(x = global_output_path)) {
                                  full_trips_selected <- private$data_selected
                                  capture.output(trips_selected <- object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                                 file = "NUL")
                                  capture.output(activities_selected <- object_r6(class_name = "activities"),
                                                 file = "NUL")
                                  capture.output(activities_selected$add(new_item = unlist(x = trips_selected$extract_l1_element_value(element = "activities"))),
                                                 file = "NUL")
                                  outputs_process_1_5_trips <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                         FUN = function(full_trip_id) {
                                                                                                           if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                             return(rep(x = full_trip_id,
                                                                                                                        length(x = full_trips_selected[[full_trip_id]])))
                                                                                                           } else {
                                                                                                             return(full_trip_id)
                                                                                                           }
                                                                                                         })),
                                                                          "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                           FUN = function(full_trip_id) {
                                                                                                             if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                               return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                          length(x = full_trips_selected[[full_trip_id]])))
                                                                                                             } else {
                                                                                                               return(names(x = full_trips_selected[full_trip_id]))
                                                                                                             }
                                                                                                           })),
                                                                          "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                          "trip_end_date" = do.call("c",
                                                                                                    trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                          "year_trip_end_date" = sapply(do.call("c",
                                                                                                                trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                        lubridate::year),
                                                                          "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                          "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))))
                                  outputs_process_1_5_activities <- data.frame("trip_id" = unlist(x = activities_selected$extract_l1_element_value(element = "trip_id")),
                                                                               "activity_id" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_id")),
                                                                               "activity_latitude" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_latitude")),
                                                                               "activity_longitude" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_longitude")),
                                                                               "activity_date" = do.call("c",
                                                                                                         activities_selected$extract_l1_element_value(element = "activity_date")),
                                                                               "activity_code" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_code")),
                                                                               "ocean_code" = unlist(x = activities_selected$extract_l1_element_value(element = "ocean_code")),
                                                                               "school_type_code" = unlist(x = activities_selected$extract_l1_element_value(element = "school_type_code")),
                                                                               "set_duration" = unlist(x = activities_selected$extract_l1_element_value(element = "set_duration")))
                                  outputs_process_1_5 <- outputs_process_1_5_activities %>%
                                    dplyr::left_join(outputs_process_1_5_trips,
                                                     by = "trip_id") %>%
                                    dplyr::relocate(full_trip_id,
                                                    full_trip_name,
                                                    trip_id,
                                                    trip_end_date,
                                                    year_trip_end_date,
                                                    vessel_code,
                                                    vessel_type_code,
                                                    activity_id,
                                                    activity_latitude,
                                                    activity_longitude,
                                                    activity_date,
                                                    activity_code,
                                                    ocean_code,
                                                    school_type_code,
                                                    set_duration)
                                  # extraction
                                  if (output_format == "us") {
                                    outputs_dec <- "."
                                    outputs_sep <- ","
                                  } else if (output_format == "eu") {
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  } else {
                                    warning(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Wrong outputs format define, European format will be applied.")
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  }
                                  write.table(x = outputs_process_1_5,
                                              file = file.path(global_output_path,
                                                               "level1",
                                                               "data",
                                                               "process_1_5.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Outputs extracted in the following directory:\n",
                                      file.path(global_output_path,
                                                "level1",
                                                "data"), "\n")
                                }
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - End process 1.5: set duration calculation.\n")
                              }
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 12 - Process 1.6: time at sea ----
                            #' @description Process for time at sea calculation (in hours).
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction egual TRUE. Path of the global outputs directory. The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            #' @param referential_template Object of class \code{\link[base]{character}} expected. By default "observe". Referential template selected (for example regarding the activity_code). You can switch to "avdth".
                            #' @param activity_code_ref Object of type \code{\link[base]{data.frame}} or \code{\link[tibble]{tbl_df}} expected. Reference table with the activity codes to be taken into account for the allocation of sea and/or fishing time,
                            #'  and/or searching time and/or set duration.
                            time_at_sea = function(global_output_path = NULL,
                                                   output_format = "eu",
                                                   activity_code_ref,
                                                   referential_template = "observe") {
                              # 12.1 - Arguments verification ----
                              if (! paste0(class(x = activity_code_ref),
                                           collapse = "_") %in% c("data.frame",
                                                                  "tbl_df_tbl_data.frame",
                                                                  "spec_tbl_df_tbl_df_tbl_data.frame")
                                  || ncol(x = activity_code_ref) !=14
                                  || nrow(x = activity_code_ref) <1) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Invalid \"activity_code_ref\" argument, class \"data.frame\" or \"tibble\" with 14 columns and at least 1 row expected.")
                              }
                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              # 12.2 - Global process ----
                              if (is.null(x = private$data_selected)) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Empty data selected in the R6 object. Process 1.6 (time at sea calculation) cancelled.")
                              } else {
                                if (referential_template == "observe") {
                                  activity_code_ref <- dplyr::mutate(.data = activity_code_ref,
                                                                     activity_code = activity_code_observe,
                                                                     activity_label = activity_label_observe,
                                                                     set_success_status = set_success_status_code_observe,
                                                                     objectoperation_code = objectoperation_code_observe,
                                                                     objectoperation_label = objectoperation_label_observe,
                                                                     code=paste(activity_code, objectoperation_code, sep="_"))
                                } else {
                                  activity_code_ref <- dplyr::mutate(.data = activity_code_ref,
                                                                     activity_code = activity_code_avdth,
                                                                     activity_label = activity_label_avdth,
                                                                     objectoperation_code = NA,
                                                                     objectoperation_label = NA,
                                                                     code=paste(activity_code, objectoperation_code,
                                                                                sep="_"))
                                }
                                activity_code_ref <- dplyr::select(.data = activity_code_ref,
                                                                   -activity_code_avdth,
                                                                   -activity_label_avdth,
                                                                   -activity_code_observe,
                                                                   -activity_label_observe,
                                                                   -set_success_status_code_observe,
                                                                   -objectoperation_code_observe,
                                                                   -objectoperation_label_observe,
                                                                   -schooltype_code_observe,
                                                                   -schooltype_label_observe,
                                                                   -status_active_observe,
                                                                   -comment)
                                # activity_objectoperation codes to take into account for time at sea allocation
                                activity_objectoperation_codes <- unique(activity_code_ref %>%
                                                                           dplyr::filter(time_at_sea==1) %>%
                                                                           dplyr::pull(code))
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 1.6: time at sea calculation.\n")
                                  }
                                  if (names(x = private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    message(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Warning on item \"",
                                            names(x = private$data_selected)[full_trip_id],
                                            "\": full trip avoided because a least one trip inside is missing.\n",
                                            "[trip: ",
                                            private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                            "]")
                                    capture.output(current_trips <- object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    current_trips$modification_l1(modification = "$path$time_at_sea <- NA_real_")
                                  } else {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.6 on item \"",
                                        names(x = private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                    for (trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                      departure_date <- current_trip$.__enclos_env__$private$departure_date
                                      trip_end_date <- current_trip$.__enclos_env__$private$trip_end_date
                                      time_departure_date <- lubridate::hms(format(x = departure_date,
                                                                                   format = "%H:%M:%S"))
                                      time_trip_end_date <- lubridate::hms(format(x = trip_end_date,
                                                                                  format = "%H:%M:%S"))
                                      if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                        capture.output(current_activities <- object_r6(class_name = "activities"),
                                                       file = "NUL")
                                        capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                                                       file = "NUL")
                                        activities_dates <- current_activities$extract_l1_element_value(element = "activity_date")
                                        activities_dates <- unique(do.call(what = "c",
                                                                           args = activities_dates))
                                        activities_dates <- sort(x = activities_dates)
                                        time_at_sea <- 0
                                        # Activities to be taken into account in time at sea allocation
                                        for (activities_dates_id in seq_len(length.out = length(activities_dates))) {
                                          capture.output(current_activities_date <- object_r6(class_name = "activities"),
                                                         file = "NUL")
                                          capture.output(current_activities_date$add(new_item = current_activities$filter_l1(filter = paste0("$path$activity_date == lubridate::parse_date_time(x = \"",
                                                                                                                                             activities_dates[activities_dates_id],
                                                                                                                                             "\",",
                                                                                                                                             "orders = c(\"ymd_HMS\", \"ymd\"), tz = \"UTC\", quiet = TRUE)"))),
                                                         file = "NUL")
                                          current_activities_code <- unlist(current_activities_date$extract_l1_element_value(element = "activity_code"))
                                          current_objectoperation_code <- unlist(current_activities_date$extract_l1_element_value(element = "objectoperation_code"))
                                          current_code <- paste(current_activities_code, current_objectoperation_code, sep='_')
                                          if (referential_template == "observe") {
                                            current_activities_date_time_at_sea_declared <-  unique(x = unlist(x = current_activities_date$extract_l1_element_value(element="time_at_sea")))
                                          } else {
                                            current_activities_date_time_at_sea_declared <- sum(unlist(x = current_activities_date$extract_l1_element_value(element = "time_at_sea")))
                                          }
                                          capture.output(current_activities_date_sea <- object_r6(class_name = "activities"),
                                                         file = "NUL")
                                          if(any(unique(x=current_code) %in% activity_objectoperation_codes)){
                                            capture.output(current_activities_date_sea$add(new_item = current_activities_date$filter_l1(filter = paste0("paste($path$activity_code,
                                                                                                                                                  $path$objectoperation_code,
                                                                                                                                                  sep='_') %in% c(\"",
                                                                                                                                                        paste(activity_objectoperation_codes,
                                                                                                                                                              collapse = "\", \""),"\")"))),
                                                           file = "NUL")
                                            current_activities_date_time_at_sea <- round(current_activities_date_time_at_sea_declared/current_activities_date_sea$count(),
                                                                                         digits=4)
                                          } else{
                                            if(current_activities_date_time_at_sea_declared!=0){
                                              new_activity <- current_activities_date$.__enclos_env__$private$data[[1]]$clone()
                                              new_activity$.__enclos_env__$private$objectoperation_code <- NA
                                              new_activity$.__enclos_env__$private$objectoperation_label <- NA
                                              new_activity$.__enclos_env__$private$objectoperation_id <- NA
                                              new_activity$.__enclos_env__$private$activity_code <- 104
                                              new_activity$.__enclos_env__$private$activity_label <- "Transit (added by t3R)"
                                              new_activity$.__enclos_env__$private$activity_number <- current_activities_date$count() + 1
                                              new_activity$.__enclos_env__$private$activity_id <- paste0("fr.ird.data.ps.logbook.Activity#666#", as.numeric(Sys.time()))
                                              capture.output(current_activities_date_sea$add(new_item = new_activity),
                                                             file = "NUL")
                                              current_trip$.__enclos_env__$private$activities <- append(current_trip$.__enclos_env__$private$activities, new_activity)
                                              message(" - Add transit activity to allocate time at sea, on date ",
                                                      activities_dates[activities_dates_id], ":",
                                                      "\n", "   [activity: ",
                                                      current_activities_date_sea$extract_l1_element_value(element="activity_id")[[1]], "]")
                                            }
                                            current_activities_date_time_at_sea <- current_activities_date_time_at_sea_declared
                                          }
                                          current_activities_date$modification_l1(modification = paste0("$path$time_at_sea = ",
                                                                                                        0))
                                          if(current_activities_date_sea$count()!=0){
                                            current_activities_date_sea$modification_l1(modification = paste0("$path$time_at_sea = ",
                                                                                                              current_activities_date_time_at_sea))
                                            time_at_sea <- time_at_sea + sum(unlist(x = current_activities_date_sea$extract_l1_element_value(element = "time_at_sea")))

                                          }
                                        }
                                      } else {
                                        if (time_departure_date > lubridate::dseconds(x = 0)
                                            & time_trip_end_date > lubridate::dseconds(x = 0)) {
                                          time_at_sea <- lubridate::int_length(lubridate::interval(start = departure_date,
                                                                                                   end = trip_end_date)) / 3600
                                        } else {
                                          time_at_sea <- lubridate::int_length(lubridate::interval(start = departure_date + lubridate::days(x = 1),
                                                                                                   end = trip_end_date - lubridate::days(x = 1))) / 3600
                                        }
                                      }
                                      current_trip$.__enclos_env__$private$time_at_sea <- time_at_sea
                                    }
                                    cat(format(x = Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Process 1.6 successfull on item \"",
                                        names(x = private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                  }
                                }
                              }
                              # 12.3 - Outputs extraction ----
                              # outputs manipulation
                              if (! is.null(x = global_output_path)) {
                                full_trips_selected <- private$data_selected
                                capture.output(trips_selected <- object_r6(class_name = "trips"),
                                               file = "NUL")
                                capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                               file = "NUL")
                                capture.output(activities_selected <- object_r6(class_name = "activities"),
                                               file = "NUL")
                                capture.output(activities_selected$add(new_item = unlist(x = trips_selected$extract_l1_element_value(element = "activities"))),
                                               file = "NUL")
                                outputs_process_1_6_trips <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                       FUN = function(full_trip_id) {
                                                                                                         if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                           return(rep(x = full_trip_id,
                                                                                                                      length(x = full_trips_selected[[full_trip_id]])))
                                                                                                         } else {
                                                                                                           return(full_trip_id)
                                                                                                         }
                                                                                                       })),
                                                                        "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                         FUN = function(full_trip_id) {
                                                                                                           if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                             return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                        length(x = full_trips_selected[[full_trip_id]])))
                                                                                                           } else {
                                                                                                             return(names(x = full_trips_selected[full_trip_id]))
                                                                                                           }
                                                                                                         })),
                                                                        "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                        "trip_end_date" = do.call("c",
                                                                                                  trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                        "year_trip_end_date" = sapply(do.call("c",
                                                                                                              trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                      lubridate::year),
                                                                        "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                        "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))))
                                outputs_process_1_6_activities <- data.frame("trip_id" = unlist(x = activities_selected$extract_l1_element_value(element = "trip_id")),
                                                                             "activity_id" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_id")),
                                                                             "objectoperation_id" = unlist(x = activities_selected$extract_l1_element_value(element = "objectoperation_id")),
                                                                             "activity_latitude" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_latitude")),
                                                                             "activity_longitude" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_longitude")),
                                                                             "activity_date" = do.call("c",
                                                                                                       activities_selected$extract_l1_element_value(element = "activity_date")),
                                                                             "activity_code" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_code")),
                                                                             "objectoperation_code" = unlist(x = activities_selected$extract_l1_element_value(element = "objectoperation_code")),
                                                                             "ocean_code" = unlist(x = activities_selected$extract_l1_element_value(element = "ocean_code")),
                                                                             "school_type_code" = unlist(x = activities_selected$extract_l1_element_value(element = "school_type_code")),
                                                                             "time_at_sea" = unlist(x = activities_selected$extract_l1_element_value(element = "time_at_sea")))
                                outputs_process_1_6 <- outputs_process_1_6_activities %>%
                                  dplyr::left_join(outputs_process_1_6_trips,
                                                   by = "trip_id") %>%
                                  dplyr::relocate(full_trip_id,
                                                  full_trip_name,
                                                  trip_id,
                                                  trip_end_date,
                                                  year_trip_end_date,
                                                  vessel_code,
                                                  vessel_type_code,
                                                  activity_id,
                                                  objectoperation_id,
                                                  activity_latitude,
                                                  activity_longitude,
                                                  activity_date,
                                                  activity_code,
                                                  objectoperation_code,
                                                  ocean_code,
                                                  school_type_code,
                                                  time_at_sea)
                                # extraction
                                if (output_format == "us") {
                                  outputs_dec <- "."
                                  outputs_sep <- ","
                                } else if (output_format == "eu") {
                                  outputs_dec <- ","
                                  outputs_sep <- ";"
                                } else {
                                  warning(format(Sys.time(),
                                                 "%Y-%m-%d %H:%M:%S"),
                                          " - Wrong outputs format define, European format will be applied.")
                                  outputs_dec <- ","
                                  outputs_sep <- ";"
                                }
                                write.table(x = outputs_process_1_6,
                                            file = file.path(global_output_path,
                                                             "level1",
                                                             "data",
                                                             "process_1_6.csv"),
                                            row.names = FALSE,
                                            sep = outputs_sep,
                                            dec = outputs_dec)
                                cat(format(x = Sys.time(),
                                           format = "%Y-%m-%d %H:%M:%S"),
                                    " - Outputs extracted in the following directory:\n",
                                    file.path(global_output_path,
                                              "level1",
                                              "data"), "\n")
                              }
                              cat(format(Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - End process 1.6: time at sea calculation.\n")
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 13 - Process 1.7: fishing_time ----
                            #' @description Process for fishing time calculation (in hours).
                            #' @param sunrise_schema Object of class {\link[base]{character}} expected. Sunrise characteristic. By default "sunrise" (top edge of the sun appears on the horizon). See below for more details.
                            #' @param sunset_schema Object of class {\link[base]{character}} expected. Sunset characteristic. By default "sunset" (sun disappears below the horizon, evening civil twilight starts). See below for more details.
                            #' @param referential_template Object of class \code{\link[base]{character}} expected. By default "observe". Referential template selected (for example regarding the activity_code). You can switch to "avdth".
                            #' @param activity_code_ref Object of type \code{\link[base]{data.frame}} or \code{\link[tibble]{tbl_df}} expected. Reference table with the activity codes to be taken into account for the allocation of sea and/or fishing time,
                            #'  and/or searching time and/or set duration.
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction egual TRUE. Path of the global outputs directory.
                            #'  The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            #' @importFrom suncalc getSunlightTimes
                            #' @details
                            #' Available variables are:
                            #' \itemize{
                            #'  \item{"sunrise": } sunrise (top edge of the sun appears on the horizon)
                            #'  \item{"sunriseEnd": } sunrise ends (bottom edge of the sun touches the horizon)
                            #'  \item{"goldenHourEnd": } morning golden hour ends(soft light, best time for photography)
                            #'  \item{"solarNoon": } solar noon (sun is in the highest position)
                            #'  \item{"goldenHour": } evening golden hour starts
                            #'  \item{"sunsetStart": } sunset starts (bottom edge of the sun touches the horizon)
                            #'  \item{"sunset": } sunset (sun disappears below the horizon, evening civil twilight starts)
                            #'  \item{"dusk": } dusk (evening nautical twilight starts)
                            #'  \item{"nauticalDusk": } nautical dusk (evening astronomical twilight starts)
                            #'  \item{"night": } night starts (dark enough for astronomical observations)
                            #'  \item{"nadir": }nadir (darkest moment of the night, sun is in the lowest position)
                            #'  \item{"nightEnd": } night ends (morning astronomical twilight starts)
                            #'  \item{"nauticalDawn": } nautical dawn (morning nautical twilight starts)
                            #'  \item{"dawn": } dawn (morning nautical twilight ends, morning civil twilight starts)
                            #' }
                            fishing_time = function(sunrise_schema = "sunrise",
                                                    sunset_schema = "sunset",
                                                    activity_code_ref,
                                                    referential_template = "observe",
                                                    global_output_path = NULL,
                                                    output_format = "eu") {
                              # 13.1 - Arguments verification ----
                              codama::r_type_checking(r_object = sunrise_schema,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("sunrise",
                                                                        "sunriseEnd",
                                                                        "goldenHourEnd",
                                                                        "solarNoon",
                                                                        "goldenHour",
                                                                        "sunsetStart",
                                                                        "sunset",
                                                                        "dusk",
                                                                        "nauticalDusk",
                                                                        "night",
                                                                        "nadir",
                                                                        "nightEnd",
                                                                        "nauticalDawn",
                                                                        "dawn"))
                              codama::r_type_checking(r_object = sunset_schema,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("sunrise",
                                                                        "sunriseEnd",
                                                                        "goldenHourEnd",
                                                                        "solarNoon",
                                                                        "goldenHour",
                                                                        "sunsetStart",
                                                                        "sunset",
                                                                        "dusk",
                                                                        "nauticalDusk",
                                                                        "night",
                                                                        "nadir",
                                                                        "nightEnd",
                                                                        "nauticalDawn",
                                                                        "dawn"))
                              if (! paste0(class(x = activity_code_ref),
                                           collapse = "_") %in% c("data.frame",
                                                                  "tbl_df_tbl_data.frame",
                                                                  "spec_tbl_df_tbl_df_tbl_data.frame")
                                  || ncol(x = activity_code_ref) !=14
                                  || nrow(x = activity_code_ref) <1) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Invalid \"activity_code_ref\" argument, class \"data.frame\" or \"tibble\" with 14 columns and at least 1 row expected.")
                              }
                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              # 13.2 - Global process ----
                              if (is.null(x = private$data_selected)) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Empty data selected in the R6 object. Process 1.7 (fishing time calculation) cancelled.")
                              } else {
                                if (referential_template == "observe") {
                                  activity_code_ref <- dplyr::mutate(.data = activity_code_ref,
                                                                     activity_code = activity_code_observe,
                                                                     activity_label = activity_label_observe,
                                                                     set_success_status = set_success_status_code_observe,
                                                                     objectoperation_code = objectoperation_code_observe,
                                                                     objectoperation_label = objectoperation_label_observe,
                                                                     code=paste(activity_code, objectoperation_code, sep="_"))
                                } else {
                                  activity_code_ref <- dplyr::mutate(.data = activity_code_ref,
                                                                     activity_code = activity_code_avdth,
                                                                     activity_label = activity_label_avdth,
                                                                     objectoperation_code = NA,
                                                                     objectoperation_label = NA,
                                                                     code=paste(activity_code, objectoperation_code,
                                                                                sep="_"))
                                }
                                activity_code_ref <- dplyr::select(.data = activity_code_ref,
                                                                   -activity_code_avdth,
                                                                   -activity_label_avdth,
                                                                   -activity_code_observe,
                                                                   -activity_label_observe,
                                                                   -set_success_status_code_observe,
                                                                   -objectoperation_code_observe,
                                                                   -objectoperation_label_observe,
                                                                   -schooltype_code_observe,
                                                                   -schooltype_label_observe,
                                                                   -status_active_observe,
                                                                   -comment)
                                # No fishing activity_objectoperation codes
                                no_fishing_codes <- unique(activity_code_ref %>%
                                                             dplyr::filter(fishing_time==0) %>%
                                                             dplyr::pull(code))
                                # Fishing activity_objectoperation codes
                                # to take into account for fishing time allocation
                                # except special case : activities with elementary catch (6,32)
                                fishing_codes <- unique(activity_code_ref %>%
                                                          dplyr::filter(fishing_time==1,
                                                                        set_duration==0) %>%
                                                          dplyr::pull(code))

                                # Activity codes associated to elementary catch (6,32)
                                catch_activity_codes <- unique(activity_code_ref
                                                               %>% dplyr::filter(set_duration==1)
                                                               %>% dplyr::pull(activity_code))
                                # activity_objectoperation codes associated to elementary catch (6,32)
                                catch_codes <- unique(activity_code_ref %>%
                                                        dplyr::filter(set_duration==1) %>%
                                                        dplyr::pull(code))

                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 1.7: fishing time calculation.\n")
                                  }
                                  if (names(x = private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    message(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Warning on item \"",
                                            names(x = private$data_selected)[full_trip_id],
                                            "\": full trip avoided because a least one trip inside is missing.\n",
                                            "[trip: ",
                                            private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                            "]")
                                    capture.output(current_trips <- object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    current_trips$modification_l1(modification = "$path$fishing_time <- NA_real_")
                                    if (length(x = current_trips$extract_l1_element_value(element = "activities")) != 0) {
                                      capture.output(current_activities <- object_r6(class_name = "activities"),
                                                     file = "NUL")
                                      capture.output(current_activities$add(new_item = unlist(current_trips$extract_l1_element_value(element = "activities"))),
                                                     file = "NUL")
                                      current_activities$modification_l1(modification = "$path$fishing_time <- NA_real_")
                                    }
                                  } else {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.7 on item \"",
                                        names(x = private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                    for (trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                      fishing_time <- 0
                                      if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                        capture.output(current_activities <- object_r6(class_name = "activities"),
                                                       file = "NUL")
                                        capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                                                       file = "NUL")
                                        activities_dates <- current_activities$extract_l1_element_value(element = "activity_date")
                                        activities_dates <- unique(do.call(what = "c",
                                                                           args = activities_dates))
                                        activities_dates <- sort(x = activities_dates)
                                        for (activities_dates_id in seq_len(length.out = length(activities_dates))) {
                                          activities_date <- activities_dates[[activities_dates_id]]
                                          capture.output(current_activities_date <- object_r6(class_name = "activities"),
                                                         file = "NUL")
                                          capture.output(current_activities_date$add(new_item = current_activities$filter_l1(filter = paste0("$path$activity_date == lubridate::parse_date_time(x = \"",
                                                                                                                                             activities_date,
                                                                                                                                             "\",",
                                                                                                                                             "orders = c(\"ymd_HMS\", \"ymd\"), tz = \"UTC\", quiet = TRUE)"))),
                                                         file = "NUL")
                                          current_activities_code <- unlist(current_activities_date$extract_l1_element_value(element = "activity_code"))
                                          current_objectoperation_code <- unlist(current_activities_date$extract_l1_element_value(element = "objectoperation_code"))
                                          current_code <- paste(current_activities_code, current_objectoperation_code, sep='_')
                                          if(!all(unique(x = current_code) %in% unique(x=activity_code_ref$code))) {
                                            wrong_codes <- unique(current_code[!(current_code %in% unique(x=activity_code_ref$code))])
                                            activity_id <-  unlist(current_activities_date_fishing$extract_l1_element_value(element="activity_id"))
                                            stop(format(Sys.time(),
                                                           "%Y-%m-%d %H:%M:%S"),
                                                    " - Association of activity code and objectoperation code not supported: ",
                                                    paste0(wrong_codes, collapse=", "),
                                                    "\n[trip: ",
                                                    current_trip$.__enclos_env__$private$trip_id,
                                                    "]\n",
                                                    paste0("[activity: ",
                                                    activity_id, collapse="];\n"),
                                                    "].")
                                          }
                                          catch_time <- 0
                                          # Only no fishing activities
                                          if(all(current_code %in% no_fishing_codes)) {
                                            current_activities_date$modification_l1(modification = "$path$fishing_time <- 0")
                                            # If only no fishing activities and fishing_time_declared >0 create searching activity
                                            # to allocate fishing time recorded in observe after get it via activities query (not done yet)
                                          }
                                          # Date including fishing activities
                                          else {
                                            # No fishing activities
                                            if(any(unique(x = current_code) %in% no_fishing_codes)) {
                                              capture.output(current_activities_date_no_fishing <- object_r6(class_name = "activities"),
                                                             file = "NUL")
                                              capture.output(current_activities_date_no_fishing$add(new_item = current_activities_date$filter_l1(filter = paste0("paste($path$activity_code,
                                                                                                                                                  $path$objectoperation_code,
                                                                                                                                                  sep='_') %in% c(\"",
                                                                                                                                                                 paste(no_fishing_codes,
                                                                                                                                                                       collapse = "\", \""),"\")"))),
                                                             file = "NUL")
                                              current_activities_date_no_fishing$modification_l1(modification = "$path$fishing_time <- 0")
                                            }
                                            # Activity corresponding to catch (6,32) with fishing_time=set_duration
                                            if (any(current_code %in% catch_codes)){
                                              capture.output(current_activities_date_catch <- object_r6(class_name = "activities"),
                                                             file = "NUL")
                                              capture.output(current_activities_date_catch$add(new_item = current_activities_date$filter_l1(filter = paste0("($path$activity_code %in% c(\"",
                                                                                                                                                            paste(catch_activity_codes,
                                                                                                                                                                  collapse = "\", \""),
                                                                                                                                                            "\"))"))),
                                                             file = "NUL")

                                              for (activity_id in seq_len(length.out = current_activities_date_catch$count())) {
                                                current_activity <- current_activities_date_catch$extract(id = activity_id)[[1]]
                                                set_duration <- current_activity$.__enclos_env__$private$set_duration
                                                current_activity$.__enclos_env__$private$fishing_time <- set_duration
                                                catch_time <- catch_time + set_duration
                                              }
                                            }
                                            # Fishing activities except activities with elementary catch
                                            if (any(unique(x = current_code) %in% fishing_codes)) {
                                              capture.output(current_activities_date_fishing <- object_r6(class_name = "activities"),
                                                             file = "NUL")

                                              capture.output(current_activities_date_fishing$add(new_item = current_activities_date$filter_l1(filter = paste0("paste($path$activity_code,
                                                                                                                                                  $path$objectoperation_code,
                                                                                                                                                  sep='_') %in% c(\"",
                                                                                                                                                              paste(fishing_codes,
                                                                                                                                                                    collapse = "\", \""),"\")"))),
                                                             file = "NUL")
                                              current_activities_latitudes <- unlist(current_activities_date_fishing$extract_l1_element_value(element = "activity_latitude"))
                                              current_activities_longitudes <- unlist(current_activities_date_fishing$extract_l1_element_value(element = "activity_longitude"))
                                              latitude_mean <- mean(x = current_activities_latitudes, na.rm=TRUE)
                                              longitude_mean <- mean(x = current_activities_longitudes, na.rm=TRUE)
                                              if(is.na(latitude_mean) | is.na(longitude_mean)){
                                                warning(format(Sys.time(),
                                                               "%Y-%m-%d %H:%M:%S"),
                                                        "Fishing activity with missing position",
                                                        "\n[trip: ",
                                                        current_trip$.__enclos_env__$private$trip_id,
                                                        ", activity: ",
                                                        unique(unlist(current_activities_date_fishing$extract_l1_element_value(element = "activity_id"))),
                                                        "]")
                                                ocean_code <- unique(unlist(current_activities_date_fishing$extract_l1_element_value(element = "ocean_code")))
                                                fishing_time_tmp <- ifelse(ocean_code==1, 12, 13)
                                              } else{
                                                current_sunrise <- suncalc::getSunlightTimes(date = as.Date(x = activities_date),
                                                                                             lat = latitude_mean,
                                                                                             lon = longitude_mean)[[sunrise_schema]]
                                                current_sunset <- suncalc::getSunlightTimes(date = as.Date(x = activities_date),
                                                                                            lat = latitude_mean,
                                                                                            lon = longitude_mean)[[sunset_schema]]
                                                fishing_time_tmp <- lubridate::int_length(lubridate::interval(start = current_sunrise,
                                                                                                              end = current_sunset))/3600
                                              }
                                              # Subtract the duration of activities with elementary catches from the total fishing time.
                                              fishing_time_tmp2 <- fishing_time_tmp - catch_time
                                              current_activities_date_fishing$modification_l1(modification = paste0("$path$fishing_time <- ",
                                                                                                                    round(fishing_time_tmp2/current_activities_date_fishing$count(),
                                                                                                                          digits=4)))

                                            } else if(all(unique(x=current_activities_code) %in% catch_activity_codes)){
                                              new_activity <- current_activities_date$.__enclos_env__$private$data[[1]]$clone()
                                              new_activity$.__enclos_env__$private$elementarycatches <- NULL
                                              new_activity$.__enclos_env__$private$objectoperation_code <- NA
                                              new_activity$.__enclos_env__$private$objectoperation_label <- NA
                                              new_activity$.__enclos_env__$private$objectoperation_id <- NA
                                              new_activity$.__enclos_env__$private$positive_set_count <- NA
                                              new_activity$.__enclos_env__$private$set_duration <- 0
                                              new_activity$.__enclos_env__$private$school_type_code <- NA
                                              new_activity$.__enclos_env__$private$set_count <- NA
                                              new_activity$.__enclos_env__$private$set_success_status_code <- NA
                                              new_activity$.__enclos_env__$private$set_success_status_label <- NA
                                              new_activity$.__enclos_env__$private$activity_code <- 105
                                              new_activity$.__enclos_env__$private$activity_label <- "Searching (added by t3R)"
                                              new_activity$.__enclos_env__$private$activity_number <- current_activities_date$count() + 1
                                              new_activity$.__enclos_env__$private$activity_id <- paste0("fr.ird.data.ps.logbook.Activity#666#", as.numeric(Sys.time()))
                                              capture.output(current_activities_date_fishing <- object_r6(class_name = "activities"),
                                                             file = "NUL")
                                              capture.output(current_activities_date_fishing$add(new_item = new_activity),
                                                             file = "NUL")
                                              # Compute fishing time according to localisation of catch activities declared
                                              current_activities_latitudes <- unlist(current_activities_date_catch$extract_l1_element_value(element = "activity_latitude"))
                                              current_activities_longitudes <- unlist(current_activities_date_catch$extract_l1_element_value(element = "activity_longitude"))
                                              latitude_mean <- mean(x = current_activities_latitudes, na.rm=TRUE)
                                              longitude_mean <- mean(x = current_activities_longitudes, na.rm=TRUE)
                                              if(is.na(latitude_mean) | is.na(longitude_mean)){
                                                warning(format(Sys.time(),
                                                               "%Y-%m-%d %H:%M:%S"),
                                                        "Catch activity with missing position",
                                                        "\n[trip: ",
                                                        current_trip$.__enclos_env__$private$trip_id,
                                                        ", activity: ",
                                                        current_activities_date_catch$extract_l1_element_value(element="activity_id")[[1]],
                                                        "]")
                                                ocean_code <- unique(unlist(current_activities_date_fishing$extract_l1_element_value(element = "ocean_code")))
                                                fishing_time_tmp <- ifelse(ocean_code==1, 12, 13)
                                              } else{
                                                current_sunrise <- suncalc::getSunlightTimes(date = as.Date(x = activities_date),
                                                                                             lat = latitude_mean,
                                                                                             lon = longitude_mean)[[sunrise_schema]]
                                                current_sunset <- suncalc::getSunlightTimes(date = as.Date(x = activities_date),
                                                                                            lat = latitude_mean,
                                                                                            lon = longitude_mean)[[sunset_schema]]
                                                fishing_time_tmp <- lubridate::int_length(lubridate::interval(start = current_sunrise,
                                                                                                              end = current_sunset))/3600
                                              }
                                              # Subtract the duration of activities with elementary catches from the total fishing time.
                                              fishing_time_tmp2 <- fishing_time_tmp - catch_time
                                              current_activities_date_fishing$modification_l1(modification = paste0("$path$fishing_time <- ",
                                                                                                                    round(fishing_time_tmp2,
                                                                                                                          digits=4)))
                                              # Add new activity to current_trip
                                              current_trip$.__enclos_env__$private$activities <- append(current_trip$.__enclos_env__$private$activities, new_activity)
                                              message(" - Add searching activity to allocate fishing time, on date ",
                                                      activities_dates[activities_dates_id], ":",
                                                      "\n", "   [activity: ",
                                                      current_activities_date_fishing$extract_l1_element_value(element="activity_id")[[1]], "]")
                                            }

                                            fishing_time <- fishing_time + fishing_time_tmp
                                          }
                                        }
                                      }
                                      current_trip$.__enclos_env__$private$fishing_time <- round(fishing_time,
                                                                                                 digits=2)
                                    }
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Process 1.7 successfull on item \"",
                                        names(x = private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                  }
                                }
                                # 13.3 - Outputs extraction ----
                                # outputs manipulation
                                if (! is.null(x = global_output_path)) {
                                  full_trips_selected <- private$data_selected
                                  capture.output(trips_selected <- object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                                 file = "NUL")
                                  capture.output(activities_selected <- object_r6(class_name = "activities"),
                                                 file = "NUL")
                                  capture.output(activities_selected$add(new_item = unlist(x = trips_selected$extract_l1_element_value(element = "activities"))),
                                                 file = "NUL")
                                  outputs_process_1_7_trips <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                         FUN = function(full_trip_id) {
                                                                                                           if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                             return(rep(x = full_trip_id,
                                                                                                                        length(x = full_trips_selected[[full_trip_id]])))
                                                                                                           } else {
                                                                                                             return(full_trip_id)
                                                                                                           }
                                                                                                         })),
                                                                          "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                           FUN = function(full_trip_id) {
                                                                                                             if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                               return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                          length(x = full_trips_selected[[full_trip_id]])))
                                                                                                             } else {
                                                                                                               return(names(x = full_trips_selected[full_trip_id]))
                                                                                                             }
                                                                                                           })),
                                                                          "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                          "trip_end_date" = do.call("c",
                                                                                                    trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                          "year_trip_end_date" = sapply(do.call("c",
                                                                                                                trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                        lubridate::year),
                                                                          "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                          "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))))
                                  outputs_process_1_7_activities <- data.frame("trip_id" = unlist(x = activities_selected$extract_l1_element_value(element = "trip_id")),
                                                                               "activity_id" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_id")),
                                                                               "objectoperation_id" = unlist(x = activities_selected$extract_l1_element_value(element = "objectoperation_id")),
                                                                               "activity_latitude" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_latitude")),
                                                                               "activity_longitude" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_longitude")),
                                                                               "activity_date" = do.call("c",
                                                                                                         activities_selected$extract_l1_element_value(element = "activity_date")),
                                                                               "activity_code" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_code")),
                                                                               "objectoperation_code" = unlist(x = activities_selected$extract_l1_element_value(element = "objectoperation_code")),
                                                                               "ocean_code" = unlist(x = activities_selected$extract_l1_element_value(element = "ocean_code")),
                                                                               "school_type_code" = unlist(x = activities_selected$extract_l1_element_value(element = "school_type_code")),
                                                                               "fishing_time" = unlist(x = activities_selected$extract_l1_element_value(element = "fishing_time")))
                                  outputs_process_1_7 <- outputs_process_1_7_activities %>%
                                    dplyr::left_join(outputs_process_1_7_trips,
                                                     by = "trip_id") %>%
                                    dplyr::relocate(full_trip_id,
                                                    full_trip_name,
                                                    trip_id,
                                                    trip_end_date,
                                                    year_trip_end_date,
                                                    vessel_code,
                                                    vessel_type_code,
                                                    activity_id,
                                                    objectoperation_id,
                                                    activity_latitude,
                                                    activity_longitude,
                                                    activity_date,
                                                    activity_code,
                                                    objectoperation_code,
                                                    ocean_code,
                                                    school_type_code,
                                                    fishing_time)
                                  # extraction
                                  if (output_format == "us") {
                                    outputs_dec <- "."
                                    outputs_sep <- ","
                                  } else if (output_format == "eu") {
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  } else {
                                    warning(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Wrong outputs format define, European format will be applied.")
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  }
                                  write.table(x = outputs_process_1_7,
                                              file = file.path(global_output_path,
                                                               "level1",
                                                               "data",
                                                               "process_1_7.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Outputs extracted in the following directory:\n",
                                      file.path(global_output_path,
                                                "level1",
                                                "data"), "\n")
                                }
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - End process 1.7: fishing time calculation.\n")
                              }
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 14 - Process 1.8: searching_time ----
                            #' @description Process for searching time calculation (in hours, fishing time minus sets durations).
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction egual TRUE. Path of the global outputs directory. The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            searching_time = function(global_output_path = NULL,
                                                      output_format = "eu") {
                              # 14.1 - Arguments verification ----
                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              # 14.2 - Global process ----
                              if (is.null(x = private$data_selected)) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Empty data selected in the R6 object.\n",
                                     "Process 1.8 (fishing time calculation) cancelled.")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 1.8: searching time calculation.\n")
                                  }
                                  if (names(x = private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    message(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Warning on item \"",
                                            names(x = private$data_selected)[full_trip_id],
                                            "\": full trip avoided because a least one trip inside is missing.\n",
                                            "[trip: ",
                                            private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                            "]")
                                    capture.output(current_trips <- object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    current_trips$modification_l1(modification = "$path$searching_time <- NA_real_")
                                    if (length(x = current_trips$extract_l1_element_value(element = "activities")) != 0) {
                                      capture.output(current_activities <- object_r6(class_name = "activities"),
                                                     file = "NUL")
                                      capture.output(current_activities$add(new_item = unlist(current_trips$extract_l1_element_value(element = "activities"))),
                                                     file = "NUL")
                                      current_activities$modification_l1(modification = "$path$searching_time <- NA_real_")
                                    }
                                  } else {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.8 on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                    for (trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[trip_id]]
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        capture.output(current_activities <- object_r6(class_name = "activities"),
                                                       file = "NUL")
                                        capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                                                       file = "NUL")
                                        for (current_activity_id in seq_len(length.out = current_activities$count())) {
                                          current_activity <- current_activities$extract(id = current_activity_id)[[1]]
                                          current_fishing_time <- lubridate::dhours(x = current_activity$.__enclos_env__$private$fishing_time)
                                          current_set_duration <- lubridate::dhours(x = current_activity$.__enclos_env__$private$set_duration)
                                          current_set_duration <- if (is.na(x = current_set_duration)) {0} else {current_set_duration}
                                          current_searching_time <- current_fishing_time - current_set_duration
                                          # return lubridate object with results in seconds in @.Data
                                          current_searching_time <- current_searching_time@.Data / 3600
                                          current_activity$.__enclos_env__$private$searching_time <- current_searching_time
                                        }
                                        searching_time <- sum(unlist(x = current_activities$extract_l1_element_value(element = "searching_time")))
                                      } else {
                                        searching_time <- 0
                                      }
                                      current_trip$.__enclos_env__$private$searching_time <- searching_time
                                    }
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Process 1.8 successfull on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                  }
                                }
                                # 14.3 - Outputs extraction ----
                                # outputs manipulation
                                if (! is.null(x = global_output_path)) {
                                  full_trips_selected <- private$data_selected
                                  capture.output(trips_selected <- object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                                 file = "NUL")
                                  capture.output(activities_selected <- object_r6(class_name = "activities"),
                                                 file = "NUL")
                                  capture.output(activities_selected$add(new_item = unlist(x = trips_selected$extract_l1_element_value(element = "activities"))),
                                                 file = "NUL")
                                  outputs_process_1_8_trips <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                         FUN = function(full_trip_id) {
                                                                                                           if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                             return(rep(x = full_trip_id,
                                                                                                                        length(x = full_trips_selected[[full_trip_id]])))
                                                                                                           } else {
                                                                                                             return(full_trip_id)
                                                                                                           }
                                                                                                         })),
                                                                          "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                           FUN = function(full_trip_id) {
                                                                                                             if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                               return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                          length(x = full_trips_selected[[full_trip_id]])))
                                                                                                             } else {
                                                                                                               return(names(x = full_trips_selected[full_trip_id]))
                                                                                                             }
                                                                                                           })),
                                                                          "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                          "trip_end_date" = do.call("c",
                                                                                                    trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                          "year_trip_end_date" = sapply(do.call("c",
                                                                                                                trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                        lubridate::year),
                                                                          "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                          "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))))
                                  outputs_process_1_8_activities <- data.frame("trip_id" = unlist(x = activities_selected$extract_l1_element_value(element = "trip_id")),
                                                                               "activity_id" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_id")),
                                                                               "objectoperation_id" = unlist(x = activities_selected$extract_l1_element_value(element = "objectoperation_id")),
                                                                               "activity_latitude" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_latitude")),
                                                                               "activity_longitude" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_longitude")),
                                                                               "activity_date" = do.call("c",
                                                                                                         activities_selected$extract_l1_element_value(element = "activity_date")),
                                                                               "activity_code" = unlist(x = activities_selected$extract_l1_element_value(element = "activity_code")),
                                                                               "ocean_code" = unlist(x = activities_selected$extract_l1_element_value(element = "ocean_code")),
                                                                               "school_type_code" = unlist(x = activities_selected$extract_l1_element_value(element = "school_type_code")),
                                                                               "objectoperation_code" = unlist(x = activities_selected$extract_l1_element_value(element = "objectoperation_code")),
                                                                               "searching_time" = unlist(x = activities_selected$extract_l1_element_value(element = "searching_time")))
                                  outputs_process_1_8 <- outputs_process_1_8_activities %>%
                                    dplyr::left_join(outputs_process_1_8_trips,
                                                     by = "trip_id") %>%
                                    dplyr::relocate(full_trip_id,
                                                    full_trip_name,
                                                    trip_id,
                                                    trip_end_date,
                                                    year_trip_end_date,
                                                    vessel_code,
                                                    vessel_type_code,
                                                    activity_id,
                                                    objectoperation_id,
                                                    activity_latitude,
                                                    activity_longitude,
                                                    activity_date,
                                                    activity_code,
                                                    objectoperation_code,
                                                    ocean_code,
                                                    school_type_code,
                                                    searching_time)
                                  # extraction
                                  if (output_format == "us") {
                                    outputs_dec <- "."
                                    outputs_sep <- ","
                                  } else if (output_format == "eu") {
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  } else {
                                    warning(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Warning: wrong outputs format define, European format will be applied.")
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  }
                                  write.table(x = outputs_process_1_8,
                                              file = file.path(global_output_path,
                                                               "level1",
                                                               "data",
                                                               "process_1_8.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Outputs extracted in the following directory:\n",
                                      file.path(global_output_path,
                                                "level1",
                                                "data"), "\n")
                                }
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - End process 1.8: searching time calculation.\n")
                              }
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 15 - Process 2.1: sample length class conversion ld1 to lf ----
                            #' @description Process for length conversion, if necessary, in length fork (lf). Furthermore, variable "sample_number_measured_extrapolated" of process 2.1 will converse in variable "sample_number_measured_extrapolated_lf" (Notably due to the creation of new lf classes during some conversions).
                            #' @param length_step Object of type \code{\link[base]{data.frame}} or \code{\link[tibble]{tbl_df}} expected. Data frame object with length ratio between ld1 and lf class.
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction egual TRUE. Path of the global outputs directory. The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            #' @param referential_template Object of class \code{\link[base]{character}} expected. By default "observe". Referential template selected (for example regarding the activity_code). You can switch to "avdth".
                            sample_length_class_ld1_to_lf =  function(length_step,
                                                                      global_output_path = NULL,
                                                                      output_format = "eu",
                                                                      referential_template = "observe") {
                              # 15.1 - Arguments verification ----
                              if (! paste0(class(x = length_step),
                                           collapse = "_") %in% c("data.frame",
                                                                  "tbl_df_tbl_data.frame")
                                  || ncol(x = length_step) != 6
                                  || nrow(x = length_step) <1) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Invalid d \"length_step\" argument, class \"data.frame\" or \"tibble\" with 6 columns and at least 1 row expected.")
                              }

                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              codama::r_type_checking(r_object = referential_template,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("observe",
                                                                        "avdth"))
                              # 15.2 - Global process ----
                              if (is.null(x = private$data_selected)) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Empty data selected in the R6 object. Process 2.1 (sample length class conversion ld1 to lf) cancelled.")
                              } else {
                                length_step_count <- length_step %>%
                                  dplyr::group_by(ocean_code,
                                                  species_fao_code,
                                                  ld1_class) %>%
                                  dplyr::summarise(nb = dplyr::n(),
                                                   .groups = "drop")
                                referential_size_measure_type_code <- if (referential_template == "observe") {c("FL", "PD1")} else {c("1", "2")}
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.1: sample length class conversion ld1 to lf.\n")
                                  }
                                  if (names(private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    message(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Warning on item \"",
                                            names(x = private$data_selected)[full_trip_id],
                                            "\": full trip avoided because a least one trip inside is missing.\n",
                                            "[trip: ",
                                            private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                            "]")
                                    capture.output(current_trips <- object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                      capture.output(current_wells <- object_r6(class_name = "wells"),
                                                     file = "NUL")
                                      capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                     file = "NUL")
                                      if (length(x = unlist(current_wells$extract_l1_element_value(element = "elementarysampleraw"))) != 0) {
                                        capture.output(current_elementarysamplesraw <- object_r6(class_name = "elementarysamplesraw"),
                                                       file = "NUL")
                                        capture.output(current_elementarysamplesraw$add(new_item = unlist(current_wells$extract_l1_element_value(element = "elementarysampleraw"))),
                                                       file = "NUL")
                                        current_elementarysamplesraw$modification_l1(modification = "$path$sample_length_class_lf <- NA_character_")
                                        current_elementarysamplesraw$modification_l1(modification = "$path$sample_number_measured_lf <- NA")
                                      }
                                    }
                                  } else {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.1 on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                    for (partial_trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        capture.output(current_activities <- object_r6(class_name = "activities"),
                                                       file = "NUL")
                                        capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                                                       file = "NUL")
                                        if (length(current_trip$.__enclos_env__$private$wells) != 0) {
                                          capture.output(current_wells <- object_r6(class_name = "wells"),
                                                         file = "NUL")
                                          capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                         file = "NUL")
                                          if (length(x = current_activities$filter_l1(filter = "length($path$elementarycatches) != 0")) != 0) {
                                            capture.output(current_activities_with_elementarycatches <- object_r6(class_name = "activities"),
                                                           file = "NUL")
                                            capture.output(current_activities_with_elementarycatches$add(new_item = current_activities$filter_l1(filter = "length($path$elementarycatches) != 0")),
                                                           file = "NUL")
                                            oceans_activities <- unique(unlist(current_activities_with_elementarycatches$extract_l1_element_value(element = "ocean_code")))
                                            if (length(oceans_activities) != 1) {
                                              capture.output(current_elementary_catches <- do.call(rbind,
                                                                                                   current_activities_with_elementarycatches$extract_l1_element_value(element = "elementarycatches")),
                                                             file = "NUL")
                                              if (any(is.null(x = current_elementary_catches$catch_weight_category_code_corrected))) {
                                                stop(format(Sys.time(),
                                                            "%Y-%m-%d %H:%M:%S"),
                                                     " - Variable \"catch_weight_category_code_corrected\" not calculated. Run steps 1.1 to 1.3 of level 1 before this step.")
                                              } else {
                                                total_current_elementary_catches <- sum(current_elementary_catches$catch_weight_category_code_corrected)
                                                oceans_activities_weight <- as.numeric()
                                                for (current_ocean_activites in oceans_activities) {
                                                  capture.output(current_elementary_catches_ocean <- dplyr::filter(.data=current_elementary_catches,
                                                                                                                   current_elementary_catches$ocean_code == current_ocean_activites),
                                                                 file = "NUL")
                                                  current_oceans_activities_weight <- sum(current_elementary_catches_ocean$catch_weight_category_code_corrected) / total_current_elementary_catches
                                                  oceans_activities_weight <- rbind(oceans_activities_weight,
                                                                                    current_oceans_activities_weight)
                                                  names(oceans_activities_weight)[length(oceans_activities_weight)] <- current_ocean_activites
                                                }
                                                major_ocean_activities <- as.integer(names(which(x = oceans_activities_weight == max(oceans_activities_weight))))
                                              }
                                            } else {
                                              major_ocean_activities <- oceans_activities
                                            }
                                            for (well_id in seq_len(length.out = current_wells$count())) {
                                              current_well <- current_wells$extract(id = well_id)[[1]]
                                              if (length(current_well$.__enclos_env__$private$elementarysampleraw) != 0) {
                                                capture.output(current_samples <- object_r6(class_name = "elementarysamplesraw"),
                                                               file = "NUL")
                                                capture.output(current_samples$add(new_item = current_well$.__enclos_env__$private$elementarysampleraw),
                                                               file = "NUL")
                                                current_samples_removed <- as.integer()
                                                for (sample_id in seq_len(length.out = current_samples$count())) {
                                                  elementary_sample_skj_removed <- as.integer()
                                                  capture.output(current_sample <- object_r6(class_name = "elementarysamplesraw"),
                                                                 file = "NUL")
                                                  capture.output(current_sample$add(new_item = current_samples$extract(id = sample_id)),
                                                                 file = "NUL")
                                                  if (any(! unique(x = unlist(x = current_sample$extract_l1_element_value(element = "size_measure_type_code"))) %in% referential_size_measure_type_code)) {
                                                    stop(format(Sys.time(),
                                                                "%Y-%m-%d %H:%M:%S"),
                                                         " - Value(s) for the variable \"size_measure_type_code\" not added yet in the function code. Check the following value(s): ",
                                                         paste(unique(x = unlist(x = current_sample$extract_l1_element_value(element = "size_measure_type_code"))),
                                                               collapse = ", "))
                                                  }
                                                  if (length(current_sample$filter_l1(filter = paste0("$path$size_measure_type_code == \"",
                                                                                                      if (referential_template == "observe") {"FL"} else {"2"},
                                                                                                      "\""))) != 0) {
                                                    capture.output(current_sample_size_measure_type_code_2 <- object_r6(class_name = "elementarysamplesraw"),
                                                                   file = "NUL")
                                                    capture.output(current_sample_size_measure_type_code_2$add(new_item = current_sample$filter_l1(filter = paste0("$path$size_measure_type_code == \"",
                                                                                                                                                                   if (referential_template == "observe") {"FL"} else {"2"},
                                                                                                                                                                   "\""))),
                                                                   file = "NUL")
                                                    current_sample_size_measure_type_code_2$modification_l1(modification = "$path$sample_length_class_lf = as.integer($path$sample_length_class)")
                                                    current_sample_size_measure_type_code_2$modification_l1(modification = "$path$sample_number_measured_lf = $path$sample_number_measured")
                                                  }
                                                  if (length(current_sample$filter_l1(filter = paste0("$path$size_measure_type_code == \"",
                                                                                                      if (referential_template == "observe") {"PD1"} else {"1"},
                                                                                                      "\""))) != 0) {
                                                    capture.output(current_sample_size_measure_type_code_1 <- object_r6(class_name = "elementarysamplesraw"),
                                                                   file = "NUL")
                                                    capture.output(current_sample_size_measure_type_code_1$add(new_item = current_sample$filter_l1(filter = paste0("$path$size_measure_type_code == \"",
                                                                                                                                                                   if (referential_template == "observe") {"PD1"} else {"1"},
                                                                                                                                                                   "\""))),
                                                                   file = "NUL")
                                                    for (elementarysampleraw_id in seq_len(length.out = current_sample_size_measure_type_code_1$count())) {
                                                      current_elementary_sample <- current_sample_size_measure_type_code_1$extract(id = elementarysampleraw_id)[[1]]
                                                      current_length_step_count <- as.numeric(length_step_count[length_step_count$ocean_code == major_ocean_activities
                                                                                                                & length_step_count$species_fao_code == current_elementary_sample$.__enclos_env__$private$species_fao_code
                                                                                                                & length_step_count$ld1_class == current_elementary_sample$.__enclos_env__$private$sample_length_class, "nb"])
                                                      if (is.na(x = current_length_step_count)) {
                                                        if (current_elementary_sample$.__enclos_env__$private$species_fao_code == "SKJ") {
                                                          warning(format(Sys.time(),
                                                                         "%Y-%m-%d %H:%M:%S"),
                                                                  " - Sample detected with length class measured in LD1 for SKJ specie. Elementary sample associated deleted.\n",
                                                                  "[trip_id: ",
                                                                  current_elementary_sample$.__enclos_env__$private$trip_id,
                                                                  " (full trip item id ",
                                                                  full_trip_id,
                                                                  ", trip item id ",
                                                                  partial_trip_id,
                                                                  "), well_id: ",
                                                                  current_elementary_sample$.__enclos_env__$private$well_id,
                                                                  " (well item id ",
                                                                  well_id,
                                                                  "), sample_id: ",
                                                                  current_elementary_sample$.__enclos_env__$private$sample_id,
                                                                  " (sample item id ",
                                                                  sample_id,
                                                                  ")]")
                                                          elementary_sample_skj_removed <- append(elementary_sample_skj_removed,
                                                                                                  elementarysampleraw_id)
                                                        } else {
                                                          stop(format(Sys.time(),
                                                                      "%Y-%m-%d %H:%M:%S"),
                                                               " - No correspondance between sample length class and ld1-lf reference table for ocean ",
                                                               major_ocean_activities,
                                                               ", species ",
                                                               current_elementary_sample$.__enclos_env__$private$species_fao_code,
                                                               " and LD1 class ",
                                                               current_elementary_sample$.__enclos_env__$private$sample_length_class,
                                                               ".\n",
                                                               "[trip_id: ",
                                                               current_elementary_sample$.__enclos_env__$private$trip_id,
                                                               " (full trip item id ",
                                                               full_trip_id,
                                                               ", trip item id ",
                                                               partial_trip_id,
                                                               "), well_id: ",
                                                               current_elementary_sample$.__enclos_env__$private$well_id,
                                                               " (well item id ",
                                                               well_id,
                                                               "), sample_id: ",
                                                               current_elementary_sample$.__enclos_env__$private$sample_id,
                                                               " (sample item id ",
                                                               sample_id,
                                                               ")]")
                                                        }
                                                      } else {
                                                        current_length_step <- length_step[length_step$ocean_code == major_ocean_activities
                                                                                           & length_step$species_fao_code == current_elementary_sample$.__enclos_env__$private$species_fao_code
                                                                                           & length_step$ld1_class == current_elementary_sample$.__enclos_env__$private$sample_length_class, ]
                                                        current_elementary_sample_tmp <- vector(mode = "list")
                                                        for (current_length_step_count_id in seq_len(length.out = current_length_step_count)) {
                                                          if (current_length_step_count_id == current_length_step_count) {
                                                            current_elementary_sample$.__enclos_env__$private$size_measure_type_code <- if (referential_template == "observe") {"FL"} else {"2"}
                                                            current_elementary_sample$.__enclos_env__$private$sample_length_class_lf <- as.integer(current_length_step[current_length_step_count_id,
                                                                                                                                                                       "lf_class"])
                                                            current_elementary_sample$.__enclos_env__$private$sample_number_measured_lf <- as.numeric(current_length_step[current_length_step_count_id,
                                                                                                                                                                          "ratio"]
                                                                                                                                                      * 10^-2
                                                                                                                                                      * current_elementary_sample$.__enclos_env__$private$sample_number_measured)
                                                          } else {
                                                            current_elementary_sample_tmpbis <- current_elementary_sample$clone()
                                                            current_elementary_sample_tmpbis$.__enclos_env__$private$size_measure_type_code <- if (referential_template == "observe") {"FL"} else {"2"}
                                                            current_elementary_sample_tmpbis$.__enclos_env__$private$sample_length_class_lf <- as.integer(current_length_step[current_length_step_count_id,
                                                                                                                                                                              "lf_class"])
                                                            current_elementary_sample_tmpbis$.__enclos_env__$private$sample_number_measured_lf <- as.numeric(current_length_step[current_length_step_count_id,
                                                                                                                                                                                 "ratio"]
                                                                                                                                                             * 10^-2
                                                                                                                                                             * current_elementary_sample_tmpbis$.__enclos_env__$private$sample_number_measured)
                                                            current_elementary_sample_tmp <- append(current_elementary_sample_tmp,
                                                                                                    current_elementary_sample_tmpbis)
                                                            if (current_length_step_count_id == (current_length_step_count - 1)) {
                                                              private$data_selected[[full_trip_id]][[partial_trip_id]]$.__enclos_env__$private$wells[[well_id]]$.__enclos_env__$private$elementarysampleraw[[sample_id]] <- append(private$data_selected[[full_trip_id]][[partial_trip_id]]$.__enclos_env__$private$wells[[well_id]]$.__enclos_env__$private$elementarysampleraw[[sample_id]],
                                                                                                                                                                                                                                   current_elementary_sample_tmp)
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                  if (length(elementary_sample_skj_removed) != 0) {
                                                    warning(format(Sys.time(),
                                                                   "%Y-%m-%d %H:%M:%S"),
                                                            " - ",
                                                            length(x = elementary_sample_skj_removed),
                                                            " elementary sample(s) with length class measured in LD1 for SKJ specie detected. Sample associated not usable and removed for next process.\n",
                                                            "[trip_id: ",
                                                            " (full trip item id ",
                                                            full_trip_id,
                                                            ", trip item id ",
                                                            partial_trip_id,
                                                            "), well_id: ",
                                                            current_well$.__enclos_env__$private$well_id,
                                                            " (well item id ",
                                                            well_id,
                                                            "), sample_id: ",
                                                            unique(unlist(current_sample$extract_l1_element_value(element = "sample_id"))),
                                                            " (sample item id ",
                                                            sample_id,
                                                            ")]")
                                                    current_samples_removed <- append(current_samples_removed,
                                                                                      sample_id)
                                                  }
                                                }
                                                if (length(x = current_samples_removed) != 0) {
                                                  for (sample_remove_id in current_samples_removed) {
                                                    private$data_selected[[full_trip_id]][[partial_trip_id]]$.__enclos_env__$private$wells[[well_id]]$.__enclos_env__$private$elementarysampleraw[[sample_remove_id]] <- NULL
                                                  }
                                                }
                                              }
                                            }
                                          } else {
                                            warning(format(Sys.time(),
                                                           "%Y-%m-%d %H:%M:%S"),
                                                    " - Well(s) detected with no elementary catch associated to the trip.\n",
                                                    "[trip_id: ",
                                                    current_trip$.__enclos_env__$private$trip_id,
                                                    " (full trip item id ",
                                                    full_trip_id,
                                                    ", trip item id ",
                                                    partial_trip_id,
                                                    "), well_id: ",
                                                    paste(unlist(current_wells$extract_l1_element_value(element = "well_id")),
                                                          collapse = ", "),
                                                    "]")
                                          }
                                        }
                                      } else {
                                        if (length(x = current_trip$.__enclos_env__$private$wells) != 0) {
                                          capture.output(current_wells <- object_r6(class_name = "wells"),
                                                         file = "NUL")
                                          capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                         file = "NUL")
                                          current_elementarysamplesraw <- unlist(current_wells$extract_l1_element_value(element = "elementarysampleraw"))
                                          if (length(x = current_elementarysamplesraw) != 0) {
                                            warning(format(Sys.time(),
                                                           "%Y-%m-%d %H:%M:%S"),
                                                    " - Sample(s) detected without any activity associated.\n",
                                                    "[trip_id: ",
                                                    current_trip$.__enclos_env__$private$trip_id,
                                                    " (full trip item id ",
                                                    full_trip_id,
                                                    ", trip item id ",
                                                    partial_trip_id,
                                                    ")]")
                                          }
                                        }
                                      }
                                    }
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Process 2.1 successfull on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                  }
                                }
                                # 15.3 - Outputs extraction ----
                                # outputs manipulation
                                if (! is.null(x = global_output_path)) {
                                  full_trips_selected <- private$data_selected
                                  capture.output(trips_selected <- object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                                 file = "NUL")
                                  capture.output(wells_selected <- object_r6(class_name = "wells"),
                                                 file = "NUL")
                                  capture.output(wells_selected$add(new_item = unlist(x = trips_selected$extract_l1_element_value(element = "wells"))),
                                                 file = "NUL")
                                  capture.output(elementarysamplesraw_selected <- object_r6(class_name = "elementarysamplesraw"),
                                                 file = "NUL")
                                  capture.output(elementarysamplesraw_selected$add(new_item = unlist(x = wells_selected$extract_l1_element_value(element = "elementarysampleraw"))),
                                                 file = "NUL")
                                  outputs_process_2_1_trips <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                         FUN = function(full_trip_id) {
                                                                                                           if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                             return(rep(x = full_trip_id,
                                                                                                                        length(x = full_trips_selected[[full_trip_id]])))
                                                                                                           } else {
                                                                                                             return(full_trip_id)
                                                                                                           }
                                                                                                         })),
                                                                          "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                           FUN = function(full_trip_id) {
                                                                                                             if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                               return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                          length(x = full_trips_selected[[full_trip_id]])))
                                                                                                             } else {
                                                                                                               return(names(x = full_trips_selected[full_trip_id]))
                                                                                                             }
                                                                                                           })),
                                                                          "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                          "trip_end_date" = do.call("c",
                                                                                                    trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                          "year_trip_end_date" = sapply(do.call("c",
                                                                                                                trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                        lubridate::year),
                                                                          "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                          "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))))
                                  outputs_process_2_1_elementarysamplesraw <- data.frame("trip_id" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "trip_id")),
                                                                                         "well_id" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "well_id")),
                                                                                         "sample_id" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "sample_id")),
                                                                                         "sub_sample_id" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "sub_sample_id")),
                                                                                         "elementarysampleraw_id" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "elementarysampleraw_id")),
                                                                                         "species_fao_code" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "species_fao_code")),
                                                                                         "sample_length_class" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "sample_length_class")),
                                                                                         "sample_number_measured" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "sample_number_measured")),
                                                                                         "sample_length_class_lf" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "sample_length_class_lf")),
                                                                                         "sample_number_measured_lf" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "sample_number_measured_lf")))
                                  outputs_process_2_1 <- outputs_process_2_1_elementarysamplesraw %>%
                                    dplyr::left_join(outputs_process_2_1_trips,
                                                     by = "trip_id") %>%
                                    dplyr::relocate(full_trip_id,
                                                    full_trip_name,
                                                    trip_id,
                                                    trip_end_date,
                                                    year_trip_end_date,
                                                    vessel_code,
                                                    vessel_type_code)
                                  # extraction
                                  if (output_format == "us") {
                                    outputs_dec <- "."
                                    outputs_sep <- ","
                                  } else if (output_format == "eu") {
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  } else {
                                    warning(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Wrong outputs format define, European format will be applied.")
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  }
                                  write.table(x = outputs_process_2_1,
                                              file = file.path(global_output_path,
                                                               "level2",
                                                               "data",
                                                               "process_2_1.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Outputs extracted in the following directory:\n",
                                      file.path(global_output_path,
                                                "level2",
                                                "data"), "\n",
                                      sep = "")
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - End process 2.1 sample length class conversion ld1 to lf.\n")
                              }
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 16 - Process 2.2: sample number measured extrapolation ----
                            #' @description Process for sample number measured individuals extrapolation to sample number individuals counted.
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction egual TRUE. Path of the global outputs directory. The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            sample_number_measured_extrapolation = function(global_output_path = NULL,
                                                                            output_format = "eu") {
                              # 16.1 - Arguments verification ----
                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              # 16.2 - Global process ----
                              if (is.null(x = private$data_selected)) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Empty data selected in the R6 object. Process 2.2 (sample number measured extrapolation) cancelled.")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.2: sample number measured extrapolation.\n")
                                  }
                                  if (names(x = private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    message(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Warning on item \"",
                                            names(x = private$data_selected)[full_trip_id],
                                            "\": full trip avoided because a least one trip inside is missing.\n",
                                            "[trip: ",
                                            private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                            "]")
                                    capture.output(current_trips <- object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                      capture.output(current_wells <- object_r6(class_name = "wells"),
                                                     file = "NUL")
                                      capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                     file = "NUL")
                                      if (length(x = unlist(current_wells$extract_l1_element_value(element = "elementarysampleraw"))) != 0) {
                                        capture.output(current_elementarysamplesraw <- object_r6(class_name = "elementarysamplesraw"),
                                                       file = "NUL")
                                        capture.output(current_elementarysamplesraw$add(new_item = unlist(current_wells$extract_l1_element_value(element = "elementarysampleraw"))),
                                                       file = "NUL")
                                        current_elementarysamplesraw$modification_l1(modification = "$path$rf4 <- NA_real_")
                                        current_elementarysamplesraw$modification_l1(modification = "$path$sample_number_measured_extrapolated_lf <- NA")
                                      }
                                    }
                                  } else {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.2 on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                    for (partial_trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                      if (length(x = current_trip$.__enclos_env__$private$wells) != 0) {
                                        capture.output(current_wells <- object_r6(class_name = "wells"),
                                                       file = "NUL")
                                        capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                       file = "NUL")
                                        for (well_id in seq_len(length.out = current_wells$count())) {
                                          current_well <- current_wells$extract(id = well_id)[[1]]
                                          if (length(x = current_well$.__enclos_env__$private$elementarysampleraw) != 0) {
                                            capture.output(current_samples <- object_r6(class_name = "elementarysamplesraw"),
                                                           file = "NUL")
                                            capture.output(current_samples$add(new_item = current_well$.__enclos_env__$private$elementarysampleraw),
                                                           file = "NUL")
                                            for (sample_id in seq_len(length.out = current_samples$count())) {
                                              capture.output(current_sample <- object_r6(class_name = "elementarysamplesraw"),
                                                             file = "NUL")
                                              capture.output(current_sample$add(new_item = current_samples$extract(id = sample_id)),
                                                             file = "NUL")
                                              if (any(unlist(x = lapply(X = current_sample$extract_l1_element_value(element = "sample_number_measured_lf"),
                                                                        FUN = is.null)))) {
                                                stop(format(Sys.time(),
                                                            "%Y-%m-%d %H:%M:%S"),
                                                     " - Run process 2.1 (sample length class conversion ld1 to lf) before this process.")
                                              }
                                              for (sub_sample_id in unique(unlist(current_sample$extract_l1_element_value(element = "sub_sample_id")))) {
                                                capture.output(current_sub_sample <- object_r6(class_name = "elementarysamplesraw"),
                                                               file = "NUL")
                                                capture.output(current_sub_sample$add(new_item = current_sample$filter_l1(filter = paste0("$path$sub_sample_id == ",
                                                                                                                                          sub_sample_id))),
                                                               file = "NUL")
                                                for (sample_specie_id in unique(unlist(current_sub_sample$extract_l1_element_value(element = "species_fao_code")))) {
                                                  capture.output(current_sub_sample_species <- object_r6(class_name = "elementarysamplesraw"),
                                                                 file = "NUL")
                                                  capture.output(current_sub_sample_species$add(new_item = current_sub_sample$filter_l1(filter = paste0("$path$species_fao_code == \"",
                                                                                                                                                        sample_specie_id,
                                                                                                                                                        "\""))),
                                                                 file = "NUL")
                                                  sum_sub_sample_specie_number_measured_lf <- sum(unlist(current_sub_sample_species$extract_l1_element_value(element = "sample_number_measured_lf")),
                                                                                                  na.rm = TRUE)
                                                  sum_sub_sample_specie_total_count <- 0
                                                  for (sub_sample_total_count_id in unique(unlist(current_sub_sample_species$extract_l1_element_value(element = "sub_sample_total_count_id")))) {
                                                    capture.output(current_sub_sample_species_total_count <- object_r6(class_name = "elementarysamplesraw"),
                                                                   file = "NUL")
                                                    capture.output(current_sub_sample_species_total_count$add(new_item = current_sub_sample_species$filter_l1(filter = paste0("$path$sub_sample_total_count_id == \"",
                                                                                                                                                                              sub_sample_total_count_id,
                                                                                                                                                                              "\""))),
                                                                   file = "NUL")
                                                    sum_sub_sample_specie_total_count <- sum_sub_sample_specie_total_count + unique(unlist(current_sub_sample_species_total_count$extract_l1_element_value(element = "sample_total_count")))
                                                  }
                                                  rf4 <- sum_sub_sample_specie_total_count / sum_sub_sample_specie_number_measured_lf
                                                  # rf4 verification
                                                  # pint console precision 7 digits by default
                                                  options(digits=9)
                                                  if (round(rf4,10) != 1 & sample_specie_id != "SKJ") {
                                                    warning(format(Sys.time(),
                                                                   "%Y-%m-%d %H:%M:%S"),
                                                            " - Rf4 not egal to 1 (",
                                                            rf4,
                                                            ") for sampled specie different from SKJ.\n",
                                                            "[trip: ",
                                                            current_trip$.__enclos_env__$private$trip_id,
                                                            " (full trip item id ",
                                                            full_trip_id,
                                                            ", trip item id ",
                                                            partial_trip_id,
                                                            "), well: ",
                                                            current_well$.__enclos_env__$private$well_id,
                                                            " (well item id ",
                                                            well_id,
                                                            "), sample: ",
                                                            current_sample$extract(id = 1)[[1]]$.__enclos_env__$private$sample_id,
                                                            " (sample item id ",
                                                            sample_id,
                                                            "), sub sample: ",
                                                            current_sub_sample$extract(id = 1)[[1]]$.__enclos_env__$private$sub_sample_id,
                                                            " (sub sample item id ",
                                                            sub_sample_id,
                                                            "), specie: ",
                                                            current_sub_sample_species$extract(id = 1)[[1]]$.__enclos_env__$private$species_fao_code,
                                                            " (species item id ",
                                                            sample_specie_id,
                                                            ")]")
                                                  } else if (rf4 < 1) {
                                                    warning(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                            " - Rf4 inferior to 1 (",
                                                            rf4,
                                                            ").\n",
                                                            "[trip: ",
                                                            current_trip$.__enclos_env__$private$trip_id,
                                                            " (full trip item id ",
                                                            full_trip_id,
                                                            ", trip item id ",
                                                            partial_trip_id,
                                                            "), well: ",
                                                            current_well$.__enclos_env__$private$well_id,
                                                            " (well item id ",
                                                            well_id,
                                                            "), sample: ",
                                                            current_sample$extract(id = 1)[[1]]$.__enclos_env__$private$sample_id,
                                                            " (sample item id ",
                                                            sample_id,
                                                            "), sub sample: ",
                                                            current_sub_sample$extract(id = 1)[[1]]$.__enclos_env__$private$sub_sample_id,
                                                            " (sub sample item id ",
                                                            sub_sample_id,
                                                            "), specie: ",
                                                            current_sub_sample_species$extract(id = 1)[[1]]$.__enclos_env__$private$species_fao_code,
                                                            " (species item id ",
                                                            sample_specie_id,
                                                            "]")
                                                  }
                                                  current_sub_sample_species$modification_l1(modification = paste0("$path$rf4 <- ",
                                                                                                                   rf4))
                                                  current_sub_sample_species$modification_l1(modification = paste0("$path$sample_number_measured_extrapolated_lf <- $path$sample_number_measured_lf * ",
                                                                                                                   rf4))
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }

                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Process 2.2 successfull on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                  }
                                }
                                # 16.3 - Outputs extraction ----
                                # outputs manipulation
                                if (! is.null(x = global_output_path)) {
                                  full_trips_selected <- private$data_selected
                                  capture.output(trips_selected <- object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                                 file = "NUL")
                                  capture.output(wells_selected <- object_r6(class_name = "wells"),
                                                 file = "NUL")
                                  capture.output(wells_selected$add(new_item = unlist(x = trips_selected$extract_l1_element_value(element = "wells"))),
                                                 file = "NUL")
                                  capture.output(elementarysamplesraw_selected <- object_r6(class_name = "elementarysamplesraw"),
                                                 file = "NUL")
                                  capture.output(elementarysamplesraw_selected$add(new_item = unlist(x = wells_selected$extract_l1_element_value(element = "elementarysampleraw"))),
                                                 file = "NUL")
                                  outputs_process_2_2_trips <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                         FUN = function(full_trip_id) {
                                                                                                           if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                             return(rep(x = full_trip_id,
                                                                                                                        length(x = full_trips_selected[[full_trip_id]])))
                                                                                                           } else {
                                                                                                             return(full_trip_id)
                                                                                                           }
                                                                                                         })),
                                                                          "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                           FUN = function(full_trip_id) {
                                                                                                             if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                               return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                          length(x = full_trips_selected[[full_trip_id]])))
                                                                                                             } else {
                                                                                                               return(names(x = full_trips_selected[full_trip_id]))
                                                                                                             }
                                                                                                           })),
                                                                          "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                          "trip_end_date" = do.call("c",
                                                                                                    trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                          "year_trip_end_date" = sapply(do.call("c",
                                                                                                                trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                        lubridate::year),
                                                                          "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                          "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))))
                                  outputs_process_2_2_elementarysamplesraw <- data.frame("trip_id" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "trip_id")),
                                                                                         "well_id" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "well_id")),
                                                                                         "sample_id" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "sample_id")),
                                                                                         "sub_sample_id" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "sub_sample_id")),
                                                                                         "sub_sample_total_count_id" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "sub_sample_total_count_id")),
                                                                                         "elementarysampleraw_id" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "elementarysampleraw_id")),
                                                                                         "species_fao_code" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "species_fao_code")),
                                                                                         "sample_length_class_lf" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "sample_length_class_lf")),
                                                                                         "sample_number_measured_lf" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "sample_number_measured_lf")),
                                                                                         "sample_total_count" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "sample_total_count")),
                                                                                         "sample_number_measured_extrapolated_lf" = unlist(x = elementarysamplesraw_selected$extract_l1_element_value(element = "sample_number_measured_extrapolated_lf")))
                                  outputs_process_2_2 <- outputs_process_2_2_elementarysamplesraw %>%
                                    dplyr::left_join(outputs_process_2_2_trips,
                                                     by = "trip_id") %>%
                                    dplyr::relocate(full_trip_id,
                                                    full_trip_name,
                                                    trip_id,
                                                    trip_end_date,
                                                    year_trip_end_date,
                                                    vessel_code,
                                                    vessel_type_code)
                                  # extraction
                                  if (output_format == "us") {
                                    outputs_dec <- "."
                                    outputs_sep <- ","
                                  } else if (output_format == "eu") {
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  } else {
                                    warning(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Wrong outputs format define, European format will be applied.")
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  }
                                  write.table(x = outputs_process_2_2,
                                              file = file.path(global_output_path,
                                                               "level2",
                                                               "data",
                                                               "process_2_2.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Outputs extracted in the following directory:\n",
                                      file.path(global_output_path,
                                                "level2",
                                                "data"), "\n",
                                      sep = "")
                                }
                                cat(format(x = Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - End process 2.2: sample number measured extrapolation.\n")
                              }
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 17 - Process 2.3: sample_length_class_step_standardisation ----
                            #' @description Process for step standardisation of lf length class.
                            #' @param maximum_lf_class Object of type \code{\link[base]{integer}} expected. Theorical maximum lf class that can occur (all species considerated). By default 500.
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction egual TRUE. Path of the global outputs directory. The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            sample_length_class_step_standardisation = function(maximum_lf_class = as.integer(500),
                                                                                global_output_path = NULL,
                                                                                output_format = "eu") {
                              # 17.1 - Arguments verification ----
                              codama::r_type_checking(r_object = maximum_lf_class,
                                                      type = "integer",
                                                      length = 1L)
                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              # 17.2 - Global process ----
                              if (is.null(x = private$data_selected)) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Empty data selected in the R6 object. Process 2.3 (sample length class step standardisation) cancelled.")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.3: sample length class step standardisation.\n")
                                  }
                                  if (names(x = private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    # full trip is not complete (missing at least one trip)
                                    message(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Warning on item \"",
                                            names(x = private$data_selected)[full_trip_id],
                                            "\": full trip avoided because a least one trip inside is missing.\n",
                                            "[trip: ",
                                            private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                            "]")
                                    capture.output(current_trips <- object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                      capture.output(current_wells <- object_r6(class_name = "wells"),
                                                     file = "NUL")
                                      capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                     file = "NUL")
                                      if (length(x = unlist(current_wells$extract_l1_element_value(element = "elementarysampleraw"))) != 0) {
                                        current_wells$modification_l1(modification = "$path$elementarysample <- NA")
                                      }
                                    }
                                  } else {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.3 on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                    for (partial_trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                      if (length(x = current_trip$.__enclos_env__$private$wells) != 0) {
                                        capture.output(current_wells <- object_r6(class_name = "wells"),
                                                       file = "NUL")
                                        capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                       file = "NUL")
                                        for (well_id in seq_len(length.out = current_wells$count())) {
                                          current_well <- current_wells$extract(id = well_id)[[1]]
                                          if (length(x = current_well$.__enclos_env__$private$elementarysampleraw) != 0) {
                                            capture.output(current_samples <- object_r6(class_name = "elementarysamplesraw"),
                                                           file = "NUL")
                                            capture.output(current_samples$add(new_item = unlist(x = current_well$.__enclos_env__$private$elementarysampleraw)),
                                                           file = "NUL")
                                            capture.output(current_elementarysamples <- object_r6(class_name = "elementarysamples"),
                                                           file = "NUL")
                                            for (sample_id in unique(x = unlist(x = current_samples$extract_l1_element_value(element = "sample_id")))) {
                                              capture.output(current_sample <- object_r6(class_name = "elementarysamplesraw"),
                                                             file = "NUL")
                                              capture.output(current_sample$add(new_item = current_samples$filter_l1(filter = paste0("$path$sample_id == \"",
                                                                                                                                     sample_id,
                                                                                                                                     "\""))),
                                                             file = "NUL")
                                              sample_species <- unique(x = unlist(x = current_sample$extract_l1_element_value(element = "species_fao_code")))
                                              current_sample_by_species <- vector(mode = "list",
                                                                                  length = length(x = sample_species))
                                              for (specie_id in seq_len(length.out = length(x = sample_species))) {
                                                current_sample_by_species[[specie_id]] <- current_sample$filter_l1(filter = paste0("$path$species_fao_code == \"",
                                                                                                                                   sample_species[specie_id],
                                                                                                                                   "\""))
                                              }
                                              for (sample_id_specie in seq_len(length.out = length(x = sample_species))) {
                                                capture.output(current_sample_specie <- object_r6(class_name = "elementarysamplesraw"),
                                                               file = "NUL")
                                                capture.output(current_sample_specie$add(new_item = current_sample_by_species[[sample_id_specie]]),
                                                               file = "NUL")
                                                sample_length_class_lf <- sort(x = unique(x = unlist(x = current_sample_specie$extract_l1_element_value(element = "sample_length_class_lf"))))
                                                if (sample_species[sample_id_specie] %in% c("SKJ", "LTA", "FRI")) {
                                                  step <- 1
                                                } else if (sample_species[sample_id_specie] %in% c("YFT", "BET", "ALB")) {
                                                  step <- 2
                                                } else {
                                                  step <- NA
                                                }
                                                if (is.na(x = step)) {
                                                  for (elementarysamplesraw_id in seq_len(length.out = current_sample_specie$count())) {
                                                    object_elementarysample <- elementarysample$new(trip_id = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$trip_id,
                                                                                                    well_id = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$well_id,
                                                                                                    sample_id = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$sample_id,
                                                                                                    sub_sample_id = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$sub_sample_id,
                                                                                                    sample_quality_code = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$sample_quality_code,
                                                                                                    sample_type_code = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$sample_type_code,
                                                                                                    species_fao_code = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$species_fao_code,
                                                                                                    sample_standardised_length_class_lf = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$sample_length_class_lf,
                                                                                                    sample_number_measured_extrapolated_lf = as.numeric(current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$sample_number_measured_extrapolated_lf),
                                                                                                    sample_total_count = as.integer(current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$sample_total_count))
                                                    capture.output(current_elementarysamples$add(new_item = object_elementarysample),
                                                                   file = "NUL")
                                                  }
                                                } else {
                                                  lower_border_reference <- seq(from = 0,
                                                                                to = maximum_lf_class - 1,
                                                                                by = step)
                                                  upper_border_reference <- seq(from = step,
                                                                                to = maximum_lf_class,
                                                                                by = step)
                                                  sample_length_class_lf_id <- 1
                                                  while (sample_length_class_lf_id <= length(sample_length_class_lf)) {
                                                    lower_border <- as.integer(dplyr::last(x = lower_border_reference[which(lower_border_reference <= trunc(sample_length_class_lf[sample_length_class_lf_id]))]))
                                                    upper_border <- as.integer(dplyr::first(x = upper_border_reference[which(upper_border_reference > trunc(sample_length_class_lf[sample_length_class_lf_id]))]))
                                                    sample_length_class_lf_for_merge <- sample_length_class_lf[which(sample_length_class_lf >= lower_border
                                                                                                                     & sample_length_class_lf < upper_border)]
                                                    capture.output(current_sample_specie_by_step <- object_r6(class_name = "elementarysamplesraw"),
                                                                   file = "NUL")
                                                    capture.output(current_sample_specie_by_step$add(new_item = current_sample_specie$filter_l1(filter = paste0("$path$sample_length_class_lf %in% c(",
                                                                                                                                                                paste0(sample_length_class_lf_for_merge,
                                                                                                                                                                       collapse = ", "),
                                                                                                                                                                ")"))),
                                                                   file = "NUL")
                                                    current_sample_specie_by_step_subid <- unique(x = unlist(x = current_sample_specie_by_step$extract_l1_element_value(element = "sub_sample_id")))
                                                    for (sub_sample_id in current_sample_specie_by_step_subid) {
                                                      capture.output(current_sample_specie_by_step_by_subid <- object_r6(class_name = "elementarysamplesraw"),
                                                                     file = "NUL")
                                                      capture.output(current_sample_specie_by_step_by_subid$add(new_item = current_sample_specie_by_step$filter_l1(filter = paste0("$path$sub_sample_id == ",
                                                                                                                                                                                   sub_sample_id))),
                                                                     file = "NUL")
                                                      object_elementarysample <- elementarysample$new(trip_id = current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$trip_id,
                                                                                                      well_id = current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$well_id,
                                                                                                      sample_id = current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$sample_id,
                                                                                                      sub_sample_id = current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$sub_sample_id,
                                                                                                      sample_quality_code = current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$sample_quality_code,
                                                                                                      sample_type_code = current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$sample_type_code,
                                                                                                      species_fao_code = current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$species_fao_code,
                                                                                                      sample_standardised_length_class_lf = lower_border,
                                                                                                      sample_number_measured_extrapolated_lf = sum(unlist(current_sample_specie_by_step_by_subid$extract_l1_element_value(element = "sample_number_measured_extrapolated_lf"))),
                                                                                                      sample_total_count = as.integer(current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$sample_total_count))
                                                      capture.output(current_elementarysamples$add(new_item = object_elementarysample),
                                                                     file = "NUL")
                                                    }
                                                    sample_length_class_lf_id <- sample_length_class_lf_id + length(x = sample_length_class_lf_for_merge)
                                                  }
                                                }
                                              }
                                            }
                                            current_well$.__enclos_env__$private$elementarysample <- current_elementarysamples
                                          } else {
                                            current_well$.__enclos_env__$private$elementarysample <- NA
                                          }
                                        }
                                      }
                                    }
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Process 2.3 successfull on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                  }
                                }
                                # 17.3 - Outputs extraction ----
                                # outputs manipulation
                                if (! is.null(x = global_output_path)) {
                                  full_trips_selected <- private$data_selected
                                  capture.output(trips_selected <- object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                                 file = "NUL")
                                  capture.output(wells_selected <- object_r6(class_name = "wells"),
                                                 file = "NUL")
                                  capture.output(wells_selected$add(new_item = unlist(x = trips_selected$extract_l1_element_value(element = "wells"))),
                                                 file = "NUL")
                                  capture.output(elementarysamples_selected <- object_r6(class_name = "elementarysamples"),
                                                 file = "NUL")
                                  capture.output(elementarysamples_selected$add(new_item = unlist(lapply(X = seq_len(length.out = length(x = wells_selected$extract_l1_element_value(element = "elementarysample"))),
                                                                                                         FUN = function(elementarysample_id) {
                                                                                                           if (paste(class(x = wells_selected$extract_l1_element_value(element = "elementarysample")[[elementarysample_id]]),
                                                                                                                     collapse = " ") != "logical") {
                                                                                                             wells_selected$extract_l1_element_value(element = "elementarysample")[[elementarysample_id]]$extract()
                                                                                                           }
                                                                                                         }))),
                                                 file = "NUL")
                                  outputs_process_2_3_trips <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                         FUN = function(full_trip_id) {
                                                                                                           if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                             return(rep(x = full_trip_id,
                                                                                                                        length(x = full_trips_selected[[full_trip_id]])))
                                                                                                           } else {
                                                                                                             return(full_trip_id)
                                                                                                           }
                                                                                                         })),
                                                                          "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                           FUN = function(full_trip_id) {
                                                                                                             if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                               return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                          length(x = full_trips_selected[[full_trip_id]])))
                                                                                                             } else {
                                                                                                               return(names(x = full_trips_selected[full_trip_id]))
                                                                                                             }
                                                                                                           })),
                                                                          "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                          "trip_end_date" = do.call("c",
                                                                                                    trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                          "year_trip_end_date" = sapply(do.call("c",
                                                                                                                trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                        lubridate::year),
                                                                          "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                          "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))))
                                  outputs_process_2_3_elementarysamples <- data.frame("trip_id" = unlist(x = elementarysamples_selected$extract_l1_element_value(element = "trip_id")),
                                                                                      "well_id" = unlist(x = elementarysamples_selected$extract_l1_element_value(element = "well_id")),
                                                                                      "sample_id" = unlist(x = elementarysamples_selected$extract_l1_element_value(element = "sample_id")),
                                                                                      "sample_type_code" = unlist(x = elementarysamples_selected$extract_l1_element_value(element = "sample_type_code")),
                                                                                      "sample_quality_code" = unlist(x = elementarysamples_selected$extract_l1_element_value(element = "sample_quality_code")),
                                                                                      "sub_sample_id" = unlist(x = elementarysamples_selected$extract_l1_element_value(element = "sub_sample_id")),
                                                                                      "species_fao_code" = unlist(x = elementarysamples_selected$extract_l1_element_value(element = "species_fao_code")),
                                                                                      "sample_total_count" = unlist(x = elementarysamples_selected$extract_l1_element_value(element = "sample_total_count")),
                                                                                      "sample_standardised_length_class_lf" = unlist(x = elementarysamples_selected$extract_l1_element_value(element = "sample_standardised_length_class_lf")),
                                                                                      "sample_number_measured_extrapolated_lf" = unlist(x = elementarysamples_selected$extract_l1_element_value(element = "sample_number_measured_extrapolated_lf")))
                                  outputs_process_2_3 <- outputs_process_2_3_elementarysamples %>%
                                    dplyr::left_join(outputs_process_2_3_trips,
                                                     by = "trip_id") %>%
                                    dplyr::relocate(full_trip_id,
                                                    full_trip_name,
                                                    trip_id,
                                                    trip_end_date,
                                                    year_trip_end_date,
                                                    vessel_code,
                                                    vessel_type_code)
                                  # extraction
                                  if (output_format == "us") {
                                    outputs_dec <- "."
                                    outputs_sep <- ","
                                  } else if (output_format == "eu") {
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  } else {
                                    warning(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Wrong outputs format define, European format will be applied\n",
                                            sep = "")
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  }
                                  write.table(x = outputs_process_2_3,
                                              file = file.path(global_output_path,
                                                               "level2",
                                                               "data",
                                                               "process_2_3.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Outputs extracted in the following directory:\n",
                                      file.path(global_output_path,
                                                "level2",
                                                "data"), "\n",
                                      sep = "")
                                }
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - End process 2.3: sample length class step standardisation.\n")
                              }
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 18 - Process 2.4: well_set_weigth_categories ----
                            #' @description Process for well set weigth categories definition.
                            #' @param sample_set Object of type \code{\link[base]{data.frame}} expected. Data frame object with weighted weigh of each set sampled.
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction egual TRUE. Path of the global outputs directory. The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            #' @param referential_template Object of class \code{\link[base]{character}} expected. By default "observe". Referential template selected (for example regarding the activity_code). You can switch to "avdth".
                            well_set_weigth_categories = function(sample_set,
                                                                  global_output_path = NULL,
                                                                  output_format = "eu",
                                                                  referential_template = "observe") {
                              # 18.1 - Arguments verification ----
                              if (! paste0(class(x = sample_set),
                                           collapse = "_") %in% c("data.frame",
                                                                  "tbl_df_tbl_data.frame")
                                  || ncol(x = sample_set) != 5
                                  || nrow(x = sample_set) == 0) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Invalid \"sample_set\" argument, class \"data.frame\" or \"tibble\" with 5 columns and at least 1 row expected.")
                              }
                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              codama::r_type_checking(r_object = referential_template,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("observe",
                                                                        "avdth"))
                              # 18.2 - Global process ----
                              if (is.null(x = private$data_selected)) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Empty data selected in the R6 object. Process 2.4 (well-set weight categories definition) cancelled.")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(x = private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.4: well-set weight categories definition.\n")
                                  }
                                  cat(format(Sys.time(),
                                             "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 2.4 on item \"",
                                      names(private$data_selected)[full_trip_id],
                                      "\".\n",
                                      "[trip: ",
                                      private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n", sep="")
                                  if (names(x = private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    message(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Warning on item \"",
                                            names(x = private$data_selected)[full_trip_id],
                                            "\": full trip avoided because a least one trip inside is missing.\n",
                                            "[trip: ",
                                            private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                            "]")
                                    capture.output(current_trips <- object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                      capture.output(current_wells <- object_r6(class_name = "wells"),
                                                     file = "NUL")
                                      capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                     file = "NUL")
                                      current_wells$modification_l1(modification = "$path$wellsets <- NA")
                                    }
                                  } else {
                                    for (partial_trip_id in seq_len(length.out = length(x = private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                      if (current_trip$.__enclos_env__$private$vessel_type_code %in% as.integer(x = c(4, 5, 6))) {
                                        if (length(x = current_trip$.__enclos_env__$private$wells) != 0) {
                                          capture.output(current_wells <- object_r6(class_name = "wells"),
                                                         file = "NUL")
                                          capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                         file = "NUL")
                                          wells_activities_samples_id <- vector(mode = "list",
                                                                                length = current_wells$count())
                                          for (well_id in seq_len(length.out = current_wells$count())) {
                                            current_well <- current_wells$extract(id = well_id)[[1]]
                                            if (length(x = current_well$.__enclos_env__$private$wellplan) != 0) {
                                              capture.output(current_well_plans <- object_r6(class_name = "elementarywellplans"),
                                                             file = "NUL")
                                              capture.output(current_well_plans$add(new_item = current_well$.__enclos_env__$private$wellplan),
                                                             file = "NUL")
                                              activities_id <- unique(x = unlist(x = current_well_plans$extract_l1_element_value(element = "activity_id")))
                                              wells_activities_samples_id[[well_id]][[1]] <- activities_id
                                            } else {
                                              wells_activities_samples_id[[well_id]][[1]] <- "no_well_plan_available"
                                            }
                                            if (length(x = current_well$.__enclos_env__$private$elementarysampleraw) != 0) {
                                              capture.output(current_elementarysamplesraw <- object_r6(class_name = "elementarysamplesraw"),
                                                             file = "NUL")
                                              capture.output(current_elementarysamplesraw$add(new_item = unlist(x = current_well$.__enclos_env__$private$elementarysampleraw)),
                                                             file = "NUL")
                                              samples_id <- unique(unlist(current_elementarysamplesraw$extract_l1_element_value(element = "sample_id")))
                                              wells_activities_samples_id[[well_id]][[2]] <- samples_id
                                            } else {
                                              wells_activities_samples_id[[well_id]][[2]] <- "well_not_sampled"
                                            }
                                          }
                                          for (well_id in seq_len(length.out = current_wells$count())) {
                                            current_well <- current_wells$extract(id = well_id)[[1]]
                                            # information from the well plan
                                            # do we have a well plan associated to the current well ?
                                            if (length(x = current_well$.__enclos_env__$private$wellplan) != 0) {
                                              # yes
                                              capture.output(current_well_plans <- object_r6(class_name = "elementarywellplans"),
                                                             file = "NUL")
                                              capture.output(current_well_plans$add(new_item = current_well$.__enclos_env__$private$wellplan),
                                                             file = "NUL")
                                              # calcul of proportion of minus and plus 10 kg
                                              current_wellplan_weigth_category <- unique(x = unlist(x = current_well_plans$extract_l1_element_value(element = "weight_category_code")))
                                              if (referential_template == "observe") {
                                                current_wellplan_weigth_category <- stringr::str_extract(string = current_wellplan_weigth_category,
                                                                                                         pattern = "[:digit:]+$")
                                              }
                                              current_wellplan_weigth_category <- as.integer(x = current_wellplan_weigth_category)
                                              well_prop_minus10_weigth <- 0
                                              well_prop_plus10_weigth <- 0
                                              well_prop_global_weigth <- 0
                                              if (! any(current_wellplan_weigth_category %in% as.integer(x = c(8, 9)))) {
                                                for (well_plan_id in seq_len(length.out = current_well_plans$count())) {
                                                  current_well_plan <- current_well_plans$extract(id = well_plan_id)[[1]]
                                                  current_well_plan_weight_category_code <- if (referential_template == "observe") {as.integer(x = stringr::str_extract(string = current_well_plan$.__enclos_env__$private$weight_category_code,
                                                                                                                                                                        pattern = "[:digit:]+$"))} else {as.integer(x = current_well_plan$.__enclos_env__$private$weight_category_code)}
                                                  if (current_well_plan_weight_category_code == 1L) {
                                                    well_prop_minus10_weigth <- well_prop_minus10_weigth + current_well_plan$.__enclos_env__$private$wellplan_weight
                                                    well_prop_global_weigth <- well_prop_global_weigth + current_well_plan$.__enclos_env__$private$wellplan_weight
                                                  } else if (current_well_plan_weight_category_code == 2L) {
                                                    well_prop_plus10_weigth <- well_prop_plus10_weigth + current_well_plan$.__enclos_env__$private$wellplan_weight
                                                    well_prop_global_weigth <- well_prop_global_weigth + current_well_plan$.__enclos_env__$private$wellplan_weight
                                                  } else {
                                                    stop(format(Sys.time(),
                                                                "%Y-%m-%d %H:%M:%S"),
                                                         " - Well plan weight category unknown.\n",
                                                         "[trip: ",
                                                         current_well$.__enclos_env__$private$trip_id,
                                                         ", well: ",
                                                         current_well$.__enclos_env__$private$well_id,
                                                         "]")
                                                  }
                                                }
                                              } else {
                                                well_prop_minus10_weigth <- NA_real_
                                                well_prop_plus10_weigth <- NA_real_
                                                for (well_plan_id in seq_len(length.out = current_well_plans$count())) {
                                                  current_well_plan <- current_well_plans$extract(id = well_plan_id)[[1]]
                                                  well_prop_global_weigth <- well_prop_global_weigth + current_well_plan$.__enclos_env__$private$wellplan_weight
                                                }
                                              }



                                              current_well$.__enclos_env__$private$well_prop_minus10_weigth <- well_prop_minus10_weigth / well_prop_global_weigth
                                              current_well$.__enclos_env__$private$well_prop_plus10_weigth <- well_prop_plus10_weigth / well_prop_global_weigth
                                              capture.output(current_well_sets <- object_r6(class_name = "wellsets"),
                                                             file = "NUL")
                                              # do we have more than one well associated to the trip ?
                                              if (length(x = wells_activities_samples_id) == 1) {
                                                # no, one unique well
                                                for (activity_id in wells_activities_samples_id[[1]][[1]]) {
                                                  current_weighted_weight <- sum(sapply(X = seq_len(length.out = current_well_plans$count()),
                                                                                        FUN = function(s) {
                                                                                          if (current_well_plans$extract(id = s)[[1]]$.__enclos_env__$private$activity_id == activity_id) {
                                                                                            current_well_plans$extract(id = s)[[1]]$.__enclos_env__$private$wellplan_weight
                                                                                          } else {
                                                                                            0
                                                                                          }
                                                                                        }))
                                                  capture.output(current_well_sets$add(new_item = wellset$new(trip_id = current_trip$.__enclos_env__$private$trip_id,
                                                                                                              activity_id = activity_id,
                                                                                                              well_id = current_well$.__enclos_env__$private$well_id,
                                                                                                              sample_id = unlist(wells_activities_samples_id[[well_id]][[2]]),
                                                                                                              weighted_weight = current_weighted_weight,
                                                                                                              weighted_weight_minus10 =  current_weighted_weight * current_well$.__enclos_env__$private$well_prop_minus10_weigth,
                                                                                                              weighted_weight_plus10 =  current_weighted_weight * current_well$.__enclos_env__$private$well_prop_plus10_weigth)),
                                                                 file = "NUL")
                                                }
                                              } else {
                                                # yes, at least two wells for the trip
                                                current_well_activities_samples <- wells_activities_samples_id[[well_id]]
                                                for (current_well_activitie_id in current_well_activities_samples[[1]]) {
                                                  wells_associated <- as.integer()
                                                  for (other_well_id in seq_len(length.out = length(x = wells_activities_samples_id))[seq_len(length.out = length(x = wells_activities_samples_id)) != well_id]) {
                                                    if (current_well_activitie_id %in% wells_activities_samples_id[[other_well_id]][[1]]) {
                                                      wells_associated <- append(wells_associated,
                                                                                 other_well_id)
                                                    }
                                                  }
                                                  # do we have at least one activity of the current well store in one or more other well(s) ?
                                                  if (length(x = wells_associated) != 0) {
                                                    # yes
                                                    # the well of current well has been sample ?
                                                    if (! current_well_activities_samples[[2]][1] == "well_not_sampled") {
                                                      # yes
                                                      w1 <- sum(sapply(X = seq_len(length.out = current_well_plans$count()),
                                                                       FUN = function(x) {
                                                                         if (current_well_plans$extract(id = x)[[1]]$.__enclos_env__$private$activity_id == current_well_activitie_id) {
                                                                           current_well_plans$extract(id = x)[[1]]$.__enclos_env__$private$wellplan_weight
                                                                         } else {
                                                                           0
                                                                         }
                                                                       }))
                                                      w2 <- w1
                                                      wt <- w1
                                                      for (well_associated_id in wells_associated) {
                                                        current_well_activities_samples_tmp <- wells_activities_samples_id[[well_associated_id]]
                                                        current_well_plan_tmp <- current_wells$extract(id = well_associated_id)[[1]]$.__enclos_env__$private$wellplan
                                                        if (current_well_activities_samples_tmp[[2]][1] == "well_not_sampled") {
                                                          for (elementarywellplan_id in seq_len(length.out = length(x = current_well_plan_tmp))) {
                                                            if (current_well_plan_tmp[[elementarywellplan_id]]$.__enclos_env__$private$activity_id == current_well_activitie_id) {
                                                              wt <- wt + current_well_plan_tmp[[elementarywellplan_id]]$.__enclos_env__$private$wellplan_weight
                                                            }
                                                          }
                                                        } else {
                                                          for (elementarywellplan_id in seq_len(length.out = length(current_well_plan_tmp))) {
                                                            if (current_well_plan_tmp[[elementarywellplan_id]]$.__enclos_env__$private$activity_id == current_well_activitie_id) {
                                                              w2 <- w2 + current_well_plan_tmp[[elementarywellplan_id]]$.__enclos_env__$private$wellplan_weight
                                                              wt <- wt + current_well_plan_tmp[[elementarywellplan_id]]$.__enclos_env__$private$wellplan_weight
                                                            }
                                                          }
                                                        }
                                                      }
                                                      current_weighted_weight <- w1 / w2 * wt
                                                    } else {
                                                      # no
                                                      current_weighted_weight <- 0
                                                    }
                                                  } else {
                                                    # do we have at least one activity of the current well store in one or more other well(s) ?
                                                    # no
                                                    current_weighted_weight <- sum(sapply(X = seq_len(length.out = current_well_plans$count()),
                                                                                          FUN = function(w) {
                                                                                            if (current_well_plans$extract(id = w)[[1]]$.__enclos_env__$private$activity_id == current_well_activitie_id) {
                                                                                              current_well_plans$extract(id = w)[[1]]$.__enclos_env__$private$wellplan_weight
                                                                                            } else {
                                                                                              0
                                                                                            }
                                                                                          }))
                                                  }
                                                  capture.output(current_well_sets$add(new_item = wellset$new(trip_id = current_trip$.__enclos_env__$private$trip_id,
                                                                                                              activity_id = current_well_activitie_id,
                                                                                                              well_id = current_well$.__enclos_env__$private$well_id,
                                                                                                              sample_id = unlist(wells_activities_samples_id[[well_id]][[2]]),
                                                                                                              weighted_weight = current_weighted_weight,
                                                                                                              weighted_weight_minus10 =  current_weighted_weight * current_well$.__enclos_env__$private$well_prop_minus10_weigth,
                                                                                                              weighted_weight_plus10 =  current_weighted_weight * current_well$.__enclos_env__$private$well_prop_plus10_weigth)),
                                                                 file = "NUL")
                                                }
                                              }
                                              current_well$.__enclos_env__$private$wellsets <- current_well_sets
                                              sum_weighted_weight <- sum(unlist(current_well_sets$extract_l1_element_value(element = "weighted_weight")))
                                              current_well_sets$modification_l1(modification = paste0("$path$prop_weighted_weight <- $path$weighted_weight / ",
                                                                                                      sum_weighted_weight))
                                            } else {
                                              # no well plan available for the current well
                                              warning(format(Sys.time(),
                                                             "%Y-%m-%d %H:%M:%S"),
                                                      " - No well plan availabe for this well.\n",
                                                      "[trip: ",
                                                      current_well$.__enclos_env__$private$trip_id,
                                                      ", well: ",
                                                      current_well$.__enclos_env__$private$well_id,
                                                      "]")
                                              current_well$.__enclos_env__$private$well_prop_minus10_weigth <- current_well$.__enclos_env__$private$well_minus10_weigth / (current_well$.__enclos_env__$private$well_minus10_weigth + current_well$.__enclos_env__$private$well_plus10_weigth)
                                              current_well$.__enclos_env__$private$well_prop_plus10_weigth <- current_well$.__enclos_env__$private$well_plus10_weigth / (current_well$.__enclos_env__$private$well_minus10_weigth + current_well$.__enclos_env__$private$well_plus10_weigth)
                                              if (is.na(current_well$.__enclos_env__$private$well_id)) {
                                                # for now, if a well_id is na, you can only have one sample inside (if more than 1, the well is avoid in model incrementation, check "R6 object wells creation")
                                                sample_set_well <- dplyr::filter(.data = sample_set,
                                                                                 sample_id == current_well$.__enclos_env__$private$elementarysampleraw[[1]][[1]]$.__enclos_env__$private$sample_id)
                                              } else {
                                                sample_set_well <- dplyr::filter(.data = sample_set,
                                                                                 well_id == current_well$.__enclos_env__$private$well_id)
                                              }
                                              if (nrow(sample_set_well) == 0) {
                                                warning(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                        " - No weighted weight availabe for this well in the database.\n",
                                                        "[trip: ",
                                                        current_well$.__enclos_env__$private$trip_id,
                                                        ", well: ",
                                                        current_well$.__enclos_env__$private$well_id,
                                                        "]")
                                                current_well$.__enclos_env__$private$wellsets <- NA
                                              } else {
                                                capture.output(current_well_sets <- object_r6(class_name = "wellsets"),
                                                               file = "NUL")
                                                for (sample_set_well_id in seq_len(length.out = nrow(sample_set_well))) {
                                                  capture.output(current_well_sets$add(new_item = wellset$new(trip_id = current_trip$.__enclos_env__$private$trip_id,
                                                                                                              activity_id = sample_set_well$activity_id[[sample_set_well_id]],
                                                                                                              well_id = sample_set_well$well_id[[sample_set_well_id]],
                                                                                                              sample_id = sample_set_well$sample_id[[sample_set_well_id]],
                                                                                                              weighted_weight = sample_set_well$well_set_weighted_weight[[sample_set_well_id]],
                                                                                                              weighted_weight_minus10 =  sample_set_well$well_set_weighted_weight[[sample_set_well_id]] * current_well$.__enclos_env__$private$well_prop_minus10_weigth,
                                                                                                              weighted_weight_plus10 =  sample_set_well$well_set_weighted_weight[[sample_set_well_id]] * current_well$.__enclos_env__$private$well_prop_plus10_weigth)),
                                                                 file = "NUL")
                                                }
                                                current_well$.__enclos_env__$private$wellsets <- current_well_sets
                                                sum_weighted_weight <- sum(unlist(current_well_sets$extract_l1_element_value(element = "weighted_weight")))
                                                current_well_sets$modification_l1(modification = paste0("$path$prop_weighted_weight <- $path$weighted_weight / ",
                                                                                                        sum_weighted_weight))
                                              }
                                            }
                                          }
                                        }
                                      } else {
                                        stop(format(Sys.time(),
                                                    "%Y-%m-%d %H:%M:%S"),
                                             " - Process not available for this vessel type.\n",
                                             "[trip: ",
                                             current_trip$.__enclos_env__$private$trip_id,
                                             "]")
                                      }
                                    }
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Process 2.4 successfull on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                  }
                                }
                                # 18.3 - Outputs extraction ----
                                # outputs manipulation
                                if (! is.null(x = global_output_path)) {
                                  full_trips_selected <- private$data_selected
                                  capture.output(trips_selected <- object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                                 file = "NUL")
                                  capture.output(wells_selected <- object_r6(class_name = "wells"),
                                                 file = "NUL")
                                  capture.output(wells_selected$add(new_item = unlist(x = trips_selected$extract_l1_element_value(element = "wells"))),
                                                 file = "NUL")
                                  capture.output(wellsets_selected <- object_r6(class_name = "wellsets"),
                                                 file = "NUL")
                                  capture.output(wellsets_selected$add(new_item = unlist(lapply(X = seq_len(length.out = length(x = wells_selected$extract_l1_element_value(element = "wellsets"))),
                                                                                                FUN = function(wellsets_id) {
                                                                                                  if (paste(class(x = wells_selected$extract_l1_element_value(element = "wellsets")[[wellsets_id]]),
                                                                                                            collapse = " ") != "logical") {
                                                                                                    wells_selected$extract_l1_element_value(element = "wellsets")[[wellsets_id]]$extract()
                                                                                                  }
                                                                                                }))),
                                                 file = "NUL")
                                  outputs_process_2_4_trips <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                         FUN = function(full_trip_id) {
                                                                                                           if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                             return(rep(x = full_trip_id,
                                                                                                                        length(x = full_trips_selected[[full_trip_id]])))
                                                                                                           } else {
                                                                                                             return(full_trip_id)
                                                                                                           }
                                                                                                         })),
                                                                          "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                           FUN = function(full_trip_id) {
                                                                                                             if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                               return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                          length(x = full_trips_selected[[full_trip_id]])))
                                                                                                             } else {
                                                                                                               return(names(x = full_trips_selected[full_trip_id]))
                                                                                                             }
                                                                                                           })),
                                                                          "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                          "trip_end_date" = do.call("c",
                                                                                                    trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                          "year_trip_end_date" = sapply(do.call("c",
                                                                                                                trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                        lubridate::year),
                                                                          "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                          "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))))
                                  outputs_process_2_4_wellsets <- data.frame("trip_id" = unlist(x = wellsets_selected$extract_l1_element_value(element = "trip_id")),
                                                                             "well_id" = unlist(x = wellsets_selected$extract_l1_element_value(element = "well_id")),
                                                                             "activity_id" = unlist(x = wellsets_selected$extract_l1_element_value(element = "activity_id")),
                                                                             "weighted_weight_minus10" = unlist(x = wellsets_selected$extract_l1_element_value(element = "weighted_weight_minus10")),
                                                                             "weighted_weight_plus10" = unlist(x = wellsets_selected$extract_l1_element_value(element = "weighted_weight_plus10")),
                                                                             "weighted_weight" = unlist(x = wellsets_selected$extract_l1_element_value(element = "weighted_weight")))
                                  outputs_process_2_4 <- outputs_process_2_4_wellsets %>%
                                    dplyr::left_join(outputs_process_2_4_trips,
                                                     by = "trip_id") %>%
                                    dplyr::relocate(full_trip_id,
                                                    full_trip_name,
                                                    trip_id,
                                                    trip_end_date,
                                                    year_trip_end_date,
                                                    vessel_code,
                                                    vessel_type_code)
                                  # extraction
                                  if (output_format == "us") {
                                    outputs_dec <- "."
                                    outputs_sep <- ","
                                  } else if (output_format == "eu") {
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  } else {
                                    warning(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Wrong outputs format define, European format will be applied\n",
                                            sep = "")
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  }
                                  write.table(x = outputs_process_2_4,
                                              file = file.path(global_output_path,
                                                               "level2",
                                                               "data",
                                                               "process_2_4.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Outputs extracted in the following directory:\n",
                                      file.path(global_output_path,
                                                "level2",
                                                "data"), "\n")
                                }
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - End process 2.4 well-set weight categories definition.\n")
                              }
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 19 - Process 2.5: standardised_sample_creation ----
                            #' @description Object standardised sample creation.
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction egual TRUE. Path of the global outputs directory. The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            standardised_sample_creation = function(global_output_path = NULL,
                                                                    output_format = "eu") {
                              # 19.1 - Arguments verification ----
                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              # 19.2 - Global process ----
                              if (is.null(x = private$data_selected)) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Empty data selected in the R6 object. Process 2.5 (standardised sample creation) cancelled.")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.5: standardised sample creation.\n")
                                  }
                                  if (names(x = private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    message(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Warning on item \"",
                                            names(x = private$data_selected)[full_trip_id],
                                            "\": full trip avoided because a least one trip inside is missing.\n",
                                            "[trip: ",
                                            private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                            "]")
                                    capture.output(current_trips <- object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                      capture.output(current_wells <- object_r6(class_name = "wells"),
                                                     file = "NUL")
                                      capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                     file = "NUL")
                                      current_wells$modification_l1(modification = "$path$standardisedsample <- NA")
                                    }
                                  } else {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.5 on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                    for (partial_trip_id in seq_len(length.out = length(x = private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                      if (length(x = current_trip$.__enclos_env__$private$wells) != 0) {
                                        capture.output(current_wells <- object_r6(class_name = "wells"),
                                                       file = "NUL")
                                        capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                       file = "NUL")
                                        for (well_id in seq_len(length.out = current_wells$count())) {
                                          current_well <- current_wells$extract(id = well_id)[[1]]
                                          if (is.null(x = current_well$.__enclos_env__$private$elementarysample)) {
                                            stop(format(Sys.time(),
                                                        "%Y-%m-%d %H:%M:%S"),
                                                 " - The object elementarysample is NULL, please run processes 2.1 to 2.4 before this one.\n",
                                                 "[trip_id: ",
                                                 current_well$.__enclos_env__$private$trip_id,
                                                 ", well_id: ",
                                                 current_well$.__enclos_env__$private$well_id,
                                                 "]")
                                          }
                                          if (paste0(class(x = current_well$.__enclos_env__$private$elementarysample),
                                                     collapse = "_") == "elementarysamples_list_t3_R6") {
                                            capture.output(current_standardisedsamples <- object_r6(class_name = "standardisedsamples"),
                                                           file = "NUL")
                                            capture.output(current_elementarysamples <- object_r6(class_name = "elementarysamples"),
                                                           file = "NUL")
                                            capture.output(current_elementarysamples$add(new_item = current_well$.__enclos_env__$private$elementarysample$.__enclos_env__$private$data),
                                                           file = "NUL")
                                            current_elementarysamples_species <- unique(unlist(current_elementarysamples$extract_l1_element_value(element = "species_fao_code")))
                                            for (elementarysamples_species_id in current_elementarysamples_species) {
                                              capture.output(current_elementarysamples_specie <- object_r6(class_name = "elementarysamples"),
                                                             file = "NUL")
                                              capture.output(current_elementarysamples_specie$add(new_item = current_elementarysamples$filter_l1(filter = paste0("$path$species_fao_code == \"",
                                                                                                                                                                 elementarysamples_species_id,
                                                                                                                                                                 "\""))),
                                                             file = "NUL")
                                              current_elementarysamples_specie_classes <- unique(x = unlist(x = current_elementarysamples_specie$extract_l1_element_value(element = "sample_standardised_length_class_lf")))
                                              for (current_elementarysamples_specie_class_id in current_elementarysamples_specie_classes) {
                                                capture.output(current_elementarysamples_specie_class <- object_r6(class_name = "elementarysamples"),
                                                               file = "NUL")
                                                capture.output(current_elementarysamples_specie_class$add(new_item = current_elementarysamples_specie$filter_l1(filter = paste0("$path$sample_standardised_length_class_lf == ",
                                                                                                                                                                                current_elementarysamples_specie_class_id))),
                                                               file = "NUL")
                                                current_elementarysamples_sample_types <- unique(x = unlist(x = current_elementarysamples_specie_class$extract_l1_element_value(element = "sample_type_code")))
                                                for (current_elementarysamples_sample_type_id in current_elementarysamples_sample_types) {
                                                  capture.output(current_elementarysamples_sample_type <- object_r6(class_name = "elementarysamples"),
                                                                 file = "NUL")
                                                  capture.output(current_elementarysamples_sample_type$add(new_item = current_elementarysamples_specie_class$filter_l1(filter = paste0("$path$sample_type_code == ",
                                                                                                                                                                                       current_elementarysamples_sample_type_id))),
                                                                 file = "NUL")
                                                  current_elementarysamples_sample_qualities <- unique(x = unlist(x = current_elementarysamples_sample_type$extract_l1_element_value(element = "sample_quality_code")))
                                                  for (current_elementarysamples_sample_quality_id in current_elementarysamples_sample_qualities) {
                                                    capture.output(current_elementarysamples_sample_quality <- object_r6(class_name = "elementarysamples"),
                                                                   file = "NUL")
                                                    capture.output(current_elementarysamples_sample_quality$add(new_item = current_elementarysamples_sample_type$filter_l1(filter = paste0("$path$sample_quality_code == ",
                                                                                                                                                                                           current_elementarysamples_sample_quality_id))),
                                                                   file = "NUL")
                                                    current_standardisedsample <- standardisedsample$new(trip_id = current_well$.__enclos_env__$private$trip_id,
                                                                                                         well_id = current_well$.__enclos_env__$private$well_id,
                                                                                                         sample_id = unique(x = unlist(x = current_elementarysamples_sample_quality$extract_l1_element_value(element = "sample_id"))),
                                                                                                         sample_quality_code = as.integer(x = current_elementarysamples_sample_quality_id),
                                                                                                         sample_type_code = as.integer(x = current_elementarysamples_sample_type_id),
                                                                                                         species_fao_code = elementarysamples_species_id,
                                                                                                         sample_standardised_length_class_lf = as.integer(current_elementarysamples_specie_class_id),
                                                                                                         sample_number_measured_extrapolated_lf = sum(unlist(current_elementarysamples_sample_quality$extract_l1_element_value(element = "sample_number_measured_extrapolated_lf"))),
                                                                                                         sample_total_count = sum(unlist(x = current_elementarysamples_sample_quality$extract_l1_element_value(element = "sample_total_count"))))
                                                    capture.output(current_standardisedsamples$add(new_item = current_standardisedsample),
                                                                   file = "NUL")
                                                  }
                                                }
                                              }
                                            }
                                          } else {
                                            current_standardisedsamples <- NA
                                          }
                                          current_well$.__enclos_env__$private$standardisedsample <- current_standardisedsamples
                                        }
                                      }
                                    }
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Process 2.5 successfull on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                  }
                                }
                                # 19.3 - Outputs extraction ----
                                # outputs manipulation
                                if (! is.null(x = global_output_path)) {
                                  full_trips_selected <- private$data_selected
                                  capture.output(trips_selected <- object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                                 file = "NUL")
                                  capture.output(wells_selected <- object_r6(class_name = "wells"),
                                                 file = "NUL")
                                  capture.output(wells_selected$add(new_item = unlist(x = trips_selected$extract_l1_element_value(element = "wells"))),
                                                 file = "NUL")
                                  capture.output(standardisedsamples_selected <- object_r6(class_name = "standardisedsamples"),
                                                 file = "NUL")
                                  capture.output(standardisedsamples_selected$add(new_item = unlist(lapply(X = seq_len(length.out = length(x = wells_selected$extract_l1_element_value(element = "standardisedsample"))),
                                                                                                           FUN = function(standardisedsample_id) {
                                                                                                             if (paste(class(x = wells_selected$extract_l1_element_value(element = "standardisedsample")[[standardisedsample_id]]),
                                                                                                                       collapse = " ") != "logical") {
                                                                                                               wells_selected$extract_l1_element_value(element = "standardisedsample")[[standardisedsample_id]]$extract()
                                                                                                             }
                                                                                                           }))),
                                                 file = "NUL")
                                  outputs_process_2_5_trips <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                         FUN = function(full_trip_id) {
                                                                                                           if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                             return(rep(x = full_trip_id,
                                                                                                                        length(x = full_trips_selected[[full_trip_id]])))
                                                                                                           } else {
                                                                                                             return(full_trip_id)
                                                                                                           }
                                                                                                         })),
                                                                          "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                           FUN = function(full_trip_id) {
                                                                                                             if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                               return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                          length(x = full_trips_selected[[full_trip_id]])))
                                                                                                             } else {
                                                                                                               return(names(x = full_trips_selected[full_trip_id]))
                                                                                                             }
                                                                                                           })),
                                                                          "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                          "trip_end_date" = do.call("c",
                                                                                                    trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                          "year_trip_end_date" = sapply(do.call("c",
                                                                                                                trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                        lubridate::year),
                                                                          "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                          "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))))
                                  outputs_process_2_5_standardisedsamples <- data.frame("trip_id" = unlist(x = standardisedsamples_selected$extract_l1_element_value(element = "trip_id")),
                                                                                        "well_id" = unlist(x = standardisedsamples_selected$extract_l1_element_value(element = "well_id")),
                                                                                        "sample_id" = dplyr::tibble("sample_id_ori"= standardisedsamples_selected$extract_l1_element_value(element = "sample_id")) %>%
                                                                                          dplyr::rowwise() %>%
                                                                                          dplyr::mutate(sample_id_final = paste0(sample_id_ori,
                                                                                                                                 collapse = ", ")) %>%
                                                                                          dplyr::pull(sample_id_final),
                                                                                        "species_fao_code" = unlist(x = standardisedsamples_selected$extract_l1_element_value(element = "species_fao_code")),
                                                                                        "sample_standardised_length_class_lf" = unlist(x = standardisedsamples_selected$extract_l1_element_value(element = "sample_standardised_length_class_lf")),
                                                                                        "sample_number_measured_extrapolated_lf" = unlist(x = standardisedsamples_selected$extract_l1_element_value(element = "sample_number_measured_extrapolated_lf")))
                                  outputs_process_2_5 <- outputs_process_2_5_standardisedsamples %>%
                                    dplyr::left_join(outputs_process_2_5_trips,
                                                     by = "trip_id") %>%
                                    dplyr::relocate(full_trip_id,
                                                    full_trip_name,
                                                    trip_id,
                                                    trip_end_date,
                                                    year_trip_end_date,
                                                    vessel_code,
                                                    vessel_type_code)
                                  # extraction
                                  if (output_format == "us") {
                                    outputs_dec <- "."
                                    outputs_sep <- ","
                                  } else if (output_format == "eu") {
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  } else {
                                    warning(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Wrong outputs format define, European format will be applied.")
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  }
                                  write.table(x = outputs_process_2_5,
                                              file = file.path(global_output_path,
                                                               "level2",
                                                               "data",
                                                               "process_2_5.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Outputs extracted in the following directory:\n",
                                      file.path(global_output_path,
                                                "level2",
                                                "data"), "\n")
                                }
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - End process 2.5 standardised sample creation.\n")
                              }
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 20 - Process 2.6: standardised_sample_set_creation ----
                            #' @description R6 object standardised sample set creation.
                            #' @param length_weight_relationship_data Object of type \code{\link[base]{data.frame}} or \code{\link[tibble]{tbl_df}} expected. Data frame object with parameters for length weight relationship.
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction egual TRUE. Path of the global outputs directory. The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            standardised_sample_set_creation = function(length_weight_relationship_data,
                                                                        global_output_path = NULL,
                                                                        output_format = "eu") {
                              # 20.1 - Arguments verification ----
                              if (! paste0(class(x = length_weight_relationship_data),
                                           collapse = "_") %in% c("data.frame",
                                                                  "tbl_df_tbl_data.frame")
                                  || ncol(x = length_weight_relationship_data) != 7
                                  || nrow(x = length_weight_relationship_data) == 0) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Invalid \"sample_set\" argument, class \"data.frame\" or \"tibble\" with 7 columns and at least 1 row expected.")
                              }
                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              # 20.2 - Global process ----
                              if (is.null(x = private$data_selected)) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Empty data selected in the R6 object. Process 2.6 (standardised sample set creation) cancelled.")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(x = private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.6: standardised sample set creation.\n")
                                  }
                                  if (names(x = private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    message(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Warning on item \"",
                                            names(x = private$data_selected)[full_trip_id],
                                            "\": full trip avoided because a least one trip inside is missing.\n",
                                            "[trip: ",
                                            private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                            "]")
                                    capture.output(current_trips <- object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                      capture.output(current_wells <- object_r6(class_name = "wells"),
                                                     file = "NUL")
                                      capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                     file = "NUL")
                                      current_wells$modification_l1(modification = "$path$standardisedsampleset <- NA")
                                    }
                                  } else {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.6 on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                    for (partial_trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                      capture.output(current_activities <- object_r6(class_name = "activities"),
                                                     file = "NUL")
                                      capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                                                     file = "NUL")
                                      if (length(x = current_trip$.__enclos_env__$private$wells) != 0) {
                                        capture.output(current_wells <- object_r6(class_name = "wells"),
                                                       file = "NUL")
                                        capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                       file = "NUL")
                                        for (well_id in seq_len(length.out = current_wells$count())) {
                                          current_well <- current_wells$extract(id = well_id)[[1]]
                                          current_wells_sets <- current_well$.__enclos_env__$private$wellsets
                                          current_standardised_samples <- current_well$.__enclos_env__$private$standardisedsample
                                          if (all(class(x = current_wells_sets) == c("wellsets",
                                                                                     "list_t3",
                                                                                     "R6"))
                                              && all(class(x = current_standardised_samples) == c("standardisedsamples",
                                                                                                  "list_t3",
                                                                                                  "R6"))) {
                                            capture.output(standardised_samples_sets <- object_r6(class_name = "standardisedsamplesets"),
                                                           file = "NUL")
                                            for (well_set_id in seq_len(length.out = current_wells_sets$count())) {
                                              current_well_set <- current_wells_sets$extract(id = well_set_id)[[1]]
                                              current_activity <- current_well_set$.__enclos_env__$private$activity_id
                                              current_ocean <- current_activities$filter_l1(filter = paste0("$path$activity_id == \"",
                                                                                                            current_activity,
                                                                                                            "\""))[[1]]$.__enclos_env__$private$ocean_code
                                              if (is.null(x = current_ocean)) {
                                                stop(format(Sys.time(),
                                                            "%Y-%m-%d %H:%M:%S"),
                                                     " - Sample activity missing from trip activities.\n",
                                                     "[trip: ",
                                                     private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                                     ", sample activity: ",
                                                     current_activity,
                                                     "]")
                                              }
                                              for (standardisedsample_id in seq_len(length.out = current_standardised_samples$count())) {
                                                current_standardised_sample <- current_standardised_samples$extract(id = standardisedsample_id)[[1]]
                                                current_length_weight_relationship <- dplyr::filter(.data = length_weight_relationship_data,
                                                                                                    (ocean_code == current_ocean
                                                                                                     & species_fao_code == current_standardised_sample$.__enclos_env__$private$species_fao_code)) %>%
                                                  dplyr::select(lwr_a,
                                                                lwr_b)
                                                if (nrow(x = current_length_weight_relationship) == 1) {
                                                  coef_a <- as.numeric(x = current_length_weight_relationship$lwr_a)
                                                  coef_b <- as.numeric(x = current_length_weight_relationship$lwr_b)
                                                  if (current_standardised_sample$.__enclos_env__$private$species_fao_code %in% c("SKJ",
                                                                                                                                  "LTA",
                                                                                                                                  "FRI")) {
                                                    # step of 1 cm
                                                    length_class_lf <- current_standardised_sample$.__enclos_env__$private$sample_standardised_length_class_lf + 0.5
                                                  } else if (current_standardised_sample$.__enclos_env__$private$species_fao_code %in% c("YFT",
                                                                                                                                         "BET",
                                                                                                                                         "ALB")) {
                                                    # step of 2 cm
                                                    length_class_lf <- current_standardised_sample$.__enclos_env__$private$sample_standardised_length_class_lf + 1
                                                  } else {
                                                    length_class_lf <- current_standardised_sample$.__enclos_env__$private$sample_standardised_length_class_lf
                                                  }
                                                  lwr <- coef_a * length_class_lf ^ coef_b
                                                } else {
                                                  lwr <- NA_real_
                                                  warning(format(Sys.time(),
                                                                 "%Y-%m-%d %H:%M:%S"),
                                                          " - Warning: length to weight conversion impossible.\n",
                                                          "[trip: ",
                                                          current_well$.__enclos_env__$private$trip_id,
                                                          ", well_id: ",
                                                          current_well$.__enclos_env__$private$well_id,
                                                          ", sample(s): ",
                                                          paste0(current_standardised_sample$.__enclos_env__$private$sample_id,
                                                                 collapse = " - "),
                                                          "]")
                                                }
                                                current_standardised_samples_sets <- standardisedsampleset$new(trip_id = current_well_set$.__enclos_env__$private$trip_id,
                                                                                                               activity_id = current_well_set$.__enclos_env__$private$activity_id,
                                                                                                               well_id = current_well_set$.__enclos_env__$private$well_id,
                                                                                                               sample_id = current_standardised_sample$.__enclos_env__$private$sample_id,
                                                                                                               sample_quality_code = current_standardised_sample$.__enclos_env__$private$sample_quality_code,
                                                                                                               sample_type_code = current_standardised_sample$.__enclos_env__$private$sample_type_code,
                                                                                                               species_fao_code = current_standardised_sample$.__enclos_env__$private$species_fao_code,
                                                                                                               sample_standardised_length_class_lf = current_standardised_sample$.__enclos_env__$private$sample_standardised_length_class_lf,
                                                                                                               sample_number_weighted = current_standardised_sample$.__enclos_env__$private$sample_number_measured_extrapolated_lf * current_well_set$.__enclos_env__$private$prop_weighted_weight,
                                                                                                               sample_weigth = (current_standardised_sample$.__enclos_env__$private$sample_number_measured_extrapolated_lf * current_well_set$.__enclos_env__$private$prop_weighted_weight) * lwr,
                                                                                                               sample_weight_unit = lwr,
                                                                                                               sample_category = ifelse(test = lwr <= 10,
                                                                                                                                        yes = "- 10kg",
                                                                                                                                        no = "+ 10kg"))
                                                capture.output(standardised_samples_sets$add(new_item = current_standardised_samples_sets),
                                                               file = "NUL")
                                              }
                                            }
                                          } else {
                                            standardised_samples_sets <- NA
                                          }
                                          current_well$.__enclos_env__$private$standardisedsampleset <- standardised_samples_sets
                                        }
                                      }
                                    }
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Process 2.6 successfull on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                  }
                                }
                                # 20.3 - Outputs extraction ----
                                # outputs manipulation
                                if (! is.null(x = global_output_path)) {
                                  full_trips_selected <- private$data_selected
                                  capture.output(trips_selected <- object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                                 file = "NUL")
                                  capture.output(wells_selected <- object_r6(class_name = "wells"),
                                                 file = "NUL")
                                  capture.output(wells_selected$add(new_item = unlist(x = trips_selected$extract_l1_element_value(element = "wells"))),
                                                 file = "NUL")
                                  capture.output(standardisedsamplesets_selected <- object_r6(class_name = "standardisedsamplesets"),
                                                 file = "NUL")
                                  capture.output(standardisedsamplesets_selected$add(new_item = unlist(lapply(X = seq_len(length.out = length(x = wells_selected$extract_l1_element_value(element = "standardisedsampleset"))),
                                                                                                              FUN = function(standardisedsampleset_id) {
                                                                                                                if (paste(class(x = wells_selected$extract_l1_element_value(element = "standardisedsampleset")[[standardisedsampleset_id]]),
                                                                                                                          collapse = " ") != "logical") {
                                                                                                                  wells_selected$extract_l1_element_value(element = "standardisedsampleset")[[standardisedsampleset_id]]$extract()
                                                                                                                }
                                                                                                              }))),
                                                 file = "NUL")
                                  outputs_process_2_6_trips <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                         FUN = function(full_trip_id) {
                                                                                                           if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                             return(rep(x = full_trip_id,
                                                                                                                        length(x = full_trips_selected[[full_trip_id]])))
                                                                                                           } else {
                                                                                                             return(full_trip_id)
                                                                                                           }
                                                                                                         })),
                                                                          "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                           FUN = function(full_trip_id) {
                                                                                                             if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                               return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                          length(x = full_trips_selected[[full_trip_id]])))
                                                                                                             } else {
                                                                                                               return(names(x = full_trips_selected[full_trip_id]))
                                                                                                             }
                                                                                                           })),
                                                                          "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                          "trip_end_date" = do.call("c",
                                                                                                    trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                          "year_trip_end_date" = sapply(do.call("c",
                                                                                                                trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                        lubridate::year),
                                                                          "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                          "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))))
                                  outputs_process_2_6_standardisedsamplesets <- data.frame("trip_id" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "trip_id")),
                                                                                           "well_id" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "well_id")),
                                                                                           "activity_id" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "activity_id")),
                                                                                           "species_fao_code" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "species_fao_code")),
                                                                                           "sample_standardised_length_class_lf" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "sample_standardised_length_class_lf")),
                                                                                           "sample_number_weighted" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "sample_number_weighted")),
                                                                                           "sample_weigth" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "sample_weigth")),
                                                                                           "sample_weight_unit" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "sample_weight_unit")),
                                                                                           "sample_category" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "sample_category")))
                                  outputs_process_2_6 <- outputs_process_2_6_standardisedsamplesets %>%
                                    dplyr::left_join(outputs_process_2_6_trips,
                                                     by = "trip_id") %>%
                                    dplyr::relocate(full_trip_id,
                                                    full_trip_name,
                                                    trip_id,
                                                    trip_end_date,
                                                    year_trip_end_date,
                                                    vessel_code,
                                                    vessel_type_code)
                                  # extraction
                                  if (output_format == "us") {
                                    outputs_dec <- "."
                                    outputs_sep <- ","
                                  } else if (output_format == "eu") {
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  } else {
                                    warning(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Wrong outputs format define, European format will be applied.")
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  }
                                  write.table(x = outputs_process_2_6,
                                              file = file.path(global_output_path,
                                                               "level2",
                                                               "data",
                                                               "process_2_6.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Outputs extracted in the following directory:\n",
                                      file.path(global_output_path,
                                                "level2",
                                                "data"), "\n")
                                }
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - End process 2.6: standardised sample set creation.\n")
                              }
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 21 - Process 2.7: raised_factors_determination ----
                            #' @description Raised factors determination for weigth sample set to set.
                            #' @param threshold_rf_minus10 Object of type \code{\link[base]{integer}} expected. Threshold limite value for raising factor on individuals category minus 10. By default 500.
                            #' @param threshold_rf_plus10 Object of type \code{\link[base]{integer}} expected. Threshold limite value for raising factor on individuals category plus 10. By default 500.
                            #' @param threshold_frequency_rf_minus10 Object of type \code{\link[base]{integer}} expected. Threshold limite frequency value for raising factor on individuals category minus 10. By default 75.
                            #' @param threshold_frequency_rf_plus10 Object of type \code{\link[base]{integer}} expected. Threshold limite frequency value for raising factor on individuals category plus 10. By default 75.
                            #' @param threshold_rf_total Object of type \code{\link[base]{integer}} expected. Threshold limite value for raising factor (all categories). By default 250.
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction egual TRUE. Path of the global outputs directory. The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            raised_factors_determination = function(threshold_rf_minus10 = as.integer(500),
                                                                    threshold_rf_plus10 = as.integer(500),
                                                                    threshold_frequency_rf_minus10 = as.integer(75),
                                                                    threshold_frequency_rf_plus10 = as.integer(75),
                                                                    threshold_rf_total = as.integer(250),
                                                                    global_output_path = NULL,
                                                                    output_format = "eu") {
                              # 21.1 - Arguments verification ----
                              codama::r_type_checking(r_object = threshold_rf_minus10,
                                                      type = "integer",
                                                      length = 1L)
                              codama::r_type_checking(r_object = threshold_rf_plus10,
                                                      type = "integer",
                                                      length = 1L)
                              codama::r_type_checking(r_object = threshold_frequency_rf_minus10,
                                                      type = "integer",
                                                      length = 1L)
                              codama::r_type_checking(r_object = threshold_frequency_rf_plus10,
                                                      type = "integer",
                                                      length = 1L)
                              codama::r_type_checking(r_object = threshold_rf_total,
                                                      type = "integer",
                                                      length = 1L)
                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              # 21.2 - Global process ----
                              if (is.null(x = private$data_selected)) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Empty data selected in the R6 object. Process 2.7 (raised factors determination) cancelled.")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(x = private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.7: raised factors determination.\n")
                                  }
                                  if (names(x = private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    message(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Warning on item \"",
                                            names(x = private$data_selected)[full_trip_id],
                                            "\": full trip avoided because a least one trip inside is missing.\n",
                                            "[trip: ",
                                            private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                            "]")
                                    capture.output(current_trips <- object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                      capture.output(current_wells <- object_r6(class_name = "wells"),
                                                     file = "NUL")
                                      capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                     file = "NUL")
                                      if (length(x = current_wells$filter_l1(filter = "all(class($path$wellsets) == c(\"wellsets\", \"list_t3\", \"R6\"))")) != 0) {
                                        capture.output(current_wells_bis <- object_r6(class_name = "wells"),
                                                       file = "NUL")
                                        capture.output(current_wells_bis$add(new_item = current_wells$filter_l1(filter = "all(class($path$wellsets) == c(\"wellsets\", \"list_t3\", \"R6\"))")),
                                                       file = "NUL")
                                        capture.output(current_wellsets <- object_r6(class_name = "wellsets"),
                                                       file = "NUL")
                                        capture.output(current_wellsets$add(new_item = current_wells_bis$extract_l1_element_value(element = "wellsets")),
                                                       file = "NUL")
                                        capture.output(current_wellsets_bis <- object_r6(class_name = "wellsets"),
                                                       file = "NUL")
                                        capture.output(current_wellsets_bis$add(new_item = unlist(current_wellsets$extract_l1_element_value(element = "data"))),
                                                       file = "NUL")
                                        current_wellsets_bis$modification_l1(modification = "$path$rf_validation <- NA_integer_")
                                      }
                                    }
                                  } else {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.7 on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                    for (partial_trip_id in seq_len(length.out = length(x = private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                      if (length(x = current_trip$.__enclos_env__$private$wells) != 0) {
                                        capture.output(current_wells <- object_r6(class_name = "wells"),
                                                       file = "NUL")
                                        capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                       file = "NUL")
                                        for (well_id in seq_len(length.out = current_wells$count())) {
                                          current_well <- current_wells$extract(id = well_id)[[1]]
                                          current_wells_sets <- current_well$.__enclos_env__$private$wellsets
                                          if (all(class(current_wells_sets) == c("wellsets",
                                                                                 "list_t3",
                                                                                 "R6"))) {
                                            for (well_set_id in seq_len(length.out = current_wells_sets$count())) {
                                              current_well_set <- current_wells_sets$extract(id = well_set_id)[[1]]
                                              if (all(class(current_well$.__enclos_env__$private$standardisedsampleset) == c("standardisedsamplesets",
                                                                                                                             "list_t3",
                                                                                                                             "R6"))
                                                  && length(current_well$.__enclos_env__$private$standardisedsampleset$filter_l1(filter = paste0("$path$activity_id == \"",
                                                                                                                                                 current_well_set$.__enclos_env__$private$activity_id,
                                                                                                                                                 "\""))) != 0) {
                                                capture.output(current_standardised_samples_sets <- object_r6(class_name = "standardisedsamplesets"),
                                                               file = "NUL")
                                                capture.output(current_standardised_samples_sets$add(new_item = current_well$.__enclos_env__$private$standardisedsampleset$filter_l1(filter = paste0("$path$activity_id == \"",
                                                                                                                                                                                                     current_well_set$.__enclos_env__$private$activity_id,
                                                                                                                                                                                                     "\""))),
                                                               file = "NUL")
                                                if (length(x = current_standardised_samples_sets$filter_l1(filter = "$path$sample_weight_unit <= 10")) != 0) {
                                                  capture.output(current_standardised_samples_sets_minus10 <- object_r6(class_name = "standardisedsamplesets"),
                                                                 file = "NUL")
                                                  capture.output(current_standardised_samples_sets_minus10$add(new_item = current_standardised_samples_sets$filter_l1(filter = "$path$sample_weight_unit <= 10")),
                                                                 file = "NUL")
                                                  current_well_set$.__enclos_env__$private$weighted_samples_minus10 <- sum(unlist(current_standardised_samples_sets_minus10$extract_l1_element_value(element = "sample_weigth"))) / 1000
                                                  current_standardised_samples_sets_minus10_nb <- sum(unlist(current_standardised_samples_sets_minus10$extract_l1_element_value(element = "sample_number_weighted")))
                                                } else {
                                                  current_well_set$.__enclos_env__$private$weighted_samples_minus10 <- 0
                                                  current_standardised_samples_sets_minus10_nb <- 0
                                                }
                                                if (length(x = current_standardised_samples_sets$filter_l1(filter = "$path$sample_weight_unit > 10")) != 0) {
                                                  capture.output(current_standardised_samples_sets_plus10 <- object_r6(class_name = "standardisedsamplesets"),
                                                                 file = "NUL")
                                                  capture.output(current_standardised_samples_sets_plus10$add(new_item = current_standardised_samples_sets$filter_l1(filter = "$path$sample_weight_unit > 10")),
                                                                 file = "NUL")
                                                  current_well_set$.__enclos_env__$private$weighted_samples_plus10 <- sum(unlist(current_standardised_samples_sets_plus10$extract_l1_element_value(element = "sample_weigth"))) / 1000
                                                  current_standardised_samples_sets_plus10_nb <- sum(unlist(current_standardised_samples_sets_plus10$extract_l1_element_value(element = "sample_number_weighted")))
                                                } else {
                                                  current_well_set$.__enclos_env__$private$weighted_samples_plus10 <- 0
                                                  current_standardised_samples_sets_plus10_nb <- 0
                                                }
                                                current_well_set$.__enclos_env__$private$weighted_samples_total <- sum(unlist(current_standardised_samples_sets$extract_l1_element_value(element = "sample_weigth"))) / 1000
                                                if (current_well_set$.__enclos_env__$private$weighted_samples_total == 0) {
                                                  # scenario 1
                                                  current_well_set$.__enclos_env__$private$rf_validation <- 1L
                                                  warning(format(Sys.time(),
                                                                 "%Y-%m-%d %H:%M:%S"),
                                                          " - Well-set avoided because weighted samples total value equal to zero.\n",
                                                          "[trip: ,",
                                                          current_well_set$.__enclos_env__$private$trip_id,
                                                          ", activity: ",
                                                          current_well_set$.__enclos_env__$private$activity_id,
                                                          ", well: ",
                                                          current_well_set$.__enclos_env__$private$well_id,
                                                          ", sample(s): ",
                                                          paste0(current_well_set$.__enclos_env__$private$sample_id,
                                                                 collapse = " - "),
                                                          "]")
                                                } else if (is.na(x = current_well_set$.__enclos_env__$private$weighted_weight)
                                                           || current_well_set$.__enclos_env__$private$weighted_weight == 0) {
                                                  # scenario 2
                                                  current_well_set$.__enclos_env__$private$rf_validation <- 2L
                                                  warning(format(Sys.time(),
                                                                 "%Y-%m-%d %H:%M:%S"),
                                                          " - Well-set avoided because invalid weighted weigth.\n",
                                                          "[trip: ,",
                                                          current_well_set$.__enclos_env__$private$trip_id,
                                                          ", activity: ",
                                                          current_well_set$.__enclos_env__$private$activity_id,
                                                          ", well: ",
                                                          current_well_set$.__enclos_env__$private$well_id,
                                                          ", sample(s): ",
                                                          paste0(current_well_set$.__enclos_env__$private$sample_id,
                                                                 collapse = " - "),
                                                          "]")
                                                } else {
                                                  if (current_well_set$.__enclos_env__$private$weighted_samples_minus10 == 0
                                                      || current_well_set$.__enclos_env__$private$weighted_samples_plus10 == 0) {
                                                    # scenario 3
                                                    current_well_set$.__enclos_env__$private$rf_validation <- 3L
                                                    current_well_set$.__enclos_env__$private$rf_total <- current_well_set$.__enclos_env__$private$weighted_weight / current_well_set$.__enclos_env__$private$weighted_samples_total
                                                  } else {
                                                    current_well_set$.__enclos_env__$private$rf_minus10 <- current_well_set$.__enclos_env__$private$weighted_weight_minus10 / current_well_set$.__enclos_env__$private$weighted_samples_minus10
                                                    current_well_set$.__enclos_env__$private$rf_plus10 <- current_well_set$.__enclos_env__$private$weighted_weight_plus10 / current_well_set$.__enclos_env__$private$weighted_samples_plus10
                                                    if (is.na(x = current_well_set$.__enclos_env__$private$rf_minus10)
                                                        || is.na(x = current_well_set$.__enclos_env__$private$rf_plus10)
                                                        || current_well_set$.__enclos_env__$private$rf_minus10 > threshold_rf_minus10
                                                        || current_well_set$.__enclos_env__$private$rf_plus10 > threshold_rf_plus10
                                                        || current_standardised_samples_sets_minus10_nb > threshold_frequency_rf_minus10
                                                        || current_standardised_samples_sets_plus10_nb > threshold_frequency_rf_plus10) {
                                                      # scenario 4
                                                      current_well_set$.__enclos_env__$private$rf_validation <- 4L
                                                      current_well_set$.__enclos_env__$private$rf_total <- current_well_set$.__enclos_env__$private$weighted_weight / current_well_set$.__enclos_env__$private$weighted_samples_total
                                                    } else {
                                                      # scenario 5
                                                      current_well_set$.__enclos_env__$private$rf_validation <- 5L
                                                    }
                                                  }
                                                }
                                                if (current_well_set$.__enclos_env__$private$rf_validation %in% as.integer(x = c(4, 3))
                                                    && current_well_set$.__enclos_env__$private$rf_total > threshold_rf_total) {
                                                  warning(format(Sys.time(),
                                                                 "%Y-%m-%d %H:%M:%S"),
                                                          " - Well-set \"rf_total\" argument superior to ",
                                                          threshold_rf_total,
                                                          ".\n",
                                                          "[trip: ,",
                                                          current_well_set$.__enclos_env__$private$trip_id,
                                                          ", activity: ",
                                                          current_well_set$.__enclos_env__$private$activity_id,
                                                          ", well: ",
                                                          current_well_set$.__enclos_env__$private$well_id,
                                                          ", sample(s): ",
                                                          paste0(current_well_set$.__enclos_env__$private$sample_id,
                                                                 collapse = " - "),
                                                          "]")
                                                }
                                              } else {
                                                current_well_set$.__enclos_env__$private$rf_validation <- NA_integer_
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Process 2.7 successfull on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                  }
                                }
                                # 21.3 - Outputs extraction ----
                                if (! is.null(x = global_output_path)) {
                                  full_trips_selected <- private$data_selected
                                  capture.output(trips_selected <- object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                                 file = "NUL")
                                  capture.output(wells_selected <- object_r6(class_name = "wells"),
                                                 file = "NUL")
                                  capture.output(wells_selected$add(new_item = unlist(x = trips_selected$extract_l1_element_value(element = "wells"))),
                                                 file = "NUL")
                                  capture.output(wellsets_selected <- object_r6(class_name = "wellsets"),
                                                 file = "NUL")
                                  capture.output(wellsets_selected$add(new_item = unlist(lapply(X = seq_len(length.out = length(x = wells_selected$extract_l1_element_value(element = "wellsets"))),
                                                                                                FUN = function(wellset_id) {
                                                                                                  if (paste(class(x = wells_selected$extract_l1_element_value(element = "wellsets")[[wellset_id]]),
                                                                                                            collapse = " ") != "logical") {
                                                                                                    wells_selected$extract_l1_element_value(element = "wellsets")[[wellset_id]]$extract()
                                                                                                  }
                                                                                                }))),
                                                 file = "NUL")
                                  outputs_process_2_7_trips <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                         FUN = function(full_trip_id) {
                                                                                                           if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                             return(rep(x = full_trip_id,
                                                                                                                        length(x = full_trips_selected[[full_trip_id]])))
                                                                                                           } else {
                                                                                                             return(full_trip_id)
                                                                                                           }
                                                                                                         })),
                                                                          "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                           FUN = function(full_trip_id) {
                                                                                                             if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                               return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                          length(x = full_trips_selected[[full_trip_id]])))
                                                                                                             } else {
                                                                                                               return(names(x = full_trips_selected[full_trip_id]))
                                                                                                             }
                                                                                                           })),
                                                                          "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                          "trip_end_date" = do.call("c",
                                                                                                    trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                          "year_trip_end_date" = sapply(do.call("c",
                                                                                                                trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                        lubridate::year),
                                                                          "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                          "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))))
                                  outputs_process_2_7_wellsets <- data.frame("trip_id" = unlist(x = wellsets_selected$extract_l1_element_value(element = "trip_id")),
                                                                             "well_id" = unlist(x = wellsets_selected$extract_l1_element_value(element = "well_id")),
                                                                             "activity_id" = unlist(x = wellsets_selected$extract_l1_element_value(element = "activity_id")),
                                                                             "weighted_samples_minus10" = unlist(x = wellsets_selected$extract_l1_element_value(element = "weighted_samples_minus10")),
                                                                             "weighted_samples_plus10" = unlist(x = wellsets_selected$extract_l1_element_value(element = "weighted_samples_plus10")),
                                                                             "weighted_samples_total" = unlist(x = wellsets_selected$extract_l1_element_value(element = "weighted_samples_total")),
                                                                             "rf_validation" = unlist(x = wellsets_selected$extract_l1_element_value(element = "rf_validation")))
                                  outputs_process_2_7 <- outputs_process_2_7_wellsets %>%
                                    dplyr::left_join(outputs_process_2_7_trips,
                                                     by = "trip_id") %>%
                                    dplyr::relocate(full_trip_id,
                                                    full_trip_name,
                                                    trip_id,
                                                    trip_end_date,
                                                    year_trip_end_date,
                                                    vessel_code,
                                                    vessel_type_code)
                                  # extraction
                                  if (output_format == "us") {
                                    outputs_dec <- "."
                                    outputs_sep <- ","
                                  } else if (output_format == "eu") {
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  } else {
                                    warning(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Wrong outputs format define, European format will be applied.")
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  }
                                  write.table(x = outputs_process_2_7,
                                              file = file.path(global_output_path,
                                                               "level2",
                                                               "data",
                                                               "process_2_7.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Outputs extracted in the following directory:\n",
                                      file.path(global_output_path,
                                                "level2",
                                                "data"), "\n")
                                }
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - End process 2.7: raised factors determination.\n")
                              }
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 22 - Process 2.8: raised standardised sample set ----
                            #' @description Application of process 2.8 raised factors on standardised sample set.
                            #' @param global_output_path By default object of type \code{\link[base]{NULL}} but object of type \code{\link[base]{character}} expected if parameter outputs_extraction egual TRUE. Path of the global outputs directory. The function will create subsection if necessary.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            raised_standardised_sample_set = function(global_output_path = NULL,
                                                                      output_format = "eu") {
                              # 22.1 - Arguments verification ----
                              codama::r_type_checking(r_object = global_output_path,
                                                      type = "character",
                                                      length = 1L)
                              codama::r_type_checking(r_object = output_format,
                                                      type = "character",
                                                      length = 1L,
                                                      allowed_value = c("us",
                                                                        "eu"))
                              # 22.2 - Global process ----
                              if (is.null(x = private$data_selected)) {
                                stop(format(Sys.time(),
                                            "%Y-%m-%d %H:%M:%S"),
                                     " - Empty data selected in the R6 object. Process 2.8 (raised standardised sample set) cancelled.")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.8: raised standardised sample set.\n")
                                  }
                                  if (names(x = private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    message(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Warning on item \"",
                                            names(x = private$data_selected)[full_trip_id],
                                            "\": full trip avoided because a least one trip inside is missing.\n",
                                            "[trip: ",
                                            private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                            "]")
                                    capture.output(current_trips <- object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                      capture.output(current_wells <- object_r6(class_name = "wells"),
                                                     file = "NUL")
                                      capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                     file = "NUL")
                                      if (length(x = current_wells$filter_l1(filter = "all(class($path$standardisedsampleset) == c(\"standardisedsamplesets\", \"list_t3\", \"R6\"))")) != 0) {
                                        capture.output(current_standardisedsamplesets <- object_r6(class_name = "standardisedsamplesets"),
                                                       file = "NUL")
                                        capture.output(current_standardisedsamplesets$add(new_item = current_wells$extract_l1_element_value(element = "standardisedsampleset")),
                                                       file = "NUL")
                                        capture.output(current_standardisedsamplesets_bis <- object_r6(class_name = "standardisedsamplesets"),
                                                       file = "NUL")
                                        capture.output(current_standardisedsamplesets_bis$add(new_item = unlist(current_standardisedsamplesets$extract_l1_element_value(element = "data"))),
                                                       file = "NUL")
                                        current_standardisedsamplesets_bis$modification_l1(modification = "$path$sample_number_weighted_set <- NA")
                                        current_standardisedsamplesets_bis$modification_l1(modification = "$path$sample_weigth_set <- NA")
                                      }
                                    }
                                  } else {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.8 on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                    for (partial_trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                      if (length(x = current_trip$.__enclos_env__$private$wells) != 0) {
                                        capture.output(current_wells <- object_r6(class_name = "wells"),
                                                       file = "NUL")
                                        capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                       file = "NUL")
                                        for (well_id in seq_len(length.out = current_wells$count())) {
                                          current_well <- current_wells$extract(id = well_id)[[1]]
                                          current_wells_sets <- current_well$.__enclos_env__$private$wellsets
                                          if (all(class(current_wells_sets) == c("wellsets",
                                                                                 "list_t3",
                                                                                 "R6"))) {
                                            for (well_set_id in seq_len(length.out = current_wells_sets$count())) {
                                              current_well_set <- current_wells_sets$extract(id = well_set_id)[[1]]
                                              if (all(class(current_well$.__enclos_env__$private$standardisedsampleset) == c("standardisedsamplesets",
                                                                                                                             "list_t3",
                                                                                                                             "R6"))
                                                  && length(current_well$.__enclos_env__$private$standardisedsampleset$filter_l1(filter = paste0("$path$activity_id == \"",
                                                                                                                                                 current_well_set$.__enclos_env__$private$activity_id,
                                                                                                                                                 "\""))) != 0) {
                                                capture.output(current_standardised_samples_sets <- object_r6(class_name = "standardisedsamplesets"),
                                                               file = "NUL")
                                                capture.output(current_standardised_samples_sets$add(new_item = current_well$.__enclos_env__$private$standardisedsampleset$filter_l1(filter = paste0("$path$activity_id == \"",
                                                                                                                                                                                                     current_well_set$.__enclos_env__$private$activity_id,
                                                                                                                                                                                                     "\""))),
                                                               file = "NUL")
                                                if (current_well_set$.__enclos_env__$private$rf_validation %in% as.integer(x = c(1, 2))
                                                    || is.na(x = current_well_set$.__enclos_env__$private$rf_validation)) {
                                                  warning(format(Sys.time(),
                                                                 "%Y-%m-%d %H:%M:%S"),
                                                          " - Raised factors not available for this well-set.\n",
                                                          "[trip: ,",
                                                          current_well_set$.__enclos_env__$private$trip_id,
                                                          ", activity: ",
                                                          current_well_set$.__enclos_env__$private$activity_id,
                                                          ", well: ",
                                                          current_well_set$.__enclos_env__$private$well_id,
                                                          ", sample(s): ",
                                                          paste0(current_well_set$.__enclos_env__$private$sample_id,
                                                                 collapse = " - "),
                                                          "]")
                                                  current_standardised_samples_sets$modification_l1(modification = "$path$sample_number_weighted_set <- NA")
                                                  current_standardised_samples_sets$modification_l1(modification = "$path$sample_weigth_set <- NA")
                                                } else if (current_well_set$.__enclos_env__$private$rf_validation %in% as.integer(x = c(3, 4))) {
                                                  current_rf_total <- current_well_set$.__enclos_env__$private$rf_total
                                                  current_standardised_samples_sets$modification_l1(modification = paste0("$path$sample_number_weighted_set <- $path$sample_number_weighted * ",
                                                                                                                          current_rf_total))
                                                  current_standardised_samples_sets$modification_l1(modification = "$path$sample_weigth_set <- $path$sample_weight_unit * $path$sample_number_weighted_set / 1000")
                                                } else if (current_well_set$.__enclos_env__$private$rf_validation == 5L) {
                                                  current_rf_minus10 <- current_well_set$.__enclos_env__$private$rf_minus10
                                                  current_rf_plus10 <- current_well_set$.__enclos_env__$private$rf_plus10
                                                  capture.output(current_standardised_samples_sets_minus10 <- object_r6(class_name = "standardisedsamplesets"),
                                                                 file = "NUL")
                                                  capture.output(current_standardised_samples_sets_minus10$add(new_item = current_standardised_samples_sets$filter_l1(filter = "$path$sample_weight_unit <= 10")),
                                                                 file = "NUL")
                                                  current_standardised_samples_sets_minus10$modification_l1(modification = paste0("$path$sample_number_weighted_set <- $path$sample_number_weighted * ",
                                                                                                                                  current_rf_minus10))
                                                  capture.output(current_standardised_samples_sets_plus10 <- object_r6(class_name = "standardisedsamplesets"),
                                                                 file = "NUL")
                                                  capture.output(current_standardised_samples_sets_plus10$add(new_item = current_standardised_samples_sets$filter_l1(filter = "$path$sample_weight_unit > 10")),
                                                                 file = "NUL")
                                                  current_standardised_samples_sets_plus10$modification_l1(modification = paste0("$path$sample_number_weighted_set <- $path$sample_number_weighted * ",
                                                                                                                                 current_rf_plus10))
                                                  current_standardised_samples_sets$modification_l1(modification = "$path$sample_weigth_set <- $path$sample_weight_unit * $path$sample_number_weighted_set / 1000")
                                                } else {
                                                  stop(format(Sys.time(),
                                                              "%Y-%m-%d %H:%M:%S"),
                                                       " - Raised factors verifications is not valide.\n",
                                                       "[trip: ,",
                                                       current_well_set$.__enclos_env__$private$trip_id,
                                                       ", activity: ",
                                                       current_well_set$.__enclos_env__$private$activity_id,
                                                       ", well: ",
                                                       current_well_set$.__enclos_env__$private$well_id,
                                                       ", sample(s): ",
                                                       paste0(current_well_set$.__enclos_env__$private$sample_id,
                                                              collapse = " - "),
                                                       "]")
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Process 2.8 successfull on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n", sep="")
                                  }
                                }
                                # 22.3 - Outputs extraction ----
                                if (! is.null(x = global_output_path)) {
                                  full_trips_selected <- private$data_selected
                                  capture.output(trips_selected <- object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(trips_selected$add(new_item = unlist(x = private$data_selected)),
                                                 file = "NUL")
                                  capture.output(wells_selected <- object_r6(class_name = "wells"),
                                                 file = "NUL")
                                  capture.output(wells_selected$add(new_item = unlist(x = trips_selected$extract_l1_element_value(element = "wells"))),
                                                 file = "NUL")
                                  capture.output(standardisedsamplesets_selected <- object_r6(class_name = "standardisedsamplesets"),
                                                 file = "NUL")
                                  capture.output(standardisedsamplesets_selected$add(new_item = unlist(lapply(X = seq_len(length.out = length(x = wells_selected$extract_l1_element_value(element = "standardisedsampleset"))),
                                                                                                              FUN = function(standardisedsampleset_id) {
                                                                                                                if (paste(class(x = wells_selected$extract_l1_element_value(element = "standardisedsampleset")[[standardisedsampleset_id]]),
                                                                                                                          collapse = " ") != "logical") {
                                                                                                                  wells_selected$extract_l1_element_value(element = "standardisedsampleset")[[standardisedsampleset_id]]$extract()
                                                                                                                }
                                                                                                              }))),
                                                 file = "NUL")
                                  outputs_process_2_8_trips <- data.frame("full_trip_id" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                         FUN = function(full_trip_id) {
                                                                                                           if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                             return(rep(x = full_trip_id,
                                                                                                                        length(x = full_trips_selected[[full_trip_id]])))
                                                                                                           } else {
                                                                                                             return(full_trip_id)
                                                                                                           }
                                                                                                         })),
                                                                          "full_trip_name" = unlist(sapply(X = seq_len(length.out = length(x = full_trips_selected)),
                                                                                                           FUN = function(full_trip_id) {
                                                                                                             if (length(x = full_trips_selected[[full_trip_id]]) != 1) {
                                                                                                               return(rep(x = names(x = full_trips_selected[full_trip_id]),
                                                                                                                          length(x = full_trips_selected[[full_trip_id]])))
                                                                                                             } else {
                                                                                                               return(names(x = full_trips_selected[full_trip_id]))
                                                                                                             }
                                                                                                           })),
                                                                          "trip_id" = unlist(x = (trips_selected$extract_l1_element_value(element = "trip_id"))),
                                                                          "trip_end_date" = do.call("c",
                                                                                                    trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                          "year_trip_end_date" = sapply(do.call("c",
                                                                                                                trips_selected$extract_l1_element_value(element = "trip_end_date")),
                                                                                                        lubridate::year),
                                                                          "vessel_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_code"))),
                                                                          "vessel_type_code" = unlist(x = (trips_selected$extract_l1_element_value(element = "vessel_type_code"))))
                                  outputs_process_2_8_standardisedsamplesets <- data.frame("trip_id" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "trip_id")),
                                                                                           "well_id" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "well_id")),
                                                                                           "activity_id" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "activity_id")),
                                                                                           "sample_id" = dplyr::tibble("sample_id_ori"= standardisedsamplesets_selected$extract_l1_element_value(element = "sample_id")) %>%
                                                                                             dplyr::rowwise() %>%
                                                                                             dplyr::mutate(sample_id_final = paste0(sample_id_ori,
                                                                                                                                    collapse = ", ")) %>%
                                                                                             dplyr::pull(sample_id_final),
                                                                                           "species_fao_code" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "species_fao_code")),
                                                                                           "sample_standardised_length_class_lf" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "sample_standardised_length_class_lf")),
                                                                                           "sample_number_weighted_set" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "sample_number_weighted_set")),
                                                                                           "sample_weigth_set" = unlist(x = standardisedsamplesets_selected$extract_l1_element_value(element = "sample_weigth_set")))
                                  outputs_process_2_8 <- outputs_process_2_8_standardisedsamplesets %>%
                                    dplyr::left_join(outputs_process_2_8_trips,
                                                     by = "trip_id") %>%
                                    dplyr::relocate(full_trip_id,
                                                    full_trip_name,
                                                    trip_id,
                                                    trip_end_date,
                                                    year_trip_end_date,
                                                    vessel_code,
                                                    vessel_type_code)
                                  # extraction
                                  if (output_format == "us") {
                                    outputs_dec <- "."
                                    outputs_sep <- ","
                                  } else if (output_format == "eu") {
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  } else {
                                    warning(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Wrong outputs format define, European format will be applied.")
                                    outputs_dec <- ","
                                    outputs_sep <- ";"
                                  }
                                  write.table(x = outputs_process_2_8,
                                              file = file.path(global_output_path,
                                                               "level2",
                                                               "data",
                                                               "process_2_8.csv"),
                                              row.names = FALSE,
                                              sep = outputs_sep,
                                              dec = outputs_dec)
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Outputs extracted in the following directory:\n",
                                      file.path(global_output_path,
                                                "level2",
                                                "data"), "\n")
                                }
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - End 2.8 process: raised standardised sample set.\n")
                              }
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # 22 - Path to level 3 ----
                            #' @description Temporary link to the R object model with modelisation process.
                            path_to_level3 = function() {
                              cat(format(Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - Start path creation for level 3.\n")
                              data_level3 <- list()
                              raw_inputs_level3 <- vector(mode = "list",
                                                          length = 5)
                              names(raw_inputs_level3) <- c("act",
                                                            "act3",
                                                            "samw",
                                                            "sset",
                                                            "wp")
                              act <- dplyr::tibble()
                              act3 <- dplyr::tibble()
                              samw <- dplyr::tibble()
                              sset <- dplyr::tibble()
                              wp <- dplyr::tibble()
                              for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                capture.output(current_trips <- object_r6(class_name = "trips"),
                                               file = "NUL")
                                capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                               file = "NUL")
                                for (partial_trip_id in seq_len(length.out = current_trips$count())) {
                                  current_trip <- current_trips$extract(id = partial_trip_id)[[1]]
                                  if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                    capture.output(current_activities <- object_r6(class_name = "activities"),
                                                   file = "NUL")
                                    capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                                                   file = "NUL")
                                    tmp_activity <- list(id_act = unlist(current_activities$extract_l1_element_value(element = "activity_id")),
                                                         lat = unlist(current_activities$extract_l1_element_value(element = "activity_latitude")),
                                                         lon = unlist(current_activities$extract_l1_element_value(element = "activity_longitude")),
                                                         fmod = unlist(current_activities$extract_l1_element_value(element = "school_type_code")),
                                                         date_act = do.call("c", current_activities$extract_l1_element_value(element = "activity_date")),
                                                         vessel = rep(x = current_trip$.__enclos_env__$private$vessel_code,
                                                                      current_activities$count()),
                                                         flag_code = rep(x = current_trip$.__enclos_env__$private$flag_code,
                                                                         current_activities$count()),
                                                         id_trip = unlist(current_activities$extract_l1_element_value(element = "trip_id")),
                                                         landingdate = unlist(current_activities$extract_l1_element_value(element = "trip_end_date")),
                                                         ocean = unlist(current_activities$extract_l1_element_value(element = "ocean_code")),
                                                         code_act_type = unlist(current_activities$extract_l1_element_value(element = "activity_code")))
                                    tmp_activity <- dplyr::bind_rows(tmp_activity)
                                    act <- rbind(act,
                                                 tmp_activity)
                                    if (length(x = unlist(current_activities$extract_l1_element_value(element = "elementarycatches"))) != 0) {
                                      capture.output(current_elementarycatches <- do.call(rbind,
                                                                                          current_activities$extract_l1_element_value(element = "elementarycatches")),
                                                     file = "NUL")
                                      tmp_elementarycatch <- list(id_act = current_elementarycatches$activity_id,
                                                                  w_lb_t3 = current_elementarycatches$catch_weight_category_code_corrected,
                                                                  sp_fate_code = current_elementarycatches$species_fate_code,
                                                                  sp = current_elementarycatches$species_fao_code,
                                                                  count = current_elementarycatches$catch_count,
                                                                  wcat = current_elementarycatches$weight_category_code_corrected)

                                      tmp_elementarycatch_activities <- list(id_act = unique(tmp_elementarycatch$id_act),
                                                                             date_act = do.call("c", lapply(X = unique(tmp_elementarycatch$id_act),
                                                                                                            FUN = function(a) {
                                                                                                              current_activities$filter_l1(filter = paste0("$path$activity_id == \"",
                                                                                                                                                           a,
                                                                                                                                                           "\""))[[1]]$.__enclos_env__$private$activity_date
                                                                                                            })),
                                                                             code_act_type = do.call("c", lapply(X = unique(tmp_elementarycatch$id_act),
                                                                                                                 FUN = function(a) {
                                                                                                                   current_activities$filter_l1(filter = paste0("$path$activity_id == \"",
                                                                                                                                                                a,
                                                                                                                                                                "\""))[[1]]$.__enclos_env__$private$activity_code
                                                                                                                 })))
                                      tmp_elementarycatch_final <- dplyr::bind_rows(tmp_elementarycatch) %>%
                                        dplyr::left_join(dplyr::bind_rows(tmp_elementarycatch_activities),
                                                         by = "id_act")
                                      act3 <- rbind(act3,
                                                    tmp_elementarycatch_final)
                                    }
                                  }
                                  if (length(x = current_trip$.__enclos_env__$private$wells) != 0) {
                                    capture.output(current_wells <- object_r6(class_name = "wells"),
                                                   file = "NUL")
                                    capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                   file = "NUL")
                                    if (any(! is.na(current_wells$extract_l1_element_value(element = "standardisedsampleset")))) {
                                      standardisedsampleset_not_na <- which(! is.na(current_wells$extract_l1_element_value(element = "standardisedsampleset")))
                                      capture.output(current_standardisedsamplesets <- object_r6(class_name = "list_t3"),
                                                     file = "NUL")
                                      capture.output(current_standardisedsamplesets$add(new_item = lapply(X = standardisedsampleset_not_na,
                                                                                                          FUN = function(a) {
                                                                                                            current_wells$extract_l1_element_value(element = "standardisedsampleset")[[a]]
                                                                                                          })),
                                                     file = "NUL")
                                      capture.output(current_standardisedsamplesets_data <- object_r6(class_name = "standardisedsamplesets"),
                                                     file = "NUL")
                                      capture.output(current_standardisedsamplesets_data$add(new_item = unlist(current_standardisedsamplesets$extract_l1_element_value(element = "data"))),
                                                     file = "NUL")
                                      tmp_standardisedsampleset <- list(id_act = unlist(current_standardisedsamplesets_data$extract_l1_element_value(element = "activity_id")),
                                                                        sp = unlist(current_standardisedsamplesets_data$extract_l1_element_value(element = "species_fao_code")),
                                                                        wcat = unlist(current_standardisedsamplesets_data$extract_l1_element_value(element = "sample_category")),
                                                                        w_fit_t3 = unlist(current_standardisedsamplesets_data$extract_l1_element_value(element = "sample_weigth_set")))
                                      tmp_standardisedsampleset <- dplyr::bind_rows(tmp_standardisedsampleset)
                                      samw <- rbind(samw,
                                                    tmp_standardisedsampleset)
                                      if (length(x = current_standardisedsamplesets_data$filter_l1(filter = "length($path$sample_id) == 1")) != 0) {
                                        capture.output(current_standardisedsamplesets_data_one_sample <- object_r6(class_name = "standardisedsamplesets"),
                                                       file = "NUL")
                                        capture.output(current_standardisedsamplesets_data_one_sample$add(new_item = current_standardisedsamplesets_data$filter_l1(filter = "length($path$sample_id) == 1")),
                                                       file = "NUL")
                                        tmp_standardisedsampleset_one_sample_qt <- list(id_act = unlist(current_standardisedsamplesets_data_one_sample$extract_l1_element_value(element = "activity_id")),
                                                                                        id_sample = unlist(current_standardisedsamplesets_data_one_sample$extract_l1_element_value(element = "sample_id")),
                                                                                        quality = unlist(current_standardisedsamplesets_data_one_sample$extract_l1_element_value(element = "sample_quality_code")),
                                                                                        type = unlist(current_standardisedsamplesets_data_one_sample$extract_l1_element_value(element = "sample_type_code")))
                                      }
                                      if (length(x = current_standardisedsamplesets_data$filter_l1(filter = "length($path$sample_id) != 1")) != 0) {
                                        capture.output(current_standardisedsamplesets_data_multi_samples <- object_r6(class_name = "standardisedsamplesets"),
                                                       file = "NUL")
                                        capture.output(current_standardisedsamplesets_data_multi_samples$add(new_item = current_standardisedsamplesets_data$filter_l1(filter = "length($path$sample_id) != 1")),
                                                       file = "NUL")
                                        tmp_standardisedsampleset_multi_samples_qt_final <- lapply(X = seq_len(length.out = current_standardisedsamplesets_data_multi_samples$count()),
                                                                                                   FUN = function(a) {
                                                                                                     current_standardisedsampleset <- current_standardisedsamplesets_data_multi_samples$extract(id = a)[[1]]
                                                                                                     current_number_samples <- length(x = current_standardisedsampleset$.__enclos_env__$private$sample_id)
                                                                                                     tmp_standardisedsampleset_multi_samples_qt <- list(id_act = rep(x = current_standardisedsampleset$.__enclos_env__$private$activity_id,
                                                                                                                                                                     current_number_samples),
                                                                                                                                                        id_sample = current_standardisedsampleset$.__enclos_env__$private$sample_id,
                                                                                                                                                        quality = rep(x = current_standardisedsampleset$.__enclos_env__$private$sample_quality_code,
                                                                                                                                                                      current_number_samples),
                                                                                                                                                        type = rep(x = current_standardisedsampleset$.__enclos_env__$private$sample_type_code,
                                                                                                                                                                   current_number_samples))
                                                                                                   })
                                      }
                                      tmp_standardisedsampleset_qt <- dplyr::as_tibble(x = matrix(ncol = 0,
                                                                                                  nrow = 0))
                                      if (exists(x = "tmp_standardisedsampleset_one_sample_qt")) {
                                        tmp_standardisedsampleset_qt <- tmp_standardisedsampleset_qt %>%
                                          dplyr::bind_rows(tmp_standardisedsampleset_one_sample_qt)
                                        rm(tmp_standardisedsampleset_one_sample_qt)
                                      }
                                      if (exists(x = "tmp_standardisedsampleset_multi_samples_qt_final")) {
                                        tmp_standardisedsampleset_qt <- tmp_standardisedsampleset_qt %>%
                                          dplyr::bind_rows(tmp_standardisedsampleset_multi_samples_qt_final)
                                        rm(tmp_standardisedsampleset_multi_samples_qt_final)
                                      }
                                      tmp_standardisedsampleset_qt <- unique(tmp_standardisedsampleset_qt)
                                      sset <- rbind(sset,
                                                    tmp_standardisedsampleset_qt)
                                    }
                                    if (length(x = unlist(current_wells$extract_l1_element_value(element = "wellplan"))) != 0) {
                                      capture.output(current_wellplans <- object_r6(class_name = "elementarywellplans"),
                                                     file = "NUL")
                                      capture.output(current_wellplans$add(new_item = unlist(current_wells$extract_l1_element_value(element = "wellplan"))),
                                                     file = "NUL")
                                      tmp_elementarywellplan <- list(id_well = unlist(current_wellplans$extract_l1_element_value(element = "well_id")),
                                                                     id_act = unlist(current_wellplans$extract_l1_element_value(element = "activity_id")),
                                                                     id_sample = unlist(current_wellplans$extract_l1_element_value(element = "sample_id")),
                                                                     code3l = unlist(current_wellplans$extract_l1_element_value(element = "species_fao_code")),
                                                                     weight = unlist(current_wellplans$extract_l1_element_value(element = "wellplan_weight")),
                                                                     wcat_well = unlist(current_wellplans$extract_l1_element_value(element = "weight_category_label")))
                                      tmp_elementarywellplan <- dplyr::bind_rows(tmp_elementarywellplan)
                                      wp <- rbind(wp,
                                                  tmp_elementarywellplan)
                                    }
                                  }
                                }
                              }
                              raw_inputs_level3[[1]] <- act
                              raw_inputs_level3[[2]] <- act3
                              raw_inputs_level3[[3]] <- dplyr::tibble(dplyr::group_by(.data = samw,
                                                                                      id_act,
                                                                                      sp,
                                                                                      wcat) %>%
                                                                        dplyr::summarise(w_fit_t3 = sum(w_fit_t3)) %>%
                                                                        dplyr::ungroup())
                              raw_inputs_level3[[4]] <- sset
                              raw_inputs_level3[[5]] <- wp
                              cat(format(Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - End path creation for level 3.\n")
                              data_level3 <- append(data_level3,
                                                    list(raw_inputs_level3))
                              names(data_level3)[length(data_level3)] <- "raw_inputs_level3"
                              return(data_level3)
                              capture.output(gc(full=TRUE), file="NUL")
                            },
                            # process 3.1: data preparatory ----
                            #' @description Data preparatory for the t3 modelling process (level 3).
                            #' @param inputs_level3 Object of type \code{\link[base]{data.frame}} expected. Inputs of levels 3 (see function path to level 3).
                            #' @param inputs_level3_path Object of type \code{\link[base]{character}} expected. Path to the folder containing yearly data output of the level 1 and 2 (output of the function the path to level 3). If provide, replace the inputs_level3 object.
                            #' @param output_directory Object of type \code{\link[base]{character}} expected. Path of the outputs directory.
                            #' @param periode_reference_level3 Object of type \code{\link[base]{integer}} expected. Year(s) period of reference for modelling estimation.
                            #' @param target_year Object of type \code{\link[base]{integer}} expected. Year of interest for the model estimation and prediction.Default value is current year -1.
                            #' @param period_duration Object of type \code{\link[base]{integer}} expected. number of years use for the modelling. The default value is 5
                            #' @param target_ocean Object of type \code{\link[base]{integer}} expected. The code of ocean of interest.
                            #' @param distance_maximum Object of type \code{\link[base]{integer}} expected. Maximum distance between all sets of a sampled well. By default 5.
                            #' @param number_sets_maximum Object of type \code{\link[base]{integer}} expected. Maximum number of sets allowed in mixture. By default 5.
                            #' @param set_weight_minimum Object of type \code{\link[base]{integer}} expected. Minimum set size considered. Remove smallest set for which sample could not be representative. By default 6 t.
                            #' @param minimum_set_frequency Object of type \code{\link[base]{numeric}} expected. Minimum threshold proportion of set in a well to be used for model training in the process. By default 0.1.
                            #' @param vessel_id_ignored Object of type \code{\link[base]{integer}} expected. Specify list of vessel(s) id(s) to be ignored in the model estimation and prediction .By default NULL.
                            data_preparatory = function(inputs_level3 = NULL,
                                                        inputs_level3_path = NULL,
                                                        output_directory,
                                                        periode_reference_level3 = NULL,
                                                        target_year = as.integer(lubridate::year(Sys.time() - 1)),
                                                        period_duration = 4L,
                                                        target_ocean = NULL,
                                                        distance_maximum = as.integer(5),
                                                        number_sets_maximum = as.integer(5),
                                                        set_weight_minimum = as.integer(6),
                                                        minimum_set_frequency = 0.1,
                                                        vessel_id_ignored = NULL) {
                              # 1 - Arguments verification
                              if (codama::r_type_checking(r_object = output_directory,
                                                          type = "character",
                                                          length = 1L,
                                                          output = "logical") != TRUE) {
                                return(codama::r_type_checking(r_object = output_directory,
                                                               type = "character",
                                                               length = 1L,
                                                               output = "message"))
                              }
                              if (! inherits(x = target_year,
                                             what = "integer")
                                  || length(target_year) != 1
                                  || nchar(target_year) != 4) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"target_year\" argument, one value of class integer expected with a format on 4 digits.\n",
                                    sep = "")
                                stop()
                              } else if (! inherits(x = period_duration,
                                                    what = "integer")
                                         || length(period_duration) != 1
                                         || period_duration > 99) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"period_duration\" argument, one value of class integer expected with  maximum value 99.\n",
                                    sep = "")
                                stop()
                              } else if (!is.null(periode_reference_level3)
                                         && ! inherits(x = periode_reference_level3,
                                                       what = "integer")) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"periode_reference_level3\" argument, class integer expected.\n",
                                    sep = "")
                                stop()
                              } else if (! inherits(x = distance_maximum,
                                                    what = "integer")
                                         || length(distance_maximum) != 1) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"distance_maximum\" argument, one value of class integer expected.\n",
                                    sep = "")
                                stop()
                              } else if (! inherits(x = number_sets_maximum,
                                                    what = "integer")
                                         || length(number_sets_maximum) != 1) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"number_sets_maximum\" argument, one value of class integer expected.\n",
                                    sep = "")
                                stop()
                              } else if (! inherits(x = set_weight_minimum,
                                                    what = "integer")
                                         || length(set_weight_minimum) != 1) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"set_weight_minimum\" argument, one value of class integer expected.\n",
                                    sep = "")
                                stop()
                              } else if (! inherits(x = minimum_set_frequency,
                                                    what = "numeric")
                                         || length(minimum_set_frequency) != 1) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"minimum_set_frequency\" argument, one value of class numeric expected.\n",
                                    sep = "")
                                stop()
                              } else if (! class(vessel_id_ignored) %in% c("NULL",
                                                                           "integer")) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"vessel_id_ignored\" argument, class NULL of value(s) of class integer expected.\n",
                                    sep = "")
                                stop()
                              } else {
                                # 2 - Process
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Start process 3.1: data preparatory.\n",
                                    sep = "")
                                # directories verification
                                output_path <- output_directory
                                if (! all(c("level3/data",
                                            "level3/figure") %in% stringr::str_extract(string = list.files(path = output_path,
                                                                                                           full.names = TRUE,
                                                                                                           recursive = TRUE,
                                                                                                           include.dirs = TRUE),
                                                                                       pattern = "level3/[:alpha:]{4,6}$"))) {
                                  stop(format(x = Sys.time(),
                                              "%Y-%m-%d %H:%M:%S"),
                                       " - Error: invalid \"output_directory\" argument, use the argument \"initiate_directory\" to create valid output architecture.\n",
                                       sep = "")
                                }
                                if (is.null(periode_reference_level3)) {
                                  periode_reference_level3 <- seq.int(from = target_year,
                                                                      to = target_year - period_duration)
                                }
                                if (!is.null(inputs_level3_path)) {
                                  # load from t3 levels 1 and 2 outputs and merge accordingly to the target_year  and target_ocean----
                                  if(is.null(target_ocean)) {
                                    stop("target_ocean argument is missing")
                                  }
                                  file_available <- list.files(path = inputs_level3_path,
                                                               pattern = "inputs_level3_")
                                  file_year <- as.numeric(unlist(lapply(strsplit(x = file_available,
                                                                                 split = "_"),
                                                                        function(x){return(x[3])})))
                                  file_ocean <- as.numeric(unlist(lapply(strsplit(x = file_available,
                                                                                  split = "[_|.]"),
                                                                         function(x){return(x[5])})))

                                  # target_file <- file_available[file_year %in% target_year:(target_year - period_duration) & file_ocean == target_ocean]
                                  target_file <- file_available[file_year %in% periode_reference_level3 &
                                                                  file_ocean == target_ocean]
                                  dataset_target <- vector("list",
                                                           length = 5)
                                  names(dataset_target) <- c("act_chr",
                                                             "catch_set_lb",
                                                             "samw",
                                                             "sset",
                                                             "wp")
                                  dataset_target <- lapply(dataset_target,
                                                           function(x) {
                                                             x <- vector("list",
                                                                         length = length(target_file))
                                                           })
                                  for (x in seq_len(length.out = length(x = target_file))) {
                                    load(file.path(inputs_level3_path,
                                                   target_file[x],
                                                   fsep = "/"))
                                    # WARNING line to cancel when name standardization done----
                                    if(exists("process_level3") && is.list(get("process_level3"))){
                                      data_level3 <- process_level3
                                    }
                                    # sets characteristics
                                    dataset_target$act_chr[[x]] <- data_level3$act
                                    # catch by set, species and categories from logbook (t3 level 1)
                                    dataset_target$catch_set_lb[[x]] <- data_level3$act3
                                    # catch by set, species and categories (t3 level 2)
                                    dataset_target$samw[[x]] <- data_level3$samw
                                    # link between sample and set, + sample quality and type
                                    dataset_target$sset[[x]] <- data_level3$sset
                                    # well plan
                                    dataset_target$wp[[x]] <- data_level3$wp
                                  }
                                  dataset_target <- lapply(X = dataset_target,
                                                           FUN = function(x) {
                                                             return(unique(do.call(rbind, x)))
                                                           })

                                  # stock raw data
                                  inputs_level3 <- dataset_target
                                }
                                # sets characteristics
                                act_chr <- inputs_level3[[1]]
                                # catch by set, species and categories from logbook (t3 level 1)
                                catch_set_lb <- inputs_level3[[2]]
                                # catch by set, species and categories (t3 level 2)
                                samw <- inputs_level3[[3]]
                                # link between sample and set, + sample quality and type
                                sset <- inputs_level3[[4]]
                                # well plan
                                wp <- inputs_level3[[5]]
                                # parameters
                                target_species <- c("BET","SKJ","YFT")
                                # standardize weight category
                                catch_set_lb$wcat <- gsub("kg",
                                                          "",
                                                          catch_set_lb$wcat)
                                catch_set_lb$wcat <- ifelse(catch_set_lb$wcat == "<10",
                                                            "m10",
                                                            "p10")
                                # only one category (called less 10) use for SKJ
                                catch_set_lb$wcat[catch_set_lb$sp == "SKJ"] <- "m10"
                                # period parameters ----
                                first_year <- dplyr::first(periode_reference_level3)
                                # select subset period for the modelling
                                catch_set_lb$year <- lubridate::year(x = catch_set_lb$date_act)
                                catch_set_lb<-catch_set_lb[catch_set_lb$year %in% periode_reference_level3,]
                                act_chr$year <- lubridate::year(x = act_chr$date_act)
                                act_chr <- act_chr[act_chr$year %in% periode_reference_level3, ]
                                # compute selection criteria ----
                                cdm <- act_chr$id_act[act_chr$vessel %in% vessel_id_ignored]
                                sset <- sset[! sset$id_act %in% cdm, ]
                                catch_set_lb <- catch_set_lb[! catch_set_lb$id_act %in% cdm, ]
                                # selection criteria
                                # remove bad quality sample and keep sample at landing
                                sset <- sset[sset$quality == 1 & sset$type == 1, ]
                                # number of activity by sample
                                sset2 <- sset %>%
                                  dplyr::group_by(id_sample) %>%
                                  dplyr::mutate(nset = dplyr::n()) %>%
                                  dplyr::ungroup()
                                # fishing mode homogeneity in sample
                                # add fishing mode
                                sset2 <- dplyr::inner_join(x = sset2,
                                                           y = act_chr[, c("id_act", "fmod", "lat", "lon")],
                                                           by = "id_act")
                                fmod_purity_tmp <- sset2 %>%
                                  dplyr::distinct(id_sample, fmod) %>%
                                  dplyr::group_by(id_sample) %>%
                                  dplyr::summarise(fmod_purity = dplyr::n()) %>%
                                  dplyr::ungroup()
                                sset2 <- dplyr::inner_join(sset2,
                                                           fmod_purity_tmp,
                                                           by = "id_sample")
                                # fishing mode of the sample
                                sset2 <- sset2 %>%
                                  dplyr::mutate(fmod_sample = ifelse(fmod_purity == 1,
                                                                     fmod,
                                                                     999))
                                # extent of the sample
                                agg <- aggregate(x = cbind(lat_sample_dif = lat,
                                                           lon_sample_dif = lon) ~ id_sample,
                                                 data = sset2,
                                                 FUN = function(x) {
                                                   max(x) - min(x)
                                                 })
                                sset2 <- merge(x = sset2,
                                               y = agg,
                                               sort = FALSE)
                                # compute total set weight
                                sset2 <- droplevels(sset2)
                                tmp <- catch_set_lb
                                tmp <- tmp[tmp$sp %in% target_species, ]
                                agg3 <- aggregate(x = cbind(w_lb_t3 = w_lb_t3) ~ id_act,
                                                  data = tmp,
                                                  FUN = function(x) {
                                                    sum(x,
                                                        na.rm = TRUE)
                                                  })
                                agg3 <- agg3[agg3$id_act %in% sset2$id_act, ]
                                sset3 <- dplyr::inner_join(x = sset2,
                                                           y = agg3[, c("id_act",
                                                                        "w_lb_t3")],
                                                           by = "id_act")
                                sample_set_char <- list(sset = sset,
                                                        act_chr = act_chr,
                                                        catch_set_lb = catch_set_lb)
                                # compute set weight in each sample to detect non representiveness of the sample
                                agg_wp <- aggregate(x = cbind(w_in_well = weight) ~ id_sample + id_well + id_act,
                                                    data = wp,
                                                    FUN = sum)
                                agg_wp2 <- aggregate(x = cbind(w_tot_well = weight) ~ id_sample + id_well,
                                                     data = wp,
                                                     FUN = sum)
                                agg_wp <- merge(x = agg_wp,
                                                y = agg_wp2)
                                # compute proportion of weight by set
                                agg_wp$prop_act_chr <- agg_wp$w_in_well / agg_wp$w_tot_well
                                # selection of activities ----
                                # selection based on sets extrapolated (2 first step of the t3 process)
                                kiset <- sset3
                                # on sample
                                # homogeneous fishing mode in sample
                                kiset <- kiset[kiset$fmod_purity == 1, ]
                                # spatial selection + mixture limit
                                kiset <- kiset[kiset$lat_sample_dif < distance_maximum & kiset$lon_sample_dif < distance_maximum & kiset$nset < number_sets_maximum, ]
                                # remove all small sets considered as missed catches
                                kiset <- kiset[kiset$w_lb_t3 > set_weight_minimum, ]
                                # on set weight in well
                                # sets which represented less than 10 % of the sampled well
                                remove_sets <- agg_wp[agg_wp$prop_act_chr < minimum_set_frequency, ]
                                remove_sets$unik <- paste(remove_sets$id_sample,
                                                          remove_sets$id_act,
                                                          sep = "_")
                                kiset$unik <- paste(kiset$id_sample,
                                                    kiset$id_act,
                                                    sep = "_")
                                # remove sets with a too low weight in well for which we have the well plan
                                kiset <- kiset[!kiset$unik %in% remove_sets$unik, ]
                                kiset <- droplevels(kiset)
                                kiset_end <- kiset
                                # select sets
                                act_chr <- act_chr[act_chr$id_act %in% kiset_end$id_act, ]
                                catch_set_lb <- catch_set_lb[catch_set_lb$id_act %in% kiset_end$id_act, ]
                                data_selected <- list(act_chr = act_chr,
                                                      catch_set_lb = catch_set_lb,
                                                      kiset_end = kiset_end)
                                # format data and compute proportion ----
                                # name change
                                catch_set_lb$mon <- lubridate::month(x = catch_set_lb$date_act)
                                # select and rename species
                                catch_set_lb$sp[!catch_set_lb$sp %in% target_species] <- "OTH"
                                catch_set_lb <- droplevels(catch_set_lb)
                                # remove other species from lb before calculate species composition (to be compare to sample)
                                catch_set_lb <- catch_set_lb[catch_set_lb$sp %in% target_species, ]
                                catch_set_lb <- droplevels(catch_set_lb)
                                # calculate total catch for thonidae only
                                tot <- aggregate(x = cbind(wtot_lb_t3 = w_lb_t3) ~ id_act,
                                                 data = catch_set_lb,
                                                 FUN = sum)
                                catch_set_lb <- merge(x = catch_set_lb,
                                                      y = tot,
                                                      sort = FALSE)
                                # sum p10, 10-30 and p30 categories in Atlantic ocean
                                catch_set_lb <- aggregate(x = cbind(w_lb_t3) ~ id_act + date_act + code_act_type + year + mon + wtot_lb_t3 + sp + wcat,
                                                          data = catch_set_lb,
                                                          FUN = sum)
                                # calculate proportions
                                catch_set_lb$sp_cat <- factor(paste(catch_set_lb$sp,
                                                                    catch_set_lb$wcat,
                                                                    sep = "_"))
                                catch_set_lb$sp <- NULL
                                catch_set_lb$wcat <- NULL
                                tmp <- tidyr::spread(data = catch_set_lb,
                                                     key = sp_cat,
                                                     value = w_lb_t3,
                                                     fill = 0)
                                tmp2 <- tmp[, names(tmp) %in% levels(catch_set_lb$sp_cat)]
                                tmp2 <- prop.table(as.matrix(tmp2),
                                                   1)
                                tmp[, names(tmp) %in% colnames(tmp2)] <- tmp2
                                lb_set <- tmp
                                # compute proportion from t3 step 2 ----
                                samw$sp[!samw$sp %in% target_species] <- "OTH"
                                samw <- samw[samw$sp %in% target_species, ]
                                samw$wcat <- gsub("kg",
                                                  "",
                                                  samw$wcat)
                                # group p10, 10-30 and p30 categories
                                samw$wcat <- ifelse(samw$wcat == "- 10",
                                                    "m10",
                                                    "p10")
                                # only one category (called less 10) use for SKJ
                                samw$wcat[samw$sp == "SKJ"] <- "m10"
                                samw$sp_cat <- factor(paste(samw$sp,
                                                            samw$wcat,
                                                            sep = "_"))
                                samw$sp <- NULL
                                samw$wcat <- NULL
                                # sum the weight categories for SKJ
                                samw <- aggregate(x = cbind(w_fit_t3) ~ id_act + sp_cat,
                                                  data = samw,
                                                  FUN = sum)
                                samw <- droplevels(samw)
                                tmp <- tidyr::spread(data = samw,
                                                     key = sp_cat,
                                                     value = w_fit_t3,
                                                     fill = 0)
                                tmp2 <- tmp[, names(tmp) %in% levels(samw$sp_cat)]
                                tmp2 <- prop.table(as.matrix(tmp2), 1)
                                tmp[, names(tmp) %in% colnames(tmp2)] <- tmp2
                                samp_t3 <- tmp %>%  tidyr::pivot_longer(cols = contains("10"),
                                                                        names_to = "sp_cat",
                                                                        values_to = "prop_t3" )
                                tmp <- dplyr::left_join(x = samp_t3,
                                                        y = act_chr,
                                                        by = "id_act")
                                data_sample_extract <- list(samw = samw,
                                                            samp_t3 = samp_t3)
                                # fusion of the lb and sample composition ----
                                lb_set_long <- lb_set %>% tidyr::pivot_longer(cols = contains("10"),
                                                                              names_to = "sp_cat",
                                                                              values_to = "prop_lb")
                                lb_set_long <- dplyr::left_join(lb_set_long,
                                                                catch_set_lb,
                                                                by = c("id_act",
                                                                       "date_act",
                                                                       "code_act_type",
                                                                       "year",
                                                                       "mon",
                                                                       "wtot_lb_t3",
                                                                       "sp_cat"))
                                lb_set_long$w_lb_t3[is.na(lb_set_long$w_lb_t3)] <- 0
                                data <- dplyr::inner_join(x = lb_set_long,
                                                          y = samp_t3,
                                                          by = c("id_act", "sp_cat"))
                                data4mod <- dplyr::inner_join(x = data,
                                                              y = act_chr[, names(act_chr) %in% c("id_act",
                                                                                                  "fmod",
                                                                                                  "lat",
                                                                                                  "lon",
                                                                                                  "year",
                                                                                                  "mon",
                                                                                                  "ocean",
                                                                                                  "vessel",
                                                                                                  "flag_code")],
                                                              by = c("id_act", "year"))
                                data_lb_sample_screened <- list(data4mod = data4mod)
                                # export ----
                                output_level3_process1 <- list(sample_set_char = sample_set_char,
                                                               data_selected = data_selected,
                                                               data_sample_extract = data_sample_extract,
                                                               data_lb_sample_screened = data_lb_sample_screened)
                                cat(format(x = Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - End process 3.1: data preparatory.\n",
                                    sep = "")
                                return(list("raw_inputs_level3" = inputs_level3,
                                            "output_directory" = output_directory,
                                            "output_level3_process1" = output_level3_process1))
                              }
                            },
                            # process 3.2: random forest models ----
                            #' @description Modelling proportions in sets througth random forest models.
                            #' @param output_level3_process1 Object of type \code{\link[base]{data.frame}} expected. Output table data_lb_sample_screened from process 3.1.
                            #' @param num.trees Object of type \code{\link[base]{integer}} expected. Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times. The default value is 1000.
                            #' @param mtry Object of type \code{\link[base]{integer}} expected. Number of variables randomly sampled as candidates at each split. The default value is 2.
                            #' @param min.node.size Object of type \code{\link[base]{numeric}} expected. Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time).The default value is 5.
                            #' @param seed_number Object of type \code{\link[base]{integer}} expected. Set the initial seed for the modelling. The default value is 7.
                            #' @param small_fish_only Object of type \code{\link[base]{logical}} expected. Whether the model estimate proportion for small fish only (< 10 kg).
                            random_forest_models = function(output_level3_process1,
                                                            num.trees = 1000L,
                                                            mtry = 2L,
                                                            min.node.size = 5,
                                                            seed_number = 7L,
                                                            small_fish_only = FALSE) {
                              cat(format(Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.2: random forest models.\n",
                                  sep = "")
                              warn_defaut <- options("warn")
                              on.exit(options(warn_defaut))
                              options(warn = 1)
                              data4mod <- output_level3_process1
                              # sum proportion by species when working on total
                              data4mod <- tidyr::separate(data = data4mod,
                                                          col = sp_cat,
                                                          into = c("sp",
                                                                   "wcat"),
                                                          sep = "_")
                              # select for small fish catch only if parameter = T
                              if (small_fish_only == FALSE) {
                                data4mod <- data4mod %>%
                                  dplyr::group_by(id_act, date_act, year, mon, lat, lon, sp, fmod, ocean, vessel, flag_code, wtot_lb_t3) %>%
                                  dplyr::summarise(prop_lb = sum(prop_lb),
                                                   prop_t3 = sum(prop_t3),
                                                   w_lb_t3 = sum(w_lb_t3)) %>%
                                  dplyr::ungroup()
                              } else {
                                data4mod <- data4mod %>%
                                  dplyr::mutate(prop_lb = replace (prop_lb, wcat == "p10", value = 0),
                                                prop_t3 = replace (prop_t3,
                                                                   wcat == "p10",
                                                                   value = 0)) %>%
                                  dplyr::group_by(id_act,date_act, year, mon, lat, lon, sp, fmod, ocean, vessel, flag_code, wtot_lb_t3) %>%
                                  dplyr::summarise(prop_lb = sum(prop_lb),
                                                   prop_t3 = sum(prop_t3),
                                                   w_lb_t3 = sum(w_lb_t3)) %>%
                                  dplyr::ungroup()
                              }
                              output_level3_process2 <- list()
                              for (ocean in unique(data4mod$ocean)) {
                                data4mod_ocean <- data4mod[data4mod$ocean == ocean, ]
                                for(sp in unique(data4mod_ocean$sp)) {
                                  if (! sp %in% c("SKJ",
                                                  "YFT",
                                                  "BET")) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: process 3.2 not developed yet for the specie \"",
                                        sp,
                                        "\" in the ocean \"",
                                        ocean,
                                        "\".\n",
                                        "Data associated not used for this process.\n",
                                        sep = "")
                                  } else {
                                    data4mod_ocean_specie <- data4mod_ocean[data4mod_ocean$sp == sp, ]
                                    for (fmod in unique(data4mod_ocean_specie$fmod)) {
                                      cat(format(x = Sys.time(),
                                                 "%Y-%m-%d %H:%M:%S"),
                                          " - Ongoing process 3.2 for ocean \"",
                                          ocean,
                                          "\", specie \"",
                                          sp,
                                          "\" and fishing mode \"",
                                          fmod,
                                          "\"",
                                          ".\n",
                                          sep = "")
                                      sub <- data4mod_ocean_specie[data4mod_ocean_specie$fmod == fmod, ]
                                      sub$resp <- (sub$prop_t3)
                                      sub$tlb <- (sub$prop_lb)
                                      sub$year <- factor(sub$year)
                                      sub$mon <- factor(sub$mon)
                                      sub$vessel <- factor(sub$vessel)
                                      sub <- droplevels(sub)

                                      if(sp == "BET"){
                                        data4mod_ocean_specie <- data4mod_ocean[data4mod_ocean$sp == sp, ]
                                        output_level3_process2 <- append(output_level3_process2,
                                                                         list(list(data = sub)))
                                        names(output_level3_process2)[length(output_level3_process2)] <- paste(ocean, sp, fmod, sep = "_")
                                      } else {
                                        # models ----
                                        # model with spatio temporal variable only
                                        set.seed(seed_number)
                                        model_rf_simple <- ranger::ranger(resp ~ lon + lat + year + mon,
                                                                          data = sub,
                                                                          num.trees = num.trees,
                                                                          mtry = mtry,
                                                                          min.node.size = min.node.size,
                                                                          splitrule = "variance",
                                                                          importance = "impurity",
                                                                          replace = TRUE,
                                                                          quantreg = FALSE,
                                                                          keep.inbag = FALSE)
                                        # model with no vessel id
                                        set.seed(seed_number)
                                        model_rf_wtvessel <- ranger::ranger(resp ~ tlb + lon + lat + year + mon,
                                                                            data = sub,
                                                                            num.trees = num.trees,
                                                                            mtry = mtry,
                                                                            min.node.size = min.node.size,
                                                                            splitrule = "variance",
                                                                            importance = "impurity",
                                                                            replace = TRUE,
                                                                            quantreg = FALSE,
                                                                            keep.inbag = FALSE)
                                        # full model
                                        set.seed(seed_number)
                                        model_rf_full <- ranger::ranger(resp ~ tlb + lon + lat + year + mon + vessel,
                                                                        data = sub,
                                                                        num.trees = num.trees,
                                                                        mtry = mtry,
                                                                        min.node.size = min.node.size,
                                                                        splitrule = "variance",
                                                                        importance = "impurity",
                                                                        replace = TRUE,
                                                                        quantreg = FALSE,
                                                                        keep.inbag= FALSE)

                                        output_level3_process2 <- append(output_level3_process2,
                                                                         list(list(data = sub,
                                                                                   model_rf_simple = model_rf_simple,
                                                                                   model_rf_full = model_rf_full,
                                                                                   model_rf_wtvessel = model_rf_wtvessel)))
                                        names(output_level3_process2)[length(output_level3_process2)] <- paste(ocean, sp, fmod, sep = "_")
                                      }
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - Process 3.2 successfull for ocean \"",
                                          ocean,
                                          "\", specie \"",
                                          sp,
                                          "\" and fishing mode \"",
                                          fmod,
                                          "\"",
                                          ".\n",
                                          sep = "")
                                    }
                                  }
                                }
                              }
                              return(output_level3_process2)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.2: random forest models.\n",
                                  sep = "")
                            },
                            # process 3.3: models checking ----
                            #' @description Load each full model and compute figure and tables to check the model quality. Furthermore, create a map of samples used for each model and relationship between logbook reports and samples.
                            #' @param output_level3_process2 Object of type \code{\link[base]{list}} expected. Outputs models and data from process 3.2.
                            #' @param output_directory Object of type \code{\link[base]{character}} expected. Outputs directory path.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            #' @param plot_sample \code{\link[base]{logical}}. Whether the sample figure is computed. Default value = F
                            #' @param avdth_patch_coord parameter waiting for coordinate conversion patch from avdth database
                            #' @importFrom sp coordinates fullgrid gridded SpatialPoints CRS proj4string spTransform
                            #' @importFrom ranger ranger predictions importance
                            #' @importFrom adehabitatHR kernelUD getvolumeUD
                            #' @importFrom automap autoKrige
                            #' @importFrom sf st_as_sf
                            #' @import ggplot2
                            models_checking = function(output_level3_process2,
                                                       output_directory,
                                                       output_format = "eu",
                                                       plot_sample = FALSE,
                                                       avdth_patch_coord = FALSE) {
                              # 1 - Arguments verification ----
                              if (codama::r_type_checking(r_object = output_directory,
                                                          type = "character",
                                                          length = 1L,
                                                          output = "logical") != TRUE) {
                                stop(codama::r_type_checking(r_object = output_directory,
                                                             type = "character",
                                                             length = 1L,
                                                             output = "message"))
                              }
                              if (codama::r_type_checking(r_object = output_format,
                                                          type = "character",
                                                          length = 1L,
                                                          allowed_value = c("us",
                                                                            "eu"),
                                                          output = "logical") != TRUE) {
                                stop(codama::r_type_checking(r_object = output_format,
                                                             type = "character",
                                                             length = 1L,
                                                             allowed_value = c("us",
                                                                               "eu"),
                                                             output = "message"))
                              }
                              # 2 - Process ----
                              cat(format(x = Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.3: models checking.\n",
                                  sep = "")
                              warn_defaut <- options("warn")
                              on.exit(options(warn_defaut))
                              options(warn = 1)
                              output_level3_process3 <- list()
                              # extraction specifications
                              if (output_format == "us") {
                                outputs_dec <- "."
                                outputs_sep <- ","
                              } else if (output_format == "eu") {
                                outputs_dec <- ","
                                outputs_sep <- ";"
                              }
                              for (a in seq_len(length.out = length(output_level3_process2))) {
                                current_output_level3_process3 <- vector(mode = "list",
                                                                         length = 2)
                                names(current_output_level3_process3) <- c("figure",
                                                                           "table")
                                current_model_output <- output_level3_process2[[a]]
                                ocean = unlist(strsplit(names(output_level3_process2)[[a]],
                                                        "_"))[1]
                                specie = unlist(strsplit(names(output_level3_process2)[[a]],
                                                         "_"))[2]
                                fishing_mode = unlist(strsplit(names(output_level3_process2)[[a]],
                                                               "_"))[3]
                                cat(format(x = Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Ongoing process 3.3 for ocean \"",
                                    ocean,
                                    "\", specie \"",
                                    specie,
                                    "\" and fishing mode \"",
                                    fishing_mode,
                                    "\"",
                                    ".\n",
                                    sep = "")
                                figure_directory <- file.path(output_directory,
                                                              "level3",
                                                              "figure",
                                                              names(output_level3_process2)[[a]])
                                names(figure_directory) <- "figure"
                                table_directory <- file.path(output_directory,
                                                             "level3",
                                                             "data",
                                                             names(output_level3_process2)[[a]])
                                names(table_directory) <- "data"
                                for (b in c(figure_directory,
                                            table_directory)) {
                                  if(dir.exists(b)) {
                                    cat(format(x = Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Outputs \"",
                                        names(b),
                                        "\" directory for ocean \"",
                                        ocean,
                                        "\", specie \"",
                                        specie,
                                        "\" and fishing mode \"",
                                        fishing_mode,
                                        "\" already exists.\n",
                                        "Outputs associated will used this directory (be careful of overwriting previous files).\n",
                                        sep = "")
                                  } else {
                                    dir.create(path = b,
                                               recursive = TRUE)
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Outputs \"",
                                        names(b),
                                        "\" directory for ocean \"",
                                        ocean,
                                        "\", specie \"",
                                        specie,
                                        "\" and fishing mode \"",
                                        fishing_mode,
                                        "\" created.\n",
                                        "[directory path: ",
                                        figure_directory,
                                        "]\n",
                                        sep = "")
                                  }
                                }
                                # check data subset for modeling ----
                                current_model_data <- current_model_output[[1]]
                                period <- paste0("period: ",
                                                 min(as.numeric(as.character(current_model_data$year))),
                                                 " - ",
                                                 max(as.numeric(as.character(current_model_data$year))))
                                # tests for collinearity
                                covariance_matrix <- cor(x = current_model_data[, c("lat",
                                                                                    "lon",
                                                                                    "resp",
                                                                                    "tlb",
                                                                                    "wtot_lb_t3")])
                                write.table(x = covariance_matrix,
                                            file = file.path(table_directory,
                                                             paste("covariance_matrix_",
                                                                   ocean,
                                                                   "_",
                                                                   specie,
                                                                   "_",
                                                                   fishing_mode,
                                                                   ".csv",
                                                                   sep = "")),
                                            row.names = FALSE,
                                            sep = outputs_sep,
                                            dec = outputs_dec)
                                current_output_level3_process3[[2]] <- append(current_output_level3_process3[[2]],
                                                                              list("covariance_matrix" = covariance_matrix))

                                # rfUtilities remove from CRAN - need to test collinearity with another method ----
                                # multi_collinearity_test <- rfUtilities::multi.collinear(x = current_model_data[, c("lat",
                                #                                                                                    "lon",
                                #                                                                                    "resp",
                                #                                                                                    "tlb",
                                #                                                                                    "wtot_lb_t3",
                                #                                                                                    "mon",
                                #                                                                                    "year")],
                                #                                                         perm = TRUE,
                                #                                                         leave.out = TRUE)
                                # write.table(x = multi_collinearity_test,
                                #             file = file.path(table_directory,
                                #                              paste("multi_collinearity_test_",
                                #                                    ocean,
                                #                                    "_",
                                #                                    specie,
                                #                                    "_",
                                #                                    fishing_mode,
                                #                                    ".csv",
                                #                                    sep = "")),
                                #             row.names = FALSE,
                                #             sep = outputs_sep,
                                #             dec = outputs_dec)
                                # current_output_level3_process3[[2]] <- append(current_output_level3_process3[[2]],
                                #                                               list("multi_collinearity_test" = multi_collinearity_test))
                                # figure on logbook vs sample set
                                logbook_vs_sample_1 <- ggplot2::ggplot(data = current_model_data,
                                                                       ggplot2::aes(y = prop_t3,
                                                                                    x = prop_lb,
                                                                                    color = year)) +
                                  ggplot2::geom_point() +
                                  ggplot2::geom_smooth(method = "gam",
                                                       formula = y ~ s(x,
                                                                       bs = "cs",
                                                                       fx = FALSE,
                                                                       k = 5)) +
                                  ggplot2::geom_abline(slope = 1,
                                                       intercept = 0) +
                                  ggplot2::xlab("Species Frequency in set from logbook") +
                                  ggplot2::ylab("Species Frequency in set from sample") +
                                  ggplot2::ggtitle(paste(specie,
                                                         "on",
                                                         fishing_mode,
                                                         "sets",
                                                         sep = " "))
                                logbook_vs_sample_2 <- ggplot2::ggplot(current_model_data,
                                                                       ggplot2::aes(x = prop_lb,
                                                                                    color = year)) +
                                  ggplot2::geom_density(fill = rgb(0,0,0,0.1),
                                                        ggplot2::aes(y = ggplot2::after_stat(scaled))) +
                                  ggplot2::xlab("Species Frequency in set from logbook")
                                logbook_vs_sample <- ggpubr::ggarrange(logbook_vs_sample_1,
                                                                       logbook_vs_sample_2,
                                                                       nrow = 2,
                                                                       ncol = 1)
                                ggplot2::ggsave(plot = logbook_vs_sample,
                                                file = file.path(figure_directory,
                                                                 paste("logbook_vs_sample_",
                                                                       ocean,
                                                                       "_",
                                                                       specie,
                                                                       "_",
                                                                       fishing_mode,
                                                                       ".jpeg",
                                                                       sep = "")),
                                                width = 15,
                                                height = 30,
                                                units = c("cm"))
                                current_output_level3_process3[[1]] <- append(current_output_level3_process3[[1]],
                                                                              list("logbook_vs_sample" = logbook_vs_sample))
                                # various figures to visualize some relationship from data before modelling
                                # single vessel effect
                                vessel_effect <- ggplot2::ggplot(current_model_data,
                                                                 ggplot2::aes(x = vessel,
                                                                              y = resp)) +
                                  ggplot2::geom_boxplot() +
                                  ggplot2::ylab("Species Frequency in set from sample")
                                ggplot2::ggsave(plot = vessel_effect,
                                                file = file.path(figure_directory,
                                                                 paste("vessel_effect_",
                                                                       ocean,
                                                                       "_",
                                                                       specie,
                                                                       "_",
                                                                       fishing_mode,
                                                                       ".jpeg",
                                                                       sep = "")),
                                                width = 8,
                                                height = 8,
                                                units = c("cm"))
                                current_output_level3_process3[[1]] <- append(current_output_level3_process3[[1]],
                                                                              list("vessel_effect" = vessel_effect))
                                # month effect
                                month_variation <- ggplot2::ggplot(current_model_data,
                                                                   ggplot2::aes(x = mon,
                                                                                y = resp)) +
                                  ggplot2::geom_boxplot() +
                                  ggplot2::labs(x = "Month",
                                                y = "Species Frequency in set from sample")
                                ggplot2::ggsave(plot = month_variation,
                                                file = file.path(figure_directory,
                                                                 paste("month_effect_",
                                                                       ocean,
                                                                       "_",
                                                                       specie,
                                                                       "_",
                                                                       fishing_mode,
                                                                       ".jpeg",
                                                                       sep = "")),
                                                width = 8,
                                                height = 8,
                                                units = c("cm"))
                                current_output_level3_process3[[1]] <- append(current_output_level3_process3[[1]],
                                                                              list("month_variation" = month_variation))
                                # year effect
                                year_effect <- ggplot2::ggplot(current_model_data,
                                                               ggplot2::aes(x = year,
                                                                            y = resp)) +
                                  ggplot2::geom_boxplot() +
                                  ggplot2::labs(x = NULL,
                                                y = "Species Frequency in set from sample")
                                ggplot2::ggsave(plot = year_effect,
                                                file = file.path(figure_directory,
                                                                 paste("year_effect_",
                                                                       ocean,
                                                                       "_",
                                                                       specie,
                                                                       "_",
                                                                       fishing_mode,
                                                                       ".jpeg",
                                                                       sep = "")),
                                                width = 8,
                                                height = 8,
                                                units = c("cm"))
                                current_output_level3_process3[[1]] <- append(current_output_level3_process3[[1]],
                                                                              list("year_effect" = year_effect))
                                # species composition in logbook vs sampleset (t3 level 1)
                                reporting_vs_sampling_data <- tidyr::gather(current_model_data,
                                                                            "prop_lb",
                                                                            "prop_t3",
                                                                            key = "Source" ,
                                                                            value = "prop")
                                reporting_vs_sampling <- ggplot2::ggplot(data = reporting_vs_sampling_data,
                                                                         ggplot2::aes(x = year,
                                                                                      y = prop,
                                                                                      color = Source)) +
                                  ggplot2::geom_boxplot() +
                                  ggplot2::labs(x = "Year",
                                                y = "Proportion in sets") +
                                  ggplot2::scale_color_discrete(labels = c("Reporting",
                                                                           "Sampling"))
                                ggplot2::ggsave(plot = reporting_vs_sampling,
                                                file = file.path(figure_directory,
                                                                 paste("reporting_vs_sampling_",
                                                                       ocean,
                                                                       "_",
                                                                       specie,
                                                                       "_",
                                                                       fishing_mode,
                                                                       ".jpeg",
                                                                       sep = "")),
                                                width = 16,
                                                height = 16,
                                                units = c("cm"))
                                current_output_level3_process3[[1]] <- append(current_output_level3_process3[[1]],
                                                                              list("reporting_vs_sampling" = reporting_vs_sampling))
                                # map of the data used for modelling
                                if(plot_sample == TRUE){
                                  current_data_map <- current_model_data
                                  sp::coordinates(obj = current_data_map) <- ~ lon + lat
                                  ker <- adehabitatHR::kernelUD(sp::SpatialPoints(current_data_map),
                                                                grid = 500)
                                  ker2 <- adehabitatHR::getvolumeUD(x = ker)
                                  grid <- as(object = ker2,
                                             Class = "SpatialPixelsDataFrame")
                                  newgrid <- grid
                                  sp::fullgrid(obj = newgrid) <- FALSE
                                  sp::gridded(obj = newgrid) <- FALSE
                                  newgrid[newgrid$n > 99] <- NA
                                  newgrid <- newgrid[ !is.na(newgrid$n), ]
                                  krig <- automap::autoKrige(formula = resp ~ 1,
                                                             input_data = current_data_map,
                                                             new_data = newgrid)
                                  interp_data <- as.data.frame(krig$krige_output)
                                  colnames(interp_data) = c("lon", "lat", "fit", "fit.var", "fit_stdev")
                                  # correct for abnormal fitted value with kriging
                                  interp_data$fit[interp_data$fit > 1] <- 1
                                  load(file = system.file("wrld_simpl.RData",
                                                          package = "t3"))
                                  wrld_sf <- sf::st_as_sf(wrld_simpl)
                                  set_sampled_map <- ggplot2::ggplot() +
                                    ggplot2::geom_tile(data = interp_data,
                                                       ggplot2::aes(x = lon,
                                                                    y = lat,
                                                                    fill = fit),
                                                       color = NA) +
                                    ggplot2::scale_fill_gradient2(low = "blue",
                                                                  mid = "white",
                                                                  high = "red",
                                                                  midpoint = mean(interp_data$fit),
                                                                  name = "Proportion") +
                                    ggplot2::geom_point(data = current_model_data,
                                                        ggplot2::aes(x = lon,
                                                                     y = lat),
                                                        color = "black",
                                                        size = 0.3) +
                                    ggplot2::geom_sf(data = wrld_sf) +
                                    ggplot2::coord_sf(xlim = c(min(interp_data$lon),
                                                               max(interp_data$lon)),
                                                      ylim = c(min(interp_data$lat),
                                                               max(interp_data$lat))) +
                                    ggplot2::labs(x = NULL,
                                                  y = NULL,
                                                  subtitle = paste(specie,
                                                                   ocean,
                                                                   fishing_mode,
                                                                   period,
                                                                   sep = "_")) +
                                    ggplot2::theme_classic()
                                  ggplot2::ggsave(plot = set_sampled_map,
                                                  file = file.path(figure_directory,
                                                                   paste("set_sampled_map_",
                                                                         ocean,
                                                                         "_",
                                                                         specie,
                                                                         "_",
                                                                         fishing_mode,
                                                                         ".jpeg",
                                                                         sep = "")),
                                                  width = 18,
                                                  height = 10,
                                                  units = c("cm"),
                                                  pointsize = 10)
                                  current_output_level3_process3[[1]] <- append(current_output_level3_process3[[1]],
                                                                                list("set_sampled_map" = set_sampled_map))
                                }
                                ## model checking ----
                                if(specie != "BET"){
                                  # compute model residuals
                                  resrf <- current_model_data$resp - ranger::predictions(current_model_output[[3]])
                                  current_model_data$res <- resrf
                                  current_model_data$res_ST <- resrf / sd(ranger::predictions(current_model_output[[3]]))
                                  current_model_data$fit<-ranger::predictions(current_model_output[[3]])
                                  # method
                                  # comparison of the model fitted value
                                  # look at variable importance in the model
                                  variables_importance <- as.data.frame(ranger::importance(current_model_output[[3]]))
                                  names(variables_importance) <- "value"
                                  variables_importance$var_name <- rownames(variables_importance)

                                  variables_importance <- variables_importance[order(variables_importance$value,
                                                                                     decreasing = FALSE), ]
                                  variables_importance_plot <- ggplot2::ggplot(data = variables_importance,
                                                                               ggplot2::aes(y = var_name,
                                                                                            x = value)) +
                                    ggplot2::geom_point() +
                                    ggplot2::geom_segment(data = variables_importance,
                                                          ggplot2::aes(x = 0,
                                                                       xend = value,
                                                                       y = var_name,
                                                                       yend = var_name)) +
                                    ggplot2::scale_y_discrete(name = "Variables",
                                                              limits= variables_importance$var_name) +
                                    ggplot2::xlab("Importance (impurity)")
                                  ggplot2::ggsave(plot = variables_importance_plot,
                                                  file = file.path(figure_directory,
                                                                   paste("variables_importance_",
                                                                         ocean,
                                                                         "_",
                                                                         specie,
                                                                         "_",
                                                                         fishing_mode,
                                                                         ".jpeg",
                                                                         sep = "")),
                                                  width = 8,
                                                  height = 8,
                                                  units = c("cm"),
                                                  dpi = 300,
                                                  pointsize = 10)
                                  current_output_level3_process3[[1]] <- append(current_output_level3_process3[[1]],
                                                                                list("variables_importance" = variables_importance))
                                  # test for spatial and temporal correlation on residuals
                                  if(avdth_patch_coord == TRUE){
                                    current_model_data_map <- current_model_data
                                    sp::coordinates(current_model_data_map) <- ~ lon+lat
                                    sp::proj4string(current_model_data_map) <- sp::CRS("+init=epsg:4326")
                                    current_model_data_map <- sp::spTransform(current_model_data_map,
                                                                              sp::CRS("+init=epsg:3395"))
                                    # variogram
                                    variogram_resp_data <- gstat::variogram(object = resp ~ 1,
                                                                            data = current_model_data_map,
                                                                            cutoff = 4000000,
                                                                            width = 100000)
                                    variogram_resp_data$label <- "Observed data"
                                    variogram_res_data <- gstat::variogram(object = res ~ 1,
                                                                           data = current_model_data_map,
                                                                           cutoff = 4000000,
                                                                           width = 100000)
                                    variogram_res_data$label <- "Residuals"
                                    variogram_data <- dplyr::bind_rows(variogram_resp_data,
                                                                       variogram_res_data)
                                    variogram <- ggplot2::ggplot(data = variogram_data,
                                                                 ggplot2::aes(x = dist,
                                                                              y = gamma,
                                                                              group = label,
                                                                              color = label)) +
                                      ggplot2::scale_color_manual(values = c("black", "red")) +
                                      ggplot2::geom_line() +
                                      ggplot2::theme(legend.position = c(0,1),
                                                     legend.justification = c(0,1),
                                                     legend.direction="horizontal",
                                                     legend.title = ggplot2::element_blank()) +
                                      ggplot2::xlab("Distance (m)") +
                                      ggplot2::ylab("Semivariance")
                                    # moran index on residual
                                    mil <- vector("list",
                                                  length = 10)
                                    mil2 <- vector("list",
                                                   length = 10)
                                    for (c in seq_len(10)){
                                      nb <- spdep::dnearneigh(as.matrix(current_model_data[, c("lon", "lat")]),
                                                              d1 = 0,
                                                              d2 = c * 100,
                                                              longlat = TRUE)
                                      listw <- spdep::nb2listw(neighbours = nb,
                                                               zero.policy = TRUE)
                                      mil[[c]] <- spdep::moran.mc(x = current_model_data$res,
                                                                  listw = listw,
                                                                  zero.policy = TRUE,
                                                                  nsim = 999,
                                                                  alternative = "greater")
                                      mil2[[c]] <- spdep::moran.test(x = current_model_data$res,
                                                                     listw = listw,
                                                                     zero.policy = TRUE,
                                                                     alternative = "two.sided")
                                    }
                                    moran_residual_test <- data.frame(dist = 1:10,
                                                                      estimate = unlist(lapply(mil2,
                                                                                               function(d) {
                                                                                                 d$estimate[1]
                                                                                               })))
                                    moran_residual_test$var <- unlist(lapply(mil2,
                                                                             function(e) {
                                                                               e$estimate[3]
                                                                             }))
                                    moran_residual_test$pvaltest <- unlist(lapply(mil2,
                                                                                  function(f) {
                                                                                    f$p.value
                                                                                  }))
                                    moran_residual_test$pvalmc <- unlist(lapply(mil,
                                                                                function(g) {
                                                                                  g$p.value
                                                                                }))
                                    write.table(x = moran_residual_test,
                                                file = file.path(table_directory,
                                                                 paste("moran_residual_test_",
                                                                       ocean,
                                                                       "_",
                                                                       specie,
                                                                       "_",
                                                                       fishing_mode,
                                                                       ".csv",
                                                                       sep = "")),
                                                row.names = FALSE,
                                                sep = outputs_sep,
                                                dec = outputs_dec)
                                    current_output_level3_process3[[2]] <- append(current_output_level3_process3[[2]],
                                                                                  list("moran_residual_test" = moran_residual_test))
                                    correlogram_resp <- forecast::ggAcf(current_model_data$resp[order(current_model_data$date_act)], lag.max = 300)
                                    # lag_max = 300)
                                    correlogram_resp_acf <- correlogram_resp +
                                      ggplot2::ggtitle("Autocorrelation function of observed data")
                                    moran_index <- ggplot2::ggplot(data = moran_residual_test,
                                                                   ggplot2::aes(x = dist,
                                                                                y = estimate)) +
                                      ggplot2::geom_line() +
                                      ggplot2::geom_point() +
                                      ggplot2::geom_line(ggplot2::aes(x = dist,
                                                                      y = estimate + sqrt(var)),
                                                         color = "lightskyblue") +
                                      ggplot2::geom_line(ggplot2::aes(x = dist,
                                                                      y = estimate - sqrt(var)),
                                                         color = "lightskyblue") +
                                      ggplot2::geom_line(ggplot2::aes(x = dist,
                                                                      y = 0),
                                                         color = "red",
                                                         linetype = 2) +
                                      ggplot2::ylim(-0.3, 0.4) +
                                      ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
                                      ggplot2::xlab("Distance (10^2 km)") +
                                      ggplot2::ylab("Moran index")
                                    correlogram_res <- forecast::ggAcf(current_model_data$res[order(current_model_data$date_act)], lag.max = 300)
                                    correlogram_res_acf <- correlogram_res +
                                      ggplot2::ggtitle("Autocorrelation function of residuals")+
                                      ggplot2::ylim(min(correlogram_resp$data$Freq, correlogram_res$data$Freq), max(correlogram_resp$data$Freq, correlogram_res$data$Freq))

                                    spatio_temporal_checking <- ggpubr::ggarrange(variogram,
                                                                                  correlogram_resp_acf,
                                                                                  moran_index,
                                                                                  correlogram_res_acf,
                                                                                  nrow = 2,
                                                                                  ncol = 2)
                                    ggplot2::ggsave(plot = spatio_temporal_checking,
                                                    file = file.path(figure_directory,
                                                                     paste("spatio_temporal_checking_",
                                                                           ocean,
                                                                           "_",
                                                                           specie,
                                                                           "_",
                                                                           fishing_mode,
                                                                           ".jpeg",
                                                                           sep = "")),
                                                    width = 30,
                                                    height = 20,
                                                    units = c("cm"),
                                                    dpi = 300)
                                    current_output_level3_process3[[1]] <- append(current_output_level3_process3[[1]],
                                                                                  list("spatio_temporal_checking" = spatio_temporal_checking))
                                  }
                                  ## model validation ----
                                  model_validation_density_res <- ggplot2::ggplot(current_model_data,
                                                                                  ggplot2::aes(x = res_ST)) +
                                    ggplot2::geom_density(stat = "density",
                                                          fill = rgb(1,0,0,0.2),
                                                          ggplot2::aes(y = ggplot2::after_stat(scaled))) +
                                    ggplot2::scale_x_continuous(expand = c(0, 0)) +
                                    ggplot2::labs(x = "Standardized Residuals")
                                  model_validation_qqplot_res <- ggplot2::ggplot(current_model_data, ggplot2::aes(sample = res_ST)) +
                                    ggplot2::stat_qq() +
                                    ggplot2::stat_qq_line(col = 2) +
                                    ggplot2::labs(x = "Theoretical quantiles", y = "Sample quantiles")
                                  model_validation_response_fit <- ggplot2::ggplot(data = current_model_data,
                                                                                   ggplot2::aes(x = resp,
                                                                                                y = fit)) +
                                    ggplot2::geom_point() +
                                    ggplot2::geom_smooth(method = "gam",
                                                         formula = y ~ s(x, bs = "cs")) +
                                    ggplot2::geom_abline(slope = 1,
                                                         intercept = 0,
                                                         col = "red") +
                                    ggplot2::labs(x = "Observed values",
                                                  y = "Fitted values")
                                  model_validation_fit_res <- ggplot2::ggplot(data = current_model_data,
                                                                              ggplot2::aes(x = fit,
                                                                                           y = res_ST)) +
                                    ggplot2::geom_point() +
                                    ggplot2::geom_smooth(method = "gam",
                                                         formula = y ~ s(x, bs = "cs")) +
                                    ggplot2::geom_abline(slope = 0,
                                                         intercept = 0,
                                                         col = "red") +
                                    ggplot2::labs(x = "Fitted values",
                                                  y = "Standardized Residuals")
                                  model_validation_logbook_res <- ggplot2::ggplot(data = current_model_data,
                                                                                  ggplot2::aes(x = tlb,
                                                                                               y = res_ST)) +
                                    ggplot2::geom_point() +
                                    ggplot2::geom_smooth(method = "gam",
                                                         formula = y ~ s(x, bs = "cs")) +
                                    ggplot2::geom_abline(slope = 0,
                                                         intercept = 0,
                                                         col = "red") +
                                    ggplot2::labs(x = "Proportion in logbook",
                                                  y = "Standardized Residuals")
                                  model_validation_yr_res <- ggplot2::ggplot(data = current_model_data,
                                                                             ggplot2::aes(x = year,
                                                                                          y = res_ST)) +
                                    ggplot2::geom_boxplot() +
                                    ggplot2::geom_abline(slope = 0,
                                                         intercept = 0,
                                                         col = "red") +
                                    ggplot2::labs(x = NULL,
                                                  y = "Standardized Residuals")
                                  model_validation_mon_res <- ggplot2::ggplot(data = current_model_data,
                                                                              ggplot2::aes(x = mon,
                                                                                           y = res_ST)) +
                                    ggplot2::geom_boxplot() +
                                    ggplot2::geom_abline(slope = 0,
                                                         intercept = 0,
                                                         col = "red") +
                                    ggplot2::labs(x = "Month",
                                                  y = "Standardized Residuals")
                                  model_validation_vessel_res <- ggplot2::ggplot(data = current_model_data,
                                                                                 ggplot2::aes(x = vessel,
                                                                                              y = res_ST)) +
                                    ggplot2::geom_boxplot() +
                                    ggplot2::geom_abline(slope = 0, intercept = 0, col="red") +
                                    ggplot2::labs(x = "Vessel",
                                                  y = "Standardized Residuals")
                                  model_validation <- ggpubr::ggarrange(model_validation_density_res,
                                                                        model_validation_qqplot_res,
                                                                        model_validation_response_fit,
                                                                        model_validation_fit_res,
                                                                        model_validation_logbook_res,
                                                                        model_validation_yr_res,
                                                                        model_validation_mon_res,
                                                                        model_validation_vessel_res,
                                                                        nrow = 2,
                                                                        ncol = 4)
                                  ggplot2::ggsave(plot = model_validation,
                                                  file = file.path(figure_directory,
                                                                   paste("model_validation_",
                                                                         ocean,
                                                                         "_",
                                                                         specie,
                                                                         "_",
                                                                         fishing_mode,
                                                                         ".jpeg",
                                                                         sep = "")),
                                                  width = 30,
                                                  height = 20,
                                                  units = c("cm"),
                                                  dpi = 300)
                                  current_output_level3_process3[[1]] <- append(current_output_level3_process3[[1]],
                                                                                list("model_validation" = model_validation))
                                  ## model accuracy ----
                                  # cross validation by k-folds
                                  npartition <- 10 # not a parameter
                                  df <- current_model_data
                                  if(nrow(df) < 50){
                                    cat(format(x = Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " Current dataset < 50 data. Not enougth data for model accuracy testing.\n",
                                        sep = "")
                                  } else {
                                    model_formula <- strsplit(as.character(current_model_output[[3]]$call),",")[[2]]
                                    model_ntree <- current_model_output[[3]]$num.trees
                                    model_mtry <- current_model_output[[3]]$mtry
                                    model_node <- current_model_output[[3]]$min.node.size
                                    set.seed(7)
                                    fold <- data.frame(row_ord = sample(x = seq_len(length.out = nrow(df)),
                                                                        size = nrow(df),
                                                                        replace = FALSE),
                                                       nfold = rep_len(x = seq_len(length.out = npartition),
                                                                       length.out = nrow(df)))
                                    resi <- vector(mode = "list",
                                                   length = npartition)
                                    mufit <- vector(mode = "list",
                                                    length = npartition)
                                    for (h in seq_len(length.out = npartition)) {
                                      test <- df[fold$row_ord[fold$nfold == h], ]
                                      train <- df[fold$row_ord[fold$nfold != h], ]
                                      set.seed(7)
                                      model <- ranger::ranger(formula = model_formula,
                                                              data = train,
                                                              num.trees = model_ntree,
                                                              mtry = model_mtry,
                                                              min.node.size = model_node,
                                                              splitrule = "variance")

                                      test$fit <- predict(model,data = test)$predictions
                                      resi[[h]] <- test$resp - test$fit
                                      mufit[[h]] <- mean(test$resp)
                                    }
                                    RMSE <- NULL
                                    MAE <- NULL
                                    CVMAE <- NULL
                                    RMSE <- unlist(lapply(resi,
                                                          function(i) {
                                                            ifelse(test = ! is.null(i),
                                                                   yes = sqrt(mean((i^2))),
                                                                   no = NA)
                                                          }))
                                    MAE <- unlist(lapply(resi,
                                                         function(j) {
                                                           ifelse(test = ! is.null(j),
                                                                  yes = mean(abs(j)),
                                                                  no = NA)
                                                         }))
                                    CVMAE <- MAE / (unlist(mufit))
                                    kfold <- data.frame(index = c("RMSE",
                                                                  "MAE",
                                                                  "CVMAE"),
                                                        value = c(mean(RMSE,
                                                                       na.rm = TRUE),
                                                                  mean(MAE,
                                                                       na.rm = TRUE),
                                                                  mean(CVMAE,
                                                                       na.rm = TRUE)),
                                                        stdev = c(sd(RMSE),
                                                                  sd(MAE),
                                                                  sd(CVMAE)))
                                    write.table(x = kfold,
                                                file = file.path(table_directory,
                                                                 paste("kfold_",
                                                                       ocean,
                                                                       "_",
                                                                       specie,
                                                                       "_",
                                                                       fishing_mode,
                                                                       ".csv",
                                                                       sep = "")),
                                                row.names = FALSE,
                                                sep = outputs_sep,
                                                dec = outputs_dec)
                                    current_output_level3_process3[[2]] <- append(current_output_level3_process3[[2]],
                                                                                  list("kfold" = kfold))
                                    output_level3_process3 <- append(output_level3_process3,
                                                                     list(current_output_level3_process3))
                                    names(output_level3_process3)[length(output_level3_process3)] <- paste(ocean, specie, fishing_mode, sep = "_")
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Process 3.3 successfull for ocean \"",
                                    ocean,
                                    "\", specie \"",
                                    specie,
                                    "\" and fishing mode \"",
                                    fishing_mode,
                                    "\"",
                                    ".\n",
                                    sep = "")
                              }
                              return(output_level3_process3)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.3: models checking.\n",
                                  sep = "")
                            },
                            # process 3.4: data formatting for predictions ----
                            #' @description Formatting data for model predictions.
                            #' @param inputs_level3 Object of type \code{\link[base]{data.frame}} expected. Inputs of levels 3 (see function path to level 3).
                            #' @param output_level3_process1 Object of type \code{\link[base]{data.frame}} expected. Output table data_lb_sample_screened from process 3.1.
                            #' @param target_year Object of type \code{\link[base]{integer}} expected. The year of interest for the model estimation and prediction.
                            #' @param vessel_id_ignored Object of type \code{\link[base]{integer}} expected. Specify here vessel(s) id(s) if you want to ignore it in the model estimation and prediction .By default NULL.
                            #' @param small_fish_only Object of type \code{\link[base]{logical}} expected. Whether the model estimate proportion for small fish only (< 10 kg).
                            #' @param country_flag Three letters FAO flag code of country or countries to estimate catches.
                            #' @param input_type Type of coding use in different databases. Default value is 'observe_database'. Values can be 'observe_database' or 'avdth_database'.

                            data_formatting_for_predictions = function(inputs_level3,
                                                                       output_level3_process1,
                                                                       target_year,
                                                                       vessel_id_ignored = NULL,
                                                                       country_flag = NULL,
                                                                       input_type = "observe_database",
                                                                       small_fish_only = FALSE) {
                              cat(format(x = Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.4: data formatting for predictions.\n",
                                  sep = "")
                              warn_defaut <- options("warn")
                              on.exit(options(warn_defaut))
                              options(warn = 1)
                              output_level3_process4 <- list()
                              # load from t3 levels 1 and 2 outputs ----
                              # sets characteristics
                              act_chr <- inputs_level3[[1]]
                              # catch by set, species and categories from logbook (t3 level 1)
                              catch_set_lb <- inputs_level3[[2]]
                              # catch by set, species and categories (t3 level 2)
                              samw <- inputs_level3[[3]]
                              # link between sample and set, + sample quality and type
                              sset <- inputs_level3[[4]]
                              # well plan
                              wp <- inputs_level3[[5]]

                              act_chr$yr <- lubridate::year(x = act_chr$date_act)
                              act_chr$mon <- lubridate::month(x = act_chr$date_act)
                              act_chr$fmod <- as.factor(act_chr$fmod)
                              act_chr$vessel <- as.factor(act_chr$vessel)

                              # reduce dataset to the period and flag considered in the modeling and check data availability
                              act_chr <- act_chr %>% dplyr::filter(yr %in% target_year,
                                                                   flag_code %in% country_flag)
                              if (nrow(act_chr) == 0) {
                                cat(format(x = Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Error: No data available for the selected target_year and/or country_flag\n",
                                    sep = "")
                                stop()
                              }

                              # add the weight by categories, species from logbook (corrected by t3 level 1)
                              catch_set_lb <- dplyr::inner_join(act_chr,
                                                                catch_set_lb,
                                                                by = c("id_act",
                                                                       "date_act",
                                                                       "code_act_type"))
                              ############################################################################
                              # WARNING catches remove discard----
                              # changer pour le code espece et la colonne discard
                              # catch_discard <- catch_set_lb %>% dplyr::filter(sp_code %in% c(8, 800:899))
                              # catch_set_lb <- catch_set_lb %>% dplyr::filter(!sp_code %in% c(8, 800:899))
                              # catch_set_lb$sp_code <- NULL
                              catch_discard <- catch_set_lb %>% dplyr::filter(sp_fate_code != 6)
                              catch_set_lb <- catch_set_lb %>% dplyr::filter(sp_fate_code == 6)
                              ###########################################################################
                              target_species <- c("BET", "SKJ", "YFT")
                              set_with_target_species <- catch_set_lb %>%
                                dplyr::filter(sp %in% target_species) %>%
                              dplyr::distinct(id_act)

                              set_with_mix_tuna <- catch_set_lb %>%
                                dplyr::filter(sp %in% c("MIX")) %>%
                                dplyr::distinct(id_act)
                              catch_without_target_species <- catch_set_lb %>%
                                dplyr::filter(!id_act %in% c(set_with_target_species$id_act,set_with_mix_tuna$id_act))

                              catch_with_mix_tuna <- catch_set_lb %>%
                                dplyr::filter(id_act %in% set_with_mix_tuna$id_act)
                              catch_with_target_species <- catch_set_lb %>%
                                dplyr::filter(id_act %in% set_with_target_species$id_act,
                                              !id_act %in% set_with_mix_tuna$id_act)
                              catch_with_other_species <- catch_with_target_species %>%
                                dplyr::filter(!sp %in% target_species)

                              catch_data_not_corrected <- list(catch_with_mix_tuna = catch_with_mix_tuna,
                                                               catch_without_target_species =  catch_without_target_species,
                                                               catch_with_other_species = catch_with_other_species,
                                                               catch_discard = catch_discard)

                              catch_set_lb <- catch_with_target_species %>%
                                dplyr::filter(sp %in% target_species)
                              catch_set_lb <- droplevels(catch_set_lb)
                              # standardize weight category
                              catch_set_lb$wcat <- gsub("kg",
                                                        "",
                                                        catch_set_lb$wcat)
                              catch_set_lb$wcat <- ifelse(catch_set_lb$wcat == "<10",
                                                          "m10",
                                                          "p10")
                              # only one category (called less 10) use for SKJ
                              catch_set_lb$wcat[catch_set_lb$sp == "SKJ"] <- "m10"

                              # sum duplicated
                              catch_set_lb <- catch_set_lb %>%
                                dplyr::group_by(dplyr::across(c(-w_lb_t3))) %>%
                                dplyr::summarise(w_lb_t3 = sum(w_lb_t3)) %>%
                                dplyr::ungroup()
                              # set use for modeling to remove for prediction
                              ##############################################################
                              # WARNING - data are not filtered. sample data were so also predicted #----
                              ##############################################################
                              # data4mod <- output_level3_process1
                              # sampleset <- unique(data4mod[, c("id_act",
                              #                                  "fmod",
                              #                                  "ocean",
                              #                                  "year")])

                              # catches keep onboard only = set
                              # WARNING - fix the activity code according to the dev in level 1 and 2----
                              if(input_type == "observe_database"){
                                sets <- catch_set_lb %>% dplyr::filter(code_act_type %in% c(6))

                              }else if(input_type == "avdth_database") {
                                sets <- catch_set_lb %>% dplyr::filter(code_act_type %in% c(0,1,2))
                              }
                              sets$sp <- factor(sets$sp)
                              sets$ocean <- factor(sets$ocean)
                              sets <- sets[! sets$vessel %in% vessel_id_ignored, ]
                              sets$sp_cat <- factor(paste(sets$sp,
                                                          sets$wcat,
                                                          sep = "_"))
                              sets$sp <- NULL
                              sets$wcat <- NULL
                              # calculate proportion of weight from t3 level 1
                              sets_compo <- sets %>%
                                dplyr::group_by(id_act) %>%
                                dplyr::mutate(wtot_lb_t3 = sum(w_lb_t3)) %>%
                                dplyr::mutate(prop_lb = w_lb_t3 / wtot_lb_t3) %>%
                                dplyr::ungroup()
                              sets_long <- sets_compo %>%
                                dplyr::select(id_act, sp_cat, prop_lb, w_lb_t3) %>%
                                tidyr::complete(id_act, sp_cat, fill = list(prop_lb = 0, w_lb_t3 = 0))
                              sets_long <- dplyr::left_join(sets_long, distinct(dplyr::select(.data = sets_compo, -c(prop_lb, w_lb_t3, sp_cat)))) %>%
                                group_by(id_act, sp_cat) %>% mutate(dupli = dplyr::n())
                              sets_wide <- sets_long %>% dplyr::select(-w_lb_t3) %>% tidyr::pivot_wider(values_from = prop_lb, names_from = sp_cat)
                              # sets_wide <- tidyr::spread(data = sets,
                              #                            key = sp_cat,
                              #                            value = w_lb_t3,
                              #                            fill = 0) %>%
                              #   group_by(id_act) %>% mutate(dupli = dplyr::n())
                              # sets_wide$wtot_lb_t3 <- rowSums(sets_wide[, c("YFT_p10",
                              #                                               "BET_p10",
                              #                                               "SKJ_m10",
                              #                                               "YFT_m10",
                              #                                               "BET_m10")])
                              # sets_wide$fmod <- factor(sets_wide$fmod)
                              # # remove activity with no catch
                              # sets_wide <- sets_wide[sets_wide$wtot_lb_t3 > 0, ]
                              # tmp <- sets_wide[, names(sets_wide) %in% levels(sets$sp_cat)]
                              # tmp <- prop.table(as.matrix(tmp), 1)
                              # sets_wide_tmp <- sets_wide
                              # sets_wide_tmp[, names(sets_wide_tmp) %in% colnames(tmp)] <- tmp
                              # sets_long <- tidyr::gather(data = sets_wide_tmp,
                              #                            key = "sp_cat",
                              #                            value = "prop_lb",
                              #                            "BET_m10",
                              #                            "BET_p10",
                              #                            "SKJ_m10",
                              #                            "YFT_m10",
                              #                            "YFT_p10")
                              # Assign fishing mode to unknown
                              test <- droplevels(sets_wide[sets_wide$fmod == 0, ])
                              if(nrow(test) > 0) {
                                train <- droplevels(sets_wide[sets_wide$fmod != 0, ])
                                ntree <- 1000
                                set.seed(7)
                                rfg <- ranger::ranger(fmod ~ YFT_p10 + BET_p10 + SKJ_m10 + YFT_m10 + BET_m10,
                                                      data = train,
                                                      mtry=2L,
                                                      num.trees = ntree,
                                                      min.node.size = 5L,
                                                      splitrule = "gini",
                                                      importance = "impurity",
                                                      replace = TRUE,
                                                      quantreg = FALSE,
                                                      keep.inbag= FALSE)
                                test$fmod2 <- predict(rfg,
                                                      data = test)$predictions
                                tmp <- dplyr::left_join(sets_long,
                                                        test[, c("id_act","fmod2")],
                                                        by = "id_act")

                                # Unknown fishing mode or school type code=0 in observe_database
                                tmp$fmod[tmp$fmod == 0] <- tmp$fmod2[tmp$fmod == 0]
                                tmp$fmod2 <- NULL
                                sets_long <- droplevels(tmp)
                              }
                              sets_long <- tidyr::separate(data = sets_long,
                                                           col = sp_cat,
                                                           into = c("sp","wcat"),
                                                           sep = "_")
                              # filter data for small fish catch estimation only
                              if (small_fish_only == FALSE) {
                                sets_long <- sets_long %>%
                                  dplyr::group_by(id_act,
                                                  id_trip,
                                                  date_act,
                                                  yr,
                                                  mon,
                                                  lat,
                                                  lon,
                                                  sp,
                                                  fmod,
                                                  ocean,
                                                  vessel,
                                                  flag_code,
                                                  wtot_lb_t3) %>%
                                  dplyr::summarise(prop_lb = sum(prop_lb)) %>%
                                  dplyr::ungroup()
                              } else {
                                sets_long <- sets_long %>%
                                  dplyr::mutate(prop_lb = replace (prop_lb,
                                                                   wcat == "p10",
                                                                   value = 0)) %>%
                                  dplyr::group_by(id_act,
                                                  id_trip,
                                                  date_act,
                                                  yr,
                                                  mon,
                                                  lat,
                                                  lon,
                                                  sp,
                                                  fmod,
                                                  ocean,
                                                  vessel,
                                                  flag_code,
                                                  wtot_lb_t3) %>%
                                  dplyr::summarise(prop_lb = sum(prop_lb)) %>%
                                  dplyr::ungroup()
                              }
                              output_level3_process4 <- append(output_level3_process4,
                                                               list(list("sets_long" = sets_long,
                                                                         "sets_wide" = sets_wide,
                                                                         "catch_data_not_corrected" = catch_data_not_corrected)))
                              names(output_level3_process4)[length(output_level3_process4)] <- "nonsampled_sets"
                              cat(format(x = Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.4: data formating for predictions.\n",
                                  sep = "")
                              return(output_level3_process4)
                            },
                            # process 3.5: model predictions ----
                            #' @description Model predictions for the species composition and computing of catches.
                            #' @param output_level3_process2 Object of type \code{\link[base]{list}} expected. Outputs from level 3 process 2 (random forest models).
                            #' @param output_level3_process4 Object of type \code{\link[base]{list}} expected. Outputs from level 3 process 4 (data formatting for predictions).
                            #' @param output_directory Object of type \code{\link[base]{character}} expected. Outputs directory path.
                            #' @param output_format Object of class \code{\link[base]{character}} expected. By default "eu". Select outputs format regarding European format (eu) or United States format (us).
                            #' @param ci Object of type \code{\link[base]{logical}} expected. Logical indicating whether confidence interval is computed. The default value is FALSE as it is a time consuming step.
                            #' @param ci_type Type of confidence interval to compute. The default value is "all". Other options are "set" for ci on each set, "t1" for ci on nominal catch by species, "t1-fmod" for ci on nominal catch by species and fishing mode "t2" and "t2-fmod" for ci by 1 degree square and month. A vector of several ci option can be provided. ci_type are computed only if  the ci parameter is TRUE.
                            #' @param Nboot Object of type \code{\link[base]{numeric}} expected. The number of bootstrap samples desired for the ci computation. The default value is 10.
                            #' @param plot_predict Object of type \code{\link[base]{logical}} expected. Logical indicating whether maps of catch at size have to be done.
                            #' @param country_flag Three letters FAO flag code of country or countries to estimate catches.
                            model_predictions = function(output_level3_process2,
                                                         output_level3_process4,
                                                         output_directory,
                                                         output_format = "eu",
                                                         country_flag = NULL,
                                                         ci = FALSE,
                                                         ci_type = "all",
                                                         Nboot = 50,
                                                         plot_predict = FALSE) {
                              # 1 - Arguments verification ----
                              if (codama::r_type_checking(r_object = output_directory,
                                                          type = "character",
                                                          length = 1L,
                                                          output = "logical") != TRUE) {
                                stop(codama::r_type_checking(r_object = output_directory,
                                                             type = "character",
                                                             length = 1L,
                                                             output = "message"))
                              }
                              if (codama::r_type_checking(r_object = output_format,
                                                          type = "character",
                                                          length = 1L,
                                                          allowed_value = c("us",
                                                                            "eu"),
                                                          output = "logical") != TRUE) {
                                stop(codama::r_type_checking(r_object = output_format,
                                                             type = "character",
                                                             length = 1L,
                                                             allowed_value = c("us",
                                                                               "eu"),
                                                             output = "message"))
                              }
                              # 2 - Process ----
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.5: model predictions.\n",
                                  sep = "")
                              # extraction specifications
                              if (output_format == "us") {
                                outputs_dec <- "."
                                outputs_sep <- ","
                              } else if (output_format == "eu") {
                                outputs_dec <- ","
                                outputs_sep <- ";"
                              }
                              figure_directory <- file.path(output_directory,
                                                            "level3",
                                                            "figure")
                              names(figure_directory) <- "figure"
                              table_directory <- file.path(output_directory,
                                                           "level3",
                                                           "data")
                              names(table_directory) <- "data"
                              warn_defaut <- options("warn")
                              on.exit(options(warn_defaut))
                              options(warn = 1)
                              load(file = system.file("wrld_simpl.RData",
                                                      package = "t3"))
                              # function to add empty levels for the prediction with randomforest::predict.randomforest
                              addmissinglevel <- function(df, a){
                                if (is.factor(df[, a])) {
                                  return(factor(df[, a],
                                                levels = c(levels(df[, a]),
                                                           setdiff(current_output_level3_process2[[3]]$forest$xlevels[a][[1]],
                                                                   levels(df[, a])))))
                                }
                              }
                              # function which create an empty world raster with custom pixel size
                              rastermap <- function(x, y) {
                                raster::raster(nrows = (length(x = seq(from = -180,
                                                                       to = 180, by = x * 2)) -1),
                                               ncols = (length(x = seq(from = -90,
                                                                       to = 90,
                                                                       by= y / 2)) -1),
                                               xmn = -180,
                                               xmx = 180,
                                               ymn = -90,
                                               ymx = 90,
                                               crs = raster::crs(x = "+init=epsg:4326"),
                                               vals = NA)
                              }
                              # Compute estimates for SKJ and YFT and keep BET data unchanged  ----
                              outputs_level3_process5 <- vector(mode = "list",
                                                                length = 5)
                              names(outputs_level3_process5) <- c("Estimated_catch",
                                                                  "Estimated_catch_ST",
                                                                  "Boot_output_list",
                                                                  "Boot_output_list_ST",
                                                                  "Final_output")
                              sets_long <- output_level3_process4[[1]][[1]]
                              ocean_level <- unique(do.call(what = rbind,
                                                            args = strsplit(names(output_level3_process2),
                                                                            split = "_"))[,1])
                              for (ocean in ocean_level) {
                                sets_long_ocean <- sets_long[sets_long$ocean == ocean, ]
                                for (species in unique(sets_long_ocean$sp)) {
                                  if (! species %in% c("BET","SKJ","YFT")) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: process 3.5 not developed yet for the species \"",
                                        species,
                                        "\" in the ocean \"",
                                        ocean,
                                        "\".\n",
                                        "Data associated not used for this process.\n",
                                        sep = "")
                                  } else {
                                    sets_long_species <- sets_long_ocean[sets_long_ocean$sp == species, ]
                                    for (fishing_mode in unique(sets_long_species$fmod)) {
                                      sets_long_fishing_mode <- sets_long_species[sets_long_species$fmod == fishing_mode, ]
                                      cat(format(Sys.time(),
                                                 "%Y-%m-%d %H:%M:%S"),
                                          " - Ongoing process 3.5 (Predictions step) for ocean \"",
                                          ocean,
                                          "\", species \"",
                                          species,
                                          "\" and fishing mode \"",
                                          fishing_mode,
                                          "\"",
                                          ".\n",
                                          sep = "")

                                      if(nrow(sets_long_fishing_mode) > 0) {
                                        # models
                                        # For case with no sample set on all fmod
                                        # if(paste(ocean,
                                        #       species,
                                        #       fishing_mode,
                                        #       sep = "_") %in% names(output_level3_process2))
                                        current_output_level3_process2 <- output_level3_process2[[paste(ocean,
                                                                                                        species,
                                                                                                        fishing_mode,
                                                                                                        sep = "_")]]
                                        # skip model prediction for BET only
                                        if(species == "BET"){
                                          # remove sample sets
                                          sample_data = current_output_level3_process2[[1]]
                                          sample_data$w_lb_t3 <- NULL
                                          sample_data <- sample_data %>% dplyr::mutate(fmod = factor(fmod),
                                                                                       ocean = factor(ocean))

                                          sets_long_fishing_mode_no_sample <- droplevels(sets_long_fishing_mode[!(sets_long_fishing_mode$id_act %in% unique(sample_data$id_act)),])
                                          sets_long_fishing_mode_no_sample <- sets_long_fishing_mode_no_sample %>% dplyr::mutate(year = factor(yr),
                                                                                                                                 yr = factor(yr),
                                                                                                                                 mon = factor(mon),
                                                                                                                                 fmod = factor(fmod),
                                                                                                                                 ocean = factor(ocean),
                                                                                                                                 vessel = factor(vessel),
                                                                                                                                 data_source = "not_fitted")

                                          sampled_set <- unique(sample_data[sample_data$year %in% sets_long_fishing_mode_no_sample$yr[1],])
                                          sampled_set$data_source <- "sample" # add flag
                                          sampled_set <- dplyr::rename(sampled_set,
                                                                       fit_prop = prop_t3)
                                          all_set_bet <- dplyr::bind_rows(sampled_set, sets_long_fishing_mode_no_sample)
                                          # filter flag
                                          all_set_bet <- all_set_bet %>% dplyr::filter(flag_code == country_flag)

                                          outputs_level3_process5[[1]] <- append(outputs_level3_process5[[1]],
                                                                                 list(all_set_bet))
                                          names(outputs_level3_process5[[1]])[length(outputs_level3_process5[[1]])] <- paste(ocean,
                                                                                                                             species,
                                                                                                                             fishing_mode,
                                                                                                                             sep = "_")
                                        } else {
                                          res <- tunapredict(sample_data = current_output_level3_process2[[1]],
                                                             allset_data = sets_long_fishing_mode,
                                                             Ntree = 1000,
                                                             Nmtry = 2,
                                                             Nseed = 7)
                                          # filter flag
                                          res <- res %>% dplyr::filter(flag_code == country_flag)

                                          outputs_level3_process5[[1]] <- append(outputs_level3_process5[[1]],
                                                                                 list(res))
                                          names(outputs_level3_process5[[1]])[length(outputs_level3_process5[[1]])] <- paste(ocean,
                                                                                                                             species,
                                                                                                                             fishing_mode,
                                                                                                                             sep = "_")

                                          ##############################
                                          cat(format(Sys.time(),
                                                     "%Y-%m-%d %H:%M:%S"),
                                              " - Process 3.5 (Predictions step) successfull for ocean \"",
                                              ocean,
                                              "\", species \"",
                                              species,
                                              "\" and fishing mode \"",
                                              fishing_mode,
                                              "\"",
                                              ".\n",
                                              sep = "")
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              # Standardize SKJ and YFT 'Estimated catch' and compute BET estimated catch ----
                              for (ocean in ocean_level) {

                                outputs_level3_process5_ocean <- outputs_level3_process5[[1]][grep(pattern = paste(ocean,"_", sep = ""),
                                                                                                   x = names(outputs_level3_process5[[1]]))]
                                boot_tmp_element <- dplyr::bind_rows(outputs_level3_process5_ocean)

                                boot_tmp_element <- boot_tmp_element %>%
                                  dplyr::mutate(year = lubridate::year(date_act),
                                                yr = lubridate::year(date_act))

                                if(nrow(boot_tmp_element) > 0){
                                  # boot_tmp_element_sum <- boot_tmp_element %>%
                                  #   dplyr::group_by(dplyr::across(c(-wtot_lb_t3, -prop_lb))) %>%
                                  #   dplyr::summarise(wtot_lb_t3 = sum(wtot_lb_t3),
                                  #                    prop_lb_ave = mean(prop_lb)) %>%
                                  #   ungroup()

                                  boot_tmp_element_wide <- boot_tmp_element %>%
                                    dplyr::select(id_act, fit_prop, sp) %>%
                                    tidyr::pivot_wider(values_from = fit_prop, names_from = sp)
                                  # boot_tmp_element_wide <- tidyr::spread(data = boot_tmp_element[,!names(boot_tmp_element) %in%  c("w_lb_t3","prop_lb","tlb","year","resp", "data_source")],
                                  #                                        key = "sp",
                                  #                                        value = fit_prop)
                                  boot_tmp_element_wide$S <- boot_tmp_element_wide$SKJ + boot_tmp_element_wide$YFT
                                  boot_tmp_element_wide$SKJ <- ifelse(test = boot_tmp_element_wide$S > 1,
                                                                      yes = boot_tmp_element_wide$SKJ/boot_tmp_element_wide$S,
                                                                      no = boot_tmp_element_wide$SKJ)
                                  boot_tmp_element_wide$YFT <- ifelse(test = boot_tmp_element_wide$S > 1,
                                                                      yes = boot_tmp_element_wide$YFT/boot_tmp_element_wide$S,
                                                                      no = boot_tmp_element_wide$YFT)
                                  boot_tmp_element_wide$BET <- 1 - (boot_tmp_element_wide$SKJ + boot_tmp_element_wide$YFT)
                                  # boot_tmp_element_long <- tidyr::gather(data = boot_tmp_element_wide,
                                  #                                        key = "sp",
                                  #                                        value = "fit_prop_t3_ST",
                                  #                                        "BET", "SKJ", "YFT")
                                  boot_tmp_element_long <- tidyr::pivot_longer(data = boot_tmp_element_wide,
                                                                               names_to = "sp",
                                                                               values_to = "fit_prop_t3_ST",
                                                                               cols = c("BET", "SKJ", "YFT"))
                                  boot_tmp_element <- dplyr::left_join(boot_tmp_element_long,
                                                                       dplyr::distinct(dplyr::select(.data = boot_tmp_element, -c(prop_lb, w_lb_t3)))) %>%
                                    dplyr::group_by(id_act, sp)  %>%
                                    dplyr::mutate(dupli = dplyr::n())
                                  if(any(boot_tmp_element$dupli >1)){
                                    stop("Duplicated species catch in a set")
                                  }
                                  boot_tmp_element$catch_set_fit <- round(boot_tmp_element$wtot_lb_t3 * boot_tmp_element$fit_prop_t3_ST,digits = 4)
                                  outputs_level3_process5[[2]] <- append(outputs_level3_process5[[2]],
                                                                         list(boot_tmp_element))
                                  names(outputs_level3_process5[[2]])[length(outputs_level3_process5[[2]])] <- paste("ocean",
                                                                                                                     ocean,
                                                                                                                     sep = "_")
                                }
                              }
                              # bootstrap CI
                              # bootstrap step 1 - bootstrap on models and predicts ----
                              if (ci == TRUE){
                                for (ocean in ocean_level) {
                                  sets_long_ocean <- sets_long[sets_long$ocean == ocean, ]
                                  for (species in unique(sets_long_ocean$sp)) {
                                    if (! species %in% c("SKJ","YFT")) {
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - Warning: process 3.5 not developed yet for the species \"",
                                          species,
                                          "\" in the ocean \"",
                                          ocean,
                                          "\".\n",
                                          "Data associated not used for this process.\n",
                                          sep = "")
                                    } else {
                                      sets_long_species <- sets_long_ocean[sets_long_ocean$sp == species, ]
                                      for (fishing_mode in unique(sets_long_species$fmod)) {
                                        sets_long_fishing_mode <- sets_long_species[sets_long_species$fmod == fishing_mode, ]
                                        cat(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Ongoing process 3.5 (Bootstrap step) for ocean \"",
                                            ocean,
                                            "\", species \"",
                                            species,
                                            "\" and fishing mode \"",
                                            fishing_mode,
                                            "\"",
                                            ".\n",
                                            sep = "")
                                        # filter flag
                                        sets_long_fishing_mode <- sets_long_fishing_mode %>% dplyr::filter(flag_code == country_flag)
                                        if(nrow(sets_long_fishing_mode) > 0) {
                                          current_output_level3_process2 <- output_level3_process2[[paste(ocean,
                                                                                                          species,
                                                                                                          fishing_mode,
                                                                                                          sep = "_")]]
                                          boot_output <- tunaboot(sample_data = current_output_level3_process2[[1]],
                                                                  allset_data = sets_long_fishing_mode,
                                                                  # model parameters
                                                                  Ntree = 1000,
                                                                  Nmtry = 2,
                                                                  Nseed = 7,
                                                                  # bootstrap parameters
                                                                  Nboot = Nboot,
                                                                  target_period = dplyr::first(x = sets_long_fishing_mode$yr))


                                          outputs_level3_process5[[3]] <- append(outputs_level3_process5[[3]],
                                                                                 list(boot_output))
                                          names(outputs_level3_process5[[3]])[length(outputs_level3_process5[[3]])] <- paste(ocean,
                                                                                                                             species,
                                                                                                                             fishing_mode,
                                                                                                                             sep = "_")
                                          # remove bad flag in samples ----
                                          # maybe better to apply the filtering in one time on the outputs_level3_process5[[3]] L 7500
                                          # boot_output <- boot_output %>% dplyr::filter()
                                          ##############################
                                          cat(format(Sys.time(),
                                                     "%Y-%m-%d %H:%M:%S"),
                                              " - Process 3.5 (Bootstrap step) successfull for ocean \"",
                                              ocean,
                                              "\", species \"",
                                              species,
                                              "\" and fishing mode \"",
                                              fishing_mode,
                                              "\"",
                                              ".\n",
                                              sep = "")
                                        }
                                      }
                                    }
                                  }
                                }
                                # }
                                # bootstrap step 2 - Standardize SKJ and YFT 'Estimated catch' and compute BET estimated catch ----
                                # Standardize SKJ and YFT boot output - , compute BET proportion and catch for all
                                for (ocean in ocean_level) {
                                  outputs_level3_process5_ocean <- outputs_level3_process5[[3]][grep(pattern = paste(ocean,
                                                                                                                     "_",
                                                                                                                     sep = ""),
                                                                                                     x = names(outputs_level3_process5[[3]]))]
                                  if (length(outputs_level3_process5_ocean) > 0) {
                                    list_boot_ST_ocean <- vector("list",
                                                                 length = length(outputs_level3_process5_ocean[[1]]))
                                    for (element in (seq.int(from = 1,
                                                             to = length(outputs_level3_process5_ocean[[1]])))){
                                      boot_tmp_element <- lapply(outputs_level3_process5_ocean, function(l) l[[element]])
                                      boot_tmp_element <- dplyr::bind_rows(boot_tmp_element)
                                      boot_tmp_element_wide <- tidyr::spread(data = boot_tmp_element[,!names(boot_tmp_element) %in%
                                                                                                       c("w_lb_t3","prop_lb","tlb","year","resp")],
                                                                             key = "sp",
                                                                             value = fit_prop)
                                      boot_tmp_element_wide$S <- boot_tmp_element_wide$SKJ + boot_tmp_element_wide$YFT
                                      boot_tmp_element_wide$SKJ <- ifelse(test = boot_tmp_element_wide$S > 1,
                                                                          yes = boot_tmp_element_wide$SKJ / boot_tmp_element_wide$S,
                                                                          no = boot_tmp_element_wide$SKJ)
                                      boot_tmp_element_wide$YFT <- ifelse(test = boot_tmp_element_wide$S > 1,
                                                                          yes = boot_tmp_element_wide$YFT / boot_tmp_element_wide$S,
                                                                          no = boot_tmp_element_wide$YFT)
                                      boot_tmp_element_wide$BET <- 1 - (boot_tmp_element_wide$SKJ + boot_tmp_element_wide$YFT)
                                      boot_tmp_element_long <- tidyr::gather(data = boot_tmp_element_wide,
                                                                             key = "sp",
                                                                             value = "fit_prop_t3_ST",
                                                                             "BET", "SKJ", "YFT")
                                      boot_tmp_element <- dplyr::left_join(boot_tmp_element_long, boot_tmp_element,
                                                                           by = c("id_act", "date_act", "lat", "lon", "fmod",  "vessel","flag_code", "id_trip",
                                                                                  "ocean", "yr", "mon", "wtot_lb_t3", "sp","data_source"))
                                      boot_tmp_element$catch_set_fit <- round(boot_tmp_element$wtot_lb_t3 * boot_tmp_element$fit_prop_t3_ST, digits = 4)
                                      list_boot_ST_ocean[[element]] <- boot_tmp_element
                                    }
                                    outputs_level3_process5[[4]] <- append(outputs_level3_process5[[4]],
                                                                           list(list_boot_ST_ocean))
                                    names(outputs_level3_process5[[4]])[length(outputs_level3_process5[[4]])] <- paste("ocean",
                                                                                                                       ocean,
                                                                                                                       sep = "_")
                                  }
                                }
                              }
                              # bootstrap step 3 - compute confident intervals ----
                              cat(format(x = Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.5: set catch estimations.\n",
                                  sep = "")

                              # function for rounding, rounding up and down to a specific base
                              mtrunc <- function(x,base){
                                base*trunc(x/base)
                              }
                              mroundup <- function(x, base)
                              {
                                base*(x%/%base + as.logical(x%%base))
                              }
                              # assign coordinates to cwp with a specific base
                              latlon2cwp <- function(lat,
                                                     lon,
                                                     base) {
                                quad <- ifelse(lon >= 0,
                                               ifelse(lat >= 0,
                                                      1,
                                                      2),
                                               ifelse(lat >= 0,
                                                      4,
                                                      3)) # define quadrant
                                lat_tmp <- ifelse(quad %in% c(1,4),
                                                  sprintf("%02d",
                                                          abs(mtrunc(lat,
                                                                     base))),
                                                  sprintf("%02d",
                                                          abs(mroundup(lat,
                                                                       base))))
                                lon_tmp <- ifelse(quad %in% c(1,2),
                                                  sprintf("%03d",
                                                          abs(mtrunc(lon,
                                                                     base))),
                                                  sprintf("%03d",
                                                          abs(mroundup(lon,
                                                                       base))))
                                return(paste(quad,
                                             lat_tmp,
                                             lon_tmp,
                                             sep=""))
                              }
                              dd2dms_posit <- function(x) {
                                degrees <- trunc(x)
                                minutes <- abs((x - degrees)) * 60
                                seconds <- (minutes - trunc(minutes)) * 60
                                minutes <- trunc(minutes)
                                return(data.frame(degrees = abs(degrees), minutes = abs(minutes), seconds = abs(seconds)))
                              }
                              # Compute CI by set - export catch by set
                              set_all<- dplyr::bind_rows(outputs_level3_process5$Estimated_catch_ST) %>%
                                dplyr::mutate(ci_inf = NA,ci_sup = NA)
                              if (ci == TRUE && (length(which(ci_type == "all")) > 0
                                                 || length(which(ci_type == "set")) > 0 )) {
                                set_all_boot <- lapply(outputs_level3_process5$Boot_output_list_ST,
                                                       function(x) {
                                                         set_all_boot_tmp <- dplyr::bind_rows(x)
                                                         set_all_boot_tmp$loop <- rep(1:Nboot, each = nrow(set_all_boot_tmp) / Nboot)
                                                         return(set_all_boot_tmp)
                                                       })
                                # compute final CI
                                set_all_final_ocean_list <- vector("list", length = length(outputs_level3_process5$Estimated_catch_ST))
                                names(set_all_final_ocean_list) <- names(x = outputs_level3_process5$Estimated_catch_ST)
                                for (o in names(outputs_level3_process5$Estimated_catch_ST)) {
                                  set_all_final_ocean_list[[o]] <- catch_ci_calculator(fit_data = outputs_level3_process5$Estimated_catch_ST[[o]],
                                                                                       boot_data = set_all_boot[[o]])
                                }
                                set_all_final_ocean <- do.call(rbind, set_all_final_ocean_list)
                                # old rounding issue
                                # set_all_final_ocean[, names(set_all_final_ocean) %in% c("catch_set_fit",
                                #                                                         "ci_inf",
                                #                                                         "ci_sup")] <- round(set_all_final_ocean[, names(set_all_final_ocean) %in% c("catch_set_fit","ci_inf","ci_sup")],
                                #                                                                             digits = 4)
                                set_all <- set_all_final_ocean
                              }
                              # add other species and mix tuna
                              # compute average tuna proportion in sets by fishing mode
                              # MIX with other tuna should have been corrected in process 1.3 (issue #98)
                              # only sets with only MIX should remained here
                              catch_with_mix_tuna <- output_level3_process4$nonsampled_sets$catch_data_not_corrected$catch_with_mix_tuna %>%
                                dplyr::filter(sp != "MIX") %>%
                                dplyr::mutate(catch_set_fit  = round(w_lb_t3, digits = 4),
                                              data_source = "tuna_mix",
                                              mon = as.character(mon),
                                              ocean = as.factor(ocean),
                                              wcat = NULL)
                              catch_mix_tuna <- output_level3_process4$nonsampled_sets$catch_data_not_corrected$catch_with_mix_tuna %>%
                                dplyr::filter(sp == "MIX") %>%
                                dplyr::mutate(sp = NULL,
                                              wcat = NULL)
                              tuna_compo_ave_sp_fmod <- set_all %>%
                                dplyr::group_by(sp, fmod) %>%
                                dplyr::summarise(fit_prop_t3_ST = mean(fit_prop_t3_ST))
                              # unknown fishing mode
                              if(any(catch_mix_tuna$fmod == 0)){
                                tuna_compo_ave_sp <- set_all %>%
                                  dplyr::group_by(sp) %>%
                                  dplyr::summarise(fit_prop_t3_ST = mean(fit_prop_t3_ST)) %>%
                                  dplyr::mutate(fmod = as.factor(0))
                                tuna_compo_ave_sp_fmod <- dplyr::bind_rows(tuna_compo_ave_sp_fmod, tuna_compo_ave_sp)
                              }
                              # fit_prop_t3_ST = NULL is due to the fact that we have to sum with other weight for the same specis id_act. to remove when issue #98 fix
                              catch_mix_tuna_ST <- dplyr::left_join(catch_mix_tuna, tuna_compo_ave_sp_fmod, by = dplyr::join_by(fmod)) %>%
                                dplyr::mutate(
                                  catch_set_fit = round(fit_prop_t3_ST * w_lb_t3, digits = 4),
                                  data_source = "tuna_mix",
                                  mon =as.character(mon),
                                  ocean = as.factor(ocean),
                                  fit_prop_t3_ST = NULL)

                              catch_without_target_species <- output_level3_process4$nonsampled_sets$catch_data_not_corrected$catch_without_target_species %>%
                                dplyr::mutate(data_source = "unchanged",
                                              catch_set_fit  = round(w_lb_t3, digits = 4),
                                              mon =as.character(mon),
                                              ocean = as.factor(ocean))

                              catch_with_other_species <- output_level3_process4$nonsampled_sets$catch_data_not_corrected$catch_with_other_species %>%
                                dplyr::group_by(dplyr::across(c(-wcat))) %>%
                                dplyr::summarise(w_lb_t3 = sum(w_lb_t3)) %>%
                                dplyr::mutate(data_source = "unchanged",
                                              catch_set_fit  = round(w_lb_t3, digits = 4),
                                              mon =as.character(mon),
                                              ocean = as.factor(ocean))

                              catch_discard <- output_level3_process4$nonsampled_sets$catch_data_not_corrected$catch_discard  %>%
                                dplyr::group_by(dplyr::across(c(-wcat))) %>%
                                dplyr::summarise(w_lb_t3 = sum(w_lb_t3)) %>%
                                dplyr::mutate(data_source = "discard",
                                              catch_set_fit  = round(w_lb_t3, digits = 4),
                                              mon = as.character(mon),
                                              ocean = as.factor(ocean))

                              name_to_summarise <- c("catch_set_fit", "ci_inf","ci_sup", "w_lb_t3")
                              # remove catch_with_mix_tuna when issue #98 will be corrected
                              catch_all_other <- dplyr::bind_rows(catch_mix_tuna_ST, catch_without_target_species,
                                                                  catch_with_other_species, catch_discard, catch_with_mix_tuna) %>%
                                dplyr::mutate(status = ifelse(data_source == "discard","discard", "catch"),
                                              ci_inf = catch_set_fit,
                                              ci_sup = catch_set_fit) %>%
                                dplyr::group_by(dplyr::across(-name_to_summarise)) %>%
                                dplyr::summarise(catch_set_fit = sum(catch_set_fit),
                                                 ci_inf = sum(ci_inf),
                                                 ci_sup = sum(ci_sup),
                                                 w_lb_t3 = sum(w_lb_t3)) %>% dplyr::ungroup()

                              # recover the weight declaration standardized
                              weigth_declaration_ST <- dplyr::bind_rows(outputs_level3_process5$Estimated_catch) %>% select("id_act", "sp","wtot_lb_t3","prop_lb") %>%
                                mutate(w_lb_t3 = wtot_lb_t3*prop_lb,
                                       wtot_lb_t3 = NULL)
                              # weigth_declaration_ST <- dplyr::bind_rows(outputs_level3_process5$Estimated_catch) %>% select("id_act", "sp","w_lb_t3")

                              set_all <- dplyr::left_join(set_all, weigth_declaration_ST, by = dplyr::join_by("id_act", "sp"))
                              name_to_trash <- c("code_act_type", "wcat", "status") #"sp_code"
                              set_all_output <- dplyr::full_join(set_all, dplyr::select(.data = catch_all_other,
                                                                                        !name_to_trash)) %>%
                                tidyr::separate(id_act,into = c("text_tmp", "vessel_id_tmp", "numbers"),
                                                sep = "#",
                                                remove = FALSE) %>%
                                dplyr::mutate(text_tmp = NULL,
                                              vessel_id_tmp = NULL,
                                              # landing_date = lubridate::as_date(substr(numbers,1,8)), # remove with observe db
                                              status = ifelse(data_source == "discard","discard", "catch"),
                                              fit_prop = NULL)
                              if(!(nrow(set_all)+nrow(catch_all_other)) == nrow(set_all_output)){
                                warning("Duplicated detected in 'Catch_set_detail'")
                              }
                              test_dupli <- set_all_output %>% dplyr::group_by(id_act, sp, data_source) %>% dplyr::mutate(dupli = dplyr::n())
                              if(any(test_dupli$dupli>1)){
                                warning("Duplicated catch species data in activities, check 'Catch_set_detail'")
                              }
                              outputs_level3_process5[[5]] <- append(outputs_level3_process5[[5]],
                                                                     list(set_all_output))
                              names(outputs_level3_process5[[5]])[length(outputs_level3_process5[[5]])] <- "Catch_set_detail"
                              # format output
                              # function to create every new column from a list
                              Add_multi_columns <- function(df, name_list){
                                name_list_tmp <- dplyr::setdiff(name_list, names(df))
                                for(i in name_list_tmp){
                                  df <- dplyr::mutate(.data = df, !!i := NA)
                                }
                                return(df)
                              }
                              name_select_columns_output <- c("id_act", "sp", "date_act", "lat", "lon", #,"landing_date"
                                                              "fmod", "ocean", "vessel", "flag_code", "status","data_source",
                                                              "w_lb_t3", "prop_lb", "fit_prop_t3_ST", "wtot_lb_t3",
                                                              "catch_set_fit", "ci_inf", "ci_sup")

                              name_list_ecd <- c("ocean","port","pays","engin","NUMBAT","type_bateau","categorie",
                                                 "annee_de_debarquement","mois_de_debarquement",	"jour_de_debarquement",
                                                 "annee_de_peche","mois_de_peche","jour_de_peche","heure_de_peche",
                                                 "quinzaine","trimestre","quadrant","latitude_deg","latitude_min","longitude_deg",	"longitude_min",
                                                 "cwp11","cwp55","zet","zee","heures_mer","heures_peche","heures_peche_standard","nombre_de_calees",
                                                 "nombre_de_calees_pos","nombre_de_calees_neg","numero_activite","c_opera","flagexpert",
                                                 "association1",	"association2","association3","association4","association5",
                                                 "code_assoc_reduit","code_assoc_groupe",
                                                 "temperature","direction_courant","vitesse_courant",
                                                 "rf3","duree",
                                                 "capture_YFT",	"capture_SKJ",	"capture_BET",	"capture_ALB",
                                                 "capture_LTA",	"capture_FRZ",	"capture_SHX",	"capture_DSC",
                                                 "capture_YOU",	"capture_KAW",	"capture_LOT",	"capture_BLF",
                                                 "capture_YFT_categ1_corrigee", "capture_YFT_categ2_corrigee","capture_YFT_categ3_corrigee",
                                                 "capture_BET_categ1_corrigee","capture_BET_categ2_corrigee",	"capture_BET_categ3_corrigee")

                              # selection, renaming and new column
                              set_all_output_long <- set_all_output %>% dplyr::select(name_select_columns_output) %>%
                                dplyr::rename(species = sp, latitude_dec = lat, longitude_dec = lon,
                                              vessel_id = vessel, fishing_mode = fmod,
                                              catch = catch_set_fit, catch_ci_inf = ci_inf, catch_ci_sup = ci_sup,
                                              catch_logbook_ST = w_lb_t3, prop_logbook_ST = prop_lb ,prop_fit_ST = fit_prop_t3_ST,
                                              catch_set_total_ST = wtot_lb_t3) %>%  dplyr::ungroup()

                              # format and filtering for ecd
                              name_to_remove_for_wide <- c("catch_ci_inf", "catch_ci_sup", "catch_logbook_ST", "prop_fit_ST", "catch_set_total_ST","status","prop_logbook_ST","data_source")
                              SHX_group <- c("SHX","FAL","OCS","SHK","BSH","SRX")
                              FRZ_group <- c("FRZ", "FRI","BLT","RAV")
                              species_ecd_filter <- c("ALB","BET", "SKJ", "YFT", "DSC", "SHX", "FRZ", "LTA", "YOU", "KAW", "LOT", "BLF")
                              set_all_output_long_tmp <- set_all_output_long %>% dplyr::mutate(rf3 = 1,
                                                                                               flagexpert = 9,
                                                                                               zet = 99,
                                                                                               species = dplyr::case_when(status == "discard" ~ "DSC",
                                                                                                                          species %in% SHX_group ~ "SHX",
                                                                                                                          species %in% FRZ_group ~ "FRZ",
                                                                                                                          !species %in% species_ecd_filter ~ "YOU",
                                                                                                                          TRUE ~ species)) %>%
                                dplyr::filter(species %in% species_ecd_filter) %>%
                                dplyr::select(-name_to_remove_for_wide) %>%
                                dplyr::group_by(dplyr::across(-catch)) %>%
                                dplyr::summarise(catch = sum(catch))

                              # rename to ecd format
                              set_all_output_long_tmp <- set_all_output_long_tmp %>%  dplyr::rename(NUMBAT = vessel_id, pays = flag_code,
                                                                                                     code_assoc_groupe = fishing_mode,
                                                                                                     capture = catch)
                              set_all_output_wide <- set_all_output_long_tmp %>%
                                tidyr::pivot_wider(values_from = capture,
                                                   names_from = c(species),
                                                   names_prefix = "capture_",
                                                   values_fill = 0) %>%
                                dplyr::mutate(cwp11 = latlon2cwp(lat = latitude_dec,
                                                                 lon = longitude_dec,
                                                                 base = 1),
                                              cwp55 = latlon2cwp(lat = latitude_dec,
                                                                 lon = longitude_dec,
                                                                 base = 5),
                                              quadrant = substr(cwp11,1,1),
                                              annee_de_peche = lubridate::year(date_act),
                                              mois_de_peche = lubridate::month(date_act),
                                              jour_de_peche = lubridate::mday(date_act),
                                              heure_de_peche = lubridate::hour(date_act),
                                              trimestre = lubridate::quarter(date_act),
                                              # annee_de_debarquement = lubridate::year(landing_date),
                                              # mois_de_debarquement = lubridate::month(landing_date),
                                              # jour_de_debarquement = lubridate::mday(landing_date)
                                )

                              latitude_tmp <-dplyr::bind_rows(lapply(1:nrow(set_all_output_wide), function(x){
                                dd2dms_posit(set_all_output_wide[x,]$latitude_dec)
                              })) %>% dplyr::rename(latitude_deg = "degrees",
                                                    latitude_min = "minutes")
                              longitude_tmp <- dplyr::bind_rows(lapply(1:nrow(set_all_output_wide), function(x){
                                dd2dms_posit(set_all_output_wide[x,]$longitude_dec)
                              })) %>% dplyr::rename(longitude_deg = "degrees",
                                                    longitude_min = "minutes")
                              set_all_output_wide <- dplyr::bind_cols(set_all_output_wide,
                                                                      dplyr::select(.data =latitude_tmp, -seconds),
                                                                      dplyr::select(.data =longitude_tmp, -seconds))
                              set_all_output_wide <- set_all_output_wide %>%
                                dplyr::group_by(NUMBAT,date_act) %>%
                                dplyr::mutate(numero_activite = seq(1:dplyr::n())) %>%
                                dplyr::ungroup()
                              set_all_output_wide <- Add_multi_columns(df = set_all_output_wide, name_list = name_list_ecd) %>%
                                dplyr::relocate(id_act, name_list_ecd) %>%
                                dplyr::mutate(date_act = NULL, latitude_dec = NULL,	longitude_dec = NULL) %>%
                                replace(is.na(.), 0)
                              # export dataset
                              write.table(x = set_all_output_long,
                                          file = file.path(table_directory,
                                                           paste("Catch_set_ocean_",
                                                                 paste(unique(set_all$ocean),
                                                                       collapse = "-"),
                                                                 "_",
                                                                 paste(unique(set_all$yr),
                                                                       collapse = "-"),
                                                                 "_",
                                                                 country_flag,
                                                                 "_",
                                                                 paste(strsplit(as.character(Sys.time()),split = "-|:| ")[[1]], collapse = ""),
                                                                 ".csv",
                                                                 sep = "")),
                                          row.names = FALSE,
                                          sep = outputs_sep,
                                          dec = outputs_dec)

                              write.table(x = set_all_output_wide,
                                          file = file.path(table_directory,
                                                           paste("ecd_ocean_",
                                                                 paste(unique(set_all$ocean),
                                                                       collapse = "-"),
                                                                 "_",
                                                                 paste(unique(set_all$yr),
                                                                       collapse = "-"),
                                                                 "_",
                                                                 country_flag,
                                                                 "_",
                                                                 paste(strsplit(as.character(Sys.time()),split = "-|:| ")[[1]], collapse = ""),
                                                                 ".csv",
                                                                 sep = "")),
                                          row.names = FALSE,
                                          sep = outputs_sep,
                                          dec = outputs_dec)

                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.5: set catch estimations.\n",
                                  sep = "")
                              ### nominal catch by species (task 1) ----
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.5: t1 catch estimations.\n",
                                  sep = "")
                              t1_column_names <- c("yr", "sp", "ocean", "flag_code")
                              t1_fmod_column_names <- c(t1_column_names, "fmod")
                              t2_column_names <- c(t1_column_names, "mon", "cwp")
                              t2_fmod_column_names <- c(t2_column_names, "fmod")

                              t1_all <- dplyr::bind_rows(outputs_level3_process5$Estimated_catch_ST) %>%
                                dplyr::group_by(across(t1_column_names)) %>%
                                dplyr::summarise(catch_set_fit = sum(catch_set_fit)) %>% ungroup()
                              # compute final CI
                              if (ci == TRUE && (length(which(ci_type == "all")) > 0
                                                 || length(which(ci_type == "t1")) > 0 )) {
                                t1_all_boot <- do.call(rbind,lapply(outputs_level3_process5$Boot_output_list_ST,
                                                                    function(x) {
                                                                      boot_tmp_element <- do.call(rbind,
                                                                                                  lapply(seq.int(1:length(x)),
                                                                                                         function(i) {
                                                                                                           boot_tmp_subelement <- aggregate(cbind(catch_set_fit) ~ yr + sp + ocean + flag_code,
                                                                                                                                            data = x[[i]], sum)
                                                                                                           boot_tmp_subelement$loop <- i
                                                                                                           return(boot_tmp_subelement)
                                                                                                         }))
                                                                      return(boot_tmp_element)
                                                                    }))
                                t1_all_final_ocean_list <- vector("list", length = length(x = levels(t1_all$ocean)))
                                for (o in levels(t1_all$ocean)) {
                                  t1_all_final_ocean_list[[as.numeric(o)]] <- catch_ci_calculator(fit_data = t1_all[t1_all$ocean == o, ],
                                                                                                  boot_data = t1_all_boot[t1_all_boot$ocean == o, ])
                                }
                                t1_all_final_ocean <- do.call(rbind, t1_all_final_ocean_list)
                                t1_all_final_ocean[, names(t1_all_final_ocean) %in% c("catch_set_fit",
                                                                                      "ci_inf",
                                                                                      "ci_sup")] <- round(t1_all_final_ocean[, names(t1_all_final_ocean) %in% c("catch_set_fit","ci_inf","ci_sup")],
                                                                                                          digits = 4)
                                # outputs_level3_process5[[5]] <- append(outputs_level3_process5[[5]],
                                #                                        list(t1_all_final_ocean))
                                # names(outputs_level3_process5[[5]])[length(outputs_level3_process5[[5]])] <- "Nominal_catch_species"
                                t1_all <- t1_all_final_ocean
                              }
                              # add other species and mix tuna
                              t1_all_other <- catch_all_other %>%
                                dplyr::group_by(across(t1_column_names),status) %>%
                                dplyr::summarise(catch_set_fit = sum(catch_set_fit),
                                                 ci_inf = sum(ci_inf),
                                                 ci_sup = sum(ci_sup))

                              # format output
                              t1_all <- dplyr::bind_rows(t1_all, t1_all_other) %>% dplyr::mutate(status = ifelse(is.na(status), "catch", status)) %>%
                                dplyr::group_by(across(t1_column_names),status) %>% dplyr::summarise(catch = sum(catch_set_fit),
                                                                                            catch_ci_inf = sum(ci_inf),
                                                                                            catch_ci_sup = sum(ci_sup))

                              # export dataset
                              write.table(x = t1_all,
                                          file = file.path(table_directory,
                                                           paste("t1_all_ocean_",
                                                                 paste(unique(t1_all$ocean),
                                                                       collapse = "-"),
                                                                 "_",
                                                                 paste(unique(t1_all$yr),
                                                                       collapse = "-"),
                                                                 "_",
                                                                 country_flag,
                                                                 "_",
                                                                 paste(strsplit(as.character(Sys.time()),split = "-|:| ")[[1]], collapse = ""),
                                                                 ".csv",
                                                                 sep = "")),
                                          row.names = FALSE,
                                          sep = outputs_sep,
                                          dec = outputs_dec)
                              cat(format(x = Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.5: t1 catch estimations.\n",
                                  sep = "")
                              # nominal catch by species and fishing mode (task 1 by fishing mode)
                              cat(format(x = Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.5: t1-fmod catch estimations.\n",
                                  sep = "")
                              t1_fmod <- dplyr::bind_rows(outputs_level3_process5$Estimated_catch_ST) %>%
                                dplyr::group_by(across(t1_fmod_column_names)) %>%
                                dplyr::summarise(catch_set_fit = sum(catch_set_fit)) %>% ungroup()
                              # bootstrap distribution
                              if(ci == TRUE && (length(which(ci_type == "all")) > 0
                                                || length(which(ci_type == "t1-fmod")) > 0)) {
                                t1_fmod_boot <- do.call(rbind,lapply(outputs_level3_process5$Boot_output_list_ST,
                                                                     FUN = function(x) {
                                                                       boot_tmp_element <- do.call(rbind,
                                                                                                   lapply(seq.int(1:length(x)),
                                                                                                          function(i) {
                                                                                                            boot_tmp_subelement <- aggregate(cbind(catch_set_fit) ~ yr + fmod + sp + ocean + flag_code,
                                                                                                                                             data=x[[i]],
                                                                                                                                             FUN = sum)
                                                                                                            boot_tmp_subelement$loop <- i
                                                                                                            return(boot_tmp_subelement)
                                                                                                          }))
                                                                       return(boot_tmp_element)
                                                                     }))
                                # compute final CI
                                t1_fmod_final_ocean_list <- vector("list", length = length(x = levels(t1_fmod$ocean)))
                                for (o in levels(t1_fmod$ocean)) {
                                  t1_fmod_final_ocean_list[[as.numeric(o)]] <- catch_ci_calculator(fit_data = t1_fmod[t1_fmod$ocean == o,],
                                                                                                   boot_data = t1_fmod_boot[t1_fmod_boot$ocean == o,])
                                }
                                t1_fmod_final_ocean <- do.call(rbind, t1_fmod_final_ocean_list)
                                t1_fmod_final_ocean[, names(t1_fmod_final_ocean) %in% c("catch_set_fit",
                                                                                        "ci_inf",
                                                                                        "ci_sup")] <- round(t1_fmod_final_ocean[, names(t1_fmod_final_ocean) %in% c("catch_set_fit","ci_inf","ci_sup")],
                                                                                                            digits = 4)
                                # outputs_level3_process5[[5]] <- append(outputs_level3_process5[[5]],
                                #                                        list(t1_fmod_final_ocean))
                                # names(outputs_level3_process5[[5]])[length(outputs_level3_process5[[5]])] <- "Nominal_catch_fishing_mode"
                                t1_fmod <- t1_fmod_final_ocean
                              }
                              # add other species and mix tuna
                              t1_fmod_other <- catch_all_other %>%
                                dplyr::group_by(across(t1_fmod_column_names) ,status) %>%
                                dplyr::summarise(catch_set_fit = sum(catch_set_fit),
                                                 ci_inf = sum(ci_inf),
                                                 ci_sup = sum(ci_sup))

                              t1_fmod <- bind_rows(t1_fmod, t1_fmod_other) %>%
                                dplyr::mutate(status = ifelse(is.na(status), "catch", status)) %>%
                                dplyr::group_by(across(t1_fmod_column_names), status, ocean) %>%
                                dplyr::summarise(catch_set_fit = sum(catch_set_fit),
                                                 catch_ci_inf = sum(ci_inf),
                                                 catch_ci_sup = sum(ci_sup))
                              outputs_level3_process5[[5]] <- append(outputs_level3_process5[[5]],
                                                                     list(t1_fmod))
                              names(outputs_level3_process5[[5]])[length(outputs_level3_process5[[5]])] <- "Nominal_catch_fishing_mode"

                              # export dataset
                              write.table(x = t1_fmod,
                                          file = file.path(table_directory,
                                                           paste("t1_fmod_ocean_",
                                                                 paste(unique(t1_fmod$ocean),
                                                                       collapse = "-"),
                                                                 "_",
                                                                 paste(unique(t1_fmod$yr),
                                                                       collapse = "-"),
                                                                 "_",
                                                                 country_flag,
                                                                 "_",
                                                                 paste(strsplit(as.character(Sys.time()),split = "-|:| ")[[1]], collapse = ""),
                                                                 ".csv",
                                                                 sep = "")),
                                          row.names = FALSE,
                                          sep = outputs_sep,
                                          dec = outputs_dec)
                              cat(format(x = Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.5: t1-fmod catch estimations.\n",
                                  sep = "")
                              ### catch effort (task2) ----
                              # nominal catch by species and cwp (task 2 - catch Effort)
                              cat(format(x = Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.5: t2 catch estimations.\n",
                                  sep = "")
                              # t2_all <- do.call(rbind,lapply(outputs_level3_process5$Estimated_catch_ST, function(x) {
                              #   x$cwp <- latlon2cwp(lat = x$lat,
                              #                       lon = x$lon,
                              #                       base = 1)
                              #   boot_tmp_subelement <- x %>%
                              #     dplyr::group_by(yr, mon, sp, ocean, cwp) %>%
                              #     dplyr::summarise(catch_set_fit = sum(catch_set_fit, na.rm = TRUE))
                              #   return(boot_tmp_subelement)
                              # }))
                              t2_all <- dplyr::bind_rows(outputs_level3_process5$Estimated_catch_ST) %>%
                                dplyr::mutate(cwp = latlon2cwp(lat = lat,
                                                               lon = lon,
                                                               base = 1)) %>%
                                dplyr::group_by(across(t2_column_names)) %>%
                                dplyr::summarise(catch_set_fit = sum(catch_set_fit)) %>% ungroup()

                              # bootstrap distribution
                              if(ci == TRUE && (length(which(ci_type == "all")) > 0 || length(which(ci_type == "t2")) > 0 )){
                                t2_all_boot <- do.call(rbind,lapply(outputs_level3_process5$Boot_output_list_ST,
                                                                    FUN = function(x) {
                                                                      boot_tmp_element <-do.call(rbind,
                                                                                                 lapply(seq.int(1:length(x)),
                                                                                                        function(i){
                                                                                                          x[[i]]$cwp <- latlon2cwp(lat = x[[i]]$lat,
                                                                                                                                   lon = x[[i]]$lon,
                                                                                                                                   base = 1)
                                                                                                          boot_tmp_subelement <- x[[i]] %>%
                                                                                                            dplyr::group_by(yr, mon, sp, cwp, ocean, flag_code) %>%
                                                                                                            dplyr::summarise(catch_set_fit = sum(catch_set_fit, na.rm = TRUE))
                                                                                                          boot_tmp_subelement$loop <- i
                                                                                                          return(boot_tmp_subelement)
                                                                                                        }))
                                                                      return(boot_tmp_element)
                                                                    }))
                                # compute final CI
                                t2_all_final_ocean_list <- vector("list", length = length(levels(t2_all$ocean)))
                                for (o in as.numeric(levels(t2_all$ocean))) {
                                  t2_all_final_ocean_list[[o]] <- catch_ci_calculator(fit_data = t2_all[t2_all$ocean == o,],
                                                                                      boot_data = t2_all_boot[t2_all_boot$ocean == o,])
                                }
                                t2_all_final_ocean <- do.call(rbind, t2_all_final_ocean_list)
                                t2_all_final_ocean[, names(t2_all_final_ocean) %in% c("catch_set_fit",
                                                                                      "ci_inf",
                                                                                      "ci_sup")] <- round(t2_all_final_ocean[, names(t2_all_final_ocean) %in% c("catch_set_fit","ci_inf","ci_sup")],
                                                                                                          digits = 4)
                                t2_all <- t2_all_final_ocean
                              }
                              # add other species and mix tuna
                              t2_all_other <- catch_all_other %>% dplyr::mutate(cwp = latlon2cwp(lat = lat,
                                                                                                 lon = lon,
                                                                                                 base = 1)) %>%
                                dplyr::group_by(across(t2_column_names), status) %>% dplyr::summarise(catch_set_fit = sum(catch_set_fit),
                                                                                                      ci_inf = sum(ci_inf, na.rm = FALSE),
                                                                                                      ci_sup = sum(ci_sup, na.rm = FALSE))

                              t2_all <- dplyr::bind_rows(t2_all, t2_all_other) %>% dplyr::mutate(status = ifelse(is.na(status), "catch", status)) %>%
                                dplyr::group_by(across(t2_column_names), status) %>% dplyr::summarise(catch = sum(catch_set_fit, na.rm = FALSE),
                                                                                                      catch_ci_inf = sum(ci_inf, na.rm = FALSE),
                                                                                                      catch_ci_sup = sum(ci_sup, na.rm = FALSE))
                              # export dataset
                              write.table(x = t2_all,
                                          file = file.path(table_directory,
                                                           paste("t2_all_ocean_",
                                                                 paste(unique(t2_all$ocean),
                                                                       collapse = "-"),
                                                                 "_",
                                                                 paste(unique(t2_all$yr),
                                                                       collapse = "-"),
                                                                 "_",
                                                                 country_flag,
                                                                 "_",
                                                                 paste(strsplit(as.character(Sys.time()),split = "-|:| ")[[1]], collapse = ""),
                                                                 ".csv",
                                                                 sep = "")),
                                          row.names = FALSE,
                                          sep = outputs_sep,
                                          dec = outputs_dec)
                              cat(format(x = Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.5: t2 catch estimations.\n",
                                  sep = "")
                              # nominal catch by species and cwp and fishing mode (task 2 by fishing mode) ----
                              cat(format(x = Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.5: t2-fmod catch estimations.\n",
                                  sep = "")
                              # t2_fmod <- do.call(rbind,lapply(outputs_level3_process5$Estimated_catch_ST, function(x) {
                              #   x$cwp <- latlon2cwp(lat = x$lat,
                              #                       lon = x$lon,
                              #                       base = 1)
                              #   boot_tmp_subelement <- x %>%
                              #     dplyr::group_by(yr, mon, fmod, sp, ocean, cwp) %>%
                              #     dplyr::summarise(catch_set_fit = sum(catch_set_fit, na.rm = TRUE))
                              #   return(boot_tmp_subelement)
                              # }))
                              t2_fmod <- dplyr::bind_rows(outputs_level3_process5$Estimated_catch_ST) %>%
                                dplyr::mutate(cwp = latlon2cwp(lat = lat,
                                                               lon = lon,
                                                               base = 1)) %>%
                                dplyr::group_by(across(t2_fmod_column_names)) %>%
                                dplyr::summarise(catch_set_fit = sum(catch_set_fit)) %>% ungroup()

                              # bootstrap distribution
                              if(ci == TRUE && (length(which(ci_type == "all")) > 0 || length(x = which(ci_type == "t2-fmod")) > 0 )) {
                                t2_fmod_boot <- do.call(rbind,lapply(outputs_level3_process5$Boot_output_list_ST,
                                                                     FUN = function(x) {
                                                                       boot_tmp_element <-do.call(rbind,
                                                                                                  lapply(seq.int(1:length(x)),
                                                                                                         function(i){
                                                                                                           x[[i]]$cwp <- latlon2cwp(lat = x[[i]]$lat,
                                                                                                                                    lon = x[[i]]$lon,
                                                                                                                                    base = 1)
                                                                                                           boot_tmp_subelement <- aggregate(cbind(catch_set_fit) ~ yr + mon + fmod + sp + cwp + ocean + flag_code,
                                                                                                                                            data=x[[i]],
                                                                                                                                            FUN = sum)
                                                                                                           boot_tmp_subelement$loop <- i
                                                                                                           return(boot_tmp_subelement)
                                                                                                         }))
                                                                       return(boot_tmp_element)
                                                                     }))
                                # compute final CI
                                t2_fmod_final_ocean_list <- vector("list", length = length(levels(t2_fmod$ocean)))
                                for (o in as.numeric(levels(t2_fmod$ocean))) {
                                  t2_fmod_final_ocean_list[[o]] <- catch_ci_calculator(fit_data = t2_fmod[t2_fmod$ocean == o,],
                                                                                       boot_data = t2_fmod_boot[t2_fmod_boot$ocean == o,])
                                }
                                t2_fmod_final_ocean <- do.call(rbind, t2_fmod_final_ocean_list)
                                t2_fmod_final_ocean[, names(t2_fmod_final_ocean) %in% c("catch_set_fit",
                                                                                        "ci_inf",
                                                                                        "ci_sup")] <- round(t2_fmod_final_ocean[, names(t2_fmod_final_ocean) %in% c("catch_set_fit","ci_inf","ci_sup")],
                                                                                                            digits = 4)
                                t2_fmod <- t2_fmod_final_ocean
                              }
                              # add other species and mix tuna
                              t2_fmod_other <- catch_all_other %>% dplyr::mutate(cwp = latlon2cwp(lat = lat,
                                                                                                  lon = lon,
                                                                                                  base = 1)) %>%
                                dplyr::group_by(across(t2_fmod_column_names), status) %>%
                                dplyr::summarise(catch_set_fit = sum(catch_set_fit, na.rm = FALSE),
                                                 ci_inf = sum(ci_inf, na.rm = FALSE),
                                                 ci_sup = sum(ci_sup, na.rm = FALSE))

                              t2_fmod <- dplyr::bind_rows(t2_fmod, t2_fmod_other) %>%
                                dplyr::mutate(status = ifelse(is.na(status), "catch", status)) %>%
                                dplyr::group_by(across(t2_fmod_column_names), status) %>%
                                dplyr::summarise(catch_set_fit = sum(catch_set_fit, na.rm = FALSE),
                                                 catch_ci_inf = sum(ci_inf, na.rm = FALSE),
                                                 catch_ci_sup = sum(ci_sup, na.rm = FALSE))
                              outputs_level3_process5[[5]] <- append(outputs_level3_process5[[5]],
                                                                     list(t2_fmod))
                              names(outputs_level3_process5[[5]])[length(outputs_level3_process5[[5]])] <- "Catch_effort_fishing_mode"

                              # format m11 file
                              name_list_m11 <- c('ocean','fishing_year', 'flag', 'gear', 'month', 'square',
                                                 'time_at_sea','fishing_time', 'fishing_time_std',
                                                 'obj_fishing_duration',"obj_yft", "obj_skj",	"obj_bet",	"obj_alb",	"obj_lta_kaw",
                                                 "obj_frz", "obj_blf_lot",
                                                 'fsc_fishing_duration', "fsc_yft",	"fsc_skj",	"fsc_bet",	"fsc_alb",	"fsc_lta_kaw",
                                                 "fsc_frz",	"fsc_blf_lot")

                              FRZ_group <- c("FRZ", "FRI","BLT","RAV")
                              LTA_KAW_group <- c("LTA","KAW")
                              BLT_LOT_group <- c("BLF","LOT")
                              species_m11_filter <- c("ALB","BET","SKJ","YFT","FRZ","LTA","BLF")

                              name_to_remove_for_wide <- c("catch_ci_inf", "catch_ci_sup", "status")
                              t2_fmod_output_long <- t2_fmod %>% filter(status == "catch") %>%
                                dplyr::group_by(cwp, mon, fmod) %>% dplyr::mutate(max_sp = sp[catch_set_fit == max(catch_set_fit)][1]) %>%
                                dplyr::ungroup() %>%
                                dplyr::mutate(fmod = ifelse((fmod == 0 & max_sp == "SKJ"), 1, fmod),
                                              fmod = dplyr::case_when(fmod == 1 ~ "obj",
                                                                      fmod == 2 ~ "fsc",
                                                                      TRUE ~ "unk"),
                                              sp = dplyr::case_when(sp %in% FRZ_group ~ "FRZ",
                                                                    sp %in% LTA_KAW_group ~ "LTA",
                                                                    sp %in% BLT_LOT_group ~ "BLT",
                                                                    !sp %in% species_m11_filter ~ "OTH",
                                                                    TRUE ~ sp)) %>%                                dplyr::filter(sp %in% species_m11_filter) %>%
                                dplyr::select(-name_to_remove_for_wide) %>%
                                dplyr::group_by(dplyr::across(-catch_set_fit)) %>%
                                dplyr::summarise(catch_set_fit = sum(catch_set_fit, na.rm = TRUE)) %>%
                                dplyr::ungroup() %>%
                                dplyr::mutate(sp = dplyr::recode(sp, ALB = "alb", BET = "bet", SKJ = "skj", YFT = "yft",
                                                                 FRZ = "frz", LTA = "lta_kaw", BLT = "blt_lot"))

                              t2_fmod_output_wide <- t2_fmod_output_long %>%
                                tidyr::pivot_wider(values_from = catch_set_fit,
                                                   names_from = c(fmod,sp),
                                                   values_fill = 0) %>%
                                dplyr::rename(square = cwp,
                                              fishing_year = yr,
                                              month = mon)

                              t2_fmod_output_wide <- Add_multi_columns(df = t2_fmod_output_wide, name_list = name_list_m11) %>%
                                dplyr::relocate(name_list_m11) %>%
                                dplyr::mutate(max_sp = NULL) %>%
                                replace(is.na(.), 0)


                              # export dataset
                              write.table(x = t2_fmod,
                                          file = file.path(table_directory,
                                                           paste("t2_fmod_ocean_",
                                                                 paste(unique(t2_fmod$ocean),
                                                                       collapse = "-"),
                                                                 "_",
                                                                 paste(unique(t2_fmod$yr),
                                                                       collapse = "-"),
                                                                 "_",
                                                                 country_flag,
                                                                 "_",
                                                                 paste(strsplit(as.character(Sys.time()),split = "-|:| ")[[1]], collapse = ""),
                                                                 ".csv",
                                                                 sep = "")),
                                          row.names = FALSE,
                                          sep = outputs_sep,
                                          dec = outputs_dec)

                              write.table(x = t2_fmod_output_wide,
                                          file = file.path(table_directory,
                                                           paste("m11_ocean_",
                                                                 paste(unique(t2_fmod$ocean),
                                                                       collapse = "-"),
                                                                 "_",
                                                                 paste(unique(t2_fmod$yr),
                                                                       collapse = "-"),
                                                                 "_",
                                                                 country_flag,
                                                                 "_",
                                                                 paste(strsplit(as.character(Sys.time()),split = "-|:| ")[[1]], collapse = ""),
                                                                 ".csv",
                                                                 sep = "")),
                                          row.names = FALSE,
                                          sep = outputs_sep,
                                          dec = outputs_dec)

                              cat(format(x = Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.5: t2-fmod catch estimations.\n",
                                  sep = "")
                              # figure task 2 and proportion----
                              if (plot_predict == T) {
                                sps <- t2
                                sp::coordinates(object = sps) <- ~ lon + lat
                                # select for the year
                                yr_fig <- as.character(unique(sps$year))
                                fmod_fig <- unique(sps$fmod)
                                ocean_fig <- unique(sps$ocean)
                                # common extent for all figures
                                wrl <- rastermap(x = 1,
                                                 y = 1)
                                wrld <- raster::crop(x = wrld_simpl,
                                                     y = (raster::extent(sps) + 5))
                                palette4catch <- grDevices::colorRampPalette(c("yellow", "red"))
                                outputs_level3_process5 <- append(outputs_level3_process5,
                                                                  list(list()))
                                names(outputs_level3_process5)[length(outputs_level3_process5)] <- "figure"
                                # map of the proportion
                                for (specie in unique(sps$sp)) {
                                  if (! specie %in% c("BET",
                                                      "SKJ",
                                                      "YFT")) {
                                    cat(format(x = Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: process 3.5 not developed yet for the specie \"",
                                        specie,
                                        "\".\n",
                                        sep = "")
                                  } else {
                                    sps_specie <- sps[sps$sp == specie, ]
                                    for (fishing_mode in unique(sps_specie$fmod)) {
                                      sps_fishing_mode <- sps_specie[sps_specie$fmod == fishing_mode, ]
                                      for (ocean in unique(sps_fishing_mode$ocean)) {
                                        sps_ocean <- sps_fishing_mode[sps_fishing_mode$ocean == ocean, ]
                                        for (year in unique(sps_ocean$year)) {
                                          sps_year <- sps_ocean[sps_ocean$year == year, ]
                                          figure_directory <- file.path(outputs_path,
                                                                        "figure",
                                                                        paste(ocean,
                                                                              specie,
                                                                              fishing_mode,
                                                                              sep = "_"))
                                          if (file.exists(figure_directory)) {
                                            cat(format(x = Sys.time(),
                                                       "%Y-%m-%d %H:%M:%S"),
                                                " - Outputs \"figure\" directory for ocean \"",
                                                ocean,
                                                "\", specie \"",
                                                specie,
                                                "\" and fishing mode \"",
                                                fishing_mode,
                                                "\" already exists.\n",
                                                "Outputs associated will used this directory (be careful of overwriting previous files).\n",
                                                sep = "")
                                          } else {
                                            dir.create(figure_directory)
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Outputs \"figure\" directory for ocean \"",
                                                ocean,
                                                "\", specie \"",
                                                specie,
                                                "\" and fishing mode \"",
                                                fishing_mode,
                                                "\" created.\n",
                                                "[directory path: ",
                                                figure_directory,
                                                "]\n",
                                                sep = "")
                                          }
                                          # create the raster
                                          wrl2 <- raster::rasterize(x = sps_year,
                                                                    y = wrl,
                                                                    field = "fit_prop_t3_ST")
                                          wrld_sf <- sf::st_as_sf(x = wrld)
                                          wrld_df <- as.data.frame(x = wrld)
                                          r_points = raster::rasterToPoints(x = wrl2)
                                          r_df = data.frame(r_points)
                                          f_prop <- ggplot2::ggplot() +
                                            ggplot2::geom_sf(data = wrld_sf) +
                                            ggplot2::coord_sf() +
                                            ggplot2::geom_tile(data = r_df,
                                                               ggplot2::aes(x = x,
                                                                            y = year,
                                                                            fill = layer)) +
                                            ggplot2::scale_fill_gradient2("Catches (t)",
                                                                          low = "blue",
                                                                          high = "red",
                                                                          mid = "Yellow",
                                                                          midpoint = 0.5,
                                                                          limits = c(0,1)) +
                                            ggplot2::theme_bw() +
                                            ggplot2::labs(x = "Longitude",
                                                          y = "Latitude",
                                                          title = paste(specie,
                                                                        ifelse(test = fishing_mode == 1,
                                                                               yes = "FOB",
                                                                               no = "FSC"),
                                                                        year,
                                                                        sep = "-"))
                                          outputs_level3_process5[[3]] <- append(outputs_level3_process5[[3]],
                                                                                 list(f_prop))
                                          names(outputs_level3_process5[[3]])[length(outputs_level3_process5[[3]])] <- paste("prop",
                                                                                                                             year,
                                                                                                                             ocean,
                                                                                                                             specie,
                                                                                                                             fishing_mode,
                                                                                                                             sep = "_")
                                          ggplot2::ggsave(plot = f_prop,
                                                          file = file.path(figure_directory,
                                                                           paste0("prop_",
                                                                                  year,
                                                                                  "_",
                                                                                  ocean,
                                                                                  "_",
                                                                                  specie,
                                                                                  "_",
                                                                                  fishing_mode,
                                                                                  ".jpeg")),
                                                          width = 12,
                                                          height = 10,
                                                          units = c("cm"),
                                                          dpi = 300,
                                                          pointsize = 6)
                                          # create the raster
                                          wrl2 <- raster::rasterize(x = sps_year,
                                                                    y = wrl,
                                                                    field =  "catch_t3_N3")
                                          wrld_sf <- sf::st_as_sf(x = wrld)
                                          # the world map
                                          wrld_df <- as.data.frame(x = wrld)
                                          r_points <- raster::rasterToPoints(x = wrl2)
                                          # t2
                                          r_df <- data.frame(r_points)
                                          # breaks for the legend
                                          qt <- raster::quantile(x = r_df$layer,
                                                                 na.rm = T,
                                                                 seq(0.1,1,0.1))
                                          # catch categories
                                          r_df$labs <- raster::cut(x = r_df$layer,
                                                                   right = F,
                                                                   breaks = unique(c(0, ceiling(qt))))
                                          f_catch <- ggplot2::ggplot() +
                                            ggplot2::geom_sf(data = wrld_sf) +
                                            ggplot2::coord_sf() +
                                            ggplot2::geom_tile(data = r_df,
                                                               ggplot2::aes(x = x,
                                                                            y = year,
                                                                            fill = labs)) +
                                            ggplot2::scale_fill_manual("Catches (t)",
                                                                       values = palette4catch(length(levels(r_df$labs)))) +
                                            ggplot2::theme_bw() +
                                            ggplot2::labs(x = "Longitude",
                                                          y = "Latitude",
                                                          title = paste(specie, ifelse(test = fishing_mode == 1,
                                                                                       yes = "FOB",
                                                                                       no = "FSC"),
                                                                        year,
                                                                        sep = "-"))
                                          outputs_level3_process5[[3]] <- append(outputs_level3_process5[[3]],
                                                                                 list(f_catch))
                                          names(outputs_level3_process5[[3]])[length(outputs_level3_process5[[3]])] <- paste("catch",
                                                                                                                             year,
                                                                                                                             ocean,
                                                                                                                             specie,
                                                                                                                             fishing_mode,
                                                                                                                             sep = "_")
                                          ggplot2::ggsave(plot = f_catch,
                                                          file = file.path(figure_directory,
                                                                           paste0("catch_",
                                                                                  year,
                                                                                  "_",
                                                                                  ocean,
                                                                                  "_",
                                                                                  specie,
                                                                                  "_",
                                                                                  fishing_mode,
                                                                                  ".jpeg")),
                                                          width = 12,
                                                          height = 10,
                                                          units = c("cm"),
                                                          dpi = 300,
                                                          pointsize = 6)
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.5: model predictions.\n",
                                  sep = "")
                              return(outputs_level3_process5)
                            },
                            # browser ----
                            #' @description Most powerfull and "schwifty" function in the univers for "open the T3 process" and manipulate in live R6 objects.
                            show_me_what_you_got = function() {
                              browser()
                            }),
                          private = list(
                            id_not_full_trip = NULL,
                            id_not_full_trip_retained = NULL,
                            data_selected = NULL,
                            log_summary = NULL
                          ))
