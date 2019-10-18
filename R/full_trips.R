#' @name full_trips
#' @title R6 class full_trips creation
#' @description Create R6 reference object class full_trips
#' @importFrom R6 R6Class
full_trips <- R6::R6Class(classname = "full_trips",
                          inherit = t3:::tools_t3,
                          public = list(
                            initialize = function(...) {
                              private$data <- list()
                            },
                            # full trips creation ----
                            create = function(object_trips) {
                              if (! any(class(object_trips) == "R6") | ! any(class(object_trips) == "trips")) {
                                stop("invalid \"object_trips\" argument\nClass R6 and trips expected")
                              }
                              full_trips <- list()
                              full_trips_tmp <- list()
                              full_trip_warning <- 0
                              i <- 1
                              while(i <= object_trips$count()) {
                                if (object_trips$view(i)[[1]]$.__enclos_env__$private$fish_hold_empty == 1) {
                                  full_trips <- append(full_trips, object_trips$view(i)[[1]])
                                  i = i + 1
                                } else {
                                  for (j in i:object_trips$count()) {
                                    if (j == object_trips$count()) {
                                      full_trips_tmp <- append(full_trips_tmp, object_trips$view(j)[[1]])
                                      full_trip_warning <- 1
                                      i = i + 1
                                    } else {
                                      if (object_trips$view(j)[[1]]$.__enclos_env__$private$vessel_id == object_trips$view(j+1)[[1]]$.__enclos_env__$private$vessel_id) {
                                        full_trips_tmp <- append(full_trips_tmp, object_trips$view(j)[[1]])
                                        if (object_trips$view(j+1)[[1]]$.__enclos_env__$private$fish_hold_empty == 1) {
                                          full_trips_tmp <- append(full_trips_tmp, object_trips$view(j+1)[[1]])
                                          i = j + 2
                                          break()
                                        }
                                      } else {
                                        full_trip_warning <- 1
                                        full_trips_tmp <- append(full_trips_tmp, object_trips$view(j)[[1]])
                                        i = j + 1
                                        break()
                                      }
                                    }
                                  }
                                  if (full_trip_warning == 1) {
                                    full_trip_warning <- 0
                                    private$id_not_full_trip <- append(private$id_not_full_trip, length(full_trips) + 1)
                                    warning("missing trip(s) in item ", length(full_trips) + 1,
                                            call. = FALSE)
                                  }
                                  full_trips <- append(full_trips, list(full_trips_tmp))
                                  full_trips_tmp <- list()
                                }
                              }
                              names(full_trips) <- 1:length(full_trips)
                              private$data <- full_trips
                            },
                            # full trips filter ----
                            filter = function(year_reference) {
                              if (length(class(year_reference)) == 1 && class(year_reference) != "numeric") {
                                stop("invalid \"year_reference\" argument\nclass numeric expected")
                              } else if (nchar(year_reference) != 4) {
                                stop("invalid \"year_reference\" argument\nyear format on 4 digits expected")
                              } else {
                                for (i in 1:length(private$data)) {
                                  if (i == 1) {
                                    full_trips_removed <- vector(mode = "numeric")
                                  }
                                  full_trips_tmp <- private$data[[i]]
                                  year_full_trips <- vector(mode = "numeric")
                                  if (length(class(full_trips_tmp)) != 1 || class(full_trips_tmp) != "list") {
                                    year_full_trips <- lubridate::year(x = full_trips_tmp$.__enclos_env__$private$landing_date)
                                  } else {
                                    for (j in 1:length(full_trips_tmp)) {
                                      full_trips_tmp_bis <- full_trips_tmp[[j]]
                                      year_full_trips <- append(year_full_trips,
                                                                lubridate::year(x = full_trips_tmp_bis$.__enclos_env__$private$landing_date))
                                    }
                                  }
                                  if (all(year_full_trips != year_reference)) {
                                    full_trips_removed <- append(full_trips_removed, i)
                                  }
                                }
                                private$data_removed <- super$view(full_trips_removed,
                                                                   attribut = "data")
                                names(private$data_removed) <- full_trips_removed
                                super$remove(item_id = full_trips_removed,
                                             attribut = "data")
                                if (! all(private$id_not_full_trip %in% full_trips_removed)) {
                                  warning("missing trip(s) in at least one full trip item\ncheck id(s): ",
                                          base::setdiff(private$id_not_full_trip,
                                                        full_trips_removed),
                                          call. = FALSE)
                                  private$id_not_full_trip_retained <- base::setdiff(private$id_not_full_trip,
                                                                                     full_trips_removed)
                                }
                              }
                            },
                            # Raising Factor level 1 ----
                            rf1 = function() {
                              print("cover")
                            }),
                          private = list(
                            id_not_full_trip = NULL,
                            id_not_full_trip_retained = NULL,
                            data_removed = NULL
                          ))
