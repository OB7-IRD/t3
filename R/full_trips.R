#' @name full_trips
#' @title R6 class full_trips creation
#' @description Create R6 reference object class full_trips
#' @importFrom R6 R6Class
#' @importFrom lubridate year hms dseconds int_length interval days as_date
#' @importFrom suncalc getSunlightTimes
#' @importFrom dplyr group_by summarise last first filter ungroup
#' @importFrom ranger ranger predictions importance
#' @importFrom tidyr gather spread separate
#' @importFrom sp coordinates proj4string spTransform
#' @importFrom spdep dnearneigh nb2listw moran.mc moran.test
#' @importFrom rfUtilities multi.collinear
#' @importFrom gstat variogram
full_trips <- R6::R6Class(classname = "full_trips",
                          inherit = t3:::list_t3,
                          public = list(
                            # full trips creation ----
                            #' @description Creation of full trip item from trips.
                            #' @param object_trips (R6-trips) A R6 reference object of class trips.
                            create_full_trips = function(object_trips) {
                              if (paste(class(object_trips), collapse = " ") != "trips list_t3 R6") {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"object_trips\" argument\nClass R6 and trips expected.\n",
                                    sep = "")
                                stop()
                              }
                              full_trips <- list()
                              full_trips_tmp <- list()
                              full_trip_warning <- 0
                              i <- 1
                              while (i <= object_trips$count()) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start full trips creation.\n",
                                      sep = "")
                                }
                                if (object_trips$view(i)[[1]]$.__enclos_env__$private$fish_hold_empty == 1) {
                                  full_trips <- append(full_trips, list(list(object_trips$view(i)[[1]])))
                                  i <- i + 1
                                } else {
                                  for (j in i:object_trips$count()) {
                                    if (j == object_trips$count()) {
                                      full_trips_tmp <- append(full_trips_tmp, object_trips$view(j)[[1]])
                                      full_trip_warning <- 1
                                      i <- i + 1
                                    } else {
                                      if (object_trips$view(j)[[1]]$.__enclos_env__$private$vessel_id == object_trips$view(j + 1)[[1]]$.__enclos_env__$private$vessel_id) {
                                        full_trips_tmp <- append(full_trips_tmp, object_trips$view(j)[[1]])
                                        if (object_trips$view(j + 1)[[1]]$.__enclos_env__$private$fish_hold_empty == 1) {
                                          full_trips_tmp <- append(full_trips_tmp, object_trips$view(j + 1)[[1]])
                                          i <- j + 2
                                          break ()
                                        }
                                      } else {
                                        full_trip_warning <- 1
                                        full_trips_tmp <- append(full_trips_tmp, object_trips$view(j)[[1]])
                                        i <- j + 1
                                        break ()
                                      }
                                    }
                                  }
                                  if (full_trip_warning == 1) {
                                    full_trip_warning <- 0
                                    private$id_not_full_trip <- append(private$id_not_full_trip, length(full_trips) + 1)
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: missing trip(s) in item ",
                                        length(full_trips) + 1,
                                        ".\n[trip: ",
                                        object_trips$view(j)[[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                  }
                                  full_trips <- append(full_trips, list(full_trips_tmp))
                                  full_trips_tmp <- list()
                                }
                              }
                              names(full_trips) <- seq_len(length.out = length(full_trips))
                              private$data <- full_trips
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End of full trips creation.\n",
                                  sep = "")
                            },
                            # filter full trips by periode_reference ----
                            #' @description Function for filter full trips by a reference periode.
                            #' @param periode_reference (integer) Year(s) in 4 digits format.
                            filter_by_periode = function(periode_reference) {
                              if (any(class(periode_reference) != "integer")) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"periode_reference\" argument\nclass integer expected.\n",
                                    sep = "")
                                stop()
                              } else if (any(nchar(periode_reference) != 4)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"periode_reference\" argument\nyear format on 4 digits expected.\n",
                                    sep = "")
                                stop()
                              } else {
                                for (i in seq_len(length.out = length(private$data))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start of full trips filtering by reference periode.\n",
                                        sep = "")
                                  }
                                  full_trips_tmp <- private$data[[i]]
                                  year_full_trips <- vector(mode = "integer")
                                  for (j in seq_len(length.out = length(full_trips_tmp))) {
                                    full_trips_tmp_bis <- full_trips_tmp[[j]]
                                    year_full_trips <- append(year_full_trips,
                                                              as.integer(
                                                                lubridate::year(
                                                                  x = full_trips_tmp_bis$.__enclos_env__$private$landing_date)
                                                                )
                                                              )
                                  }
                                  if (any(year_full_trips %in% periode_reference)) {
                                    private$data_selected <- append(private$data_selected,
                                                                    list(full_trips_tmp))
                                    names(private$data_selected)[length(private$data_selected)] <- names(private$data[i])
                                  }
                                }
                                if (any(private$id_not_full_trip %in% names(private$data_selected))) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: missing trip(s) in at least one full trip item.\n",
                                      "[id(s) element(s): ",
                                      paste(intersect(private$id_not_full_trip,
                                                      private$data_selected),
                                            collapse = ", "),
                                      "]\n",
                                      sep = "")
                                  private$id_not_full_trip_retained <- intersect(
                                    private$id_not_full_trip,
                                    private$data_selected
                                  )
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - End of full trips filtering.\n",
                                    sep = "")
                              }
                            },
                            # add activities ----
                            #' @description Function for add activities in full trips object.
                            #' @param object_activities (R6-activities) A R6 reference object of class activities.
                            add_activities = function(object_activities) {
                              if (length(private$data_selected) == 0) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: argument \"data_selected\" empty, ",
                                    "launch selection data before.\n",
                                    sep = "")
                                stop()
                              } else if (! any(class(object_activities) == "activities")) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"object_activities\" argument, ",
                                    "class activities expected.\n",
                                    sep = "")
                                stop()
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start of add activity.\n",
                                        sep = "")
                                  }
                                  for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                    trip_id <- private$data_selected[[i]][[j]]$.__enclos_env__$private$trip_id
                                    activities_tmp <- object_activities$filter_by_trip(trip_id = trip_id)
                                    private$data_selected[[i]][[j]]$.__enclos_env__$private$activities <- activities_tmp
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - End of add activity.\n",
                                    sep = "")
                              }
                            },
                            # add elementary catches ----
                            #' @description Function for add elementary catches in full trips object.
                            #' @param object_elementarycatches (R6-elementarycatches) A R6 reference object of class elementarycatches.
                            add_elementarycatches = function(object_elementarycatches) {
                              if (length(private$data_selected) == 0) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: argument \"data_selected\" empty, ",
                                    "launch selection data before.\n",
                                    sep = "")
                                stop()
                              } else if (! any(class(object_elementarycatches) == "elementarycatches")) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"object_elementarycatches\" argument, ",
                                    "class elementarycatches expected.\n",
                                    sep = "")
                                stop()
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start of add elementary catches.\n",
                                        sep = "")
                                  }
                                  for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                    if (length(private$data_selected[[i]][[j]]$.__enclos_env__$private$activities) != 0) {
                                      for (k in seq_len(length.out = length(private$data_selected[[i]][[j]]$.__enclos_env__$private$activities))) {
                                        if (private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$activity_code %in% c(0, 1, 2, 14)) {
                                          activity_id <- private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$activity_id
                                          elementarycatches_tmp <- object_elementarycatches$filter_by_activity(activity_id = activity_id)
                                          private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$elementarycatches <- elementarycatches_tmp
                                        }
                                      }
                                    }
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - End of add elementary catches.\n",
                                    sep = "")
                              }
                            },
                            # add elementary landings ----
                            #' @description Function for add elementary landings in full trips object.
                            #' @param object_elementarylandings (R6-elementarylandings) A R6 reference object of class elementarylandings.
                            add_elementarylandings = function(object_elementarylandings) {
                              if (length(private$data_selected) == 0) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: argument \"data_selecetd\" empty, ",
                                    "launch selection data before.\n",
                                    sep = "")
                                stop()
                              } else if (! any(class(object_elementarylandings) == "elementarylandings")) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"object_elementarylandings\" argument, ",
                                    "class elementarylandings expected.\n",
                                    sep = "")
                                stop()
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start of add elementary landings.\n",
                                        sep = "")
                                  }
                                  for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                    trip_id <- private$data_selected[[i]][[j]]$.__enclos_env__$private$trip_id
                                    elementarylandings_tmp <- object_elementarylandings$filter_by_trip(trip_id = trip_id)
                                    private$data_selected[[i]][[j]]$.__enclos_env__$private$elementarylandings <- elementarylandings_tmp
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - End of add elementary landings.\n",
                                    sep = "")
                              }
                            },
                            # add wells and samples ----
                            #' @description Function for add wells and samples caracteristics in full trips object.
                            #' @param object_wells (R6-wells) A R6 reference object of class wells.
                            add_wells_samples = function(object_wells) {
                              if (length(private$data_selected) == 0) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: argument \"data_selecetd\" empty, ",
                                    "launch selection data before.\n",
                                    sep = "")
                                stop()
                              } else if (! any(class(object_wells) == "wells")) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"object_wells\" argument, ",
                                    "class wells expected.\n",
                                    sep = "")
                                stop()
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start of add well(s) - sample(s).\n",
                                        sep = "")
                                  }
                                  for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                    trip_id <- private$data_selected[[i]][[j]]$.__enclos_env__$private$trip_id
                                    trip_wells <- object_wells$filter_by_trip(trip_id = trip_id)
                                    private$data_selected[[i]][[j]]$.__enclos_env__$private$wells <- trip_wells
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - End of add well(s) - sample(s).\n",
                                    sep = "")
                              }
                            },
                            # process 1.2: rf1 ----
                            #' @description Process of Raising Factor level 1 (rf1).
                            #' @param species_rf1 (integer) Specie(s) code(s) used for the rf1 process.
                            #' @param rf1_lowest_limit (numeric) Verification value for the lowest limit of the rf1.
                            #' @param rf1_highest_limit (numeric) Verification value for the highest limit of the rf1.
                            rf1 = function(species_rf1,
                                           rf1_lowest_limit = 0.8,
                                           rf1_highest_limit = 1.2) {
                              if (any(class(species_rf1) != "integer")) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"species_rf1\" argument, ",
                                    "class integer expected.\n",
                                    sep = "")
                                stop()
                              } else if (length(class(rf1_lowest_limit)) != 1 || class(rf1_lowest_limit) != "numeric") {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"rf1_lowest_limit\" argument, ",
                                    "class numeric with one value expected.\n",
                                    sep = "")
                                stop()
                              } else if (length(class(rf1_highest_limit)) != 1 || class(rf1_highest_limit) != "numeric") {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"rf1_highest_limit\" argument, ",
                                    "class numeric with one value expected.\n",
                                    sep = "")
                                stop()
                              } else {
                                if (is.null(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Empty data selected in the R6 object.\n",
                                      " - Process 1.1 (raising factor level 1) cancelled.\n",
                                      sep = "")
                                } else {
                                  for (i in seq_len(length.out = length(private$data_selected))) {
                                    if (i == 1) {
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - Start process 1.1: raising factor level 1.\n",
                                          sep = "")
                                    }
                                    if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                      # case 1: at least on trip is missing in the full trip item
                                      # check if functions for selection of elementary catches and elementary landings ran before
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - Warning: missing trip(s) in full trip element ",
                                          i,
                                          ".\n",
                                          sep = "")
                                      stop <- 0
                                      for (k in seq_len(length.out = length(private$data_selected[[i]]))) {
                                        # case 1.1: at least one logbook is missing in not complet full trip item
                                        if (k == 1) {
                                          logbook_availability <- vector(mode = "integer")
                                        }
                                        current_trip <- private$data_selected[[i]][[k]]
                                        logbook_availability <- append(logbook_availability,
                                                                       current_trip$.__enclos_env__$private$logbook_availability)
                                        if (k == length(private$data_selected[[i]])) {
                                          if (any(logbook_availability) == 0) {
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Warning: missing logbook in trip element ",
                                                i,
                                                ".\n",
                                                "[trip: ",
                                                current_trip$.__enclos_env__$private$trip_id,
                                                "]\n",
                                                sep = "")
                                            for (l in seq_len(length.out = length(private$data_selected[[i]]))) {
                                              current_trip <- private$data_selected[[i]][[l]]
                                              current_trip$.__enclos_env__$private$rf1 <- 1
                                              current_trip$.__enclos_env__$private$statut_rf1 <- 1.1
                                            }
                                            stop <- 1
                                          }
                                        }
                                      }
                                      if (stop != 1) {
                                        for (m in seq_len(length.out = length(private$data_selected[[i]]))) {
                                          if (m == 1) {
                                            current_elementarycatches <- NULL
                                          }
                                          current_trip <- private$data_selected[[i]][[m]]
                                          if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                            for (w in seq_len(length.out = length(current_trip$.__enclos_env__$private$activities))) {
                                              current_elementarycatches <- append(current_elementarycatches,
                                                                                  current_trip$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches)
                                            }
                                          }
                                        }
                                        if (is.null(current_elementarycatches)) {
                                          # case 1.2: trips with no catches (for example route or support) in not complet full trip item
                                          for (n in seq_len(length.out = length(private$data_selected[[i]]))) {
                                            current_trip <- private$data_selected[[i]][[n]]
                                            current_trip$.__enclos_env__$private$rf1 <- 1
                                            current_trip$.__enclos_env__$private$statut_rf1 <- 2.2
                                          }
                                        } else {
                                          for (p in seq_len(length.out = length(private$data_selected[[i]]))) {
                                            if (p == 1) {
                                              current_elementarylandings <- NULL
                                              stop_bis <- 0
                                            }
                                            current_trip <- private$data_selected[[i]][[p]]
                                            if (p == length(private$data_selected[[i]])) {
                                              if (! is.null(unlist(current_trip$.__enclos_env__$private$elementarylandings))) {
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
                                              # case 1.3: at least one elementary landing is missing in not complet full trip item
                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                  " - Warning: missing elementary landing in trip element ",
                                                  i,
                                                  ".\n",
                                                  "[trip: ",
                                                  current_trip$.__enclos_env__$private$trip_id,
                                                  "]\n",
                                                  sep = "")
                                              for (q in seq_len(length.out = length(private$data_selected[[i]]))) {
                                                current_trip <- private$data_selected[[i]][[q]]
                                                current_trip$.__enclos_env__$private$rf1 <- 1
                                                current_trip$.__enclos_env__$private$statut_rf1 <- 1.3
                                              }
                                            } else {
                                              # case 1.4: almost rocks dude ! (not complet full trip item)
                                              for (s in seq_len(length.out = length(private$data_selected[[i]]))) {
                                                current_trip <- private$data_selected[[i]][[s]]
                                                current_trip$.__enclos_env__$private$rf1 <- 1
                                                current_trip$.__enclos_env__$private$statut_rf1 <- 1.4
                                                if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                                  current_elementarycatches <- NULL
                                                  for (z in seq_len(length.out = length(current_trip$.__enclos_env__$private$activities))) {
                                                    current_elementarycatches <- append(current_elementarycatches,
                                                                                        current_trip$.__enclos_env__$private$activities[[z]]$.__enclos_env__$private$elementarycatches)
                                                  }
                                                }
                                                if (! is.null(current_elementarycatches)) {
                                                  for (t in seq_len(length.out = length(current_elementarycatches))) {
                                                    current_elementarycatches[[t]]$.__enclos_env__$private$catch_weight_rf1 <- current_elementarycatches[[t]]$.__enclos_env__$private$catch_weight
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    } else {
                                      # case 2: full trip is complet
                                      # check if functions for selection of elementary catches and elementary landings ran before
                                      stop <- 0
                                      # case 2.1: at least one logbook is missing in complet full trip item
                                      for (k in seq_len(length.out = length(private$data_selected[[i]]))) {
                                        if (k == 1) {
                                          logbook_availability <- vector(mode = "integer")
                                        }
                                        current_trip <- private$data_selected[[i]][[k]]
                                        logbook_availability <- append(logbook_availability,
                                                                       current_trip$.__enclos_env__$private$logbook_availability)
                                        if (k == length(private$data_selected[[i]])) {
                                          if (any(logbook_availability) == 0) {
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Warning: missing logbook in trip element ",
                                                i,
                                                ".\n"
                                                ,"[trip: ",
                                                current_trip$.__enclos_env__$private$trip_id,
                                                "]\n",
                                                sep = "")
                                            for (l in seq_len(length.out = length(private$data_selected[[i]]))) {
                                              current_trip <- private$data_selected[[i]][[l]]
                                              current_trip$.__enclos_env__$private$rf1 <- 1
                                              current_trip$.__enclos_env__$private$statut_rf1 <- 2.1
                                            }
                                            stop <- 1
                                          }
                                        }
                                      }
                                      if (stop != 1) {
                                        for (m in seq_len(length.out = length(private$data_selected[[i]]))) {
                                          if (m == 1) {
                                            current_elementarycatches <- NULL
                                          }
                                          current_trip <- private$data_selected[[i]][[m]]
                                          if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                            for (w in seq_len(length.out = length(current_trip$.__enclos_env__$private$activities))) {
                                              current_elementarycatches <- append(current_elementarycatches,
                                                                                  current_trip$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches)
                                            }
                                          }
                                        }
                                        if (is.null(current_elementarycatches)) {
                                          # case 2.2: trips with no catches (for example route or support) in complet full trip item
                                          for (n in seq_len(length.out = length(private$data_selected[[i]]))) {
                                            current_trip <- private$data_selected[[i]][[n]]
                                            current_trip$.__enclos_env__$private$rf1 <- 1
                                            current_trip$.__enclos_env__$private$statut_rf1 <- 2.2
                                          }
                                        } else {
                                          current_elementarycatches_weight <- vector(mode = "numeric")
                                          for (o in seq_len(length.out = length(current_elementarycatches))) {
                                            if (current_elementarycatches[[o]]$.__enclos_env__$private$specie_code %in% species_rf1) {
                                              current_elementarycatches_weight <- append(current_elementarycatches_weight,
                                                                                         current_elementarycatches[[o]]$.__enclos_env__$private$catch_weight)
                                            }
                                          }
                                          for (p in seq_len(length.out = length(private$data_selected[[i]]))) {
                                            if (p == 1) {
                                              current_elementarylandings <- NULL
                                              stop_bis <- 0
                                            }
                                            current_trip <- private$data_selected[[i]][[p]]
                                            current_elementarylandings <- append(current_elementarylandings,
                                                                                 unlist(current_trip$.__enclos_env__$private$elementarylandings))
                                          }
                                          if (is.null(current_elementarylandings)) {
                                            # case 2.3: no elementary landing in complet full trip item
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Warning: missing elementary landing in trip element ",
                                                i,
                                                ".\n",
                                                "[trip: ",
                                                current_trip$.__enclos_env__$private$trip_id,
                                                "]\n",
                                                sep = "")
                                            for (q in seq_len(length.out = length(private$data_selected[[i]]))) {
                                              current_trip <- private$data_selected[[i]][[q]]
                                              current_trip$.__enclos_env__$private$rf1 <- 1
                                              current_trip$.__enclos_env__$private$statut_rf1 <- 2.3
                                            }
                                          } else {
                                            # case 2.4: everything rocks dude !
                                            current_elementarylandings_weight <- vector(mode = "numeric")
                                            for (r in seq_len(length.out = length(current_elementarylandings))) {
                                              if (current_elementarylandings[[r]]$.__enclos_env__$private$specie_code %in% species_rf1) {
                                                current_elementarylandings_weight <- append(current_elementarylandings_weight,
                                                                                            current_elementarylandings[[r]]$.__enclos_env__$private$landing_weight)
                                              }
                                            }
                                            current_rf1 <- sum(current_elementarylandings_weight) / sum(current_elementarycatches_weight)
                                            if (current_rf1 < rf1_lowest_limit | current_rf1 > rf1_highest_limit) {
                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                  " - Warning: rf1 value of trip element ",
                                                  i,
                                                  " out of theorical boundaries: ",
                                                  round(current_rf1, 3),
                                                  ".\n",
                                                  "[trip: ",
                                                  current_trip$.__enclos_env__$private$trip_id,
                                                  "]\n",
                                                  sep = "")
                                            }
                                            for (s in seq_len(length.out = length(private$data_selected[[i]]))) {
                                              current_trip <- private$data_selected[[i]][[s]]
                                              current_trip$.__enclos_env__$private$rf1 <- current_rf1
                                              current_trip$.__enclos_env__$private$statut_rf1 <- 2.4
                                            }
                                          }
                                        }
                                      }
                                      # assign rf1 to elementary catches
                                      for (u in seq_len(length.out = length(private$data_selected[[i]]))) {
                                        current_trip <- private$data_selected[[i]][[u]]
                                        current_rf1 <- current_trip$.__enclos_env__$private$rf1
                                        if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                          current_elementarycatches <- NULL
                                          for (x in seq_len(length.out = length(current_trip$.__enclos_env__$private$activities))) {
                                            current_elementarycatches <- append(current_elementarycatches,
                                                                                current_trip$.__enclos_env__$private$activities[[x]]$.__enclos_env__$private$elementarycatches)
                                          }
                                        }
                                        if (! is.null(current_elementarycatches)) {
                                          for (v in seq_len(length.out = length(current_elementarycatches))) {
                                            current_elementarycatches[[v]]$.__enclos_env__$private$catch_weight_rf1 <- current_elementarycatches[[v]]$.__enclos_env__$private$catch_weight * current_rf1
                                          }
                                        }
                                      }
                                    }
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Successful process 1.1: raising factor level 1.\n",
                                      sep = "")
                                }
                              }
                            },
                            # process 1.2: rf2 ----
                            #' @description Process of Raising Factor level 2 (rf2).
                            rf2 = function() {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 1.2 (raising factor level 2) cancelled.\n",
                                    sep = "")
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 1.2: raising factor level 2.\n",
                                        sep = "")
                                  }
                                  if (is.null(private$data_selected[[i]][[1]]$.__enclos_env__$private$statut_rf1)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Argument \"statut_rf1\" is null for the trip element ",
                                        i,
                                        ".\n",
                                        "Process 1.2 inapplicable, switch to next element.\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                  } else {
                                    if (private$data_selected[[i]][[1]]$.__enclos_env__$private$statut_rf1 == 2.1) {
                                      # case 1: rf2 calculated
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - Error: rf2 not developped yet.\n",
                                          sep = "")
                                      stop()
                                    } else if (private$data_selected[[i]][[1]]$.__enclos_env__$private$statut_rf1 %in% c(2.2, 2.3, 2.4)) {
                                      # case 2: rf2 not need to be calculated
                                      for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                        current_trip <- private$data_selected[[i]][[j]]
                                        current_rf2 <- 1
                                        current_trip$.__enclos_env__$private$rf2 <- current_rf2
                                        current_trip$.__enclos_env__$private$statut_rf2 <- 2
                                        if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                          current_elementarycatches <- NULL
                                          for (m in seq_len(length.out = length(current_trip$.__enclos_env__$private$activities))) {
                                            current_elementarycatches <- append(current_elementarycatches,
                                                                                current_trip$.__enclos_env__$private$activities[[m]]$.__enclos_env__$private$elementarycatches)
                                          }
                                        }
                                        if (length(current_elementarycatches) != 0) {
                                          for (k in seq_len(length.out = length(current_elementarycatches))) {
                                            current_elementarycatches[[k]]$.__enclos_env__$private$catch_weight_rf2 <- current_elementarycatches[[k]]$.__enclos_env__$private$catch_weight_rf1 * current_rf2
                                          }
                                        }
                                      }
                                    } else {
                                      # case 3: full trip not complet
                                      for (l in seq_len(length.out = length(private$data_selected[[i]]))) {
                                        current_trip <- private$data_selected[[i]][[l]]
                                        current_rf2 <- 1
                                        current_trip$.__enclos_env__$private$rf2 <- current_rf2
                                        current_trip$.__enclos_env__$private$statut_rf2 <- 3
                                      }
                                    }
                                  }
                                  if (i == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End of raising factor process 2.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 1.3: conversion_weigth_category ----
                            #' @description Process of logbook weigth categories conversion.
                            conversion_weigth_category = function() {
                              category_1 <- "<10kg"
                              category_2 <- "10-30kg"
                              category_3 <- ">30kg"
                              category_4 <- ">10kg"
                              category_5 <- "unknown"
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 1.3 (logbook weight categories) cancelled.\n",
                                    sep = "")
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 1.3: logbook weight categories conversion.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    next()
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.3 on item ",
                                        i,
                                        ".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    if (is.null(private$data_selected[[i]][[1]]$.__enclos_env__$private$rf2)) {
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - Warning: rf2 is null for the item ",
                                          i,
                                          ".\n",
                                          "Check if the process 1.2 (raising factor level 2) was successfully applied.\n",
                                          "Switch to next element.\n",
                                          "[trip: ",
                                          private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                          "]\n",
                                          sep = "")
                                    } else {
                                      # first stage: conversion of all categories except for unknown (category 9)
                                      for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                        current_trip <- private$data_selected[[i]][[j]]
                                        if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                          for (w in seq_len(length.out = length(current_trip$.__enclos_env__$private$activities))) {
                                            if (current_trip$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$activity_code %in% c(0, 1, 2, 14)) {
                                              current_elementarycatches <- current_trip$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches
                                              if (length(current_elementarycatches) != 0) {
                                                ocean_activity <- current_trip$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$ocean
                                                school_type_activity <- current_trip$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$school_type
                                                for (k in seq_len(length.out = length(current_elementarycatches))) {
                                                  current_elementarycatch <- current_elementarycatches[[k]]
                                                  if (ocean_activity == 1) {
                                                    # for atlantic ocean
                                                    if (school_type_activity %in% c(2, 3)) {
                                                      # for free school and undetermined school
                                                      if (current_elementarycatch$.__enclos_env__$private$specie_code3l %in% c("YFT", "BET", "ALB")) {
                                                        # for YFT, BET and ALB
                                                        if (current_elementarycatch$.__enclos_env__$private$logbook_category %in% c(1, 2, 10)) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else if (current_elementarycatch$.__enclos_env__$private$logbook_category == 4) {
                                                          current_elementarycatch_tmp <- current_elementarycatch$clone()
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2 * 0.2
                                                          current_elementarycatch_tmp$.__enclos_env__$private$corrected_logbook_category <- category_2
                                                          current_elementarycatch_tmp$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch_tmp$.__enclos_env__$private$catch_weight_rf2 * 0.8
                                                          private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches <- append(private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches,
                                                                                                                                                                                      current_elementarycatch_tmp)
                                                        } else if (current_elementarycatch$.__enclos_env__$private$logbook_category %in% c(3, 12)) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_2
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else if (current_elementarycatch$.__enclos_env__$private$logbook_category == 6) {
                                                          current_elementarycatch_tmp <- current_elementarycatch$clone()
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_2
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2 * 0.5
                                                          current_elementarycatch_tmp$.__enclos_env__$private$corrected_logbook_category <- category_3
                                                          current_elementarycatch_tmp$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch_tmp$.__enclos_env__$private$catch_weight_rf2 * 0.5
                                                          private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches <- append(private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches,
                                                                                                                                                                                      current_elementarycatch_tmp)
                                                        } else if (current_elementarycatch$.__enclos_env__$private$logbook_category %in% c(5, 7, 8, 13)) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_3
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else if (current_elementarycatch$.__enclos_env__$private$logbook_category == 11) {
                                                          current_elementarycatch_tmp <- current_elementarycatch$clone()
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_2
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2 * 0.1
                                                          current_elementarycatch_tmp$.__enclos_env__$private$corrected_logbook_category <- category_3
                                                          current_elementarycatch_tmp$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch_tmp$.__enclos_env__$private$catch_weight_rf2 * 0.9
                                                          private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches <- append(private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches,
                                                                                                                                                                                      current_elementarycatch_tmp)
                                                        } else if (current_elementarycatch$.__enclos_env__$private$logbook_category == 9) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_5
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else {
                                                          cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                              " - Error: logbook category ",
                                                              current_elementarycatch$.__enclos_env__$private$logbook_category,
                                                              " not set in the algorithm.\n",
                                                              "[trip: ",
                                                              current_trip$.__enclos_env__$private$trip_id,
                                                              ", activity: ",
                                                              current_elementarycatch$.__enclos_env__$private$activity_id,
                                                              ", elementarycatch: ",
                                                              current_elementarycatch$.__enclos_env__$private$elementarycatch_id,
                                                              "]\n",
                                                              sep = "")
                                                          stop()
                                                        }
                                                      } else if (current_elementarycatch$.__enclos_env__$private$specie_code3l == "SKJ") {
                                                        if (current_elementarycatch$.__enclos_env__$private$logbook_category != 9) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_5
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        }
                                                      } else {
                                                        current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_5
                                                        current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                      }
                                                    } else {
                                                      # for floating object school
                                                      if (current_elementarycatch$.__enclos_env__$private$specie_code3l %in% c("YFT", "BET", "ALB")) {
                                                        if (current_elementarycatch$.__enclos_env__$private$logbook_category %in% c(1, 2, 10)) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else if (current_elementarycatch$.__enclos_env__$private$logbook_category == 4) {
                                                          current_elementarycatch_tmp <- current_elementarycatch$clone()
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2 * 0.2
                                                          current_elementarycatch_tmp$.__enclos_env__$private$corrected_logbook_category <- category_4
                                                          current_elementarycatch_tmp$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch_tmp$.__enclos_env__$private$catch_weight_rf2 * 0.8
                                                          private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches <- append(private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches,
                                                                                                                                                                                      current_elementarycatch_tmp)
                                                        } else if (current_elementarycatch$.__enclos_env__$private$logbook_category %in% c(3, 12, 5, 7, 8, 13, 6, 11)) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_4
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else if (current_elementarycatch$.__enclos_env__$private$logbook_category == 9) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_5
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else {
                                                          cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                              " - Error: logbook category ",
                                                              current_elementarycatch$.__enclos_env__$private$logbook_category,
                                                              " not set in the algorithm.\n",
                                                              "[trip: ",
                                                              current_trip$.__enclos_env__$private$trip_id,
                                                              ", activity: ",
                                                              current_elementarycatch$.__enclos_env__$private$activity_id,
                                                              ", elementarycatch: ",
                                                              current_elementarycatch$.__enclos_env__$private$elementarycatch_id,
                                                              "]\n",
                                                              sep = "")
                                                          stop()
                                                        }
                                                      } else if (current_elementarycatch$.__enclos_env__$private$specie_code3l == "SKJ") {
                                                        if (current_elementarycatch$.__enclos_env__$private$logbook_category != 9) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_5
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        }
                                                      } else {
                                                        current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_5
                                                        current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                      }
                                                    }
                                                  } else if (ocean_activity == 2) {
                                                    # for indian ocean
                                                    if (school_type_activity %in% c(2, 3)) {
                                                      # for free school and undetermined school
                                                      if (current_elementarycatch$.__enclos_env__$private$specie_code3l %in% c("YFT", "BET", "ALB")) {
                                                        if (current_elementarycatch$.__enclos_env__$private$logbook_category %in% c(1, 2, 10)) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else if (current_elementarycatch$.__enclos_env__$private$logbook_category == 4) {
                                                          current_elementarycatch_tmp <- current_elementarycatch$clone()
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2 * 0.2
                                                          current_elementarycatch_tmp$.__enclos_env__$private$corrected_logbook_category <- category_4
                                                          current_elementarycatch_tmp$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch_tmp$.__enclos_env__$private$catch_weight_rf2 * 0.8
                                                          private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches <- append(private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches,
                                                                                                                                                                                      current_elementarycatch_tmp)
                                                        } else if (current_elementarycatch$.__enclos_env__$private$logbook_category %in% c(3, 12, 5, 7, 8, 13, 6, 11)) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_4
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else if (current_elementarycatch$.__enclos_env__$private$logbook_category == 9) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_5
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else {
                                                          cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                              " - Error: logbook category ",
                                                              current_elementarycatch$.__enclos_env__$private$logbook_category,
                                                              " not set in the algorithm.\n",
                                                              "[trip: ",
                                                              current_trip$.__enclos_env__$private$trip_id,
                                                              ", activity: ",
                                                              current_elementarycatch$.__enclos_env__$private$activity_id,
                                                              ", elementarycatch: ",
                                                              current_elementarycatch$.__enclos_env__$private$elementarycatch_id,
                                                              "]\n",
                                                              sep = "")
                                                          stop()
                                                        }
                                                      } else if (current_elementarycatch$.__enclos_env__$private$specie_code3l == "SKJ") {
                                                        if (current_elementarycatch$.__enclos_env__$private$logbook_category != 9) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_5
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        }
                                                      } else {
                                                        current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_5
                                                        current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                      }
                                                    } else {
                                                      # for floating object school
                                                      if (current_elementarycatch$.__enclos_env__$private$specie_code3l %in% c("YFT", "BET", "ALB")) {
                                                        if (current_elementarycatch$.__enclos_env__$private$logbook_category %in% c(1, 2, 10)) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else if (current_elementarycatch$.__enclos_env__$private$logbook_category == 4) {
                                                          current_elementarycatch_tmp <- current_elementarycatch$clone()
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2 * 0.2
                                                          current_elementarycatch_tmp$.__enclos_env__$private$corrected_logbook_category <- category_4
                                                          current_elementarycatch_tmp$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch_tmp$.__enclos_env__$private$catch_weight_rf2 * 0.8
                                                          private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches <- append(private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches,
                                                                                                                                                                                      current_elementarycatch_tmp)
                                                        } else if (current_elementarycatch$.__enclos_env__$private$logbook_category %in% c(3, 12, 5, 7, 8, 13, 6, 11)) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_4
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else if (current_elementarycatch$.__enclos_env__$private$logbook_category == 9) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_5
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else {
                                                          cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                              " - Error: logbook category ",
                                                              current_elementarycatch$.__enclos_env__$private$logbook_category,
                                                              " not set in the algorithm.\n",
                                                              "[trip: ",
                                                              current_trip$.__enclos_env__$private$trip_id,
                                                              ", activity: ",
                                                              current_elementarycatch$.__enclos_env__$private$activity_id,
                                                              ", elementarycatch: ",
                                                              current_elementarycatch$.__enclos_env__$private$elementarycatch_id,
                                                              "]\n",
                                                              sep = "")
                                                          stop()
                                                        }
                                                      } else if (current_elementarycatch$.__enclos_env__$private$specie_code3l == "SKJ") {
                                                        if (current_elementarycatch$.__enclos_env__$private$logbook_category != 9) {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        } else {
                                                          current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_5
                                                          current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                        }
                                                      } else {
                                                        current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_5
                                                        current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                      }
                                                    }
                                                  } else {
                                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                        " - Error: algorithm not developed yet for the ocean number ",
                                                        ocean_activity,
                                                        ".\n",
                                                        "[trip: ",
                                                        current_trip$.__enclos_env__$private$trip_id,
                                                        ", activity: ",
                                                        current_trip$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$activity_id,
                                                        "]\n",
                                                        sep = "")
                                                    stop()
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                      # second stage: conversion of category unknow (category 9) if possible
                                      for (l in seq_len(length.out = length(private$data_selected[[i]]))) {
                                        current_trip <- private$data_selected[[i]][[l]]
                                        current_elementarycatches <- vector(mode = "list")
                                        if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                          for (x in seq_len(length.out = length(current_trip$.__enclos_env__$private$activities))) {
                                            current_elementarycatches <- append(current_elementarycatches,
                                                                                current_trip$.__enclos_env__$private$activities[[x]]$.__enclos_env__$private$elementarycatches)
                                          }
                                        }
                                        if (length(current_elementarycatches) != 0) {
                                          category_9 <- FALSE
                                          names(category_9) <- 0
                                          other_category <- FALSE
                                          names(other_category) <- 0
                                          for (n in seq_len(length.out = length(current_elementarycatches))) {
                                            if (current_elementarycatches[[n]]$.__enclos_env__$private$logbook_category == 9 & current_elementarycatches[[n]]$.__enclos_env__$private$specie_code3l %in% c("YFT", "BET", "ALB", "SKJ")) {
                                              category_9 <- append(category_9, TRUE)
                                              names(category_9)[length(category_9)] <- n
                                            } else {
                                              other_category <- append(other_category, TRUE)
                                              names(other_category)[length(other_category)] <- n
                                            }
                                          }
                                          if (any(category_9 == TRUE)) {
                                            if (any(other_category == TRUE)) {
                                              category_9 <- category_9[-1]
                                              strate_category_9 <- vector(mode = "character")
                                              for (m in as.numeric(names(category_9))) {
                                                strate_category_9 <- append(strate_category_9,
                                                                            paste(current_elementarycatches[[m]]$.__enclos_env__$private$school_type,
                                                                                  current_elementarycatches[[m]]$.__enclos_env__$private$ocean,
                                                                                  current_elementarycatches[[m]]$.__enclos_env__$private$specie_code3l, sep = "_"))
                                              }
                                              other_category <- other_category[-1]
                                              for (p in unique(strate_category_9)) {
                                                school_type <- unlist(strsplit(x = p, split = "_"))[1]
                                                ocean <- unlist(strsplit(x = p, split = "_"))[2]
                                                specie <- unlist(strsplit(x = p, split = "_"))[3]
                                                current_other_category <- vector(mode = "list")
                                                for (q in as.numeric(names(other_category))) {
                                                  if (current_elementarycatches[[q]]$.__enclos_env__$private$school_type == school_type &
                                                      current_elementarycatches[[q]]$.__enclos_env__$private$ocean == ocean &
                                                      current_elementarycatches[[q]]$.__enclos_env__$private$specie_code3l == specie) {
                                                    current_other_category <- append(current_other_category,
                                                                                     current_elementarycatches[[q]])
                                                  }
                                                }
                                                if (length(current_other_category) != 0) {
                                                  current_category_9 <- vector(mode = "list")
                                                  for (r in as.numeric(names(category_9))) {
                                                    if (current_elementarycatches[[r]]$.__enclos_env__$private$school_type == school_type &
                                                        current_elementarycatches[[r]]$.__enclos_env__$private$ocean == ocean &
                                                        current_elementarycatches[[r]]$.__enclos_env__$private$specie_code3l == specie) {
                                                      current_category_9 <- append(current_category_9,
                                                                                   current_elementarycatches[[r]])
                                                    }
                                                  }
                                                  total_catch_weight_category_corrected <- sum(sapply(seq_len(length.out = length(current_other_category)),
                                                                                                      function(i) {
                                                                                                        current_other_category[[i]]$.__enclos_env__$private$catch_weight_category_corrected
                                                                                                      }))
                                                  other_category_names <- unique(sapply(X = seq_len(length.out = length(current_other_category)),
                                                                                        FUN = function(i) {
                                                                                          current_other_category[[i]]$.__enclos_env__$private$corrected_logbook_category
                                                                                        }))
                                                  proportion <- vector(mode = "numeric")
                                                  for (s in other_category_names) {
                                                    weight_category_corrected <- sum(sapply(X = seq_len(length.out = length(current_other_category)),
                                                                                            FUN = function(i) {
                                                                                              if (current_other_category[[i]]$.__enclos_env__$private$corrected_logbook_category == s) {
                                                                                                current_other_category[[i]]$.__enclos_env__$private$catch_weight_category_corrected
                                                                                              } else {
                                                                                                0
                                                                                              }
                                                                                            }))
                                                    proportion <- append(proportion,
                                                                         weight_category_corrected / total_catch_weight_category_corrected)
                                                    names(proportion)[length(proportion)] <- s
                                                  }
                                                  for (t in seq_len(length.out = length(current_category_9))) {
                                                    for (u in seq_len(length.out = length(proportion))) {
                                                      if (u == length(proportion)) {
                                                        current_category_9[[t]]$.__enclos_env__$private$corrected_logbook_category <- names(proportion)[u]
                                                        current_category_9[[t]]$.__enclos_env__$private$catch_weight_category_corrected <- current_category_9[[t]]$.__enclos_env__$private$catch_weight_rf2 * as.numeric(proportion[u])
                                                      } else {
                                                        current_category_9_tmp <- current_category_9[[t]]$clone()
                                                        current_category_9_tmp$.__enclos_env__$private$corrected_logbook_category <- names(proportion)[u]
                                                        current_category_9_tmp$.__enclos_env__$private$catch_weight_category_corrected <- current_category_9_tmp$.__enclos_env__$private$catch_weight_rf2 * as.numeric(proportion[u])
                                                        for (y in seq_len(length.out = length(private$data_selected[[i]][[l]]$.__enclos_env__$private$activities))) {
                                                          if (private$data_selected[[i]][[l]]$.__enclos_env__$private$activities[[y]]$.__enclos_env__$private$activity_id == current_category_9_tmp$.__enclos_env__$private$activity_id) {
                                                            private$data_selected[[i]][[l]]$.__enclos_env__$private$activities[[y]]$.__enclos_env__$private$elementarycatches <- append(private$data_selected[[i]][[l]]$.__enclos_env__$private$activities[[y]]$.__enclos_env__$private$elementarycatches,
                                                                                                                                                                                        current_category_9_tmp)
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
                                    }
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Process 1.3 successfull on item ",
                                        i,
                                        ".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                  }
                                  if (i == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End process 1.3: logbook weight categories conversion.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 1.4: set_count ----
                            #' @description Process for postive sets count.
                            set_count = function() {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 1.4 (set count) cancelled.\n",
                                    sep = "")
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 1.4: set count.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    next()
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.4 on item ",
                                        i,
                                        ".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[j]]
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        for (k in seq_len(length.out = length(current_trip$.__enclos_env__$private$activities))) {
                                          if (current_trip$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$activity_code %in% c(0, 1, 2, 14)) {
                                            current_activity <- current_trip$.__enclos_env__$private$activities[[k]]
                                            current_elementarycatches <- current_activity$.__enclos_env__$private$elementarycatches
                                            if (length(current_elementarycatches) != 0) {
                                              catch_weight_category_corrected <- sum(sapply(X = seq_len(length.out = length(current_elementarycatches)),
                                                                                            FUN = function(l) {
                                                                                              if (is.null(current_elementarycatches[[l]]$.__enclos_env__$private$catch_weight_category_corrected)) {
                                                                                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                                                    " - Error: argument \"catch_weight_category_corrected\" is null.\n",
                                                                                                    "Check if the process 1.3 (logbook weight categories conversion) has already been launched.",
                                                                                                    "\n[trip: ",
                                                                                                    current_activity$.__enclos_env__$private$trip_id,
                                                                                                    ", activity: ",
                                                                                                    current_activity$.__enclos_env__$private$activity_id,
                                                                                                    "]\n",
                                                                                                    sep = "")
                                                                                                stop()
                                                                                              } else {
                                                                                                current_elementarycatches[[l]]$.__enclos_env__$private$catch_weight_category_corrected
                                                                                              }
                                                                                            }))
                                              if (catch_weight_category_corrected == 0) {
                                                current_activity$.__enclos_env__$private$positive_set_count <- 0
                                              } else {
                                                current_activity$.__enclos_env__$private$positive_set_count <- current_activity$.__enclos_env__$private$set_count
                                              }
                                            } else {
                                              current_activity$.__enclos_env__$private$positive_set_count <- 0
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Process 1.4 successfull on item ",
                                      i,
                                      ".\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (i == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End process 1.4: set count.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 1.5: set_duration ----
                            #' @description Process for set duration calculation (in hours).
                            #' @param set_duration_ref (data frame) Data and parameters for set duration calculation (by year, country, ocean and school type).
                            set_duration = function(set_duration_ref) {
                              if (length(class(set_duration_ref)) != 1 || class(set_duration_ref) != "data.frame" || dim(set_duration_ref)[2] != 7 || dim(set_duration_ref)[1] < 1) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"set_duration_ref\" argument, ",
                                    "class \"data.frame\" expected with 7 columns and at least 1 row.",
                                    sep = "")
                                stop()
                              }
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 1.5 (set duration calculation) cancelled.\n",
                                    sep = "")
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 1.5: set duration calculation.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    next()
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.5 on item ",
                                        i,
                                        ".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[j]]
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        for (k in seq_len(length.out = length(current_trip$.__enclos_env__$private$activities))) {
                                          if (current_trip$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$activity_code %in% c(0, 1, 2, 14)) {
                                            current_activity <- current_trip$.__enclos_env__$private$activities[[k]]
                                            if (current_activity$.__enclos_env__$private$activity_code %in% c(0, 2, 14)) {
                                              # for a set declared as null set (0), unknown set (2) or pocket capsizing (14)
                                              current_elementarycatches <- current_activity$.__enclos_env__$private$elementarycatches
                                              if (dim(set_duration_ref[set_duration_ref$year == lubridate::year(current_activity$.__enclos_env__$private$activity_date)
                                                                       & set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                       & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type, ])[1] != 1) {
                                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                    " - Error: invalid \"set_duration_ref\" argument.\n",
                                                    "No correspondance with activity parameters (ocean and/or school type).\n",
                                                    "[trip: ",
                                                    current_trip$.__enclos_env__$private$trip_id,
                                                    ", activity: ",
                                                    current_activity$.__enclos_env__$private$activity_id,
                                                    "]\n",
                                                    sep = "")
                                                stop()
                                              }
                                              if (length(current_elementarycatches) != 0) {
                                                catch_weight_category_corrected <- sum(sapply(X = seq_len(length.out = length(current_elementarycatches)),
                                                                                              FUN = function(l) {
                                                                                                if (is.null(current_elementarycatches[[l]]$.__enclos_env__$private$catch_weight_category_corrected)) {
                                                                                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                                                      " - Error: argument \"catch_weight_category_corrected\" is null.\n",
                                                                                                      "Check if the process 1.3 (logbook weight categories conversion) has already been launched.",
                                                                                                      "\n[trip: ",
                                                                                                      current_activity$.__enclos_env__$private$trip_id,
                                                                                                      ", activity: ",
                                                                                                      current_activity$.__enclos_env__$private$activity_id,
                                                                                                      ", elementarycatch: ",
                                                                                                      current_elementarycatches[[l]]$.__enclos_env__$private$elementarycatch_id,
                                                                                                      "]\n",
                                                                                                      sep = "")
                                                                                                  stop()
                                                                                                } else {
                                                                                                  current_elementarycatches[[l]]$.__enclos_env__$private$catch_weight_category_corrected
                                                                                                }
                                                                                              }))
                                                parameter_a <- set_duration_ref[set_duration_ref$year == lubridate::year(current_activity$.__enclos_env__$private$activity_date)
                                                                                & set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                                & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type, "parameter_a"]
                                                parameter_b <- set_duration_ref[set_duration_ref$year == lubridate::year(current_activity$.__enclos_env__$private$activity_date)
                                                                                & set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                                & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type, "parameter_b"]
                                                current_activity$.__enclos_env__$private$set_duration <- parameter_a * catch_weight_category_corrected + parameter_b
                                              } else {
                                                current_activity$.__enclos_env__$private$set_duration <- set_duration_ref[set_duration_ref$year == lubridate::year(current_activity$.__enclos_env__$private$activity_date)
                                                                                                                          & set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                                                                          & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type, "null_set_value"]
                                              }
                                            } else if (current_activity$.__enclos_env__$private$activity_code == 1) {
                                              # for a set declared as positive (1)
                                              current_elementarycatches <- current_activity$.__enclos_env__$private$elementarycatches
                                              if (dim(set_duration_ref[set_duration_ref$year == lubridate::year(current_activity$.__enclos_env__$private$activity_date)
                                                                       & set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                       & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type, ])[1] != 1) {
                                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                    " - Error: invalid \"set_duration_ref\" argument.\n",
                                                    "No correspondance with activity parameters (ocean and/or school type).\n",
                                                    "[trip: ",
                                                    current_trip$.__enclos_env__$private$trip_id,
                                                    ", activity: ",
                                                    current_activity$.__enclos_env__$private$activity_id,
                                                    "]\n",
                                                    sep = "")
                                                stop()
                                              } else {
                                                if (length(current_elementarycatches) != 0) {
                                                  catch_weight_category_corrected <- sum(sapply(X = seq_len(length.out = length(current_elementarycatches)),
                                                                                                FUN = function(l) {
                                                                                                  if (is.null(current_elementarycatches[[l]]$.__enclos_env__$private$catch_weight_category_corrected)) {
                                                                                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                                                        " - Error: argument \"catch_weight_category_corrected\" is null.\n",
                                                                                                        "Check if the process 1.3 (logbook weight categories conversion) has already been launched.",
                                                                                                        "\n[trip: ",
                                                                                                        current_activity$.__enclos_env__$private$trip_id,
                                                                                                        ", activity: ",
                                                                                                        current_activity$.__enclos_env__$private$activity_id,
                                                                                                        ", elementarycatch: ",
                                                                                                        current_elementarycatches[[l]]$.__enclos_env__$private$elementarycatch_id,
                                                                                                        "]\n",
                                                                                                        sep = "")
                                                                                                    stop()
                                                                                                  } else {
                                                                                                    current_elementarycatches[[l]]$.__enclos_env__$private$catch_weight_category_corrected
                                                                                                  }
                                                                                                }))
                                                  parameter_a <- set_duration_ref[set_duration_ref$year == lubridate::year(current_activity$.__enclos_env__$private$activity_date)
                                                                                  & set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                                  & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type, "parameter_a"]
                                                  parameter_b <- set_duration_ref[set_duration_ref$year == lubridate::year(current_activity$.__enclos_env__$private$activity_date)
                                                                                  & set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                                  & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type, "parameter_b"]
                                                  current_activity$.__enclos_env__$private$set_duration <- parameter_a * catch_weight_category_corrected + parameter_b
                                                } else {
                                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                      " - Error: set declared as positive but without elementary catch.",
                                                      "\n[trip: ",
                                                      current_trip$.__enclos_env__$private$trip_id,
                                                      ", activity: ",
                                                      current_activity$.__enclos_env__$private$activity_id,
                                                      "]\n",
                                                      sep = "")
                                                  stop()
                                                }
                                              }
                                            } else {
                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                  " - Error: algorithms not developed yet for the activity code ",
                                                  current_activity$.__enclos_env__$private$activity_code,
                                                  ".\n",
                                                  "[trip: ",
                                                  current_trip$.__enclos_env__$private$trip_id,
                                                  ", activity: ",
                                                  current_activity$.__enclos_env__$private$activity_id,
                                                  "]\n",
                                                  sep = "")
                                              stop()
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Process 1.5 successfull on item ",
                                      i,
                                      ".\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (i == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End process 1.5: set duration calculation\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 1.6: time at sea ----
                            #' @description Process for time at sea calculation (in hours).
                            time_at_sea = function() {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 1.6 (set duration calculation) cancelled.\n",
                                    sep = "")
                              }
                              for (i in seq_len(length.out = length(private$data_selected))) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start process 1.6: time at sea calculation.\n",
                                      sep = "")
                                }
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  next()
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 1.6 on item ",
                                      i,
                                      ".\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    departure_date <- current_trip$.__enclos_env__$private$departure_date
                                    landing_date <- current_trip$.__enclos_env__$private$landing_date
                                    time_departure_date <- lubridate::hms(format(departure_date, format = "%H:%M:%S"))
                                    time_landing_date <- lubridate::hms(format(landing_date, format = "%H:%M:%S"))
                                    if (time_departure_date > lubridate::dseconds(x = 0) & time_landing_date > lubridate::dseconds(x = 0)) {
                                      # we have time for departure_date and landing_date
                                      time_at_sea <- lubridate::int_length(lubridate::interval(start = departure_date, end = landing_date)) / 3600
                                    } else {
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        current_activities_departure_date <- vector(mode = "list")
                                        current_activities_landing_date <- vector(mode = "list")
                                        for (k in seq_len(length.out = length(current_trip$.__enclos_env__$private$activities))) {
                                          if (current_trip$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$activity_date == departure_date) {
                                            current_activities_departure_date <- append(current_activities_departure_date,
                                                                                        current_trip$.__enclos_env__$private$activities[[k]])
                                          } else if (current_trip$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$activity_date == landing_date) {
                                            current_activities_landing_date <- append(current_activities_landing_date,
                                                                                      current_trip$.__enclos_env__$private$activities[[k]])
                                          }
                                        }
                                        current_activities_departure_date_time_at_sea <- 0
                                        current_activities_landing_date_time_at_sea <- 0
                                        if (length(current_activities_departure_date) != 0) {
                                          for (l in seq_len(length.out = length(current_activities_departure_date))) {
                                            current_activities_departure_date_time_at_sea <- current_activities_departure_date_time_at_sea + current_activities_departure_date[[l]]$.__enclos_env__$private$time_at_sea
                                          }
                                        }
                                        if (length(current_activities_landing_date) != 0) {
                                          for (m in seq_len(length.out = length(current_activities_landing_date))) {
                                            current_activities_landing_date_time_at_sea <- current_activities_landing_date_time_at_sea + current_activities_landing_date[[m]]$.__enclos_env__$private$time_at_sea
                                          }
                                        }
                                        time_at_sea <- lubridate::int_length(lubridate::interval(start = departure_date + lubridate::days(x = 1),
                                                                                                 end = landing_date - lubridate::days(x = 1))
                                                                             ) / 3600
                                        time_at_sea <- time_at_sea + current_activities_departure_date_time_at_sea + current_activities_landing_date_time_at_sea
                                      } else {
                                        time_at_sea <- lubridate::int_length(lubridate::interval(start = departure_date + lubridate::days(x = 1),
                                                                                                 end = landing_date - lubridate::days(x = 1))
                                        ) / 3600
                                      }
                                    }
                                    current_trip$.__enclos_env__$private$time_at_sea <- time_at_sea
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Process 1.6 successfull on item ",
                                    i,
                                    ".\n",
                                    "[trip: ",
                                    private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                    "]\n",
                                    sep = "")
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End process 1.6: time at sea calculation.\n",
                                      sep = "")
                                }
                              }
                            },
                            # process 1.7: fishing_time ----
                            #' @description Process for fishing time calculation (in hours).
                            #' @param sunrise_schema (character) Sunrise caracteristic. By default "sunrise" (top edge of the sun appears on the horizon). See below for more details.
                            #' @param sunset_schema (character) Sunset caracteristic. By default "sunset" (sun disappears below the horizon, evening civil twilight starts). See below for more details.
                            #' @details
                            #' Available variables are:
                            #' \itemize{
                            #'  \item{"sunrise"}{sunrise (top edge of the sun appears on the horizon)}
                            #'  \item{"sunriseEnd"}{sunrise ends (bottom edge of the sun touches the horizon)}
                            #'  \item{"goldenHourEnd"}{morning golden hour (soft light, best time for photography) ends}
                            #'  \item{"solarNoon"}{solar noon (sun is in the highest position)}
                            #'  \item{"goldenHour"}{evening golden hour starts}
                            #'  \item{"sunsetStart"}{sunset starts (bottom edge of the sun touches the horizon)}
                            #'  \item{"sunset"}{sunset (sun disappears below the horizon, evening civil twilight starts)}
                            #'  \item{"dusk"}{dusk (evening nautical twilight starts)}
                            #'  \item{"nauticalDusk"}{nautical dusk (evening astronomical twilight starts)}
                            #'  \item{"night"}{night starts (dark enough for astronomical observations)}
                            #'  \item{"nadir"}{nadir (darkest moment of the night, sun is in the lowest position)}
                            #'  \item{"nightEnd"}{night ends (morning astronomical twilight starts)}
                            #'  \item{"nauticalDawn"}{nautical dawn (morning nautical twilight starts)}
                            #'  \item{"dawn"}{dawn (morning nautical twilight ends, morning civil twilight starts)}
                            #' }
                            fishing_time = function(sunrise_schema = "sunrise",
                                                    sunset_schema = "sunset") {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 1.7 (fishing time calculation) cancelled.\n",
                                    sep = "")
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 1.7: fishing time calculation.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    next()
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.7 on item ",
                                        i,
                                        ".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[j]]
                                      fishing_time <- 0
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        activities_dates <- vector(mode = "list")
                                        for (k in seq_len(length.out = length(current_trip$.__enclos_env__$private$activities))) {
                                          current_activity_date <- current_trip$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$activity_date
                                          activities_dates <- append(activities_dates,
                                                                     current_activity_date)
                                        }
                                        activities_dates <- sort(x = unique(lubridate::date(activities_dates)))
                                        for (l in activities_dates) {
                                          fishing_time_tmp <- 0
                                          current_activities_code <- unique(sapply(X = seq_len(length.out = length(current_trip$.__enclos_env__$private$activities)),
                                                                                   FUN = function(m) {
                                                                                     if (current_trip$.__enclos_env__$private$activities[[m]]$.__enclos_env__$private$activity_date == l) {
                                                                                       current_trip$.__enclos_env__$private$activities[[m]]$.__enclos_env__$private$activity_code
                                                                                     } else {
                                                                                       NA
                                                                                     }
                                                                                   }))
                                          current_activities_code <- current_activities_code[!is.na(current_activities_code)]
                                          if (any(! current_activities_code %in% c(4, 7, 10, 15, 100))) {
                                            current_activities_location <- unique(sapply(X = seq_len(length.out = length(current_trip$.__enclos_env__$private$activities)),
                                                                                         FUN = function(n) {
                                                                                           if (current_trip$.__enclos_env__$private$activities[[n]]$.__enclos_env__$private$activity_date == l) {
                                                                                             paste(current_trip$.__enclos_env__$private$activities[[n]]$.__enclos_env__$private$activity_latitude,
                                                                                                   current_trip$.__enclos_env__$private$activities[[n]]$.__enclos_env__$private$activity_longitude,
                                                                                                   sep = "_")
                                                                                           } else {
                                                                                             NA
                                                                                           }
                                                                                         }))
                                            current_activities_location <- current_activities_location[!is.na(current_activities_location)]
                                            latitude_mean <- mean(sapply(X = seq_len(length.out = length(current_activities_location)),
                                                                         FUN = function(o) {
                                                                           as.numeric(unlist(strsplit(current_activities_location[o],
                                                                                                      "_"))[1])
                                                                         }))
                                            longitude_mean <- mean(sapply(X = seq_len(length.out = length(current_activities_location)),
                                                                          FUN = function(o) {
                                                                            as.numeric(unlist(strsplit(current_activities_location[o],
                                                                                                       "_"))[2])
                                                                          }))
                                            current_sunrise <- suncalc::getSunlightTimes(date = lubridate::as_date(l),
                                                                                         lat = latitude_mean,
                                                                                         lon = longitude_mean)[[sunrise_schema]]
                                            current_sunset <- suncalc::getSunlightTimes(date = lubridate::as_date(l),
                                                                                        lat = latitude_mean,
                                                                                        lon = longitude_mean)[[sunset_schema]]
                                            fishing_time_tmp <- lubridate::int_length(lubridate::interval(start = current_sunrise,
                                                                                                          end = current_sunset)) / 3600
                                            fishing_time <- fishing_time + fishing_time_tmp
                                          }
                                        }
                                      }
                                      current_trip$.__enclos_env__$private$fishing_time <- fishing_time
                                    }
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Process 1.7 successfull on item ",
                                      i,
                                      ".\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (i == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End process 1.7: fishing time calculation.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 1.8: searching_time ----
                            #' @description Process for searching time calculation (in hours, fishing time minus sets durations).
                            searching_time = function() {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 1.8 (fishing time calculation) cancelled.\n",
                                    sep = "")
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 1.8: searching time calculation.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    next()
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.8 on item ",
                                        i,
                                        ".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[j]]
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        activities_set_duration <- sum(sapply(X = seq_len(length.out = length(current_trip$.__enclos_env__$private$activities)),
                                                                              FUN = function(k) {
                                                                                if (! is.null(current_trip$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$set_duration)) {
                                                                                  current_trip$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$set_duration
                                                                                } else {
                                                                                  0
                                                                                }
                                                                              })) / 60
                                        if (is.null(current_trip$.__enclos_env__$private$fishing_time)) {
                                          cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                              " - Error: run process 1. 7 (fishing time calculation) before this process.\n",
                                              sep = "")
                                          stop()
                                        } else {
                                          searching_time <- current_trip$.__enclos_env__$private$fishing_time - activities_set_duration
                                        }
                                      } else {
                                        searching_time <- 0
                                      }
                                      current_trip$.__enclos_env__$private$searching_time <- searching_time
                                    }
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Process 1.8 successfull on item ",
                                      i,
                                      ".\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (i == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End process 1.8: searching time calculation.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 2.1: sample_number_measured_extrapolation ----
                            #' @description Process for sample number measured individuals extrapolation to sample number individuals counted.
                            sample_number_measured_extrapolation = function() {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 2.1 (sample number measured extrapolation) cancelled.\n",
                                    sep = "")
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.1: sample number measured extrapolation.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    next()
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.1 on item ",
                                        i,
                                        ".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[j]]
                                      current_wells <- current_trip$.__enclos_env__$private$wells
                                      if (length(current_wells) != 0) {
                                        for (q in seq_len(length.out = length(current_wells))) {
                                          current_samples <- current_wells[[q]]$.__enclos_env__$private$elementarysampleraw
                                          if (length(current_samples) != 0) {
                                            for (k in seq_len(length.out = length(current_samples))) {
                                              current_sample <- current_samples[[k]]
                                              sub_sample_id <- unique(sapply(X = seq_len(length.out = length(current_sample)),
                                                                             FUN = function(a) {
                                                                               current_sample[[a]]$.__enclos_env__$private$sub_sample_id
                                                                             }))
                                              for (b in sub_sample_id) {
                                                current_sub_sample <- Filter(Negate(is.null),
                                                                             lapply(X = seq_len(length.out = length(current_sample)),
                                                                                    FUN = function(c) {
                                                                                      if (current_sample[[c]]$.__enclos_env__$private$sub_sample_id == b) {
                                                                                        current_sample[[c]]
                                                                                      }
                                                                                    }))
                                                sample_specie <- vector(mode = "integer")
                                                for (l in seq_len(length.out = length(current_sub_sample))) {
                                                  sample_specie <- append(sample_specie,
                                                                          current_sub_sample[[l]]$.__enclos_env__$private$specie_code)
                                                }
                                                sample_specie <- unique(sample_specie)
                                                for (m in sample_specie) {
                                                  sum_sub_sample_number_measured <- 0
                                                  sub_sample_total_count_tmp <- vector(mode = "character")
                                                  current_sub_sample_tmp <- vector(mode = "list")
                                                  for (n in seq_len(length.out = length(current_sub_sample))) {
                                                    if (current_sub_sample[[n]]$.__enclos_env__$private$specie_code == m) {
                                                      sum_sub_sample_number_measured <- sum_sub_sample_number_measured + current_sub_sample[[n]]$.__enclos_env__$private$sample_number_measured
                                                      sub_sample_total_count_tmp <- append(sub_sample_total_count_tmp,
                                                                                       paste(current_sub_sample[[n]]$.__enclos_env__$private$sub_sample_id,
                                                                                             current_sub_sample[[n]]$.__enclos_env__$private$length_type,
                                                                                             current_sub_sample[[n]]$.__enclos_env__$private$sample_total_count,
                                                                                             sep = "_"))
                                                      current_sub_sample_tmp <- append(current_sub_sample_tmp,
                                                                                       current_sub_sample[[n]])
                                                    }
                                                  }
                                                  sub_sample_total_count_tmp <- unique(sub_sample_total_count_tmp)
                                                  sum_sample_total_count <- sum(sapply(X = seq_len(length.out = length(sub_sample_total_count_tmp)),
                                                                                       FUN = function(o) {
                                                                                         as.numeric(unlist(strsplit(sub_sample_total_count_tmp[o],
                                                                                                                    "_"))[3])
                                                                                       }))
                                                  rf4 <- sum_sample_total_count / sum_sub_sample_number_measured
                                                  # rf4 verification ----
                                                  if (rf4 != 1 & (! m %in% c(2))) {
                                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                        " - Warning: rf4 not egal to 1 (",
                                                        rf4,
                                                        ") for sampled specie different from SKJ.\n",
                                                        "[trip: ",
                                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                                        ", sample: ",
                                                        current_sub_sample[[1]]$.__enclos_env__$private$sample_id,
                                                        "]\n",
                                                        sep = "")
                                                  }
                                                  if (rf4 < 1) {
                                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                        " - Warning: rf4 inferior to 1 (",
                                                        rf4,
                                                        ").\n",
                                                        "[trip: ",
                                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                                        ", sample: ",
                                                        current_sub_sample[[1]]$.__enclos_env__$private$sample_id,
                                                        "]\n",
                                                        sep = "")
                                                  }
                                                  for (p in seq_len(length.out = length(current_sub_sample_tmp))) {
                                                    current_sub_sample_tmp[[p]]$.__enclos_env__$private$rf4 <- rf4
                                                    current_sub_sample_tmp[[p]]$.__enclos_env__$private$sample_number_measured_extrapolated <- current_sub_sample_tmp[[p]]$.__enclos_env__$private$sample_number_measured * rf4
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
                                      " - Process 2.1 successfull on item ",
                                      i,
                                      ".\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (i == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End process 2.1: sample number measured extrapolation.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 2.2: sample_length_class_ld1_to_lf ----
                            #' @description Process for length conversion, if necessary, in length fork (lf). Furthermore, variable "sample_number_measured_extrapolated" of process 2.1 will converse in variable "sample_number_measured_extrapolated_lf" (Notably due to the creation of new lf classes during some conversions).
                            #' @param length_step (data.frame) Data frame object with length ratio between ld1 and lf class.
                            sample_length_class_ld1_to_lf =  function(length_step) {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 2.2 (sample length class conversion ld1 to lf) cancelled.\n",
                                    sep = "")
                              } else {
                                if (class(length_step) != "data.frame" || ncol(length_step) != 6 || nrow(length_step) == 0) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - invalid \"length_step\" argument, class \"data.frame\" with 6 columns and at least 1 row expected.\n",
                                      sep = "")
                                } else {
                                  length_step_count <- length_step %>%
                                    dplyr::group_by(ocean, specie_code, specie_code3l, ld1_class) %>%
                                    dplyr::summarise(nb = n())
                                }
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.2: sample length class conversion ld1 to lf.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    next()
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.2 on item ",
                                        i,
                                        ".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[j]]
                                      current_wells <- current_trip$.__enclos_env__$private$wells
                                      if (length(current_wells) != 0) {
                                        for (k in seq_len(length.out = length(current_wells))) {
                                          current_samples <- current_wells[[k]]$.__enclos_env__$private$elementarysampleraw
                                          if (length(current_samples) != 0) {
                                            for (l in seq_len(length.out = length(current_samples))) {
                                              current_sample <- current_samples[[l]]
                                              for (m in seq_len(length.out = length(current_sample))) {
                                                current_elementary_sample <- current_sample[[m]]
                                                if (current_elementary_sample$.__enclos_env__$private$length_type == 2) {
                                                  current_elementary_sample$.__enclos_env__$private$sample_length_class_lf <- current_elementary_sample$.__enclos_env__$private$sample_length_class
                                                  current_elementary_sample$.__enclos_env__$private$sample_number_measured_extrapolated_lf <- current_elementary_sample$.__enclos_env__$private$sample_number_measured_extrapolated
                                                } else {
                                                  current_activities <- current_trip$.__enclos_env__$private$activities
                                                  if (length(current_activities) != 0) {
                                                    ocean_activities <- vector(mode = "integer")
                                                    for (n in seq_len(length.out = length(current_activities))) {
                                                      ocean_activities <- append(ocean_activities,
                                                                                 current_activities[[n]]$.__enclos_env__$private$ocean)
                                                    }
                                                    ocean_activities <- unique(ocean_activities)
                                                    if (length(ocean_activities) != 1) {
                                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                          " - Error: activites associated to sample in more than one ocean.\n",
                                                          "[trip_id: ",
                                                          current_elementary_sample$.__enclos_env__$private$trip_id,
                                                          ", well_id: ",
                                                          current_elementary_sample$.__enclos_env__$private$well_id,
                                                          ", sample_id: ",
                                                          current_elementary_sample$.__enclos_env__$private$sample_id,
                                                          "]\n",
                                                          sep = "")
                                                      stop()
                                                    } else {
                                                      current_length_step_count <- as.numeric(length_step_count[length_step_count$ocean == ocean_activities
                                                                                                                & length_step_count$specie_code == current_elementary_sample$.__enclos_env__$private$specie_code
                                                                                                                & length_step_count$ld1_class == current_elementary_sample$.__enclos_env__$private$sample_length_class, "nb"])
                                                      if (is.na(current_length_step_count)) {
                                                        cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                            " - Error: no correspondance between sample length class and ld1-lf reference table.\n",
                                                            "[trip_id: ",
                                                            current_elementary_sample$.__enclos_env__$private$trip_id,
                                                            ", well_id: ",
                                                            current_elementary_sample$.__enclos_env__$private$well_id,
                                                            ", sample_id: ",
                                                            current_elementary_sample$.__enclos_env__$private$sample_id,
                                                            "]\n",
                                                            sep = "")
                                                        stop()
                                                      } else {
                                                        current_length_step <- length_step[length_step$ocean == ocean_activities
                                                                                           & length_step$specie_code == current_elementary_sample$.__enclos_env__$private$specie_code
                                                                                           & length_step$ld1_class == current_elementary_sample$.__enclos_env__$private$sample_length_class, ]
                                                        current_elementary_sample_tmp <- vector(mode = "list")
                                                        for (o in seq_len(length.out = current_length_step_count)) {
                                                          if (o == current_length_step_count) {
                                                            current_elementary_sample$.__enclos_env__$private$length_type <- 2
                                                            current_elementary_sample$.__enclos_env__$private$sample_length_class_lf <- current_length_step[o, "lf_class"]
                                                            current_elementary_sample$.__enclos_env__$private$sample_number_measured_extrapolated_lf <- current_length_step[o, "ratio"] * 10^-2 * current_elementary_sample$.__enclos_env__$private$sample_number_measured_extrapolated
                                                          } else {
                                                            current_elementary_sample_tmpbis <- current_elementary_sample$clone()
                                                            current_elementary_sample_tmpbis$.__enclos_env__$private$length_type <- 2
                                                            current_elementary_sample_tmpbis$.__enclos_env__$private$sample_length_class_lf <- current_length_step[o, "lf_class"]
                                                            current_elementary_sample_tmpbis$.__enclos_env__$private$sample_number_measured_extrapolated_lf <- current_length_step[o, "ratio"] * 10^-2 * current_elementary_sample_tmpbis$.__enclos_env__$private$sample_number_measured_extrapolated
                                                            current_elementary_sample_tmp <- append(current_elementary_sample_tmp,
                                                                                                    current_elementary_sample_tmpbis)
                                                            if (o == current_length_step_count - 1) {
                                                              private$data_selected[[i]][[j]]$.__enclos_env__$private$wells[[k]]$.__enclos_env__$private$elementarysampleraw[[l]] <- append(private$data_selected[[i]][[j]]$.__enclos_env__$private$wells[[k]]$.__enclos_env__$private$elementarysampleraw[[l]],
                                                                                                                                                                                            current_elementary_sample_tmp)
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  } else {
                                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                        " - Error: sample detected without any activity associated.\n",
                                                        "[trip_id: ",
                                                        current_elementary_sample$.__enclos_env__$private$trip_id,
                                                        ", well_id: ",
                                                        current_elementary_sample$.__enclos_env__$private$well_id,
                                                        ", sample_id: ",
                                                        current_elementary_sample$.__enclos_env__$private$sample_id,
                                                        "]\n",
                                                        sep = "")
                                                    stop()
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
                                      " - Process 2.2 successfull on item ",
                                      i,
                                      ".\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (i == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End process 2.2 sample length class conversion ld1 to lf.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 2.3: sample_length_class_step_standardisation ----
                            #' @description Process for step standardisation of lf length class.
                            #' @param maximum_lf_class (integer) Theorical maximum lf class that can occur (all species considerated). By default 500.
                            sample_length_class_step_standardisation = function(maximum_lf_class = as.integer(500)) {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 2.3 (sample length class step standardisation) cancelled.\n",
                                    sep = "")
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.3: sample length class step standardisation.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                    # full trip is not complet (missing at least one trip)
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    next()
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.3 on item ",
                                        i,
                                        ".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[j]]
                                      current_wells <- current_trip$.__enclos_env__$private$wells
                                      if (length(current_wells) != 0) {
                                        for (k in seq_len(length.out = length(current_wells))) {
                                          current_well <- current_wells[[k]]
                                          current_samples <- current_well$.__enclos_env__$private$elementarysampleraw
                                          current_well$.__enclos_env__$private$elementarysample <- vector(mode = "list",
                                                                                                          length = length(current_samples))
                                          for (l in seq_len(length.out = length(current_samples))) {
                                            current_sample <- current_samples[[l]]
                                            sample_species <- unique(sapply(X = seq_len(length.out = length(current_sample)),
                                                                            FUN = function(m) {
                                                                              current_sample[[m]]$.__enclos_env__$private$specie_code
                                                                            }))
                                            current_sample_by_species <- vector(mode = "list", length = length(sample_species))
                                            for (n in seq_len(length.out = length(current_sample))) {
                                              for (o in seq_len(length.out = length(sample_species))) {
                                                if (current_sample[[n]]$.__enclos_env__$private$specie_code == sample_species[o]) {
                                                  current_sample_by_species[[o]] <- append(current_sample_by_species[[o]],
                                                                                           current_sample[[n]])
                                                }
                                              }
                                            }
                                            current_sample_standardised <- vector(mode = "list")
                                            for (p in seq_len(length.out = length(current_sample_by_species))) {
                                              current_sample_specie <- current_sample_by_species[[p]]
                                              sample_length_class_lf <- sort(unique(sapply(X = seq_len(length.out = length(current_sample_specie)),
                                                                                           FUN = function(q) {
                                                                                             current_sample_specie[[q]]$.__enclos_env__$private$sample_length_class_lf
                                                                                           })))
                                              if (current_sample_specie[[1]]$.__enclos_env__$private$specie_code3l %in% c("SKJ", "LTA", "FRI")) {
                                                # for step of 1
                                                step <- 1
                                              } else if (current_sample_specie[[1]]$.__enclos_env__$private$specie_code3l %in% c("YFT", "BET", "ALB")) {
                                                # for step of 2
                                                step <- 2
                                              } else {
                                                step <- NA
                                              }
                                              if (is.na(step)) {
                                                for (r in seq_len(length.out = length(current_sample_specie))) {
                                                  object_elementarysample <- elementarysample$new(trip_id = current_sample_specie[[r]]$.__enclos_env__$private$trip_id,
                                                                                                  well_id = current_sample_specie[[r]]$.__enclos_env__$private$well_id,
                                                                                                  sample_id = current_sample_specie[[r]]$.__enclos_env__$private$sample_id,
                                                                                                  sub_sample_id = current_sample_specie[[r]]$.__enclos_env__$private$sub_sample_id,
                                                                                                  sample_quality = current_sample_specie[[r]]$.__enclos_env__$private$sample_quality,
                                                                                                  sample_type = current_sample_specie[[r]]$.__enclos_env__$private$sample_type,
                                                                                                  specie_code = current_sample_specie[[r]]$.__enclos_env__$private$specie_code,
                                                                                                  specie_code3l = current_sample_specie[[r]]$.__enclos_env__$private$specie_code3l,
                                                                                                  sample_standardised_length_class_lf = current_sample_specie[[r]]$.__enclos_env__$private$sample_length_class_lf,
                                                                                                  sample_number_measured_extrapolated_lf = as.numeric(current_sample_specie[[r]]$.__enclos_env__$private$sample_number_measured_extrapolated_lf),
                                                                                                  sample_total_count = as.integer(current_sample_specie[[r]]$.__enclos_env__$private$sample_total_count),
                                                                                                  elementarysampleraw = current_sample_specie[[r]])
                                                  current_well$.__enclos_env__$private$elementarysample[[l]] <- append(current_well$.__enclos_env__$private$elementarysample[[l]],
                                                                                                                  object_elementarysample)
                                                }
                                              } else {
                                                lower_border_reference <- seq(from = 0, to = maximum_lf_class - 1, by = step)
                                                upper_border_reference <- seq(from = step, to = maximum_lf_class, by = step)
                                                q <- 1
                                                while(q <= length(sample_length_class_lf)) {
                                                  lower_border <- as.integer(dplyr::last(x = lower_border_reference[which(lower_border_reference <= trunc(sample_length_class_lf[q]))]))
                                                  upper_border <- as.integer(dplyr::first(x = upper_border_reference[which(upper_border_reference > trunc(sample_length_class_lf[q]))]))
                                                  sample_length_class_lf_for_merge <- sample_length_class_lf[which(sample_length_class_lf >= lower_border
                                                                                                                   & sample_length_class_lf < upper_border)]
                                                  current_sample_specie_by_step <- unlist(lapply(X = seq_len(length.out = length(current_sample_specie)),
                                                                                                 FUN = function(s) {
                                                                                                   if (current_sample_specie[[s]]$.__enclos_env__$private$sample_length_class_lf %in% sample_length_class_lf_for_merge) {
                                                                                                     current_sample_specie[[s]]
                                                                                                   }
                                                                                                 }))
                                                  current_sample_specie_by_step_subid <- unique(sapply(X = seq_len(length.out = length(current_sample_specie_by_step)),
                                                                                                       FUN = function(t) {
                                                                                                         current_sample_specie_by_step[[t]]$.__enclos_env__$private$sub_sample_id
                                                                                                       }))
                                                  for (u in current_sample_specie_by_step_subid) {
                                                    current_sample_specie_by_step_by_subid <- unlist(lapply(X = seq_len(length.out = length(current_sample_specie_by_step)),
                                                                                                            FUN = function(v) {
                                                                                                              if (current_sample_specie_by_step[[v]]$.__enclos_env__$private$sub_sample_id == u) {
                                                                                                                current_sample_specie_by_step[[v]]
                                                                                                              }
                                                                                                            }))
                                                    object_elementarysample <- elementarysample$new(trip_id = current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$trip_id,
                                                                                                    well_id = current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$well_id,
                                                                                                    sample_id = current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$sample_id,
                                                                                                    sub_sample_id = current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$sub_sample_id,
                                                                                                    sample_quality = current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$sample_quality,
                                                                                                    sample_type = current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$sample_type,
                                                                                                    specie_code = current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$specie_code,
                                                                                                    specie_code3l = current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$specie_code3l,
                                                                                                    sample_standardised_length_class_lf = lower_border,
                                                                                                    sample_number_measured_extrapolated_lf = sum(sapply(X = seq_len(length.out = length(current_sample_specie_by_step_by_subid)),
                                                                                                                                                        FUN = function(w) {
                                                                                                                                                          current_sample_specie_by_step_by_subid[[w]]$.__enclos_env__$private$sample_number_measured_extrapolated_lf
                                                                                                                                                        })),
                                                                                                    sample_total_count = as.integer(current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$sample_total_count),
                                                                                                    elementarysampleraw = current_sample_specie_by_step_by_subid)
                                                    current_well$.__enclos_env__$private$elementarysample[[l]] <- append(current_well$.__enclos_env__$private$elementarysample[[l]],
                                                                                                                         object_elementarysample)
                                                  }
                                                  q <- q + length(sample_length_class_lf_for_merge)
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Process 2.3 successfull on item ",
                                      i,
                                      ".\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (i == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End process 2.3: sample length class step standardisation.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 2.4: well_set_weigth_categories ----
                            #' @description Process for well set weigth categories definition.
                            #' @param sample_set (data.frame) Data frame object with weighted weigth of each set sampled.
                            well_set_weigth_categories = function(sample_set) {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 2.4 (well-set weight categories definition) cancelled.\n",
                                    sep = "")
                              } else {
                                if (class(sample_set) != "data.frame" || ncol(sample_set) != 5 || nrow(sample_set) == 0) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - invalid \"sample_set\" argument, class \"data.frame\" with 5 columns and at least 1 row expected.\n",
                                      sep = "")
                                } else {
                                  for (i in seq_len(length.out = length(private$data_selected))) {
                                    if (i == 1) {
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - Start process 2.4: well-set weight categories definition.\n",
                                          sep = "")
                                    }
                                    if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                          "[trip: ",
                                          private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                          "]\n",
                                          sep = "")
                                      next()
                                    } else {
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - Ongoing process 2.4 on item ",
                                          i,
                                          ".\n",
                                          "[trip: ",
                                          private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                          "]\n",
                                          sep = "")
                                      for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                        current_trip <- private$data_selected[[i]][[j]]
                                        current_wells <- current_trip$.__enclos_env__$private$wells
                                        if (length(current_wells) != 0) {
                                          for (k in seq_len(length.out = length(current_wells))) {
                                            current_well <- current_wells[[k]]
                                            if (current_well$.__enclos_env__$private$well_minus10_weigth != 0 || current_well$.__enclos_env__$private$well_plus10_weigth != 0) {
                                              # case 1: we use proportion of -10/+10 in the well
                                              proportion_verification <- 1
                                            } else if (current_well$.__enclos_env__$private$well_global_weigth != 0) {
                                              # case 2: we don't know the proportion of -10/+10 in the well, we use global weight
                                              proportion_verification <- 2
                                            } else {
                                              # case 3: damn we know nothing... sample(s) in the well is/are not useable
                                              proportion_verification <- 3
                                            }
                                            current_well$.__enclos_env__$private$proportion_verification <- proportion_verification
                                            if (proportion_verification %in% c(1, 2)) {
                                              if (proportion_verification == 1) {
                                                current_well$.__enclos_env__$private$well_prop_minus10_weigth <- current_well$.__enclos_env__$private$well_minus10_weigth / (current_well$.__enclos_env__$private$well_minus10_weigth + current_well$.__enclos_env__$private$well_plus10_weigth)
                                                current_well$.__enclos_env__$private$well_prop_plus10_weigth <- current_well$.__enclos_env__$private$well_plus10_weigth / (current_well$.__enclos_env__$private$well_minus10_weigth + current_well$.__enclos_env__$private$well_plus10_weigth)
                                              } else {
                                                current_well$.__enclos_env__$private$well_prop_minus10_weigth <- NA
                                                current_well$.__enclos_env__$private$well_prop_plus10_weigth <- NA
                                              }
                                              if (is.na(current_well$.__enclos_env__$private$well_id)) {
                                                # for now, if a well_id is na, you can only have one sample inside (if more than 1, the well is avoid in model incrementation, check "R6 object wells creation")
                                                sample_set_well <- dplyr::filter(.data = sample_set,
                                                                                 sample_id == current_well$.__enclos_env__$private$elementarysample[[1]][[1]]$.__enclos_env__$private$sample_id)
                                              } else {
                                                sample_set_well <- dplyr::filter(.data = sample_set,
                                                                                 well_id == current_well$.__enclos_env__$private$well_id)
                                              }
                                              if (length(unique(sample_set_well$sample_id)) != 1) {
                                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                    " - Warning: ",
                                                    length(unique(sample_set_well$sample_id)),
                                                    " samples detected in a single well.\n",
                                                    " Only the first sample well set weighted weight will be considerated.\n",
                                                    "[trip: ",
                                                    current_well$.__enclos_env__$private$trip_id,
                                                    ", well: ",
                                                    current_well$.__enclos_env__$private$well_id,
                                                    ", samples: ",
                                                    paste0(unique(sample_set_well$sample_id),
                                                           collapse = " - "),
                                                    "]\n",
                                                    sep = "")
                                                sample_set_well <- dplyr::filter(.data = sample_set_well,
                                                                                 sample_id == unique(sample_set_well$sample_id)[[1]])
                                              }
                                              current_well$.__enclos_env__$private$wellsets <- lapply(X = seq_len(length.out = nrow(x = sample_set_well)),
                                                                                                      FUN = function(l) {
                                                                                                        t3:::wellset$new(trip_id = sample_set_well[l, 1],
                                                                                                                         activity_id = sample_set_well[l, 2],
                                                                                                                         well_id = sample_set_well[l, 3],
                                                                                                                         sample_id = sample_set_well[l, 4],
                                                                                                                         weighted_weight = sample_set_well[l, 5],
                                                                                                                         weighted_weight_minus10 =  sample_set_well[l, 5] * current_well$.__enclos_env__$private$well_prop_minus10_weigth,
                                                                                                                         weighted_weight_plus10 =  sample_set_well[l, 5] * current_well$.__enclos_env__$private$well_prop_plus10_weigth)
                                                                                                      })
                                              sum_weighted_weight <- sum(sapply(X = seq_len(length.out = length(current_well$.__enclos_env__$private$wellsets)),
                                                                                FUN = function(m) {
                                                                                  current_well$.__enclos_env__$private$wellsets[[m]]$.__enclos_env__$private$weighted_weight
                                                                                }))
                                              for (n in seq_len(length.out = length(current_well$.__enclos_env__$private$wellsets))) {
                                                current_well$.__enclos_env__$private$wellsets[[n]]$.__enclos_env__$private$prop_weighted_weight <- current_well$.__enclos_env__$private$wellsets[[n]]$.__enclos_env__$private$weighted_weight / sum_weighted_weight
                                              }
                                            } else {
                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                  " - Warning: we don't know le proportion of -10/+10 in the well or the global weight.\n",
                                                  "Sample(s) in the well is/are not useable.\n",
                                                  "[trip: ",
                                                  current_well$.__enclos_env__$private$trip_id,
                                                  ", well: ",
                                                  current_well$.__enclos_env__$private$well_id,
                                                  "]\n",
                                                  sep = "")
                                            }
                                          }
                                        }
                                      }
                                    }
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Process 2.4 successfull on item ",
                                        i,
                                        ".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    if (i == length(private$data_selected)) {
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - End process 2.4 well-set weight categories definition.\n",
                                          sep = "")
                                    }
                                  }
                                }
                              }
                            },
                            # process 2.5: standardised_sample_creation ----
                            #' @description Object standardised sample creation.
                            standardised_sample_creation = function() {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 2.5 (standardised sample creation) cancelled.\n",
                                    sep = "")
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.5: standardised sample creation.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    next()
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.5 on item ",
                                        i,
                                        ".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[j]]
                                      current_wells <- current_trip$.__enclos_env__$private$wells
                                      if (length(current_wells) != 0) {
                                        for (k in seq_len(length.out = length(current_wells))) {
                                          current_well <- current_wells[[k]]
                                          current_elementarysamples <- unlist(current_well$.__enclos_env__$private$elementarysample)
                                          if (is.null(current_elementarysamples)) {
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Error: the object elementarysample is NULL, please run processes 2.1 to 2.4 before this one.\n",
                                                "[trip_id: ",
                                                current_well$.__enclos_env__$private$trip_id,
                                                ", well_id: ",
                                                current_well$.__enclos_env__$private$well_id,
                                                "]\n",
                                                sep = "")
                                            stop()
                                          } else if (length(current_elementarysamples) != 0) {
                                            current_elementarysamples_species <- unique(sapply(X = seq_len(length.out = length(current_elementarysamples)),
                                                                                               FUN = function(l) {
                                                                                                 paste(current_elementarysamples[[l]]$.__enclos_env__$private$specie_code,
                                                                                                       current_elementarysamples[[l]]$.__enclos_env__$private$specie_code3l,
                                                                                                       sep = "_")
                                                                                               }))
                                            for (m in current_elementarysamples_species) {
                                              current_elementarysamples_specie <- Filter(Negate(is.null),
                                                                                         lapply(X = seq_len(length.out = length(current_elementarysamples)),
                                                                                                FUN = function(n) {
                                                                                                  if (current_elementarysamples[[n]]$.__enclos_env__$private$specie_code == as.integer(unlist(strsplit(m, "_"))[1])) {
                                                                                                    current_elementarysamples[[n]]
                                                                                                  }
                                                                                                }))
                                              current_elementarysamples_specie_classes <- unique(sapply(X = seq_len(length.out = length(current_elementarysamples_specie)),
                                                                                                        FUN = function(o) {
                                                                                                          current_elementarysamples_specie[[o]]$.__enclos_env__$private$sample_standardised_length_class_lf
                                                                                                        }))
                                              for (p in current_elementarysamples_specie_classes) {
                                                current_elementarysamples_specie_class <- Filter(Negate(is.null),
                                                                                                 lapply(X = seq_len(length.out = length(current_elementarysamples_specie)),
                                                                                                        FUN = function(q) {
                                                                                                          if (current_elementarysamples_specie[[q]]$.__enclos_env__$private$sample_standardised_length_class_lf == p) {
                                                                                                            current_elementarysamples_specie[[q]]
                                                                                                          }
                                                                                                        }))
                                                current_elementarysamples_sample_types <- unique(sapply(X = seq_len(length.out = length(current_elementarysamples_specie_class)),
                                                                                                        FUN = function(u) {
                                                                                                          current_elementarysamples_specie_class[[u]]$.__enclos_env__$private$sample_type
                                                                                                        }))
                                                for (v in current_elementarysamples_sample_types) {
                                                  current_elementarysamples_sample_type <- Filter(Negate(is.null),
                                                                                                  lapply(X = seq_len(length.out = length(current_elementarysamples_specie_class)),
                                                                                                         FUN = function(w) {
                                                                                                           if (current_elementarysamples_specie_class[[w]]$.__enclos_env__$private$sample_type == v) {
                                                                                                             current_elementarysamples_specie_class[[w]]
                                                                                                           }
                                                                                                         }))
                                                  current_elementarysamples_sample_qualities <- unique(sapply(X = seq_len(length.out = length(current_elementarysamples_sample_type)),
                                                                                                              FUN = function(x) {
                                                                                                                current_elementarysamples_sample_type[[x]]$.__enclos_env__$private$sample_quality
                                                                                                              }))
                                                  for (y in current_elementarysamples_sample_qualities) {
                                                    current_elementarysamples_sample_quality <- Filter(Negate(is.null),
                                                                                                       lapply(X = seq_len(length.out = length(current_elementarysamples_sample_type)),
                                                                                                              FUN = function(z) {
                                                                                                                if (current_elementarysamples_sample_type[[z]]$.__enclos_env__$private$sample_quality == y) {
                                                                                                                  current_elementarysamples_sample_type[[z]]
                                                                                                                }
                                                                                                              }))
                                                    standardisedsample <- t3:::standardisedsample$new(trip_id = current_well$.__enclos_env__$private$trip_id,
                                                                                                      well_id = current_well$.__enclos_env__$private$well_id,
                                                                                                      sample_id = unique(sapply(X = seq_len(length.out = length(current_elementarysamples_sample_quality)),
                                                                                                                                FUN = function(r) {
                                                                                                                                  current_elementarysamples_sample_quality[[r]]$.__enclos_env__$private$sample_id
                                                                                                                                })),
                                                                                                      sample_quality = as.integer(y),
                                                                                                      sample_type = as.integer(v),
                                                                                                      specie_code = as.integer(unlist(strsplit(m, "_"))[1]),
                                                                                                      specie_code3l = unlist(strsplit(m, "_"))[2],
                                                                                                      sample_standardised_length_class_lf = as.integer(p),
                                                                                                      sample_number_measured_extrapolated_lf = sum(sapply(X = seq_len(length.out = length(current_elementarysamples_sample_quality)),
                                                                                                                                                          FUN = function(s) {
                                                                                                                                                            current_elementarysamples_sample_quality[[s]]$.__enclos_env__$private$sample_number_measured_extrapolated_lf
                                                                                                                                                          })),
                                                                                                      sample_total_count = sum(sapply(X = seq_len(length.out = length(current_elementarysamples_sample_quality)),
                                                                                                                                      FUN = function(t) {
                                                                                                                                        current_elementarysamples_sample_quality[[t]]$.__enclos_env__$private$sample_total_count
                                                                                                                                      })),
                                                                                                      elementarysample = current_elementarysamples_sample_quality)
                                                    current_well$.__enclos_env__$private$standardisedsample <- append(current_well$.__enclos_env__$private$standardisedsample,
                                                                                                                      standardisedsample)
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
                                      " - Process 2.5 successfull on item ",
                                      i,
                                      ".\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (i == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End process 2.5 standardised sample creation.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 2.6: standardised_sample_set_creation ----
                            #' @description R6 object standardised sample set creation.
                            #' @param length_weight_relationship_data (data.frame) Data frame object with parameters for length weight relationship.
                            standardised_sample_set_creation = function(length_weight_relationship_data) {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 2.6 (standardised sample set creation) cancelled.\n",
                                    sep = "")
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.6: standardised sample set creation.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    next()
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.6 on item ",
                                        i,
                                        ".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[j]]
                                      current_wells <- current_trip$.__enclos_env__$private$wells
                                      if (length(current_wells) != 0) {
                                        for (k in seq_len(length.out = length(current_wells))) {
                                          current_well <- current_wells[[k]]
                                          current_wells_sets <- current_well$.__enclos_env__$private$wellsets
                                          if (is.null(current_wells_sets)) {
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Error: the object wellsets is NULL, please run process 2.4 before this one.\n",
                                                "[trip_id: ",
                                                current_well$.__enclos_env__$private$trip_id,
                                                ", well_id: ",
                                                current_well$.__enclos_env__$private$well_id,
                                                "]\n",
                                                sep = "")
                                            stop()
                                          }
                                          current_standardised_samples <- current_well$.__enclos_env__$private$standardisedsample
                                          if (is.null(current_standardised_samples)) {
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Error: the object wellsets is NULL, please run processes 2.1 to 2.5 before this one.\n",
                                                "[trip_id: ",
                                                current_well$.__enclos_env__$private$trip_id,
                                                ", well_id: ",
                                                current_well$.__enclos_env__$private$well_id,
                                                "]\n",
                                                sep = "")
                                            stop()
                                          }
                                          standardised_samples_sets <- vector(mode = "list",
                                                                              length = length(current_wells_sets))
                                          for (l in seq_len(length.out = length(current_wells_sets))) {
                                            current_well_sets <- current_wells_sets[[l]]
                                            current_activity <- current_well_sets$.__enclos_env__$private$activity_id
                                            current_ocean <- unlist(Filter(Negate(is.null), sapply(X = seq_len(length.out = length(current_trip$.__enclos_env__$private$activities)),
                                                                                                   FUN = function(m) {
                                                                                                     if (current_trip$.__enclos_env__$private$activities[[m]]$.__enclos_env__$private$activity_id == current_activity) {
                                                                                                       current_trip$.__enclos_env__$private$activities[[m]]$.__enclos_env__$private$ocean
                                                                                                     }
                                                                                                   })))
                                            if (is.null(current_ocean)) {
                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                  " - Error: sample activity missing from trip activities.\n",
                                                  "[trip: ",
                                                  private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                                  ", sample activity: ",
                                                  current_activity,
                                                  "]\n",
                                                  sep = "")
                                              stop()
                                            }
                                            current_standardised_samples_sets <- lapply(X = seq_len(length.out = length(current_standardised_samples)),
                                                                                        FUN = function(m) {
                                                                                          current_length_weight_relationship <- dplyr::filter(.data = length_weight_relationship_data,
                                                                                                                                              ocean == current_ocean & specie_code3l == current_standardised_samples[[m]]$.__enclos_env__$private$specie_code3l)[4:5]
                                                                                          if (nrow(current_length_weight_relationship) == 1) {
                                                                                            coef_a <- current_length_weight_relationship[1, 1]
                                                                                            coef_b <- current_length_weight_relationship[1, 2]
                                                                                            if (current_standardised_samples[[m]]$.__enclos_env__$private$specie_code3l %in% c("SKJ", "LTA", "FRI")) {
                                                                                              # step of 1 cm
                                                                                              length_class_lf <- current_standardised_samples[[m]]$.__enclos_env__$private$sample_standardised_length_class_lf + 0.5
                                                                                            } else if (current_standardised_samples[[m]]$.__enclos_env__$private$specie_code3l %in% c("YFT", "BET", "ALB")) {
                                                                                              # step of 2 cm
                                                                                              length_class_lf <- current_standardised_samples[[m]]$.__enclos_env__$private$sample_standardised_length_class_lf + 1
                                                                                            } else {
                                                                                              length_class_lf <- current_standardised_samples[[m]]$.__enclos_env__$private$sample_standardised_length_class_lf
                                                                                            }
                                                                                            lwr <- coef_a * length_class_lf ^ coef_b
                                                                                          } else {
                                                                                            lwr <- NA
                                                                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                                                " - Warning: length to weight conversion impossible.\n",
                                                                                                "[trip: ",
                                                                                                private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                                                                                ", well_id: ",
                                                                                                current_standardised_samples[[m]]$.__enclos_env__$private$well_id,
                                                                                                ", sample(s): ",
                                                                                                paste0(current_standardised_samples[[m]]$.__enclos_env__$private$sample_id,
                                                                                                       collapse = " - "),
                                                                                                "]\n",
                                                                                                sep = "")
                                                                                          }
                                                                                          t3:::standardisedsampleset$new(trip_id = current_well_sets$.__enclos_env__$private$trip_id,
                                                                                                                         activity_id = current_well_sets$.__enclos_env__$private$activity_id,
                                                                                                                         well_id = current_well_sets$.__enclos_env__$private$well_id,
                                                                                                                         sample_id = current_standardised_samples[[m]]$.__enclos_env__$private$sample_id,
                                                                                                                         sample_quality = current_standardised_samples[[m]]$.__enclos_env__$private$sample_quality,
                                                                                                                         sample_type = current_standardised_samples[[m]]$.__enclos_env__$private$sample_type,
                                                                                                                         specie_code = current_standardised_samples[[m]]$.__enclos_env__$private$specie_code,
                                                                                                                         specie_code3l = current_standardised_samples[[m]]$.__enclos_env__$private$specie_code3l,
                                                                                                                         sample_standardised_length_class_lf = current_standardised_samples[[m]]$.__enclos_env__$private$sample_standardised_length_class_lf,
                                                                                                                         sample_number_weighted = current_standardised_samples[[m]]$.__enclos_env__$private$sample_number_measured_extrapolated_lf * current_well_sets$.__enclos_env__$private$prop_weighted_weight,
                                                                                                                         sample_weigth = (current_standardised_samples[[m]]$.__enclos_env__$private$sample_number_measured_extrapolated_lf * current_well_sets$.__enclos_env__$private$prop_weighted_weight) * lwr,
                                                                                                                         sample_weight_unit = lwr,
                                                                                                                         sample_category = ifelse(lwr <= 10,
                                                                                                                                                  "- 10kg",
                                                                                                                                                  "+ 10kg"),
                                                                                                                         standardisedsample = current_standardised_samples[[m]])
                                                                                        })
                                            standardised_samples_sets[[l]] <- current_standardised_samples_sets
                                          }
                                          current_well$.__enclos_env__$private$standardisedsampleset <- standardised_samples_sets
                                        }
                                      }
                                    }
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Process 2.6 successfull on item ",
                                      i,
                                      ".\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (i == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End process 2.6: standardised sample set creation.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 2.7: raised_factors_determination ----
                            #' @description Raised factors determination for weigth sample set to set.
                            #' @param threshold_rf_minus10 (integer) Threshold limite value for raising factor on individuals category minus 10. By default 500.
                            #' @param threshold_rf_plus10 (integer) Threshold limite value for raising factor on individuals category plus 10. By default 500.
                            #' @param threshold_frequency_rf_minus10 (integer) Threshold limite frequency value for raising factor on individuals category minus 10. By default 75.
                            #' @param threshold_frequency_rf_plus10 (integer) Threshold limite frequency value for raising factor on individuals category plus 10. By default 75.
                            #' @param threshold_rf_total (integer) Threshold limite value for raising factor (all categories). By default 250.
                            raised_factors_determination = function(threshold_rf_minus10 = as.integer(500),
                                                                    threshold_rf_plus10 = as.integer(500),
                                                                    threshold_frequency_rf_minus10 = as.integer(75),
                                                                    threshold_frequency_rf_plus10 = as.integer(75),
                                                                    threshold_rf_total = as.integer(250)) {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 2.7 (raised factors determination) cancelled.\n",
                                    sep = "")
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.7: raised factors determination.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    next()
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.7 on item ",
                                        i,
                                        ".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[j]]
                                      current_wells <- current_trip$.__enclos_env__$private$wells
                                      if (length(current_wells) != 0) {
                                        for (k in seq_len(length.out = length(current_wells))) {
                                          current_well <- current_wells[[k]]
                                          current_wells_sets <- current_well$.__enclos_env__$private$wellsets
                                          if (is.null(current_wells_sets)) {
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Error: the object wellsets is NULL, please run process 2.4 before this one.\n",
                                                "[trip_id: ",
                                                current_well$.__enclos_env__$private$trip_id,
                                                ", well_id: ",
                                                current_well$.__enclos_env__$private$well_id,
                                                "]\n",
                                                sep = "")
                                            stop()
                                          }
                                          for (l in seq_len(length.out = length(current_wells_sets))) {
                                            current_well_sets <- current_wells_sets[[l]]
                                            current_well_standardisedsampleset <- current_well$.__enclos_env__$private$standardisedsampleset[[l]]
                                            if (is.null(current_well_standardisedsampleset)) {
                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                  " - Error: the object wellsets is NULL, please run process 2.1 to 2.6 before this one.\n",
                                                  "[trip_id: ",
                                                  current_well$.__enclos_env__$private$trip_id,
                                                  ", well_id: ",
                                                  current_well$.__enclos_env__$private$well_id,
                                                  "]\n",
                                                  sep = "")
                                              stop()
                                            }
                                            current_well_sets$.__enclos_env__$private$weighted_samples_minus10 <- sum(unlist(lapply(X = seq_len(length.out = length(current_well_standardisedsampleset)),
                                                                                                                                    FUN = function(m) {
                                                                                                                                      if (current_well_standardisedsampleset[[m]]$.__enclos_env__$private$sample_weight_unit <= 10) {
                                                                                                                                        current_well_standardisedsampleset[[m]]$.__enclos_env__$private$sample_weigth
                                                                                                                                      }
                                                                                                                                    }))) / 1000
                                            current_well_sets$.__enclos_env__$private$weighted_samples_plus10 <- sum(unlist(lapply(X = seq_len(length.out = length(current_well_standardisedsampleset)),
                                                                                                                                   FUN = function(n) {
                                                                                                                                     if (current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_weight_unit > 10) {
                                                                                                                                       current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_weigth
                                                                                                                                     }
                                                                                                                                   }))) / 1000
                                            current_well_sets$.__enclos_env__$private$weighted_samples_total <- sum(unlist(lapply(X = seq_len(length.out = length(current_well_standardisedsampleset)),
                                                                                                                                  FUN = function(o) {
                                                                                                                                    current_well_standardisedsampleset[[o]]$.__enclos_env__$private$sample_weigth
                                                                                                                                  }))) / 1000
                                            if (current_well_sets$.__enclos_env__$private$weighted_samples_total <= 0) {
                                              # scenario 1
                                              current_well_sets$.__enclos_env__$private$rf_validation <- 1
                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                  " - Warning: well-set avoided because weighted samples total value egal to zero.\n",
                                                  "[trip: ,",
                                                  current_well_sets$.__enclos_env__$private$trip_id,
                                                  ", activity: ",
                                                  current_well_sets$.__enclos_env__$private$activity_id,
                                                  ", well: ",
                                                  current_well_sets$.__enclos_env__$private$well_id,
                                                  ", sample(s): ",
                                                  paste0(current_well_sets$.__enclos_env__$private$sample_id,
                                                         collapse = " - "),
                                                  "]\n",
                                                  sep = "")
                                            } else if (is.null(current_well_sets$.__enclos_env__$private$weighted_weight)
                                                       || is.na(current_well_sets$.__enclos_env__$private$weighted_weight)
                                                       || current_well_sets$.__enclos_env__$private$weighted_weight <= 0) {
                                              # scenario 2
                                              current_well_sets$.__enclos_env__$private$rf_validation <- 2
                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                  " - Warning: well-set avoided because invalid weighted weigth.\n",
                                                  "[trip: ,",
                                                  current_well_sets$.__enclos_env__$private$trip_id,
                                                  ", activity: ",
                                                  current_well_sets$.__enclos_env__$private$activity_id,
                                                  ", well: ",
                                                  current_well_sets$.__enclos_env__$private$well_id,
                                                  ", sample(s): ",
                                                  paste0(current_well_sets$.__enclos_env__$private$sample_id,
                                                         collapse = " - "),
                                                  "]\n",
                                                  sep = "")
                                            } else {
                                              if (current_well_sets$.__enclos_env__$private$weighted_samples_minus10 == 0
                                                  || current_well_sets$.__enclos_env__$private$weighted_samples_plus10 == 0
                                                  || is.na(current_well_sets$.__enclos_env__$private$weighted_weight_minus10)
                                                  || is.na(current_well_sets$.__enclos_env__$private$weighted_weight_plus10)) {
                                                # scenario 3
                                                current_well_sets$.__enclos_env__$private$rf_validation <- 3
                                                current_well_sets$.__enclos_env__$private$rf_total <- current_well_sets$.__enclos_env__$private$weighted_weight / current_well_sets$.__enclos_env__$private$weighted_samples_total
                                              } else {
                                                current_well_sets$.__enclos_env__$private$rf_minus10 <- current_well_sets$.__enclos_env__$private$weighted_weight_minus10 / current_well_sets$.__enclos_env__$private$weighted_samples_minus10
                                                current_well_sets$.__enclos_env__$private$rf_plus10 <- current_well_sets$.__enclos_env__$private$weighted_weight_plus10 / current_well_sets$.__enclos_env__$private$weighted_samples_plus10
                                                if (current_well_sets$.__enclos_env__$private$rf_minus10 > threshold_rf_minus10
                                                    || current_well_sets$.__enclos_env__$private$rf_plus10 > threshold_rf_plus10
                                                    || sum(unlist(sapply(X = seq_len(length.out = length(current_well_standardisedsampleset)),
                                                                         FUN = function(p) {
                                                                           if (current_well_standardisedsampleset[[p]]$.__enclos_env__$private$sample_weight_unit <= 10) {
                                                                             current_well_standardisedsampleset[[p]]$.__enclos_env__$private$sample_number_weighted
                                                                           }
                                                                         }))) > threshold_frequency_rf_minus10
                                                    || sum(unlist(sapply(X = seq_len(length.out = length(current_well_standardisedsampleset)),
                                                                         FUN = function(p) {
                                                                           if (current_well_standardisedsampleset[[p]]$.__enclos_env__$private$sample_weight_unit > 10) {
                                                                             current_well_standardisedsampleset[[p]]$.__enclos_env__$private$sample_number_weighted
                                                                           }
                                                                         }))) > threshold_frequency_rf_plus10) {
                                                  # scenario 4
                                                  current_well_sets$.__enclos_env__$private$rf_validation <- 4
                                                  current_well_sets$.__enclos_env__$private$rf_total <- current_well_sets$.__enclos_env__$private$weighted_weight / current_well_sets$.__enclos_env__$private$weighted_samples_total
                                                } else {
                                                  # scenario 5
                                                  current_well_sets$.__enclos_env__$private$rf_validation <- 5
                                                }
                                              }
                                            }
                                            if (current_well_sets$.__enclos_env__$private$rf_validation %in% c(4, 3)
                                                && current_well_sets$.__enclos_env__$private$rf_total > threshold_rf_total) {
                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                  " - Warning: well-set \"rf_total\" argument superior to ",
                                                  threshold_rf_total,
                                                  ".\n",
                                                  "[trip: ,",
                                                  current_well_sets$.__enclos_env__$private$trip_id,
                                                  ", activity: ",
                                                  current_well_sets$.__enclos_env__$private$activity_id,
                                                  ", well: ",
                                                  current_well_sets$.__enclos_env__$private$well_id,
                                                  ", sample(s): ",
                                                  paste0(current_well_sets$.__enclos_env__$private$sample_id,
                                                         collapse = " - "),
                                                  "]\n",
                                                  sep = "")
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Process 2.7 successfull on item ",
                                      i,
                                      ".\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (i == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End process 2.7: raised factors determination.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 2.8: raised standardised sample set ----
                            #' @description Application of process 2.8 raised factors on standardised sample set.
                            raised_standardised_sample_set = function() {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 2.8 (raised standardised sample set) cancelled.\n",
                                    sep = "")
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.8: raised standardised sample set.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    next()
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.8 on item ",
                                        i,
                                        ".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[j]]
                                      current_wells <- current_trip$.__enclos_env__$private$wells
                                      if (length(current_wells) != 0) {
                                        for (k in seq_len(length.out = length(current_wells))) {
                                          current_well <- current_wells[[k]]
                                          current_wells_sets <- current_well$.__enclos_env__$private$wellsets
                                          if (is.null(current_wells_sets)) {
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Error: the object wellsets is NULL, please run process 2.4 before this one.\n",
                                                "[trip_id: ",
                                                current_well$.__enclos_env__$private$trip_id,
                                                ", well_id: ",
                                                current_well$.__enclos_env__$private$well_id,
                                                "]\n",
                                                sep = "")
                                            stop()
                                          }
                                          for (l in seq_len(length.out = length(current_wells_sets))) {
                                            current_well_sets <- current_wells_sets[[l]]
                                            current_well_standardisedsampleset <- current_well$.__enclos_env__$private$standardisedsampleset[[l]]
                                            if (is.null(current_well_standardisedsampleset)) {
                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                  " - Error: the object wellsets is NULL, please run process 2.1 to 2.7 before this one.\n",
                                                  "[trip_id: ",
                                                  current_well$.__enclos_env__$private$trip_id,
                                                  ", well_id: ",
                                                  current_well$.__enclos_env__$private$well_id,
                                                  "]\n",
                                                  sep = "")
                                              stop()
                                            }
                                            if (current_well_sets$.__enclos_env__$private$rf_validation %in% c(1, 2)) {
                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                  " - Warning: raised factors not available for this well-set.\n",
                                                  "[trip: ,",
                                                  current_well_sets$.__enclos_env__$private$trip_id,
                                                  ", activity: ",
                                                  current_well_sets$.__enclos_env__$private$activity_id,
                                                  ", well: ",
                                                  current_well_sets$.__enclos_env__$private$well_id,
                                                  ", sample(s): ",
                                                  paste0(current_well_sets$.__enclos_env__$private$sample_id,
                                                         collapse = " - "),
                                                  "]\n",
                                                  sep = "")
                                            } else if (current_well_sets$.__enclos_env__$private$rf_validation %in% c(3, 4)) {
                                              current_rf_total <- current_well_sets$.__enclos_env__$private$rf_total
                                              for (m in seq_len(length.out = length(current_well_standardisedsampleset))) {
                                                current_well_standardisedsampleset[[m]]$.__enclos_env__$private$sample_number_weighted_set <- current_well_standardisedsampleset[[m]]$.__enclos_env__$private$sample_number_weighted * current_rf_total
                                                current_well_standardisedsampleset[[m]]$.__enclos_env__$private$sample_weigth_set <- current_well_standardisedsampleset[[m]]$.__enclos_env__$private$sample_weight_unit * current_well_standardisedsampleset[[m]]$.__enclos_env__$private$sample_number_weighted_set / 1000
                                              }
                                            } else if (current_well_sets$.__enclos_env__$private$rf_validation == 5) {
                                              current_rf_minus10 <- current_well_sets$.__enclos_env__$private$rf_minus10
                                              current_rf_plus10 <- current_well_sets$.__enclos_env__$private$rf_plus10
                                              for (n in seq_len(length.out = length(current_well_standardisedsampleset))) {
                                                if (current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_weight_unit <= 10) {
                                                  current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_number_weighted_set <- current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_number_weighted * current_rf_minus10
                                                } else {
                                                  current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_number_weighted_set <- current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_number_weighted * current_rf_plus10
                                                }
                                                current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_weigth_set <- current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_weight_unit * current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_number_weighted_set / 1000
                                              }
                                            } else {
                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                  " - Error: raised factors verifications is not valide. Run process 2.7 before this one.\n",
                                                  "[trip: ,",
                                                  current_well_sets$.__enclos_env__$private$trip_id,
                                                  ", activity: ",
                                                  current_well_sets$.__enclos_env__$private$activity_id,
                                                  ", well: ",
                                                  current_well_sets$.__enclos_env__$private$well_id,
                                                  ", sample(s): ",
                                                  paste0(current_well_sets$.__enclos_env__$private$sample_id,
                                                         collapse = " - "),
                                                  "]\n",
                                                  sep = "")
                                              stop()
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Process 2.8 successfull on item ",
                                      i,
                                      ".\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (i == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End 2.8 process: raised standardised sample set.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # path to level 3 ----
                            #' @description Temporary link to the R object model with Antoine D. modelisation process.
                            path_to_level3 = function() {
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Start path creation for level 3.\n",
                                  sep = "")
                              data_level3 <- list()
                              raw_inputs_level3 <- vector(mode = "list",
                                                          length = 5)
                              names(raw_inputs_level3) <- c("act", "act3", "samw", "sset", "wp")
                              act <- data.frame(stringsAsFactors = FALSE)
                              act3 <- data.frame(stringsAsFactors = FALSE)
                              samw <- data.frame(stringsAsFactors = FALSE)
                              sset <- data.frame(stringsAsFactors = FALSE)
                              wp <- data.frame(stringsAsFactors = FALSE)
                              for (i in seq_len(length.out = length(private$data_selected))) {
                                for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                  current_trip <- private$data_selected[[i]][[j]]
                                  current_activities <- current_trip$.__enclos_env__$private$activities
                                  current_wells <- current_trip$.__enclos_env__$private$wells
                                  if (length(current_activities) != 0) {
                                    for (k in seq_len(length.out = length(current_activities))) {
                                      current_activity <- current_activities[[k]]
                                      tmp_activity <- data.frame("id_act" = current_activity$.__enclos_env__$private$activity_id,
                                                                 "lat" = current_activity$.__enclos_env__$private$activity_latitude,
                                                                 "lon" = current_activity$.__enclos_env__$private$activity_longitude,
                                                                 "fmod" = current_activity$.__enclos_env__$private$school_type,
                                                                 "date_act" = current_activity$.__enclos_env__$private$activity_date,
                                                                 "vessel" = current_trip$.__enclos_env__$private$vessel_id,
                                                                 "id_trip" = current_activity$.__enclos_env__$private$trip_id,
                                                                 "landingdate" = current_trip$.__enclos_env__$private$landing_date,
                                                                 "ocean" = current_activity$.__enclos_env__$private$ocean,
                                                                 "code_act_type" = current_activity$.__enclos_env__$private$activity_code,
                                                                 stringsAsFactors = FALSE)
                                      act <- rbind(act, tmp_activity)
                                      current_elementarycatches <- current_activity$.__enclos_env__$private$elementarycatches
                                      if (! is.null(current_elementarycatches) && length(current_elementarycatches) != 0) {
                                        for (l in seq_len(length.out = length(current_elementarycatches))) {
                                          current_elementarycatch <- current_elementarycatches[[l]]
                                          tmp_elementarycatch <- data.frame("id_act" = current_elementarycatch$.__enclos_env__$private$activity_id,
                                                                            "w_lb_t3" = current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected,
                                                                            "date_act" = current_activity$.__enclos_env__$private$activity_date,
                                                                            "sp_code" = current_elementarycatch$.__enclos_env__$private$specie_code,
                                                                            "sp" = current_elementarycatch$.__enclos_env__$private$specie_code3l,
                                                                            "wcat" = current_elementarycatch$.__enclos_env__$private$corrected_logbook_category,
                                                                            "code_act_type" = current_activity$.__enclos_env__$private$activity_code,
                                                                            stringsAsFactors = FALSE)
                                          act3 <- rbind(act3, tmp_elementarycatch)
                                        }
                                      }
                                    }
                                  }
                                  if (length(current_wells) != 0) {
                                    for (m in seq_len(length.out = length(current_wells))) {
                                      current_well <- current_wells[[m]]
                                      current_standardisedsamplesets <- unlist(current_well$.__enclos_env__$private$standardisedsampleset)
                                      current_wellplan <- current_well$.__enclos_env__$private$wellplan
                                      if (! is.null(current_standardisedsamplesets)) {
                                        for (n in seq_len(length.out = length(current_standardisedsamplesets))) {
                                          current_standardisedsampleset <- current_standardisedsamplesets[[n]]
                                          tmp_standardisedsampleset <- data.frame("id_act" = current_standardisedsampleset$.__enclos_env__$private$activity_id,
                                                                                  "sp_code" = current_standardisedsampleset$.__enclos_env__$private$specie_code,
                                                                                  "sp" = current_standardisedsampleset$.__enclos_env__$private$specie_code3l,
                                                                                  "wcat" = current_standardisedsampleset$.__enclos_env__$private$sample_category,
                                                                                  "w_fit_t3" = current_standardisedsampleset$.__enclos_env__$private$sample_weigth_set,
                                                                                  stringsAsFactors = FALSE)
                                          samw <- rbind(samw, tmp_standardisedsampleset)
                                          tmp_standardisedsampleset_qt <- data.frame("id_act" = current_standardisedsampleset$.__enclos_env__$private$activity_id,
                                                                                     "id_sample" = current_standardisedsampleset$.__enclos_env__$private$sample_id,
                                                                                     "quality" = current_standardisedsampleset$.__enclos_env__$private$sample_quality,
                                                                                     "type" = current_standardisedsampleset$.__enclos_env__$private$sample_type,
                                                                                     stringsAsFactors = FALSE)
                                          sset <- rbind(sset, tmp_standardisedsampleset_qt)
                                        }
                                      }
                                      if (! is.null(current_wellplan)) {
                                        for (o in seq_len(length.out = length(current_wellplan))) {
                                          current_elementarywellplan <- current_wellplan[[o]]
                                          tmp_elementarywellplan <- data.frame("id_well" = current_elementarywellplan$.__enclos_env__$private$well_id,
                                                                               "id_act" = current_elementarywellplan$.__enclos_env__$private$activity_id,
                                                                               "id_sample" = current_elementarywellplan$.__enclos_env__$private$sample_id,
                                                                               "sp_code" = current_elementarywellplan$.__enclos_env__$private$specie_code,
                                                                               "code3l" = current_elementarywellplan$.__enclos_env__$private$specie_code3l,
                                                                               "weight" = current_elementarywellplan$.__enclos_env__$private$wellplan_weight,
                                                                               "wcat_well" = current_elementarywellplan$.__enclos_env__$private$wellplan_weigth_category_label,
                                                                               stringsAsFactors = FALSE)
                                          wp <- rbind(wp, tmp_elementarywellplan)
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              raw_inputs_level3[[1]] <- act
                              raw_inputs_level3[[2]] <- act3
                              raw_inputs_level3[[3]] <- data.frame(dplyr::group_by(.data = samw,
                                                                                   id_act, sp_code, sp, wcat) %>%
                                                                     dplyr::summarise(w_fit_t3 = sum(w_fit_t3)) %>%
                                                                     dplyr::ungroup())
                              raw_inputs_level3[[4]] <- unique(sset)
                              raw_inputs_level3[[5]] <- wp
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End path creation for level 3.\n",
                                  sep = "")
                              data_level3 <- append(data_level3,
                                                    list(raw_inputs_level3))
                              names(data_level3)[length(data_level3)] <- "raw_inputs_level3"
                              assign(x = "data_level3",
                                     value = data_level3,
                                     envir = .GlobalEnv)
                            },
                            # process 3.1: data preparatory ----
                            #' @description Data preparatory for the t3 modelling process (level 3).
                            #' @param inputs_level3 (data frame) Imputs of levels 3 (see function path to level 3).
                            #' @param outputs_directory (character) Path of the t3 processes outputs directory.
                            #' @param periode_reference (integer) Year(s) period of reference for modelling estimation.
                            #' @param targeted_year (integer) Targeted year for model estimaton and prediction.
                            #' @param distance_maximum (integer) Maximum distance between all sets of a sampled well. By default 5.
                            #' @param number_sets_maximum (integer) Maximum number of sets allowed in mixture. By default 5.
                            #' @param set_weight_minimum (integer) Minimum set size considered. Remove smallest set for which sample could not be representative. By default 6.
                            #' @param minimum_set_frequency (numeric) Minimum freqency that a set could represent in a well. Another filter considering other set size in the well. By default 0.1.
                            #' @param vessel_id_ignored (integer) Specify here vessel(s) id(s) if you whant to ignore it in the model estimation and prediction .By default NULL.
                            data_preparatory = function(inputs_level3,
                                                        outputs_directory,
                                                        periode_reference,
                                                        targeted_year,
                                                        distance_maximum = as.integer(5),
                                                        number_sets_maximum = as.integer(5),
                                                        set_weight_minimum = as.integer(6),
                                                        minimum_set_frequency = 0.1,
                                                        vessel_id_ignored = NULL) {
                              if (class(outputs_directory) != "character") {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"outputs_directory\" argument, class character expected.\n",
                                    sep = "")
                                stop()
                              } else if (class(periode_reference) != "integer") {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"periode_reference\" argument, class integer expected.\n",
                                    sep = "")
                                stop()
                              } else if (class(targeted_year) != "integer" || length(targeted_year) != 1 || nchar(targeted_year) != 4) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"targeted_year\" argument, one value of class integer expected with a format on 4 digits.\n",
                                    sep = "")
                                stop()
                              } else if (class(distance_maximum) != "integer" || length(distance_maximum) != 1) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"distance_maximum\" argument, one value of class integer expected.\n",
                                    sep = "")
                                stop()
                              } else if (class(number_sets_maximum) != "integer" || length(number_sets_maximum) != 1) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"number_sets_maximum\" argument, one value of class integer expected.\n",
                                    sep = "")
                                stop()
                              } else if (class(set_weight_minimum) != "integer" || length(set_weight_minimum) != 1) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"set_weight_minimum\" argument, one value of class integer expected.\n",
                                    sep = "")
                                stop()
                              } else if (class(minimum_set_frequency) != "numeric" || length(minimum_set_frequency) != 1) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"minimum_set_frequency\" argument, one value of class numeric expected.\n",
                                    sep = "")
                                stop()
                              } else if (! class(vessel_id_ignored) %in% c("NULL", "integer")) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"vessel_id_ignored\" argument, class NULL of value(s) of class integer expected.\n",
                                    sep = "")
                                stop()
                              } else {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Start process 3.1: data preparatory.\n",
                                    sep = "")
                                outputs_directory_name <- format(Sys.time(), "%Y%m%d_%H%M%S_t3_level3_outputs")
                                dir.create(path = file.path(outputs_directory,
                                                            outputs_directory_name))
                                for (directory in c("data", "figures", "tables", "functions", "outputs")) {
                                  dir.create(path = file.path(outputs_directory,
                                                              outputs_directory_name,
                                                              directory))
                                }
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
                                first_year <- dplyr::first(periode_reference)
                                # select subset period for the modelling
                                catch_set_lb$year <- lubridate::year(x = catch_set_lb$date_act)
                                catch_set_lb<-catch_set_lb[catch_set_lb$year > first_year & catch_set_lb$year <= targeted_year, ]
                                act_chr$year <- lubridate::year(x = act_chr$date_act)
                                act_chr<-act_chr[act_chr$year > first_year & act_chr$year <= targeted_year, ]
                                # compute selection criteria ----
                                cdm <- act_chr$id_act[act_chr$vessel %in% vessel_id_ignored]
                                sset <- sset[! sset$id_act %in% cdm, ]
                                catch_set_lb <- catch_set_lb[! catch_set_lb$id_act %in% cdm, ]
                                # selection criteria
                                # remove bad quality sample and keep sample at landing
                                sset <- sset[sset$quality == 1 & sset$type == 1, ]
                                # number of act_chrivity by sample
                                agg <- aggregate(formula = cbind(nset = id_act) ~ id_sample,
                                                 data = sset,
                                                 FUN = length)
                                sset <- merge(x = sset,
                                              y = agg,
                                              sort = F)
                                # fishing mode homogeneity in sample
                                # add fishing mode
                                sset <- dplyr::inner_join(x = sset,
                                                          y = act_chr[, c("id_act", "fmod", "lat", "lon")],
                                                          by = "id_act")
                                tmp <- unique(sset[, c("id_sample","fmod")])
                                agg <- aggregate(formula = cbind(fm_purity = fmod) ~ id_sample,
                                                 data = tmp,
                                                 FUN = length)
                                sset <- merge(x = sset,
                                              y = agg,
                                              sort = F)
                                # fishing mode of the sample
                                tmp <- unique(sset[sset$fm_purity == 1,
                                                   c("id_sample", "fmod")])
                                names(tmp)[2] <- "fmod_sample"
                                sset <- merge(x = sset,
                                              y = tmp,
                                              all = T,
                                              sort = F)
                                # code for mixed fishing mode
                                sset$fmod_sample[is.na(sset$fmod_sample)] <- 999
                                sset$fmod_sample <- factor(sset$fmod_sample)
                                # extent of the sample
                                agg <- aggregate(formula = cbind(lat_sample_dif = lat, lon_sample_dif = lon) ~ id_sample,
                                                 data = sset,
                                                 FUN = function(x) {
                                                   max(x) - min(x)
                                                 })
                                sset <- merge(x = sset,
                                              y = agg,
                                              sort = F)
                                # compute total set weight
                                sset <- droplevels(sset)

                                tmp <- catch_set_lb
                                tmp <- tmp[tmp$sp %in% c("YFT","BET","SKJ"), ]
                                agg3 <- aggregate(formula = cbind(w_lb_t3 = w_lb_t3) ~ id_act,
                                                  data = tmp,
                                                  FUN = function(x) {
                                                    sum(x,
                                                        na.rm = T)
                                                  })
                                agg3 <- agg3[agg3$id_act %in% sset$id_act, ]
                                sset2 <- dplyr::inner_join(x = sset,
                                                           y = agg3[, c("id_act","w_lb_t3")],
                                                           by = "id_act",
                                                           sort = F)
                                sample_set_char <- list(sset = sset,
                                                        act_chr = act_chr,
                                                        catch_set_lb = catch_set_lb)
                                # compute set weight in each sample to detect non representivness of the sample
                                agg_wp <- aggregate(formula = cbind(w_in_well = weight) ~ id_sample + id_well + id_act,
                                                    data = wp,
                                                    FUN = sum)
                                agg_wp2 <- aggregate(formula = cbind(w_tot_well = weight) ~ id_sample + id_well,
                                                     data = wp,
                                                     FUN = sum)
                                agg_wp <- merge(x = agg_wp,
                                                y = agg_wp2)
                                # compute proportion of weight by set
                                agg_wp$prop_act_chr <- agg_wp$w_in_well / agg_wp$w_tot_well
                                # selection of act_chrivities ----
                                # selection based on sets extrapolated (2 first step of the t3 process)
                                kiset <- sset2
                                # on sample
                                # homogneous fishing mode in sample
                                kiset <- kiset[kiset$fm_purity == 1, ]
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
                                catch_set_lb$sp[!catch_set_lb$sp %in% c("YFT","BET","SKJ")] <- "OTH"
                                catch_set_lb <- droplevels(catch_set_lb)
                                # remove other species from lb before calculate species composition (to be compare to sample)
                                catch_set_lb <- catch_set_lb[catch_set_lb$sp_code %in% c(1, 2, 3), ]
                                catch_set_lb <- droplevels(catch_set_lb)
                                # calculate total catch for thonidae only
                                tot <- aggregate(formula = cbind(wtot_lb_t3 = w_lb_t3) ~ id_act,
                                                 data = catch_set_lb,
                                                 FUN = sum)
                                catch_set_lb <- merge(x = catch_set_lb,
                                                      y = tot,
                                                      sort = F)
                                # sum p10, 10-30 and p30 categories in Atlantic ocean
                                catch_set_lb <- aggregate(formula = cbind(w_lb_t3) ~ id_act + date_act + code_act_type + year + mon + wtot_lb_t3 + sp + wcat,
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
                                tmp2 <- prop.table(as.matrix(tmp2), 1)
                                tmp[, names(tmp) %in% colnames(tmp2)] <- tmp2
                                lb_set<-tmp
                                # compute proportion from t3 step 2 ----
                                samw$sp[!samw$sp %in% c("YFT","BET","SKJ")] <- "OTH"
                                samw <- samw[samw$sp_code %in% c(1, 2, 3), ]
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
                                samw <- aggregate(formula = cbind(w_fit_t3) ~ id_act + sp_cat,
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
                                samp_t3 <- tidyr::gather(data = tmp,
                                                         key = "sp_cat",
                                                         value = "prop_t3",
                                                         "BET_m10",
                                                         "BET_p10",
                                                         "SKJ_m10",
                                                         "YFT_m10",
                                                         "YFT_p10")
                                tmp <- dplyr::left_join(x = samp_t3,
                                                        y = act_chr,
                                                        by = "id_act")
                                data_sample_extract <- list(samw = samw,
                                                            samp_t3 = samp_t3)
                                # fusion of the lb and sample composition ----
                                lb_set_long <- tidyr::gather(data = lb_set,
                                                             "BET_m10",
                                                             "BET_p10",
                                                             "SKJ_m10",
                                                             "YFT_m10",
                                                             "YFT_p10",
                                                             key = "sp_cat",
                                                             value = "prop_lb",
                                                             -"code_act_type")
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
                                                                                                  "vessel")],
                                                              by = c("id_act", "year"))
                                data_lb_sample_screened = list(data4mod = data4mod)
                                # export to global environment
                                outputs_level3_process1 <- list(sample_set_char = sample_set_char,
                                                                data_selected = data_selected,
                                                                data_sample_extract = data_sample_extract,
                                                                data_lb_sample_screened = data_lb_sample_screened)
                                if (exists(x = "data_level3",
                                           envir = .GlobalEnv)) {
                                  data_level3 <- get(x = "data_level3",
                                                     envir = .GlobalEnv)
                                  data_level3 <- append(data_level3,
                                                        c(file.path(outputs_directory,
                                                                    outputs_directory_name),
                                                          list(outputs_level3_process1)))
                                  names(data_level3)[length(data_level3) - 1] <- "outputs_directory"
                                  names(data_level3)[length(data_level3)] <- "outputs_level3_process1"
                                } else {
                                  data_level3 <- list("raw_inputs_level3" = inputs_level3,
                                                      'outputs_directory' = file.path(outputs_directory,
                                                                                      outputs_directory_name),
                                                      "outputs_level3_process1" = outputs_level3_process1)
                                }
                                assign(x = "data_level3",
                                       value = data_level3,
                                       envir = .GlobalEnv)
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - End process 3.1: data preparatory.\n",
                                    sep = "")
                              }
                            },
                            # process 3.2: random forest models ----
                            #' @description Modelling proportions in sets througth random forest models.
                            #' @param outputs_level3_process1 (data frame) Output table data_lb_sample_screened from process 3.1.
                            #' @param num.trees (integer) Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times. The default value is 1000.
                            #' @param mtry Number of variables randomly sampled as candidates at each split. The default value is 2.
                            #' @param min.node.size Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time).The default value is 5.
                            #' @param seed_number  Set the initial seed for the modelling. The default value is 7.
                            random_forest_models = function(outputs_level3_process1,
                                                            num.trees = 1000L,
                                                            mtry = 2L,
                                                            min.node.size = 5,
                                                            seed_number = 7L) {
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.2: random forest models.\n",
                                  sep = "")
                              warn_defaut <- options("warn")
                              on.exit(options(warn_defaut))
                              options(warn = 1)
                              data4mod <- outputs_level3_process1
                              # sum proportion by species when working on total
                              data4mod <- tidyr::separate(data = data4mod,
                                                          col = sp_cat,
                                                          into = c("sp","wcat"),
                                                          sep = "_")
                              data4mod<- aggregate(formula = cbind(prop_lb, prop_t3) ~ id_act + date_act + year + mon + lat + lon + sp + fmod + ocean + vessel + wtot_lb_t3,
                                                   data = data4mod,
                                                   FUN = sum)
                              outputs_level3_process2 <- list()
                              for (ocean in unique(data4mod$ocean)) {
                                data4mod_ocean <- data4mod[data4mod$ocean == ocean, ]
                                for(sp in unique(data4mod_ocean$sp)) {
                                  if (! sp %in% c("SKJ","YFT")) {
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
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
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
                                                                        keep.inbag= FALSE)

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
                                                                          keep.inbag= FALSE)

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

                                      outputs_level3_process2 <- append(outputs_level3_process2,
                                                                        list(list(data = sub,
                                                                                  model_rf_simple = model_rf_simple,
                                                                                  model_rf_full = model_rf_full,
                                                                                  model_rf_wtvessel = model_rf_wtvessel)))
                                      names(outputs_level3_process2)[length(outputs_level3_process2)] <- paste(ocean, sp, fmod, sep = "_")
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
                              if (exists(x = "data_level3",
                                         envir = .GlobalEnv)) {
                                data_level3 <- get(x = "data_level3",
                                                   envir = .GlobalEnv)
                                data_level3 <- append(data_level3,
                                                      list(outputs_level3_process2))
                                names(data_level3)[length(data_level3)] <- "outputs_level3_process2"
                              } else {
                                data_level3 <- list("outputs_level3_process1" = outputs_level3_process1,
                                                    "outputs_level3_process2" = outputs_level3_process2)
                              }
                              assign(x = "data_level3",
                                     value = data_level3,
                                     envir = .GlobalEnv)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.2: random forest models.\n",
                                  sep = "")
                            },
                            # process 3.3: models checking ----
                            #' @description Load each full model and compute figures and tables to check the model quality. Furthermore, create a map of samples used for each model and relationship between logbook reports and samples.
                            #' @param outputs_level3_process2 (list) Outputs models and data from process 3.2.
                            #' @param outputs_path (character) Outputs directory path.
                            models_checking = function(outputs_level3_process2,
                                                       outputs_path) {
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.3: models checking.\n",
                                  sep = "")
                              warn_defaut <- options("warn")
                              on.exit(options(warn_defaut))
                              options(warn = 1)
                              outputs_level3_process3 <- list()
                              for (a in seq_len(length.out = length(outputs_level3_process2))) {
                                current_outputs_level3_process3 <- vector(mode = "list",
                                                                          length = 2)
                                names(current_outputs_level3_process3) <- c("figures", "tables")
                                current_model_outputs <- outputs_level3_process2[[a]]
                                ocean = unlist(strsplit(names(outputs_level3_process2)[[a]],
                                                        '_'))[1]
                                specie = unlist(strsplit(names(outputs_level3_process2)[[a]],
                                                         '_'))[2]
                                fishing_mode = unlist(strsplit(names(outputs_level3_process2)[[a]],
                                                               '_'))[3]
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Ongoing process 3.3 for ocean \"",
                                    ocean,
                                    "\", specie \"",
                                    specie,
                                    "\" and fishing mode \"",
                                    fishing_mode,
                                    "\"",
                                    ".\n",
                                    sep = "")
                                figures_directory <- file.path(outputs_path,
                                                               "figures",
                                                               names(outputs_level3_process2)[[a]])
                                names(figures_directory) <- "figures"
                                tables_directory <- file.path(outputs_path,
                                                              "tables",
                                                              names(outputs_level3_process2)[[a]])
                                names(tables_directory) <- "tables"
                                for (b in c(figures_directory, tables_directory)) {
                                  if (file.exists(b)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
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
                                    dir.create(b)
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
                                        figures_directory,
                                        "]\n",
                                        sep = "")
                                  }
                                }
                                # check data subset for modelling ----
                                current_model_data <- current_model_outputs[[1]]
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
                                write.csv2(x = covariance_matrix,
                                           file = file.path(tables_directory,
                                                            paste("covariance_matrix_",
                                                                  ocean,
                                                                  "_",
                                                                  specie,
                                                                  "_",
                                                                  fishing_mode,
                                                                  ".csv",
                                                                  sep = "")),
                                           row.names = TRUE)
                                current_outputs_level3_process3[[2]] <- append(current_outputs_level3_process3[[2]],
                                                                               list("covariance_matrix" = covariance_matrix))
                                multi_collinearity_test <- rfUtilities::multi.collinear(x = current_model_data[, c("lat",
                                                                                                                   "lon",
                                                                                                                   "resp",
                                                                                                                   "tlb",
                                                                                                                   "wtot_lb_t3",
                                                                                                                   "mon",
                                                                                                                   "year")],
                                                                                        perm = TRUE,
                                                                                        leave.out = TRUE)
                                write.csv2(x = multi_collinearity_test,
                                           file = file.path(tables_directory,
                                                            paste("multi_collinearity_test_",
                                                                  ocean,
                                                                  "_",
                                                                  specie,
                                                                  "_",
                                                                  fishing_mode,
                                                                  ".csv",
                                                                  sep = "")),
                                           row.names = FALSE)
                                current_outputs_level3_process3[[2]] <- append(current_outputs_level3_process3[[2]],
                                                                               list("multi_collinearity_test" = multi_collinearity_test))
                                # figure on logbook vs sample set
                                logbook_vs_sample_1 <- ggplot2::ggplot(data = current_model_data,
                                                                       ggplot2::aes(y = prop_t3,
                                                                                    x = prop_lb,
                                                                                    color = year)) +
                                  ggplot2::geom_point() +
                                  ggplot2::geom_smooth(method = "loess",
                                                       se = FALSE,
                                                       formula = "y ~ x") +
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
                                                        ggplot2::aes(y = ..scaled..)) +
                                  ggplot2::xlab("Species Frequency in set from logbook")
                                logbook_vs_sample <- ggpubr::ggarrange(logbook_vs_sample_1,
                                                                       logbook_vs_sample_2,
                                                                       nrow = 2,
                                                                       ncol = 1)
                                ggplot2::ggsave(plot = logbook_vs_sample,
                                                file = file.path(figures_directory,
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
                                current_outputs_level3_process3[[1]] <- append(current_outputs_level3_process3[[1]],
                                                                               list("logbook_vs_sample" = logbook_vs_sample))
                                # various figures to visualize some relationship from data before modelling
                                # single vessel effect
                                vessel_effect <- ggplot2::ggplot(current_model_data,
                                                                 ggplot2::aes(x = vessel,
                                                                              y = resp)) +
                                  ggplot2::geom_boxplot() +
                                  ggplot2::ylab("Species Frequency in set from sample")
                                ggplot2::ggsave(plot = vessel_effect,
                                                file = file.path(figures_directory,
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
                                current_outputs_level3_process3[[1]] <- append(current_outputs_level3_process3[[1]],
                                                                               list("vessel_effect" = vessel_effect))
                                # month effect
                                month_variation <- ggplot2::ggplot(current_model_data,
                                                                   ggplot2::aes(x = mon,
                                                                                y = resp)) +
                                  ggplot2::geom_boxplot() +
                                  ggplot2::labs(x = "Month",
                                                y = "Species Frequency in set from sample")
                                ggplot2::ggsave(plot = month_variation,
                                                file = file.path(figures_directory,
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
                                current_outputs_level3_process3[[1]] <- append(current_outputs_level3_process3[[1]],
                                                                               list("month_variation" = month_variation))
                                # year effect
                                year_effect <- ggplot2::ggplot(current_model_data,
                                                               ggplot2::aes(x = year,
                                                                            y = resp)) +
                                  ggplot2::geom_boxplot() +
                                  ggplot2::labs(x = NULL,
                                                y = "Species Frequency in set from sample")
                                ggplot2::ggsave(plot = year_effect,
                                                file = file.path(figures_directory,
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
                                current_outputs_level3_process3[[1]] <- append(current_outputs_level3_process3[[1]],
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
                                                file = file.path(figures_directory,
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
                                current_outputs_level3_process3[[1]] <- append(current_outputs_level3_process3[[1]],
                                                                               list("reporting_vs_sampling" = reporting_vs_sampling))
                                # map of the data used for modelling
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
                                wrld_sf <- data("wrld_simpl",
                                                package = "maptools")
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
                                                file = file.path(figures_directory,
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
                                current_outputs_level3_process3[[1]] <- append(current_outputs_level3_process3[[1]],
                                                                               list("set_sampled_map" = set_sampled_map))
                                # model checking
                                # compute model residuals
                                resrf <- current_model_data$resp - ranger::predictions(current_model_outputs[[3]])
                                current_model_data$res <- resrf
                                current_model_data$res_ST <- resrf / sd(ranger::predictions(current_model_outputs[[3]]))
                                current_model_data$fit<-ranger::predictions(current_model_outputs[[3]])

                                # method
                                # comparison of the model fitted value

                                # look at variable importance in the model
                                variables_importance <- as.data.frame(ranger::importance(current_model_outputs[[3]]))
                                names(variables_importance) <- "value"
                                variables_importance$var_name <- rownames(variables_importance)

                                variables_importance <- variables_importance[order(variables_importance$value, decreasing = F),]

                                variables_importance_plot <- ggplot2::ggplot(data = variables_importance,
                                                ggplot2::aes(y = var_name,
                                                             x = value))+
                                  ggplot2::geom_point()+
                                  ggplot2::geom_segment(data = variables_importance,
                                                        ggplot2::aes(x = 0,
                                                                     xend = value,
                                                                     y = var_name,
                                                                     yend = var_name)) +
                                  ggplot2::scale_y_discrete(name = "Variables",
                                                   limits= variables_importance$var_name)+
                                  ggplot2::xlab("Importance (impurity)")


                                  ggplot2::ggsave(plot = variables_importance_plot,
                                                file = file.path(figures_directory,
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
                                current_outputs_level3_process3[[1]] <- append(current_outputs_level3_process3[[1]],
                                                                               list("variables_importance" = variables_importance))
                                # test for spatial and temporal correlation on residuals
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
                                write.csv2(x = moran_residual_test,
                                           file = file.path(tables_directory,
                                                            paste("moran_residual_test_",
                                                                  ocean,
                                                                  "_",
                                                                  specie,
                                                                  "_",
                                                                  fishing_mode,
                                                                  ".csv",
                                                                  sep = "")),
                                           row.names = FALSE)
                                current_outputs_level3_process3[[2]] <- append(current_outputs_level3_process3[[2]],
                                                                               list("moran_residual_test" = moran_residual_test))

                                correlogram_resp <- furdeb::ggplot_corr(data = current_model_data$resp[order(current_model_data$date_act)],
                                                                        lag_max = 300)
                                correlogram_resp_acf <- correlogram_resp[[1]] +
                                  ggplot2::ggtitle(paste0(correlogram_resp[[1]][["labels"]][["title"]],
                                                          " (observed data)"))
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
                                correlogram_res <- furdeb::ggplot_corr(data = current_model_data$res[order(current_model_data$date_act)],
                                                                       lag_max = 300)
                                correlogram_res_acf <- correlogram_res[[1]] +
                                  ggplot2::ggtitle(paste0(correlogram_res[[1]][["labels"]][["title"]],
                                                          " (residuals)"))
                                spatio_temporal_checking <- ggpubr::ggarrange(variogram,
                                                                              correlogram_resp_acf,
                                                                              moran_index,
                                                                              correlogram_res_acf,
                                                                              nrow = 2,
                                                                              ncol = 2)
                                ggplot2::ggsave(plot = spatio_temporal_checking,
                                                file = file.path(figures_directory,
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
                                current_outputs_level3_process3[[1]] <- append(current_outputs_level3_process3[[1]],
                                                                               list("spatio_temporal_checking" = spatio_temporal_checking))
                                # model validity
                                model_validation_density_res <- ggplot2::ggplot(current_model_data,
                                                      ggplot2::aes(x = res_ST)) +
                                  ggplot2::geom_density(stat = "density",
                                                        fill = rgb(1,0,0,0.2),
                                                        ggplot2::aes(y = ..scaled..)) +
                                  ggplot2::scale_x_continuous(expand = c(0, 0)) +
                                  ggplot2::labs(x = "Standardized Residuals")
                                model_validation_qqplot_res <- ggplot2::ggplot(current_model_data, ggplot2::aes(sample = res_ST)) +
                                  ggplot2::stat_qq()+
                                  ggplot2::stat_qq_line(col = 2)+
                                  ggplot2::labs(x = "Theoretical quantiles", y = "Sample quantiles")
                                model_validation_response_fit <- ggplot2::ggplot(data = current_model_data,
                                                                            ggplot2::aes(x = resp,
                                                                                         y = fit)) +
                                  ggplot2::geom_point() +
                                  ggplot2::geom_smooth(method = "gam",
                                                       formula = y ~ s(x, bs = "cs"))+
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
                                                       formula = y ~ s(x, bs = "cs"))+
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
                                                file = file.path(figures_directory,
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
                                current_outputs_level3_process3[[1]] <- append(current_outputs_level3_process3[[1]],
                                                                               list("model_validation" = model_validation))
                                # model accuracy
                                # cross validation by k-folds
                                npartition <- 10 # not a parameter
                                df <- current_model_data

                                model_formula <- strsplit(as.character(current_model_outputs[[3]]$call),",")[[2]]
                                model_ntree <- current_model_outputs[[3]]$num.trees
                                model_mtry <- current_model_outputs[[3]]$mtry
                                model_node <- current_model_outputs[[3]]$min.node.size

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

                                  test$fit <- ranger:::predict.ranger(model,data = test)$predictions
                                  resi[[h]] = test$resp - test$fit
                                  mufit[[h]] = mean(test$resp)
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
                                write.csv2(x = kfold,
                                           file = file.path(tables_directory,
                                                            paste("kfold_",
                                                                  ocean,
                                                                  "_",
                                                                  specie,
                                                                  "_",
                                                                  fishing_mode,
                                                                  ".csv",
                                                                  sep = "")),
                                           row.names = FALSE)
                                current_outputs_level3_process3[[2]] <- append(current_outputs_level3_process3[[2]],
                                                                               list("kfold" = kfold))
                                outputs_level3_process3 <- append(outputs_level3_process3,
                                                                  list(current_outputs_level3_process3))
                                names(outputs_level3_process3)[length(outputs_level3_process3)] <- paste(ocean, specie, fishing_mode, sep = "_")
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
                              if (exists(x = "data_level3",
                                         envir = .GlobalEnv)) {
                                data_level3 <- get(x = "data_level3",
                                                   envir = .GlobalEnv)
                                data_level3 <- append(data_level3,
                                                      list(outputs_level3_process3))
                                names(data_level3)[length(data_level3)] <- "outputs_level3_process3"
                              } else {
                                data_level3 <- list("outputs_level3_process2" = outputs_level3_process2,
                                                    "outputs_level3_process3" = outputs_level3_process3)
                              }
                              assign(x = "data_level3",
                                     value = data_level3,
                                     envir = .GlobalEnv)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.3: models checking.\n",
                                  sep = "")
                            },
                            # process 3.4: data formatting for predictions ----
                            #' @description Formatting data for model predictions.
                            #' @param inputs_level3 (data frame) Imputs of levels 3 (see function path to level 3).
                            #' @param outputs_level3_process1 (data frame) Output table data_lb_sample_screened from process 3.1.
                            #' @param targeted_year (integer) Targeted year for model estimaton and prediction.
                            #' @param vessel_id_ignored (integer) Specify here vessel(s) id(s) if you whant to ignore it in the model estimation and prediction .By default NULL.
                            data_formatting_for_predictions = function(inputs_level3,
                                                                       outputs_level3_process1,
                                                                       targeted_year,
                                                                       vessel_id_ignored = NULL) {
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.4: data formatting for predictions.\n",
                                  sep = "")
                              warn_defaut <- options("warn")
                              on.exit(options(warn_defaut))
                              options(warn = 1)
                              outputs_level3_process4 <- list()
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
                              # standardize weight category
                              catch_set_lb$wcat <- gsub("kg",
                                                        "",
                                                        catch_set_lb$wcat)
                              catch_set_lb$wcat <- ifelse(catch_set_lb$wcat == "<10",
                                                          "m10",
                                                          "p10")
                              # only one category (called less 10) use for SKJ
                              catch_set_lb$wcat[catch_set_lb$sp == "SKJ"] <- "m10"
                              # set use for modeling to remove for prediction
                              data4mod <- outputs_level3_process1
                              sampleset <- unique(data4mod[, c("id_act",
                                                               "fmod",
                                                               "ocean",
                                                               "year")])
                              act_chr$yr <- lubridate::year(x = act_chr$date)
                              act_chr$mon <- lubridate::month(x = act_chr$date)
                              act_chr$fmod <- as.factor(act_chr$fmod)
                              act_chr$vessel <- as.factor(act_chr$vessel)
                              # non sampled set
                              # reduce data to the period considered in the modelling
                              act_chr <- act_chr[act_chr$yr %in% targeted_year, ]
                              # add the weigth by categoies, species from logbook (corrected by t3 level 1)
                              catch_set_lb$yr <- lubridate::year(x = catch_set_lb$date)
                              catch_set_lb$mon <- lubridate::month(x = catch_set_lb$date)
                              catch_set_lb <- catch_set_lb[catch_set_lb$sp_code %in% c(1, 2, 3), ]
                              catch_set_lb <- droplevels(catch_set_lb)
                              # format data
                              # sum catches categories of sets
                              agg <- catch_set_lb %>%
                                dplyr::group_by(id_act, sp) %>%
                                dplyr::summarise(w_lb_t3 = sum(w_lb_t3))
                              sets <- dplyr::inner_join(act_chr,
                                                        agg,
                                                        by = "id_act")
                              # catches keep onboard only = set
                              sets <- sets[sets$code_act_type %in% c(0,1,2), ]
                              sets$sp <- factor(sets$sp)
                              sets$ocean <- factor(sets$ocean)
                              sets <- sets[! sets$vessel %in% vessel_id_ignored, ]
                              # calculate proportion of weight from t3 level 1
                              sets_wide <- tidyr::spread(data = sets,
                                                         key = sp,
                                                         value = w_lb_t3,
                                                         fill = 0)
                              sets_wide$w_tuna <- rowSums(sets_wide[, c("BET","SKJ","YFT")])
                              # remove activity with no catch
                              sets_wide <- sets_wide[sets_wide$w_tuna > 0, ]
                              tmp <- sets_wide[, names(sets_wide) %in% c("id_act", as.character(unique(sets$sp)))]
                              tmp[is.na(tmp)] <- 0
                              tmp[as.character(unique(sets$sp))] <- prop.table(as.matrix(tmp[, as.character(unique(sets$sp))]), 1)
                              tmp2 <- tidyr::gather(as.data.frame(tmp),
                                                    key = "sp",
                                                    value = "prop_lb",
                                                    as.character(unique(sets$sp)))
                              colnames(tmp) <- c("id_act", paste("p_",
                                                                 levels(sets$sp),
                                                                 sep = ""))
                              sets_wide <- dplyr::inner_join(sets_wide,
                                                             tmp,
                                                             by = "id_act",
                                                             sort = FALSE)
                              sets_long <- tidyr::gather(sets_wide[, ! names(sets_wide) %in% c("p_BET", "p_SKJ", "p_YFT")],
                                                         key = "sp",
                                                         value = w_lb_t3,
                                                         BET,
                                                         SKJ,
                                                         YFT,
                                                         factor_key = TRUE)
                              sets_long$sp <- as.character(sets_long$sp)
                              sets_long <- dplyr::inner_join(sets_long,
                                                             tmp2,
                                                             by = c("id_act", "sp"),
                                                             sort = FALSE)
                              # Assign fishing mode to unknown
                              sets_wide$fmod <- factor(sets_wide$fmod)
                              sets_long$fmod <- factor(sets_long$fmod)
                              train <- droplevels(sets_wide[sets_wide$fmod != 3, ])
                              test <- droplevels(sets_wide[sets_wide$fmod == 3, ])
                              if(nrow(test) >0) {
                                ntree <- 1000
                                set.seed(7)
                                rfg <- ranger::ranger(fmod ~ p_YFT + p_SKJ + p_BET,
                                                      data = train,
                                                      mtry=2L,
                                                      num.trees = ntree,
                                                      min.node.size = 5L,
                                                      splitrule = "gini",
                                                      importance = "impurity",
                                                      replace = TRUE,
                                                      quantreg = FALSE,
                                                      keep.inbag= FALSE)
                              test$fmod2 <- ranger:::predict.ranger(rfg,
                                                                    data = test)$predictions
                              tmp <- dplyr::left_join(sets_long,
                                                      test[, c("id_act","fmod2")],
                                                      by = "id_act")
                              tmp$fmod[tmp$fmod == 3] <- tmp$fmod2[tmp$fmod == 3]
                              tmp$fmod2 <- NULL
                              sets_long <- droplevels(tmp)
                              }
                              outputs_level3_process4 <- append(outputs_level3_process4,
                                                                list(list("sets_long" = sets_long,
                                                                          "sets_wide" = sets_wide)))
                              names(outputs_level3_process4)[length(outputs_level3_process4)] <- "nonsampled_sets"
                              if (exists(x = "data_level3",
                                         envir = .GlobalEnv)) {
                                data_level3 <- get(x = "data_level3",
                                                   envir = .GlobalEnv)
                                data_level3 <- append(data_level3,
                                                      list(outputs_level3_process4))
                                names(data_level3)[length(data_level3)] <- "outputs_level3_process4"
                              } else {
                                data_level3 <- list("outputs_level3_process1" = outputs_level3_process2,
                                                    "outputs_level3_process4" = outputs_level3_process4)
                              }
                              assign(x = "data_level3",
                                     value = data_level3,
                                     envir = .GlobalEnv)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.4: data formatting for predictions.\n",
                                  sep = "")
                            },
                            # process 3.5: model predictions ----
                            #' @description Model predictions for the species composition and computing of catches.
                            #' @param outputs_level3_process2 (list) Outputs from level 3 process 2 (random forest models).
                            #' @param outputs_level3_process4 (list) Outputs from level 3 process 4 (data formatting for predictions).
                            #' @param outputs_path (character) Outputs directory path.
                            #' @param ci Logical indicating whether confidence interval is computed. The default value is FALSE as it is a time consuming step.
                            #' @param Nboot The number of bootstrap samples desired for the ci computation. The fefault value is 10.
                            model_predictions = function(outputs_level3_process2,
                                                         outputs_level3_process4,
                                                         outputs_path,
                                                         ci = FALSE,
                                                         Nboot = 10) {
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.5: model predictions.\n",
                                  sep = "")
                              warn_defaut <- options("warn")
                              on.exit(options(warn_defaut))
                              options(warn = 1)
                              wrld_sf <- data("wrld_simpl",
                                              package = "maptools")
                              # function to add empty levels for the prediction with randomforest::predict.randomforest
                              addmissinglevel <- function(df, a){
                                if (is.factor(df[, a])) {
                                  return(factor(df[, a],
                                                levels = c(levels(df[, a]),
                                                           setdiff(current_outputs_level3_process2[[3]]$forest$xlevels[a][[1]],
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
                              outputs_level3_process5 <- vector(mode = "list",
                                                                length = 5)
                              names(outputs_level3_process5) <- c("Estimated_catch",
                                                                  "Estimated_catch_ST",
                                                                  "Boot_output_list",
                                                                  "Boot_output_list_ST",
                                                                  "Final_output")
                              sets_long <- outputs_level3_process4[[1]][[1]]
                              for (ocean in unique(sets_long$ocean)) {
                                sets_long_ocean <- sets_long[sets_long$ocean == ocean, ]
                                for (specie in unique(sets_long_ocean$sp)) {
                                  if (! specie %in% c("SKJ","YFT")) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: process 3.5 not developed yet for the specie \"",
                                        specie,
                                        "\" in the ocean \"",
                                        ocean,
                                        "\".\n",
                                        "Data associated not used for this process.\n",
                                        sep = "")
                                  } else {
                                    sets_long_specie <- sets_long_ocean[sets_long_ocean$sp == specie, ]
                                    for (fishing_mode in unique(sets_long_specie$fmod)) {
                                      sets_long_fishing_mode <- sets_long_specie[sets_long_specie$fmod == fishing_mode, ]
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - Ongoing process 3.5 (Predictions step) for ocean \"",
                                          ocean,
                                          "\", specie \"",
                                          specie,
                                          "\" and fishing mode \"",
                                          fishing_mode,
                                          "\"",
                                          ".\n",
                                          sep = "")
                                      if(nrow(sets_long_fishing_mode) > 0){
                                        # models
                                        current_outputs_level3_process2 <- outputs_level3_process2[[paste(ocean,
                                                                                                          specie,
                                                                                                          fishing_mode,
                                                                                                          sep = "_")]]
                                         res <- t3:::tunapredict(sample_data = current_outputs_level3_process2[[1]],
                                                                allset_data = sets_long_fishing_mode,
                                                                                          Ntree = 1000,
                                                                                          Nmtry = 2,
                                                                                          Nseed = 7)

                                      outputs_level3_process5[[1]] <- append(outputs_level3_process5[[1]],
                                                                             list(res))
                                      names(outputs_level3_process5[[1]])[length(outputs_level3_process5[[1]])] <- paste(ocean,
                                                                                                                         specie,
                                                                                                                         fishing_mode,
                                                                                                                         sep = "_")
                                        cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                            " - Process 3.5 (Predictions step) successfull for ocean \"",
                                            ocean,
                                            "\", specie \"",
                                            specie,
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

                              # Standardize SKJ and YFT 'Estimated catch' and compute BET estimated catch
                              for (ocean in unique(sets_long$ocean)) {
                                outputs_level3_process5_ocean <- outputs_level3_process5[[1]][grep(pattern = paste(ocean,"_", sep = ""),
                                                                      x = names(outputs_level3_process5[[1]]))]
                                boot_tmp_element <- do.call(what = rbind,
                                                            args = outputs_level3_process5_ocean)
                                boot_tmp_element <- tidyr::spread(data = boot_tmp_element,
                                                                  key = "sp",
                                                                  value = fit_prop)
                                boot_tmp_element$S <- boot_tmp_element$SKJ + boot_tmp_element$YFT
                                boot_tmp_element$SKJ <- base::ifelse(test = boot_tmp_element$S > 1,
                                                                     yes = boot_tmp_element$SKJ/boot_tmp_element$S,
                                                                     no = boot_tmp_element$SKJ)
                                boot_tmp_element$YFT <- base::ifelse(test = boot_tmp_element$S > 1,
                                                                     yes = boot_tmp_element$YFT/boot_tmp_element$S,
                                                                     no = boot_tmp_element$YFT)
                                boot_tmp_element$BET <- 1 - (boot_tmp_element$SKJ + boot_tmp_element$YFT)

                                boot_tmp_element <- tidyr::gather(data = boot_tmp_element,
                                                                  key = "sp",
                                                                  value = "fit_prop_t3_ST",
                                                                  "BET", "SKJ", "YFT")

                                boot_tmp_element$catch_set_fit <- boot_tmp_element$wtot_lb_t3 * boot_tmp_element$fit_prop_t3_ST

                                outputs_level3_process5[[2]] <- append(outputs_level3_process5[[2]],
                                                                       list(boot_tmp_element))
                                names(outputs_level3_process5[[2]])[length(outputs_level3_process5[[2]])] <- paste("ocean",
                                                                                                                   ocean,
                                                                                                                   sep = "_")
                              }

                              # bootstrap CI
                              # bootstrap step 1 - bootstrap on models and predicts

                              if (ci == TRUE){

                                for (ocean in unique(sets_long$ocean)) {
                                  sets_long_ocean <- sets_long[sets_long$ocean == ocean, ]
                                  for (specie in unique(sets_long_ocean$sp)) {
                                    if (! specie %in% c("SKJ","YFT")) {
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - Warning: process 3.5 not developed yet for the specie \"",
                                          specie,
                                          "\" in the ocean \"",
                                          ocean,
                                          "\".\n",
                                          "Data associated not used for this process.\n",
                                          sep = "")
                                    } else {
                                      sets_long_specie <- sets_long_ocean[sets_long_ocean$sp == specie, ]
                                      for (fishing_mode in unique(sets_long_specie$fmod)) {
                                        sets_long_fishing_mode <- sets_long_specie[sets_long_specie$fmod == fishing_mode, ]
                                        cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                            " - Ongoing process 3.5 (Bootstrap step) for ocean \"",
                                            ocean,
                                            "\", specie \"",
                                            specie,
                                            "\" and fishing mode \"",
                                            fishing_mode,
                                            "\"",
                                            ".\n",
                                            sep = "")
                                        if(nrow(sets_long_fishing_mode) > 0){
                                            current_outputs_level3_process2 <- outputs_level3_process2[[paste(ocean,
                                                                                                              specie,
                                                                                                              fishing_mode,
                                                                                                              sep = "_")]]
                                      boot_output <- tunaboot(sample_data = current_outputs_level3_process2[[1]],
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
                                                                                                                         specie,
                                                                                                                         fishing_mode,
                                                                                                                         sep = "_")
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - Process 3.5 (Bootstrap step) successfull for ocean \"",
                                          ocean,
                                          "\", specie \"",
                                          specie,
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


                              # bootrstap step 2 - Standardize SKJ and YFT 'Estimated catch' and compute BET estimated catch #
                              # Standardize SKJ and YFT boot output - , compute BET proportion and catch for all
                              for (ocean in unique(sets_long$ocean)) {
                                outputs_level3_process5_ocean <- outputs_level3_process5[[3]][grep(pattern = paste(ocean,"_", sep = ""),
                                                                                                   x = names(outputs_level3_process5[[3]]))]

                                list_boot_ST_ocean <- vector("list", length = length(outputs_level3_process5_ocean[[1]]))

                                  for(element in (seq.int(from = 1,
                                                          to = length(outputs_level3_process5_ocean[[1]])))){
                                  boot_tmp_element <- lapply(outputs_level3_process5_ocean, function(l) l[[element]])
                                  boot_tmp_element <- do.call(what = rbind,
                                                              args = boot_tmp_element)

                                  boot_tmp_element <- tidyr::spread(boot_tmp_element,
                                                                    key = "sp",
                                                                    value = fit_prop)
                                  boot_tmp_element$S <- boot_tmp_element$SKJ + boot_tmp_element$YFT
                                  boot_tmp_element$SKJ <- base::ifelse(boot_tmp_element$S > 1,
                                                                       boot_tmp_element$SKJ/boot_tmp_element$S,
                                                                       boot_tmp_element$SKJ)
                                  boot_tmp_element$YFT <- base::ifelse(boot_tmp_element$S > 1,
                                                                       boot_tmp_element$YFT/boot_tmp_element$S,
                                                                       boot_tmp_element$YFT)
                                  boot_tmp_element$BET <- 1 - (boot_tmp_element$SKJ + boot_tmp_element$YFT)

                                  boot_tmp_element <- tidyr::gather(boot_tmp_element,
                                                                    key = "sp",
                                                                    value = "fit_prop_t3_ST","BET", "SKJ", "YFT")

                                  boot_tmp_element$catch_set_fit <- boot_tmp_element$wtot_lb_t3 * boot_tmp_element$fit_prop_t3_ST

                                  list_boot_ST_ocean[[element]] <- boot_tmp_element

                                }

                                outputs_level3_process5[[4]] <- append(outputs_level3_process5[[4]],
                                                                       list(list_boot_ST_ocean))
                                names(outputs_level3_process5[[4]])[length(outputs_level3_process5[[4]])] <- paste("ocean",
                                                                                                                   ocean,
                                                                                                                   sep = "_")
                              }


                              # bootstrap step 3 - compute confident intervals
                              # nominal catch by species (task 1)
                             t1_all <- do.call(rbind,lapply(outputs_level3_process5$Estimated_catch_ST,
                                                             function(x){
                             t1_tmp_element <-aggregate(cbind(catch_set_fit) ~ yr + sp + ocean ,
                                                  data = x,
                                                  FUN = sum)
                             return(t1_tmp_element)
                                                             }))

                              t1_all_boot <- do.call(rbind,lapply(outputs_level3_process5$Boot_output_list_ST,
                                                                  function(x){
                                                                  boot_tmp_element <-do.call(rbind,
                                                                                             lapply(seq.int(1:length(x)),
                                                                                                    function(i){
                                                                                                      boot_tmp_subelement <- aggregate(cbind(catch_set_fit) ~ yr + sp + ocean,
                                                                                                                                       data=x[[i]], sum)
                                                                    boot_tmp_subelement$loop <- i
                                                                    return(boot_tmp_subelement)
                                                                  }))
                                return(boot_tmp_element)
                              }))


                              # compute final CI
                              t1_all_final_ocean_list <- vector("list", length = length(levels(t1_all$ocean)))

                              for (o in levels(t1_all$ocean)){
                                t1_all_final_ocean_list[[as.numeric(o)]] <- catch_ci_calculator(fit_data = t1_all[t1_all$ocean == o,],
                                                                  boot_data = t1_all_boot[t1_all_boot$ocean == o,])
                              }

                              t1_all_final_ocean <- do.call(rbind, t1_all_final_ocean_list)

                              outputs_level3_process5[[5]] <- append(outputs_level3_process5[[5]],
                                                                     list(t1_all_final_ocean))
                              names(outputs_level3_process5[[5]])[length(outputs_level3_process5[[5]])] <- "Nominal_catch_species"


                              # nominal catch by species and fishing mode (task 1 by fishing mode)
                              t1_fmod <- do.call(rbind,lapply(outputs_level3_process5$Estimated_catch_ST, function(x){
                                boot_tmp_subelement <- aggregate(cbind(catch_set_fit) ~ yr + fmod + sp + ocean ,data=x, sum)
                                return(boot_tmp_subelement)

                              }))

                              # bootstrap distribution
                              t1_fmod_boot <- do.call(rbind,lapply(outputs_level3_process5$Boot_output_list_ST, function(x){
                                boot_tmp_element <-do.call(rbind,
                                                           lapply(seq.int(1:length(x)),
                                                                  function(i){
                                  boot_tmp_subelement <- aggregate(cbind(catch_set_fit) ~ yr + fmod + sp + ocean ,data=x[[i]], sum)
                                  boot_tmp_subelement$loop <- i
                                  return(boot_tmp_subelement)
                                }))
                                return(boot_tmp_element)
                              }))

                              # compute final CI
                              t1_fmod_final_ocean_list <- vector("list", length = length(levels(t1_fmod$ocean)))

                              for (o in levels(t1_fmod$ocean)){
                                t1_fmod_final_ocean_list[[as.numeric(o)]] <- catch_ci_calculator(fit_data = t1_fmod[t1_fmod$ocean == o,],
                                                                   boot_data = t1_fmod_boot[t1_fmod_boot$ocean == o,])
                              }

                              t1_fmod_final_ocean <- do.call(rbind, t1_fmod_final_ocean_list)

                              outputs_level3_process5[[5]] <- append(outputs_level3_process5[[5]],
                                                                     list(t1_fmod_final_ocean))
                              names(outputs_level3_process5[[5]])[length(outputs_level3_process5[[5]])] <- "Nominal_catch_fishing_mode"
                              browser()
                                                           # catch_set_t3_long <- do.call(what = rbind,
                              #                              args = outputs_level3_process5[[1]])
                              # catch_set_t3_wide <- tidyr::spread(data = catch_set_t3_long[, names(catch_set_t3_long) != "catch_t3_N3"],
                              #                                    key = "sp",
                              #                                    value = fit_prop)
                              # # standardized YFT and SKJ estimates for case with more than 100 percent
                              # catch_set_t3_wide$S <- catch_set_t3_wide$SKJ + catch_set_t3_wide$YFT
                              # catch_set_t3_wide$SKJ <- ifelse(test = catch_set_t3_wide$S > 1,
                              #                                 yes = catch_set_t3_wide$SKJ / catch_set_t3_wide$S,
                              #                                 no = catch_set_t3_wide$SKJ)
                              # catch_set_t3_wide$YFT <- ifelse(test = catch_set_t3_wide$S > 1,
                              #                                 yes = catch_set_t3_wide$YFT / catch_set_t3_wide$S,
                              #                                 no = catch_set_t3_wide$YFT)
                              # catch_set_t3_wide$BET <- 1 - (catch_set_t3_wide$SKJ + catch_set_t3_wide$YFT)
                              # tmp <- tidyr::gather(data = catch_set_t3_wide,
                              #                      key = "sp",
                              #                      value = "fit_prop_t3_ST", "BET", "SKJ", "YFT")
                              # # add column with standardized estimates
                              # catch_set_t3_long <- dplyr::right_join(x = catch_set_t3_long,
                              #                                        y = tmp,
                              #                                        by = c("id_act", "fmod", "sp", "lat", "lon", "date_act", "vessel", "ocean", "year", "mon", "wtot_lb_t3", "data_source"))
                              # catch_set_t3_long$catch_t3_N3 <- catch_set_t3_long$fit_prop_t3_ST * catch_set_t3_long$wtot_lb_t3
                              # # task 1
                              # tot <- aggregate(formula = cbind(catch_t3_N3) ~ year + fmod + ocean,
                              #                  data = catch_set_t3_long,
                              #                 FUN = sum)
                              # t1 <- aggregate(formula = cbind(catch_t3_N3) ~ year + fmod + sp + ocean,
                              #                 data = catch_set_t3_long,
                              #                 FUN = sum)
                              # # task 2
                              # tmp <- catch_set_t3_long
                              # cwp <- furdeb::lat_lon_cwp_manipulation(manipulation_process = "lat_lon_to_cwp",
                              #                                         data_longitude = as.character(tmp$lon),
                              #                                         data_latitude = as.character(tmp$lat),
                              #                                         input_degree_format = "decimal_degree",
                              #                                         cwp_resolution = "1deg_x_1deg")
                              # cwp$longitude_decimal_degree <- as.numeric(cwp$longitude_decimal_degree)
                              # cwp$latitude_decimal_degree <- as.numeric(cwp$latitude_decimal_degree)
                              # tmp <- dplyr::inner_join(x = tmp,
                              #                          y = cwp,
                              #                          by = c("lon" = "longitude_decimal_degree",
                              #                                 "lat" = "latitude_decimal_degree")) %>%
                              #   dplyr::rename(cwp1 = cwp)
                              # # mean proportion by 1 degree / month
                              # tmp2 <- aggregate(formula = cbind(fit_prop_t3_ST) ~ year + fmod + sp + ocean + cwp1,
                              #                   data = tmp,
                              #                   FUN = mean)
                              # # catch by 1 degree/month
                              # tmp3 <- aggregate(formula = cbind(catch_t3_N3) ~ year + fmod + sp + ocean + cwp1,
                              #                   data = tmp,
                              #                   FUN = sum)
                              # t2 <-  dplyr::inner_join(x = tmp3,
                              #                          y = tmp2,
                              #                          by = c("year", "fmod", "sp", "ocean", "cwp1"))
                              # lon_lat <- furdeb::lat_lon_cwp_manipulation(manipulation_process = "cwp_to_lat_lon",
                              #                                             data_cwp = as.character(t2$cwp1),
                              #                                             output_degree_format = "decimal_degree",
                              #                                             output_degree_cwp_parameter = "centroid",
                              #                                             cwp_resolution = "1deg_x_1deg")
                              # lon_lat$longitude_decimal_degree_centroid <- as.numeric(lon_lat$longitude_decimal_degree_centroid)
                              # lon_lat$latitude_decimal_degree_centroid <- as.numeric(lon_lat$latitude_decimal_degree_centroid)
                              # t2 <- dplyr::inner_join(x = t2,
                              #                         y = lon_lat,
                              #                         by = c("cwp1" = "cwp")) %>%
                              #   dplyr::rename(lon = longitude_decimal_degree_centroid,
                              #                 lat = latitude_decimal_degree_centroid)
                              # outputs_level3_process5 <- append(outputs_level3_process5,
                              #                                   list(list(catch_set_t3_long,
                              #                                             t1,
                              #                                             t2)))
                              # names(outputs_level3_process5)[[2]] <- paste0("raw_t1_t2_",
                              #                                               unique(catch_set_t3_long$year))
                              # names(outputs_level3_process5[[2]]) <- c("catch_set_t3_long",
                              #                                          "t1",
                              #                                          "t2")
                              # figure task 2 and proportion
                              sps <- t2
                              sp::coordinates(object = sps) <- ~ lon + lat
                              # select for the year
                              yr_fig = as.character(unique(sps$year))
                              fmod_fig = unique(sps$fmod)
                              ocean_fig <- unique(sps$ocean)
                              # common extent for all figures
                              wrl <- rastermap(x = 1,
                                               y = 1)
                              wrld <- raster::crop(x = wrld_simpl,
                                                   y = (raster::extent(sps) + 5))
                              palette4catch <- grDevices::colorRampPalette(c("yellow", "red"))
                              outputs_level3_process5 <- append(outputs_level3_process5,
                                                                list(list()))
                              names(outputs_level3_process5)[length(outputs_level3_process5)] <- "figures"
                              # map of the proportion
                              for (specie in unique(sps$sp)) {
                                if (! specie %in% c("BET", "SKJ", "YFT")) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
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
                                        figures_directory <- file.path(outputs_path,
                                                                       "figures",
                                                                       paste(ocean,
                                                                             specie,
                                                                             fishing_mode,
                                                                             sep = "_"))
                                        if (file.exists(figures_directory)) {
                                          cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                              " - Outputs \"figures\" directory for ocean \"",
                                              ocean,
                                              "\", specie \"",
                                              specie,
                                              "\" and fishing mode \"",
                                              fishing_mode,
                                              "\" already exists.\n",
                                              "Outputs associated will used this directory (be careful of overwriting previous files).\n",
                                              sep = "")
                                        } else {
                                          dir.create(figures_directory)
                                          cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                              " - Outputs \"figures\" directory for ocean \"",
                                              ocean,
                                              "\", specie \"",
                                              specie,
                                              "\" and fishing mode \"",
                                              fishing_mode,
                                              "\" created.\n",
                                              "[directory path: ",
                                              figures_directory,
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
                                                        file = file.path(figures_directory,
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
                                        r_points = raster::rasterToPoints(x = wrl2)
                                        # t2
                                        r_df = data.frame(r_points)
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
                                                                          fill = labs))+
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
                                                        file = file.path(figures_directory,
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
                              if (exists(x = "data_level3",
                                         envir = .GlobalEnv)) {
                                data_level3 <- get(x = "data_level3",
                                                   envir = .GlobalEnv)
                                data_level3 <- append(data_level3,
                                                      list(outputs_level3_process5))
                                names(data_level3)[length(data_level3)] <- "outputs_level3_process5"
                              } else {
                                data_level3 <- list("outputs_level3_process5" = outputs_level3_process5)
                              }
                              assign(x = "data_level3",
                                     value = data_level3,
                                     envir = .GlobalEnv)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.5: model predictions.\n",
                                  sep = "")
                            },
                            # browser ----
                            #' @description Most powerfull and "schwifty" function in the univers for "open the T3 process" and manipulate in live R6 objects.
                            show_me_what_you_got = function() {
                              browser()
                            }),
                          private = list(
                            id_not_full_trip = NULL,
                            id_not_full_trip_retained = NULL,
                            data_selected = NULL
                          ))

