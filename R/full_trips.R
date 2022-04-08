#' @name full_trips
#' @title R6 class full_trips
#' @description Create R6 reference object class full_trips
#' @importFrom R6 R6Class
#' @importFrom lubridate year hms dseconds int_length interval days as_date
#' @importFrom suncalc getSunlightTimes
#' @importFrom dplyr group_by summarise last first filter ungroup
#' @importFrom boot boot.ci
#' @importFrom ranger ranger predictions importance
#' @importFrom tidyr gather spread separate
#' @importFrom sp coordinates proj4string spTransform
#' @importFrom spdep dnearneigh nb2listw moran.mc moran.test
#' @importFrom rfUtilities multi.collinear
#' @importFrom gstat variogram
full_trips <- R6::R6Class(classname = "full_trips",
                          inherit = list_t3,
                          public = list(
                            # full trips creation ----
                            #' @description Creation of full trip item from trips.
                            #' @param object_trips Object of type R6-trips expected. A R6 reference object of class trips.
                            create_full_trips = function(object_trips) {
                              if (paste(class(object_trips),
                                        collapse = " ") != "trips list_t3 R6") {
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"object_trips\" argument.\n",
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
                                  full_trips <- append(full_trips,
                                                       list(list(object_trips$view(i)[[1]]$clone())))
                                  i <- i + 1
                                } else {
                                  for (j in i:object_trips$count()) {
                                    if (j == object_trips$count()) {
                                      full_trips_tmp <- append(full_trips_tmp,
                                                               object_trips$view(j)[[1]]$clone())
                                      full_trip_warning <- 1
                                      i <- i + 1
                                    } else {
                                      if (object_trips$view(j)[[1]]$.__enclos_env__$private$vessel_id == object_trips$view(j + 1)[[1]]$.__enclos_env__$private$vessel_id) {
                                        full_trips_tmp <- append(full_trips_tmp,
                                                                 object_trips$view(j)[[1]]$clone())
                                        if (object_trips$view(j + 1)[[1]]$.__enclos_env__$private$fish_hold_empty == 1) {
                                          full_trips_tmp <- append(full_trips_tmp,
                                                                   object_trips$view(j + 1)[[1]]$clone())
                                          i <- j + 2
                                          break ()
                                        }
                                      } else {
                                        full_trip_warning <- 1
                                        full_trips_tmp <- append(full_trips_tmp,
                                                                 object_trips$view(j)[[1]]$clone())
                                        i <- j + 1
                                        break ()
                                      }
                                    }
                                  }
                                  if (full_trip_warning == 1) {
                                    full_trip_warning <- 0
                                    private$id_not_full_trip <- append(private$id_not_full_trip,
                                                                       length(full_trips) + 1)
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: missing trip(s) in item ",
                                        length(full_trips) + 1,
                                        ".\n[trip: ",
                                        object_trips$view(j)[[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                  }
                                  full_trips <- append(full_trips,
                                                       list(full_trips_tmp))
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
                            #' @param periode_reference Object of class {\link[base]{integer}} expected. Year(s) in 4 digits format.
                            filter_by_periode = function(periode_reference) {
                              if (any(class(x = periode_reference) != "integer")) {
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"periode_reference\" argument.\n",
                                    sep = "")
                                stop()
                              } else if (any(nchar(x = periode_reference) != 4)) {
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"periode_reference\" argument.\n",
                                    sep = "")
                                stop()
                              } else {
                                for (i in seq_len(length.out = length(x = private$data))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start of full trips filtering by reference periode.\n",
                                        sep = "")
                                  }
                                  full_trips_tmp <- private$data[[i]]
                                  year_full_trips <- vector(mode = "integer")
                                  for (j in seq_len(length.out = length(x = full_trips_tmp))) {
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
                                                                    list(lapply(X = seq_len(length.out = length(x = full_trips_tmp)),
                                                                                FUN = function(list_id) {
                                                                                  full_trips_tmp[[list_id]]$clone()
                                                                                })))
                                    names(private$data_selected)[length(private$data_selected)] <- names(private$data[i])
                                  }
                                }
                                if (any(private$id_not_full_trip %in% names(private$data_selected))) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: missing trip(s) in at least one full trip item.\n",
                                      "[name(s)/id(s) of element(s): ",
                                      paste0(private$id_not_full_trip,
                                             "/",
                                             which(x = names(private$data_selected) %in% private$id_not_full_trip),
                                             collapse = ", "),
                                      "]\n",
                                      sep = "")
                                  private$id_not_full_trip_retained <- which(x = names(private$data_selected) %in% private$id_not_full_trip)
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - End of full trips filtering.\n",
                                    sep = "")
                              }
                            },
                            # add activities ----
                            #' @description Function for add activities in full trips object.
                            #' @param object_activities Object of type R6-activities expected. A R6 reference object of class activities.
                            add_activities = function(object_activities) {
                              if (length(private$data_selected) == 0) {
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Error: argument \"data_selected\" empty, ",
                                    "launch selection data before.\n",
                                    sep = "")
                                stop()
                              } else if (! any(class(x = object_activities) == "activities")) {
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"object_activities\" argument.\n",
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
                                    private$data_selected[[i]][[j]]$.__enclos_env__$private$activities <- lapply(X = seq_len(length.out = length(x = activities_tmp)),
                                                                                                                 FUN = function(list_id) {
                                                                                                                   activities_tmp[[list_id]]$clone()
                                                                                                                 })
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - End of add activity.\n",
                                    sep = "")
                              }
                            },
                            # add elementary catches ----
                            #' @description Function for add elementary catches in full trips object.
                            #' @param object_elementarycatches Object of type R6-elementarycatches expected. A R6 reference object of class elementarycatches.
                            add_elementarycatches = function(object_elementarycatches) {
                              if (length(private$data_selected) == 0) {
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Error: argument \"data_selected\" empty, ",
                                    "launch selection data before.\n",
                                    sep = "")
                                stop()
                              } else if (! any(class(object_elementarycatches) == "elementarycatches")) {
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"object_elementarycatches\" argument, ",
                                    "class elementarycatches expected.\n",
                                    sep = "")
                                stop()
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start of add elementary catches.\n",
                                        sep = "")
                                  }
                                  for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                    if (length(private$data_selected[[i]][[j]]$.__enclos_env__$private$activities) != 0) {
                                      for (k in seq_len(length.out = length(private$data_selected[[i]][[j]]$.__enclos_env__$private$activities))) {
                                        if (private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$activity_code %in% c(0, 1, 2, 14)) {
                                          activity_id <- private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$activity_id
                                          elementarycatches_tmp <- object_elementarycatches$filter_by_activity(activity_id = activity_id)
                                          private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$elementarycatches <- lapply(X = seq_len(length.out = length(x = elementarycatches_tmp)),
                                                                                                                                                                      FUN = function(list_id) {
                                                                                                                                                                        elementarycatches_tmp[[list_id]]$clone()
                                                                                                                                                                      })
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
                            #' @param object_elementarylandings Object of type R6-elementarylandings expected. A R6 reference object of class elementarylandings.
                            add_elementarylandings = function(object_elementarylandings) {
                              if (length(private$data_selected) == 0) {
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Error: argument \"data_selecetd\" empty, ",
                                    "launch selection data before.\n",
                                    sep = "")
                                stop()
                              } else if (! any(class(object_elementarylandings) == "elementarylandings")) {
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"object_elementarylandings\" argument, ",
                                    "class elementarylandings expected.\n",
                                    sep = "")
                                stop()
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start of add elementary landings.\n",
                                        sep = "")
                                  }
                                  for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                    trip_id <- private$data_selected[[i]][[j]]$.__enclos_env__$private$trip_id
                                    elementarylandings_tmp <- object_elementarylandings$filter_by_trip(trip_id = trip_id)
                                    private$data_selected[[i]][[j]]$.__enclos_env__$private$elementarylandings <- lapply(X = seq_len(length.out = length(x = elementarylandings_tmp)),
                                                                                                                         FUN = function(list_id) {
                                                                                                                           elementarylandings_tmp[[list_id]]$clone()
                                                                                                                         })
                                  }
                                }
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - End of add elementary landings.\n",
                                    sep = "")
                              }
                            },
                            # add wells and samples ----
                            #' @description Function for add wells and samples caracteristics in full trips object.
                            #' @param object_wells Object of type R6-wells expected. A R6 reference object of class wells.
                            add_wells_samples = function(object_wells) {
                              if (length(private$data_selected) == 0) {
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Error: argument \"data_selecetd\" empty, ",
                                    "launch selection data before.\n",
                                    sep = "")
                                stop()
                              } else if (! any(class(object_wells) == "wells")) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"object_wells\" argument.\n",
                                    sep = "")
                                stop()
                              } else {
                                for (i in seq_len(length.out = length(private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start of add well(s) - sample(s).\n",
                                        sep = "")
                                  }
                                  for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                    trip_id <- private$data_selected[[i]][[j]]$.__enclos_env__$private$trip_id
                                    trip_wells <- object_wells$filter_by_trip(trip_id = trip_id)
                                    private$data_selected[[i]][[j]]$.__enclos_env__$private$wells <- lapply(X = seq_len(length.out = length(x = trip_wells)),
                                                                                                            FUN = function(list_id) {
                                                                                                              trip_wells[[list_id]]$clone()
                                                                                                            })
                                  }
                                }
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - End of add well(s) - sample(s).\n",
                                    sep = "")
                              }
                            },
                            # process 1.1: rf1 ----
                            #' @description Process of Raising Factor level 1 (RF1).
                            #' @param species_rf1 Object of type \code{\link[base]{integer}} expected. Specie(s) code(s) used for the RF1 process. By default 1 (YFT), 2 (SKJ), 3 (BET), 4 (ALB), 9 (MIX) and 11 (LOT).
                            #' @param rf1_lowest_limit Object of type \code{\link[base]{numeric}} expected. Verification value for the lowest limit of the RF1. By default 0.8.
                            #' @param rf1_highest_limit Object of type \code{\link[base]{numeric}} expected. Verification value for the highest limit of the RF1. By default 1.2.
                            rf1 = function(species_rf1 = as.integer(c(1, 2, 3, 4, 9, 11)),
                                           rf1_lowest_limit = 0.8,
                                           rf1_highest_limit = 1.2) {
                              # function parameters verification ----
                              if (any(class(x = species_rf1) != "integer")) {
                                cat(format(x = Sys.time(),
                                           format = "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"species_rf1\" argument, ",
                                    "class integer expected.\n",
                                    sep = "")
                                stop()
                              } else if (length(x = class(x = rf1_lowest_limit)) != 1
                                         || class(x = rf1_lowest_limit) != "numeric") {
                                cat(format(x = Sys.time(),
                                           format = "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"rf1_lowest_limit\" argument, ",
                                    "class numeric with one value expected.\n",
                                    sep = "")
                                stop()
                              } else if (length(x = class(rf1_highest_limit)) != 1
                                         || class(x = rf1_highest_limit) != "numeric") {
                                cat(format(x = Sys.time(),
                                           format = "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"rf1_highest_limit\" argument, ",
                                    "class numeric with one value expected.\n",
                                    sep = "")
                                stop()
                              } else {
                                if (is.null(x = private$data_selected)) {
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Empty data selected in the R6 object.\n",
                                      " - Process 1.1 (Raising Factor level 1) cancelled.\n",
                                      sep = "")
                                } else {
                                  for (i in seq_len(length.out = length(x = private$data_selected))) {
                                    if (i == 1) {
                                      cat(format(x = Sys.time(),
                                                 format = "%Y-%m-%d %H:%M:%S"),
                                          " - Start process 1.1: Raising Factor level 1.\n",
                                          sep = "")
                                    }
                                    if (names(x = private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                      cat(format(x = Sys.time(),
                                                 format = "%Y-%m-%d %H:%M:%S"),
                                          " - Warning: missing trip(s) in full trip element \"",
                                          names(x = private$data_selected)[i],
                                          "\".\n",
                                          sep = "")
                                      stop <- 0
                                      for (k in seq_len(length.out = length(x = private$data_selected[[i]]))) {
                                        # case 1.1 ----
                                        # at least one logbook is missing in not complete full trip item
                                        if (k == 1) {
                                          logbook_availability <- vector(mode = "integer")
                                        }
                                        current_trip <- private$data_selected[[i]][[k]]
                                        logbook_availability <- append(logbook_availability,
                                                                       current_trip$.__enclos_env__$private$logbook_availability)
                                        if (k == length(x = private$data_selected[[i]])) {
                                          if (any(logbook_availability) == 0) {
                                            cat(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - Warning: missing logbook in trip element \"",
                                                names(x = private$data_selected)[i],
                                                "\".\n",
                                                "[trip: ",
                                                current_trip$.__enclos_env__$private$trip_id,
                                                "]\n",
                                                sep = "")
                                            for (l in seq_len(length.out = length(x = private$data_selected[[i]]))) {
                                              current_trip <- private$data_selected[[i]][[l]]
                                              current_trip$.__enclos_env__$private$rf1 <- NA
                                              current_trip$.__enclos_env__$private$statut_rf1 <- 1.1
                                            }
                                            stop <- 1
                                          }
                                        }
                                      }
                                      if (stop != 1) {
                                        for (m in seq_len(length.out = length(x = private$data_selected[[i]]))) {
                                          if (m == 1) {
                                            current_elementarycatches <- NULL
                                          }
                                          current_trip <- private$data_selected[[i]][[m]]
                                          if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                            for (w in seq_len(length.out = length(x = current_trip$.__enclos_env__$private$activities))) {
                                              current_elementarycatches <- append(current_elementarycatches,
                                                                                  current_trip$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches)
                                            }
                                          }
                                        }
                                        if (is.null(x = current_elementarycatches)) {
                                          # case 1.2 ----
                                          # trips with no catches (for example route or support) in not complete full trip item
                                          for (n in seq_len(length.out = length(private$data_selected[[i]]))) {
                                            current_trip <- private$data_selected[[i]][[n]]
                                            current_trip$.__enclos_env__$private$rf1 <- NA
                                            current_trip$.__enclos_env__$private$statut_rf1 <- 1.2
                                          }
                                        } else {
                                          for (p in seq_len(length.out = length(private$data_selected[[i]]))) {
                                            if (p == 1) {
                                              current_elementarylandings <- NULL
                                              stop_bis <- 0
                                            }
                                            current_trip <- private$data_selected[[i]][[p]]
                                            if (p == length(x = private$data_selected[[i]])) {
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
                                              # case 1.3 ----
                                              # at least one elementary landing is missing in not complete full trip item
                                              cat(format(x = Sys.time(),
                                                         format = "%Y-%m-%d %H:%M:%S"),
                                                  " - Warning: missing elementary landing in trip element \"",
                                                  names(x = private$data_selected)[i],
                                                  "\".\n",
                                                  "[trip: ",
                                                  current_trip$.__enclos_env__$private$trip_id,
                                                  "]\n",
                                                  sep = "")
                                              for (q in seq_len(length.out = length(x = private$data_selected[[i]]))) {
                                                current_trip <- private$data_selected[[i]][[q]]
                                                current_trip$.__enclos_env__$private$rf1 <- NA
                                                current_trip$.__enclos_env__$private$statut_rf1 <- 1.3
                                              }
                                            } else {
                                              # case 1.4 ----
                                              # almost rocks dude ! (not complete full trip item)
                                              for (s in seq_len(length.out = length(x = private$data_selected[[i]]))) {
                                                current_trip <- private$data_selected[[i]][[s]]
                                                current_trip$.__enclos_env__$private$rf1 <- NA
                                                current_trip$.__enclos_env__$private$statut_rf1 <- 1.4
                                              }
                                            }
                                          }
                                        }
                                      }
                                    } else {
                                      stop <- 0
                                      for (k in seq_len(length.out = length(x = private$data_selected[[i]]))) {
                                        # case 2.1 ----
                                        # at least one logbook is missing in complete full trip item
                                        if (k == 1) {
                                          logbook_availability <- vector(mode = "integer")
                                        }
                                        current_trip <- private$data_selected[[i]][[k]]
                                        logbook_availability <- append(logbook_availability,
                                                                       current_trip$.__enclos_env__$private$logbook_availability)
                                        if (k == length(x = private$data_selected[[i]])) {
                                          if (any(logbook_availability) == 0) {
                                            cat(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - Warning: missing logbook in trip element \"",
                                                names(x = private$data_selected)[i],
                                                "\".\n"
                                                ,"[trip: ",
                                                current_trip$.__enclos_env__$private$trip_id,
                                                "]\n",
                                                sep = "")
                                            for (l in seq_len(length.out = length(x = private$data_selected[[i]]))) {
                                              current_trip <- private$data_selected[[i]][[l]]
                                              current_trip$.__enclos_env__$private$rf1 <- 1
                                              current_trip$.__enclos_env__$private$statut_rf1 <- 2.1
                                            }
                                            stop <- 1
                                          }
                                        }
                                      }
                                      if (stop != 1) {
                                        current_elementarycatches <- NULL
                                        for (m in seq_len(length.out = length(x = private$data_selected[[i]]))) {
                                          current_trip <- private$data_selected[[i]][[m]]
                                          if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                            for (w in seq_len(length.out = length(x = current_trip$.__enclos_env__$private$activities))) {
                                              current_elementarycatches <- append(current_elementarycatches,
                                                                                  current_trip$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches)
                                            }
                                          }
                                        }
                                        if (is.null(x = current_elementarycatches)) {
                                          # case 2.2 ----
                                          # trips with no catches (for example route or support) in complete full trip item
                                          for (n in seq_len(length.out = length(x = private$data_selected[[i]]))) {
                                            current_trip <- private$data_selected[[i]][[n]]
                                            current_trip$.__enclos_env__$private$rf1 <- 1
                                            current_trip$.__enclos_env__$private$statut_rf1 <- 2.2
                                          }
                                        } else {
                                          current_elementarycatches_weight <- vector(mode = "numeric")
                                          for (o in seq_len(length.out = length(x = current_elementarycatches))) {
                                            if (current_elementarycatches[[o]]$.__enclos_env__$private$specie_code %in% species_rf1) {
                                              current_elementarycatches_weight <- append(current_elementarycatches_weight,
                                                                                         current_elementarycatches[[o]]$.__enclos_env__$private$catch_weight)
                                            }
                                          }
                                          current_elementarylandings <- NULL
                                          for (p in seq_len(length.out = length(x = private$data_selected[[i]]))) {
                                            current_trip <- private$data_selected[[i]][[p]]
                                            current_elementarylandings <- append(current_elementarylandings,
                                                                                 unlist(current_trip$.__enclos_env__$private$elementarylandings))
                                          }
                                          if (is.null(x = current_elementarylandings)) {
                                            # case 2.3 ----
                                            # no elementary landing in complete full trip item
                                            cat(format(x = Sys.time(),
                                                       format = "%Y-%m-%d %H:%M:%S"),
                                                " - Warning: missing elementary landing in trip element \"",
                                                names(x = private$data_selected)[i],
                                                "\".\n",
                                                "[trip: ",
                                                current_trip$.__enclos_env__$private$trip_id,
                                                "]\n",
                                                sep = "")
                                            for (q in seq_len(length.out = length(x = private$data_selected[[i]]))) {
                                              current_trip <- private$data_selected[[i]][[q]]
                                              current_trip$.__enclos_env__$private$rf1 <- 1
                                              current_trip$.__enclos_env__$private$statut_rf1 <- 2.3
                                            }
                                          } else {
                                            # case 2.4 ----
                                            # everything rocks dude !
                                            current_elementarylandings_weight <- vector(mode = "numeric")
                                            for (r in seq_len(length.out = length(x = current_elementarylandings))) {
                                              if (current_elementarylandings[[r]]$.__enclos_env__$private$specie_code %in% species_rf1) {
                                                current_elementarylandings_weight <- append(current_elementarylandings_weight,
                                                                                            current_elementarylandings[[r]]$.__enclos_env__$private$landing_weight)
                                              }
                                            }
                                            current_rf1 <- sum(current_elementarylandings_weight) / sum(current_elementarycatches_weight)
                                            if (current_rf1 < rf1_lowest_limit
                                                | current_rf1 > rf1_highest_limit) {
                                              cat(format(x = Sys.time(),
                                                         format = "%Y-%m-%d %H:%M:%S"),
                                                  " - Warning: rf1 value of trip element \"",
                                                  names(x = private$data_selected)[i],
                                                  "\" out of theorical boundaries: ",
                                                  round(x = current_rf1,
                                                        digits = 3),
                                                  ".\n",
                                                  "[trip: ",
                                                  current_trip$.__enclos_env__$private$trip_id,
                                                  "]\n",
                                                  sep = "")
                                            }
                                            for (s in seq_len(length.out = length(x = private$data_selected[[i]]))) {
                                              current_trip <- private$data_selected[[i]][[s]]
                                              current_trip$.__enclos_env__$private$rf1 <- current_rf1
                                              current_trip$.__enclos_env__$private$statut_rf1 <- 2.4
                                            }
                                          }
                                        }
                                      }
                                    }
                                    # assign rf1 to elementary catches ----
                                    for (u in seq_len(length.out = length(x = private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[u]]
                                      current_rf1 <- current_trip$.__enclos_env__$private$rf1
                                      if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                        for (x in seq_len(length.out = length(x = current_trip$.__enclos_env__$private$activities))) {
                                          current_elementarycatches <- current_trip$.__enclos_env__$private$activities[[x]]$.__enclos_env__$private$elementarycatches
                                          if (! is.null(x = current_elementarycatches)) {
                                            for (v in seq_len(length.out = length(x = current_elementarycatches))) {
                                              current_elementarycatches[[v]]$.__enclos_env__$private$catch_weight_rf1 <- current_elementarycatches[[v]]$.__enclos_env__$private$catch_weight * current_rf1
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  cat(format(x = Sys.time(),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                      " - Successful process 1.1: Raising Factor level 1.\n",
                                      sep = "")
                                }
                              }
                            },
                            # process 1.2: rf2 ----
                            #' @description Process of Raising Factor level 2 (rf2).
                            rf2 = function() {
                              if (is.null(x = private$data_selected)) {
                                cat(format(x = Sys.time(),
                                           format = "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 1.2 (raising factor level 2) cancelled.\n",
                                    sep = "")
                              } else {
                                for (i in seq_len(length.out = length(x = private$data_selected))) {
                                  if (i == 1) {
                                    cat(format(x = Sys.time(),
                                               format = "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 1.2: raising factor level 2.\n",
                                        sep = "")
                                  }
                                  if (is.null(x = private$data_selected[[i]][[1]]$.__enclos_env__$private$statut_rf1)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Error: rf1 is null for the item \"",
                                        names(private$data_selected)[i],
                                        "\".\n",
                                        "Check if the process 1.1 (raising factor level 1) was successfully applied.\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    stop()
                                  } else {
                                    if (private$data_selected[[i]][[1]]$.__enclos_env__$private$statut_rf1 == 2.1) {
                                      # case 1 ----
                                      # rf2 calculated
                                      cat(format(x = Sys.time(),
                                                 format = "%Y-%m-%d %H:%M:%S"),
                                          " - Error: rf2 not developped yet.\n",
                                          sep = "")
                                      stop()
                                    } else {
                                      if (private$data_selected[[i]][[1]]$.__enclos_env__$private$statut_rf1 %in% c(2.2, 2.3, 2.4)) {
                                        # case 2 ----
                                        # rf2 not need to be calculated
                                        for (j in seq_len(length.out = length(x = private$data_selected[[i]]))) {
                                          current_trip <- private$data_selected[[i]][[j]]
                                          current_rf2 <- 1
                                          current_trip$.__enclos_env__$private$rf2 <- current_rf2
                                          current_trip$.__enclos_env__$private$statut_rf2 <- 2
                                          current_elementarycatches <- NULL
                                          if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                            for (m in seq_len(length.out = length(x = current_trip$.__enclos_env__$private$activities))) {
                                              current_elementarycatches <- append(current_elementarycatches,
                                                                                  current_trip$.__enclos_env__$private$activities[[m]]$.__enclos_env__$private$elementarycatches)
                                            }
                                          }
                                          if (length(x = current_elementarycatches) != 0) {
                                            for (k in seq_len(length.out = length(x = current_elementarycatches))) {
                                              current_elementarycatches[[k]]$.__enclos_env__$private$catch_weight_rf2 <- current_elementarycatches[[k]]$.__enclos_env__$private$catch_weight_rf1
                                            }
                                          }
                                        }
                                      } else {
                                        # case 3 ----
                                        # full trip not complete
                                        for (l in seq_len(length.out = length(x = private$data_selected[[i]]))) {
                                          current_trip <- private$data_selected[[i]][[l]]
                                          current_rf2 <- 1
                                          current_trip$.__enclos_env__$private$rf2 <- NA
                                          current_trip$.__enclos_env__$private$statut_rf2 <- 3
                                          current_elementarycatches <- NULL
                                          if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                            for (m in seq_len(length.out = length(x = current_trip$.__enclos_env__$private$activities))) {
                                              current_elementarycatches <- append(current_elementarycatches,
                                                                                  current_trip$.__enclos_env__$private$activities[[m]]$.__enclos_env__$private$elementarycatches)
                                            }
                                          }
                                          if (length(x = current_elementarycatches) != 0) {
                                            for (k in seq_len(length.out = length(x = current_elementarycatches))) {
                                              current_elementarycatches[[k]]$.__enclos_env__$private$catch_weight_rf2 <- current_elementarycatches[[k]]$.__enclos_env__$private$catch_weight_rf1
                                            }
                                          }
                                        }
                                      }
                                    }
                                    if (i == length(x = private$data_selected)) {
                                      cat(format(x = Sys.time(),
                                                 format = "%Y-%m-%d %H:%M:%S"),
                                          " - End of raising factor process 2.\n",
                                          sep = "")
                                    }
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
                                    capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[i]]),
                                                   file = "NUL")
                                    capture.output(current_activities <- t3::object_r6(class_name = "activities"),
                                                   file = "NUL")
                                    capture.output(current_activities$add(new_item = unlist(current_trips$extract_l1_element_value(element = "activities"))),
                                                   file = "NUL")
                                    capture.output(current_elementarycatches <- t3::object_r6(class_name = "elementarycatches"),
                                                   file = "NUL")
                                    capture.output(current_elementarycatches$add(new_item = unlist(current_activities$extract_l1_element_value(element = "elementarycatches"))),
                                                   file = "NUL")
                                    current_elementarycatches$modification_l1(modification = "$path$corrected_logbook_category <- NA")
                                    current_elementarycatches$modification_l1(modification = "$path$catch_weight_category_corrected <- NA")
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.3 on item \"",
                                        names(private$data_selected)[i],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    if (is.null(private$data_selected[[i]][[1]]$.__enclos_env__$private$rf2)) {
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - Warning: rf2 is null for the item \"",
                                          names(private$data_selected)[i],
                                          "\".\n",
                                          "Check if the process 1.2 (raising factor level 2) was successfully applied.\n",
                                          "[trip: ",
                                          private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                          "]\n",
                                          sep = "")
                                      stop()
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
                                            if (current_elementarycatches[[n]]$.__enclos_env__$private$logbook_category == 9
                                                & current_elementarycatches[[n]]$.__enclos_env__$private$specie_code3l %in% c("YFT", "BET", "ALB", "SKJ")) {
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
                                        " - Process 1.3 successfull on item \"",
                                        names(private$data_selected)[i],
                                        "\".\n",
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
                                    capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[i]]),
                                                   file = "NUL")
                                    capture.output(current_activities <- t3::object_r6(class_name = "activities"),
                                                   file = "NUL")
                                    capture.output(current_activities$add(new_item = unlist(current_trips$extract_l1_element_value(element = "activities"))),
                                                   file = "NUL")
                                    current_activities$modification_l1(modification = "$path$positive_set_count <- NA")
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.4 on item \"",
                                        names(private$data_selected)[i],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[j]]
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        for (k in seq_len(length.out = length(current_trip$.__enclos_env__$private$activities))) {
                                          current_activity <- current_trip$.__enclos_env__$private$activities[[k]]
                                          if (current_trip$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$activity_code %in% c(0, 1, 2, 14)) {
                                            capture.output(current_elementarycatches <- t3::object_r6(class_name = "elementarycatches"),
                                                           file = "NUL")
                                            if (length(current_activity$.__enclos_env__$private$elementarycatches) != 0) {
                                              capture.output(current_elementarycatches$add(new_item = current_activity$.__enclos_env__$private$elementarycatches),
                                                             file = "NUL")
                                              catch_weight_category_corrected <- sum(sapply(X = seq_len(length.out = current_elementarycatches$count()),
                                                                                            FUN = function(l) {
                                                                                              if (is.null(current_elementarycatches$extract(id = l)[[1]]$.__enclos_env__$private$catch_weight_category_corrected)) {
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
                                                                                                current_elementarycatches$extract(id = l)[[1]]$.__enclos_env__$private$catch_weight_category_corrected
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
                                          } else {
                                            current_activity$.__enclos_env__$private$positive_set_count <- NA
                                          }
                                        }
                                      }
                                    }
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Process 1.4 successfull on item \"",
                                      names(private$data_selected)[i],
                                      "\".\n",
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
                            #' @param set_duration_ref Object of type \code{\link[base]{data.frame}} expected. Data and parameters for set duration calculation (by year, country, ocean and school type).
                            set_duration = function(set_duration_ref) {
                              if (length(class(set_duration_ref)) != 1
                                  || class(set_duration_ref) != "data.frame"
                                  || dim(set_duration_ref)[2] != 7
                                  || dim(set_duration_ref)[1] < 1) {
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
                                    capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[i]]),
                                                   file = "NUL")
                                    capture.output(current_activities <- t3::object_r6(class_name = "activities"),
                                                   file = "NUL")
                                    capture.output(current_activities$add(new_item = unlist(current_trips$extract_l1_element_value(element = "activities"))),
                                                   file = "NUL")
                                    current_activities$modification_l1(modification = "$path$set_duration <- NA")
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.5 on item \"",
                                        names(private$data_selected)[i],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[j]]
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        capture.output(current_activities <- t3::object_r6(class_name = "activities"),
                                                       file = "NUL")
                                        capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                                                       file = "NUL")
                                        for (k in seq_len(length.out = current_activities$count())) {
                                          current_activity <- current_activities$extract(id = k)[[1]]
                                          # for activity declared as null set (0), positive set (1), unknown set (2) or pocket capsizing (14)
                                          if (current_trip$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$activity_code %in% c(0, 1, 2, 14)) {
                                            if (dim(set_duration_ref[set_duration_ref$year == lubridate::year(current_activity$.__enclos_env__$private$activity_date)
                                                                     & set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                     & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type
                                                                     & set_duration_ref$country == current_trip$.__enclos_env__$private$fleet, ])[1] != 1) {
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
                                              if (length(current_activity$.__enclos_env__$private$elementarycatches) != 0) {
                                                capture.output(current_elementarycatches <- t3::object_r6(class_name = "elementarycatches"),
                                                               file = "NUL")
                                                capture.output(current_elementarycatches$add(new_item = current_activity$.__enclos_env__$private$elementarycatches),
                                                               file = "NUL")
                                                catch_weight_category_corrected <- sum(sapply(X = seq_len(length.out = current_elementarycatches$count()),
                                                                                              FUN = function(l) {
                                                                                                if (is.null(current_elementarycatches$extract(id = l)[[1]]$.__enclos_env__$private$catch_weight_category_corrected)) {
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
                                                                                                  current_elementarycatches$extract(id = l)[[1]]$.__enclos_env__$private$catch_weight_category_corrected
                                                                                                }
                                                                                              }))
                                                parameter_a <- set_duration_ref[set_duration_ref$year == lubridate::year(current_activity$.__enclos_env__$private$activity_date)
                                                                                & set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                                & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type
                                                                                & set_duration_ref$country == current_trip$.__enclos_env__$private$fleet, "parameter_a"]
                                                parameter_b <- set_duration_ref[set_duration_ref$year == lubridate::year(current_activity$.__enclos_env__$private$activity_date)
                                                                                & set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                                & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type
                                                                                & set_duration_ref$country == current_trip$.__enclos_env__$private$fleet, "parameter_b"]
                                                current_activity$.__enclos_env__$private$set_duration <- parameter_a * catch_weight_category_corrected + parameter_b
                                              } else {
                                                if (current_activity$.__enclos_env__$private$activity_code == 1) {
                                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                      " - Error: set declared as positive but without elementary catch.",
                                                      "\n[trip: ",
                                                      current_trip$.__enclos_env__$private$trip_id,
                                                      ", activity: ",
                                                      current_activity$.__enclos_env__$private$activity_id,
                                                      "]\n",
                                                      sep = "")
                                                  stop()
                                                } else {
                                                  current_activity$.__enclos_env__$private$set_duration <- set_duration_ref[set_duration_ref$year == lubridate::year(current_activity$.__enclos_env__$private$activity_date)
                                                                                                                            & set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                                                                            & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type
                                                                                                                            & set_duration_ref$country == current_trip$.__enclos_env__$private$fleet, "null_set_value"]
                                                }
                                              }
                                            }
                                          } else {
                                            current_activity$.__enclos_env__$private$set_duration <- NA
                                          }
                                        }
                                      }
                                    }
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Process 1.5 successfull on item \"",
                                      names(private$data_selected)[i],
                                      "\".\n",
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
                                  capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                                                 file = "NUL")
                                  capture.output(current_trips$add(new_item = private$data_selected[[i]]),
                                                 file = "NUL")
                                  current_trips$modification_l1(modification = "$path$time_at_sea <- NA")
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 1.6 on item \"",
                                      names(private$data_selected)[i],
                                      "\".\n",
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
                                    if (time_departure_date > lubridate::dseconds(x = 0)
                                        & time_landing_date > lubridate::dseconds(x = 0)) {
                                      # we have time for departure_date and landing_date
                                      time_at_sea <- lubridate::int_length(lubridate::interval(start = departure_date,
                                                                                               end = landing_date)) / 3600
                                    } else {
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        capture.output(current_activities <- t3::object_r6(class_name = "activities"),
                                                       file = "NUL")
                                        capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                                                       file = "NUL")
                                        if (length(current_activities$filter_l1(filter = paste0("$path$activity_date == \"",
                                                                                                departure_date,
                                                                                                "\""))) != 0) {
                                          capture.output(current_activities_departure_date <- t3::object_r6(class_name = "activities"),
                                                         file = "NUL")
                                          capture.output(current_activities_departure_date$add(new_item = current_activities$filter_l1(filter = paste0("$path$activity_date == \"",
                                                                                                                                                       departure_date,
                                                                                                                                                       "\""))),
                                                         file = "NUL")
                                          current_activities_departure_date_time_at_sea <- sum(unlist(current_activities_departure_date$extract_l1_element_value(element = "time_at_sea")))
                                        } else {
                                          current_activities_departure_date_time_at_sea <- 0
                                        }
                                        if (length(current_activities$filter_l1(filter = paste0("$path$activity_date == \"",
                                                                                                landing_date,
                                                                                                "\""))) != 0) {
                                          capture.output(current_activities_landing_date <- t3::object_r6(class_name = "activities"),
                                                         file = "NUL")
                                          capture.output(current_activities_landing_date$add(new_item = current_activities$filter_l1(filter = paste0("$path$activity_date == \"",
                                                                                                                                                     landing_date,
                                                                                                                                                     "\""))),
                                                         file = "NUL")
                                          current_activities_landing_date_time_at_sea <- sum(unlist(current_activities_landing_date$extract_l1_element_value(element = "time_at_sea")))
                                        } else {
                                          current_activities_landing_date_time_at_sea <- 0
                                        }
                                        time_at_sea_tmp <- lubridate::int_length(lubridate::interval(start = departure_date + lubridate::days(x = 1),
                                                                                                     end = landing_date - lubridate::days(x = 1)))
                                        time_at_sea <- (time_at_sea_tmp
                                                        + lubridate::dhours(x = current_activities_departure_date_time_at_sea)
                                                        + lubridate::dhours(x = current_activities_landing_date_time_at_sea))
                                        time_at_sea <- time_at_sea@.Data
                                      } else {
                                        time_at_sea <- lubridate::int_length(lubridate::interval(start = departure_date + lubridate::days(x = 1),
                                                                                                 end = landing_date - lubridate::days(x = 1)))
                                      }
                                    }
                                    current_trip$.__enclos_env__$private$time_at_sea <- time_at_sea / 3600
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Process 1.6 successfull on item \"",
                                    names(private$data_selected)[i],
                                    "\".\n",
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
                            #' @param sunrise_schema Object of class {\link[base]{character}} expected. Sunrise caracteristic. By default "sunrise" (top edge of the sun appears on the horizon). See below for more details.
                            #' @param sunset_schema Object of class {\link[base]{character}} expected. Sunset caracteristic. By default "sunset" (sun disappears below the horizon, evening civil twilight starts). See below for more details.
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
                                    capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[i]]),
                                                   file = "NUL")
                                    current_trips$modification_l1(modification = "$path$fishing_time <- NA")
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.7 on item \"",
                                        names(private$data_selected)[i],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[j]]
                                      fishing_time <- 0
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        capture.output(current_activities <- t3::object_r6(class_name = "activities"),
                                                       file = "NUL")
                                        capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                                                       file = "NUL")
                                        activities_dates <- current_activities$extract_l1_element_value(element = "activity_date")
                                        activities_dates <- unique(do.call(what = "c",
                                                                           args = activities_dates))
                                        activities_dates <- sort(x = activities_dates)
                                        for (l in seq_len(length.out = length(activities_dates))) {
                                          activities_date <- activities_dates[[l]]
                                          capture.output(current_activities_date <- t3::object_r6(class_name = "activities"),
                                                         file = "NUL")
                                          capture.output(current_activities_date$add(new_item = current_activities$filter_l1(filter = paste0("$path$activity_date == \"",
                                                                                                                                             activities_date,
                                                                                                                                             "\""))),
                                                         file = "NUL")
                                          current_activities_code <- unique(unlist(current_activities_date$extract_l1_element_value(element = "activity_code")))
                                          if (any(! current_activities_code %in% c(4, 7, 10, 15, 100))) {
                                            current_activities_latitudes <- unlist(current_activities_date$extract_l1_element_value(element = "activity_latitude"))
                                            current_activities_longitudes <- unlist(current_activities_date$extract_l1_element_value(element = "activity_longitude"))
                                            latitude_mean <- mean(x = current_activities_latitudes)
                                            longitude_mean <- mean(x = current_activities_longitudes)
                                            current_sunrise <- suncalc::getSunlightTimes(date = activities_date,
                                                                                         lat = latitude_mean,
                                                                                         lon = longitude_mean)[[sunrise_schema]]
                                            current_sunset <- suncalc::getSunlightTimes(date = activities_date,
                                                                                        lat = latitude_mean,
                                                                                        lon = longitude_mean)[[sunset_schema]]
                                            fishing_time_tmp <- lubridate::int_length(lubridate::interval(start = current_sunrise,
                                                                                                          end = current_sunset))
                                            fishing_time <- fishing_time + fishing_time_tmp
                                          }
                                        }
                                      }
                                      current_trip$.__enclos_env__$private$fishing_time <- fishing_time / 3600
                                    }
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Process 1.7 successfull on item \"",
                                      names(private$data_selected)[i],
                                      "\".\n",
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
                                    capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[i]]),
                                                   file = "NUL")
                                    current_trips$modification_l1(modification = "$path$searching_time <- NA")
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 1.8 on item \"",
                                        names(private$data_selected)[i],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (j in seq_len(length.out = length(private$data_selected[[i]]))) {
                                      current_trip <- private$data_selected[[i]][[j]]
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        capture.output(current_activities <- t3::object_r6(class_name = "activities"),
                                                       file = "NUL")
                                        capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                                                       file = "NUL")
                                        activities_set_duration <- unlist(current_activities$extract_l1_element_value(element = "set_duration"))
                                        if (any(is.null(activities_set_duration))) {
                                          cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                              " - Error: run process 1.5 (set duration calculation) before this process.\n",
                                              sep = "")
                                          stop()
                                        } else {
                                          sum_activities_set_duration <- sum(activities_set_duration,
                                                                             na.rm = TRUE)
                                        }
                                        if (is.null(current_trip$.__enclos_env__$private$fishing_time)) {
                                          cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                              " - Error: run process 1.7 (fishing time calculation) before this process.\n",
                                              sep = "")
                                          stop()
                                        } else {
                                          current_fishing_time <- current_trip$.__enclos_env__$private$fishing_time
                                          searching_time <- lubridate::dhours(x = current_fishing_time) - lubridate::dminutes(x = sum_activities_set_duration)
                                          searching_time <- searching_time@.Data
                                        }
                                      } else {
                                        searching_time <- 0
                                      }
                                      current_trip$.__enclos_env__$private$searching_time <- searching_time / 3600
                                    }
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Process 1.8 successfull on item \"",
                                      names(private$data_selected)[i],
                                      "\".\n",
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
                            # process 2.1: sample length class conversion ld1 to lf ----
                            #' @description Process for length conversion, if necessary, in length fork (lf). Furthermore, variable "sample_number_measured_extrapolated" of process 2.1 will converse in variable "sample_number_measured_extrapolated_lf" (Notably due to the creation of new lf classes during some conversions).
                            #' @param length_step Object of type \code{\link[base]{data.frame}} expected. Data frame object with length ratio between ld1 and lf class.
                            sample_length_class_ld1_to_lf =  function(length_step) {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 2.1 (sample length class conversion ld1 to lf) cancelled.\n",
                                    sep = "")
                              } else {
                                if (! paste0(class(length_step),
                                             collapse = "_") %in% c("data.frame",
                                                                    "tbl_df_tbl_data.frame")
                                    || ncol(length_step) != 6
                                    || nrow(length_step) == 0) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - invalid \"length_step\" argument, class \"data.frame\" or \"tibble\" with 6 columns and at least 1 row expected.\n",
                                      sep = "")
                                  stop()
                                } else {
                                  length_step_count <- length_step %>%
                                    dplyr::group_by(ocean,
                                                    specie_code,
                                                    specie_code3l,
                                                    ld1_class) %>%
                                    dplyr::summarise(nb = dplyr::n(),
                                                     .groups = "drop")
                                }
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.1: sample length class conversion ld1 to lf.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                      capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                     file = "NUL")
                                      capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                     file = "NUL")
                                      if (length(x = unlist(current_wells$extract_l1_element_value(element = "elementarysampleraw"))) != 0) {
                                        capture.output(current_elementarysamplesraw <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                       file = "NUL")
                                        capture.output(current_elementarysamplesraw$add(new_item = unlist(current_wells$extract_l1_element_value(element = "elementarysampleraw"))),
                                                       file = "NUL")
                                        current_elementarysamplesraw$modification_l1(modification = "$path$sample_length_class_lf <- as.integer(NA)")
                                        current_elementarysamplesraw$modification_l1(modification = "$path$sample_number_measured_lf <- NA")
                                      }
                                    }
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.1 on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (partial_trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        capture.output(current_activities <- t3::object_r6(class_name = "activities"),
                                                       file = "NUL")
                                        capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                                                       file = "NUL")
                                        if (length(current_trip$.__enclos_env__$private$wells) != 0) {
                                          capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                         file = "NUL")
                                          capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                         file = "NUL")
                                          if (length(x = current_activities$filter_l1(filter = "length($path$elementarycatches) != 0")) != 0) {
                                            capture.output(current_activities_with_elementarycatches <- t3::object_r6(class_name = "activities"),
                                                           file = "NUL")
                                            capture.output(current_activities_with_elementarycatches$add(new_item = current_activities$filter_l1(filter = "length($path$elementarycatches) != 0")),
                                                           file = "NUL")
                                            oceans_activities <- unique(unlist(current_activities_with_elementarycatches$extract_l1_element_value(element = "ocean")))
                                            if (length(oceans_activities) != 1) {
                                              capture.output(current_elementary_catches <- t3::object_r6(class_name = "elementarycatches"),
                                                             file = "NUL")
                                              capture.output(current_elementary_catches$add(new_item = unlist(current_activities_with_elementarycatches$extract_l1_element_value(element = "elementarycatches"))),
                                                             file = "NUL")
                                              total_current_elementary_catches <- sum(unlist(current_elementary_catches$extract_l1_element_value(element = "catch_weight_category_corrected")))
                                              oceans_activities_weight <- as.numeric()
                                              for (current_ocean_activites in oceans_activities) {
                                                capture.output(current_elementary_catches_ocean <- t3::object_r6(class_name = "activities"),
                                                               file = "NUL")
                                                capture.output(current_elementary_catches_ocean$add(new_item = current_elementary_catches$filter_l1(filter = paste0("$path$ocean == ",
                                                                                                                                                                    current_ocean_activites))),
                                                               file = "NUL")
                                                current_oceans_activities_weight <- sum(unlist(current_elementary_catches_ocean$extract_l1_element_value(element = "catch_weight_category_corrected"))) / total_current_elementary_catches
                                                oceans_activities_weight <- append(oceans_activities_weight,
                                                                                   current_oceans_activities_weight)
                                                names(oceans_activities_weight)[length(oceans_activities_weight)] <- current_ocean_activites
                                              }
                                              major_ocean_activities <- as.integer(names(which(x = oceans_activities_weight == max(oceans_activities_weight))))
                                            } else {
                                              major_ocean_activities <- oceans_activities
                                            }
                                            for (well_id in seq_len(length.out = current_wells$count())) {
                                              current_well <- current_wells$extract(id = well_id)[[1]]
                                              if (length(current_well$.__enclos_env__$private$elementarysampleraw) != 0) {
                                                capture.output(current_samples <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                               file = "NUL")
                                                capture.output(current_samples$add(new_item = current_well$.__enclos_env__$private$elementarysampleraw),
                                                               file = "NUL")
                                                current_samples_removed <- as.integer()
                                                for (sample_id in seq_len(length.out = current_samples$count())) {
                                                  elementary_sample_skj_removed <- as.integer()
                                                  capture.output(current_sample <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                                 file = "NUL")
                                                  capture.output(current_sample$add(new_item = current_samples$extract(id = sample_id)),
                                                                 file = "NUL")
                                                  if (length(current_sample$filter_l1(filter = "$path$length_type == 2")) != 0) {
                                                    capture.output(current_sample_length_type_2 <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                                   file = "NUL")
                                                    capture.output(current_sample_length_type_2$add(new_item = current_sample$filter_l1(filter = "$path$length_type == 2")),
                                                                   file = "NUL")
                                                    current_sample_length_type_2$modification_l1(modification = "$path$sample_length_class_lf = as.integer($path$sample_length_class)")
                                                    current_sample_length_type_2$modification_l1(modification = "$path$sample_number_measured_lf = $path$sample_number_measured")
                                                  }
                                                  if (length(current_sample$filter_l1(filter = "$path$length_type == 1")) != 0) {
                                                    capture.output(current_sample_length_type_1 <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                                   file = "NUL")
                                                    capture.output(current_sample_length_type_1$add(new_item = current_sample$filter_l1(filter = "$path$length_type == 1")),
                                                                   file = "NUL")
                                                    for (elementarysampleraw_id in seq_len(length.out = current_sample_length_type_1$count())) {
                                                      current_elementary_sample <- current_sample_length_type_1$extract(id = elementarysampleraw_id)[[1]]
                                                      current_length_step_count <- as.numeric(length_step_count[length_step_count$ocean == major_ocean_activities
                                                                                                                & length_step_count$specie_code == current_elementary_sample$.__enclos_env__$private$specie_code
                                                                                                                & length_step_count$ld1_class == current_elementary_sample$.__enclos_env__$private$sample_length_class, "nb"])
                                                      if (is.na(current_length_step_count)) {
                                                        if (current_elementary_sample$.__enclos_env__$private$specie_code3l == "SKJ") {
                                                          cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                              " - Warning: sample detected with length class measured in LD1 for SKJ specie. Elementary sample associated deleted.\n",
                                                              "[trip_id: ",
                                                              current_elementary_sample$.__enclos_env__$private$trip_id,
                                                              ", well_id: ",
                                                              current_elementary_sample$.__enclos_env__$private$well_id,
                                                              ", sample_id: ",
                                                              current_elementary_sample$.__enclos_env__$private$sample_id,
                                                              "]\n",
                                                              sep = "")
                                                          elementary_sample_skj_removed <- append(elementary_sample_skj_removed,
                                                                                                  elementarysampleraw_id)
                                                        } else {
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
                                                        }
                                                      } else {
                                                        current_length_step <- length_step[length_step$ocean == major_ocean_activities
                                                                                           & length_step$specie_code == current_elementary_sample$.__enclos_env__$private$specie_code
                                                                                           & length_step$ld1_class == current_elementary_sample$.__enclos_env__$private$sample_length_class, ]
                                                        current_elementary_sample_tmp <- vector(mode = "list")
                                                        for (current_length_step_count_id in seq_len(length.out = current_length_step_count)) {
                                                          if (current_length_step_count_id == current_length_step_count) {
                                                            current_elementary_sample$.__enclos_env__$private$length_type <- 2
                                                            current_elementary_sample$.__enclos_env__$private$sample_length_class_lf <- as.integer(current_length_step[current_length_step_count_id,
                                                                                                                                                                       "lf_class"])
                                                            current_elementary_sample$.__enclos_env__$private$sample_number_measured_lf <- as.numeric(current_length_step[current_length_step_count_id,
                                                                                                                                                                          "ratio"]
                                                                                                                                                      * 10^-2
                                                                                                                                                      * current_elementary_sample$.__enclos_env__$private$sample_number_measured)
                                                          } else {
                                                            current_elementary_sample_tmpbis <- current_elementary_sample$clone()
                                                            current_elementary_sample_tmpbis$.__enclos_env__$private$length_type <- 2
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
                                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                        " - Warning: ",
                                                        length(elementary_sample_skj_removed)
                                                        ," elementary sample(s) with length class measured in LD1 for SKJ specie detected. Sample associated not usable and removed for next process.\n",
                                                        "[trip_id: ",
                                                        current_well$.__enclos_env__$private$trip_id,
                                                        ", well_id: ",
                                                        current_well$.__enclos_env__$private$well_id,
                                                        ", sample_id: ",
                                                        unique(unlist(current_sample$extract_l1_element_value(element = "sample_id"))),
                                                        "]\n",
                                                        sep = "")
                                                    current_samples_removed <- append(current_samples_removed,
                                                                                      sample_id)
                                                  }
                                                }
                                                if (length(current_samples_removed) != 0) {
                                                  for (sample_remove_id in current_samples_removed) {
                                                    private$data_selected[[full_trip_id]][[partial_trip_id]]$.__enclos_env__$private$wells[[well_id]]$.__enclos_env__$private$elementarysampleraw[[sample_remove_id]] <- NULL
                                                  }
                                                }
                                              }
                                            }
                                          } else {
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Warning: ",
                                                " Well(s) detected with no elementary catch associated to the trip.\n",
                                                "[trip_id: ",
                                                current_trip$.__enclos_env__$private$trip_id,
                                                ", well_id(s): ",
                                                paste(unlist(current_wells$extract_l1_element_value(element = "well_id")),
                                                      collapse = ", "),
                                                "]\n",
                                                sep = "")
                                          }
                                        }
                                      } else {
                                        if (length(current_trip$.__enclos_env__$private$wells) != 0) {
                                          capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                         file = "NUL")
                                          capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                         file = "NUL")
                                          current_elementarysamplesraw <- unlist(current_wells$extract_l1_element_value(element = "elementarysampleraw"))
                                          if (length(current_elementarysamplesraw) != 0) {
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Warning: sample(s) detected without any activity associated.\n",
                                                "[trip_id: ",
                                                current_trip$.__enclos_env__$private$trip_id,
                                                "]\n",
                                                sep = "")
                                          }
                                        }
                                      }
                                    }
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Process 2.1 successfull on item \"",
                                      names(private$data_selected)[full_trip_id],
                                      "\".\n",
                                      "[trip: ",
                                      private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (full_trip_id == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End process 2.1 sample length class conversion ld1 to lf.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 2.2: sample number measured extrapolation ----
                            #' @description Process for sample number measured individuals extrapolation to sample number individuals counted.
                            sample_number_measured_extrapolation = function() {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 2.2 (sample number measured extrapolation) cancelled.\n",
                                    sep = "")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.2: sample number measured extrapolation.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                      capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                     file = "NUL")
                                      capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                     file = "NUL")
                                      if (length(x = unlist(current_wells$extract_l1_element_value(element = "elementarysampleraw"))) != 0) {
                                        capture.output(current_elementarysamplesraw <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                       file = "NUL")
                                        capture.output(current_elementarysamplesraw$add(new_item = unlist(current_wells$extract_l1_element_value(element = "elementarysampleraw"))),
                                                       file = "NUL")
                                        current_elementarysamplesraw$modification_l1(modification = "$path$rf4 <- NA")
                                        current_elementarysamplesraw$modification_l1(modification = "$path$sample_number_measured_extrapolated_lf <- NA")
                                      }
                                    }
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.2 on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (partial_trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                      if (length(current_trip$.__enclos_env__$private$wells) != 0) {
                                        capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                       file = "NUL")
                                        capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                       file = "NUL")
                                        for (well_id in seq_len(length.out = current_wells$count())) {
                                          current_well <- current_wells$extract(id = well_id)[[1]]
                                          if (length(current_well$.__enclos_env__$private$elementarysampleraw) != 0) {
                                            capture.output(current_samples <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                           file = "NUL")
                                            capture.output(current_samples$add(new_item = current_well$.__enclos_env__$private$elementarysampleraw),
                                                           file = "NUL")
                                            for (sample_id in seq_len(length.out = current_samples$count())) {
                                              capture.output(current_sample <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                             file = "NUL")
                                              capture.output(current_sample$add(new_item = current_samples$extract(id = sample_id)),
                                                             file = "NUL")
                                              if (any(unlist(x = lapply(X = current_sample$extract_l1_element_value(element = "sample_number_measured_lf"),
                                                                        FUN = is.null)))) {
                                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                    " - Error: run process 2.1 (sample length class conversion ld1 to lf) before this process.\n",
                                                    sep = "")
                                                stop()
                                              }
                                              for (sub_sample_id in unique(unlist(current_sample$extract_l1_element_value(element = "sub_sample_id")))) {
                                                capture.output(current_sub_sample <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                               file = "NUL")
                                                capture.output(current_sub_sample$add(new_item = current_sample$filter_l1(filter = paste0("$path$sub_sample_id == ",
                                                                                                                                          sub_sample_id))),
                                                               file = "NUL")
                                                for (sample_specie_id in unique(unlist(current_sub_sample$extract_l1_element_value(element = "specie_code")))) {
                                                  capture.output(current_sub_sample_specie <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                                 file = "NUL")
                                                  capture.output(current_sub_sample_specie$add(new_item = current_sub_sample$filter_l1(filter = paste0("$path$specie_code == ",
                                                                                                                                                       sample_specie_id))),
                                                                 file = "NUL")
                                                  sum_sub_sample_specie_number_measured_lf <- sum(unlist(current_sub_sample_specie$extract_l1_element_value(element = "sample_number_measured_lf")),
                                                                                                  na.rm = TRUE)
                                                  sum_sub_sample_specie_total_count <- 0
                                                  for (sub_sample_id_total_count in unique(unlist(current_sub_sample_specie$extract_l1_element_value(element = "sub_sample_id_total_count")))) {
                                                    capture.output(current_sub_sample_specie_total_count <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                                   file = "NUL")
                                                    capture.output(current_sub_sample_specie_total_count$add(new_item = current_sub_sample_specie$filter_l1(filter = paste0("$path$sub_sample_id_total_count == \"",
                                                                                                                                                                            sub_sample_id_total_count,
                                                                                                                                                                            "\""))),
                                                                   file = "NUL")
                                                    sum_sub_sample_specie_total_count <- sum_sub_sample_specie_total_count + unique(unlist(current_sub_sample_specie_total_count$extract_l1_element_value(element = "sample_total_count")))
                                                  }
                                                  rf4 <- sum_sub_sample_specie_total_count / sum_sub_sample_specie_number_measured_lf
                                                  # rf4 verification
                                                  if (rf4 != 1 & sample_specie_id != 2) {
                                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                        " - Warning: rf4 not egal to 1 (",
                                                        rf4,
                                                        ") for sampled specie different from SKJ.\n",
                                                        "[trip: ",
                                                        current_trip$.__enclos_env__$private$trip_id,
                                                        ", well: ",
                                                        current_well$.__enclos_env__$private$well_id,
                                                        ", sample: ",
                                                        current_sample$extract(id = 1)[[1]]$.__enclos_env__$private$sample_id,
                                                        ", sub sample: ",
                                                        current_sub_sample$extract(id = 1)[[1]]$.__enclos_env__$private$sub_sample_id,
                                                        ", specie: ",
                                                        current_sub_sample_specie$extract(id = 1)[[1]]$.__enclos_env__$private$specie_code3l,
                                                        "]\n",
                                                        sep = "")
                                                  } else if (rf4 < 1) {
                                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                        " - Warning: rf4 inferior to 1 (",
                                                        rf4,
                                                        ").\n",
                                                        "[trip: ",
                                                        current_trip$.__enclos_env__$private$trip_id,
                                                        ", well: ",
                                                        current_well$.__enclos_env__$private$well_id,
                                                        ", sample: ",
                                                        current_sample$extract(id = 1)[[1]]$.__enclos_env__$private$sample_id,
                                                        ", sub sample: ",
                                                        current_sub_sample$extract(id = 1)[[1]]$.__enclos_env__$private$sub_sample_id,
                                                        ", specie: ",
                                                        current_sub_sample_specie$extract(id = 1)[[1]]$.__enclos_env__$private$specie_code3l,
                                                        "]\n",
                                                        sep = "")
                                                  }
                                                  current_sub_sample_specie$modification_l1(modification = paste0("$path$rf4 <- ",
                                                                                                                  rf4))
                                                  current_sub_sample_specie$modification_l1(modification = paste0("$path$sample_number_measured_extrapolated_lf <- $path$sample_number_measured_lf * ",
                                                                                                                  rf4))
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Process 2.2 successfull on item \"",
                                      names(private$data_selected)[full_trip_id],
                                      "\".\n",
                                      "[trip: ",
                                      private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (full_trip_id == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End process 2.2: sample number measured extrapolation.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 2.3: sample_length_class_step_standardisation ----
                            #' @description Process for step standardisation of lf length class.
                            #' @param maximum_lf_class Object of type \code{\link[base]{integer}} expected. Theorical maximum lf class that can occur (all species considerated). By default 500.
                            sample_length_class_step_standardisation = function(maximum_lf_class = as.integer(500)) {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 2.3 (sample length class step standardisation) cancelled.\n",
                                    sep = "")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.3: sample length class step standardisation.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    # full trip is not complete (missing at least one trip)
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                      capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                     file = "NUL")
                                      capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                     file = "NUL")
                                      if (length(x = unlist(current_wells$extract_l1_element_value(element = "elementarysampleraw"))) != 0) {
                                        current_wells$modification_l1(modification = "$path$elementarysample <- NA")
                                      }
                                    }
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.3 on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (partial_trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                      if (length(current_trip$.__enclos_env__$private$wells) != 0) {
                                        capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                       file = "NUL")
                                        capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                       file = "NUL")
                                        for (well_id in seq_len(length.out = current_wells$count())) {
                                          current_well <- current_wells$extract(id = well_id)[[1]]
                                          if (length(current_well$.__enclos_env__$private$elementarysampleraw) != 0) {
                                            capture.output(current_samples <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                           file = "NUL")
                                            capture.output(current_samples$add(new_item = unlist(x = current_well$.__enclos_env__$private$elementarysampleraw)),
                                                           file = "NUL")
                                            capture.output(current_elementarysamples <- t3::object_r6(class_name = "elementarysamples"),
                                                           file = "NUL")
                                            for (sample_id in unique(x = unlist(x = current_samples$extract_l1_element_value(element = "sample_id")))) {
                                              capture.output(current_sample <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                             file = "NUL")
                                              capture.output(current_sample$add(new_item = current_samples$filter_l1(filter = paste0("$path$sample_id == \"",
                                                                                                                                     sample_id,
                                                                                                                                     "\""))),
                                                             file = "NUL")
                                              sample_species <- unique(unlist(current_sample$extract_l1_element_value(element = "specie_code")))
                                              current_sample_by_species <- vector(mode = "list",
                                                                                  length = length(sample_species))
                                              for (specie_id in sample_species) {
                                                current_sample_by_species[[specie_id]] <- current_sample$filter_l1(filter = paste0("$path$specie_code == ",
                                                                                                                                   specie_id))
                                              }
                                              for (sample_id_specie in sample_species) {
                                                capture.output(current_sample_specie <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                               file = "NUL")
                                                capture.output(current_sample_specie$add(new_item = current_sample_by_species[[sample_id_specie]]),
                                                               file = "NUL")
                                                sample_length_class_lf <- sort(x = unique(x = unlist(x = current_sample_specie$extract_l1_element_value(element = "sample_length_class_lf"))))
                                                if (sample_id_specie %in% c(2, 802, 5, 805, 6, 806)) {
                                                  step <- 1
                                                } else if (sample_id_specie %in% c(1, 801, 3, 803, 4, 804)) {
                                                  step <- 2
                                                } else {
                                                  step <- NA
                                                }
                                                if (is.na(step)) {
                                                  for (elementarysamplesraw_id in seq_len(length.out = current_sample_specie$count())) {
                                                    object_elementarysample <- elementarysample$new(trip_id = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$trip_id,
                                                                                                    well_id = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$well_id,
                                                                                                    sample_id = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$sample_id,
                                                                                                    sub_sample_id = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$sub_sample_id,
                                                                                                    sample_quality = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$sample_quality,
                                                                                                    sample_type = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$sample_type,
                                                                                                    specie_code = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$specie_code,
                                                                                                    specie_code3l = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$specie_code3l,
                                                                                                    sample_standardised_length_class_lf = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$sample_length_class_lf,
                                                                                                    sample_number_measured_extrapolated_lf = as.numeric(current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$sample_number_measured_extrapolated_lf),
                                                                                                    sample_total_count = as.integer(current_sample_specie$extract(id = elementarysamplesraw_id)[[1]]$.__enclos_env__$private$sample_total_count),
                                                                                                    elementarysampleraw = current_sample_specie$extract(id = elementarysamplesraw_id)[[1]])
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
                                                  while(sample_length_class_lf_id <= length(sample_length_class_lf)) {
                                                    lower_border <- as.integer(dplyr::last(x = lower_border_reference[which(lower_border_reference <= trunc(sample_length_class_lf[sample_length_class_lf_id]))]))
                                                    upper_border <- as.integer(dplyr::first(x = upper_border_reference[which(upper_border_reference > trunc(sample_length_class_lf[sample_length_class_lf_id]))]))
                                                    sample_length_class_lf_for_merge <- sample_length_class_lf[which(sample_length_class_lf >= lower_border
                                                                                                                     & sample_length_class_lf < upper_border)]
                                                    capture.output(current_sample_specie_by_step <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                                   file = "NUL")
                                                    capture.output(current_sample_specie_by_step$add(new_item = current_sample_specie$filter_l1(filter = paste0("$path$sample_length_class_lf %in% c(",
                                                                                                                                                                paste0(sample_length_class_lf_for_merge,
                                                                                                                                                                       collapse = ", "),
                                                                                                                                                                ")"))),
                                                                   file = "NUL")
                                                    current_sample_specie_by_step_subid <- unique(unlist(current_sample_specie_by_step$extract_l1_element_value(element = "sub_sample_id")))
                                                    for (sub_sample_id in current_sample_specie_by_step_subid) {
                                                      capture.output(current_sample_specie_by_step_by_subid <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                                     file = "NUL")
                                                      capture.output(current_sample_specie_by_step_by_subid$add(new_item = current_sample_specie_by_step$filter_l1(filter = paste0("$path$sub_sample_id == ",
                                                                                                                                                                                   sub_sample_id))),
                                                                     file = "NUL")
                                                      object_elementarysample <- elementarysample$new(trip_id = current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$trip_id,
                                                                                                      well_id = current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$well_id,
                                                                                                      sample_id = current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$sample_id,
                                                                                                      sub_sample_id = current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$sub_sample_id,
                                                                                                      sample_quality = current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$sample_quality,
                                                                                                      sample_type = current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$sample_type,
                                                                                                      specie_code = current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$specie_code,
                                                                                                      specie_code3l = current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$specie_code3l,
                                                                                                      sample_standardised_length_class_lf = lower_border,
                                                                                                      sample_number_measured_extrapolated_lf = sum(unlist(current_sample_specie_by_step_by_subid$extract_l1_element_value(element = "sample_number_measured_extrapolated_lf"))),
                                                                                                      sample_total_count = as.integer(current_sample_specie_by_step_by_subid$extract(id = 1)[[1]]$.__enclos_env__$private$sample_total_count),
                                                                                                      elementarysampleraw = current_sample_specie_by_step_by_subid)
                                                      capture.output(current_elementarysamples$add(new_item = object_elementarysample),
                                                                     file = "NUL")
                                                    }
                                                    sample_length_class_lf_id <- sample_length_class_lf_id + length(sample_length_class_lf_for_merge)
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
                                  }
                                  cat(format(Sys.time(),
                                             "%Y-%m-%d %H:%M:%S"),
                                      " - Process 2.3 successfull on item \"",
                                      names(private$data_selected)[full_trip_id],
                                      "\".\n",
                                      "[trip: ",
                                      private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (full_trip_id == length(private$data_selected)) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - End process 2.3: sample length class step standardisation.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 2.4: well_set_weigth_categories ----
                            #' @description Process for well set weigth categories definition.
                            #' @param sample_set Object of type \code{\link[base]{data.frame}} expected. Data frame object with weighted weigh of each set sampled.
                            well_set_weigth_categories = function(sample_set) {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 2.4 (well-set weight categories definition) cancelled.\n",
                                    sep = "")
                              } else {
                                if (! paste0(class(sample_set),
                                             collapse = "_") %in% c("data.frame",
                                                                    "tbl_df_tbl_data.frame")
                                    || ncol(sample_set) != 5
                                    || nrow(sample_set) == 0) {
                                  cat(format(Sys.time(),
                                             "%Y-%m-%d %H:%M:%S"),
                                      " - Error: invalid \"sample_set\" argument, class \"data.frame\" or \"tibble\" with 5 columns and at least 1 row expected.\n",
                                      sep = "")
                                  stop()
                                } else {
                                  for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                    if (full_trip_id == 1) {
                                      cat(format(Sys.time(),
                                                 "%Y-%m-%d %H:%M:%S"),
                                          " - Start process 2.4: well-set weight categories definition.\n",
                                          sep = "")
                                    }
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.4 on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    if (names(private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                      cat(format(Sys.time(),
                                                 "%Y-%m-%d %H:%M:%S"),
                                          " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                          "[trip: ",
                                          private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                          "]\n",
                                          sep = "")
                                      capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                                                     file = "NUL")
                                      capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                     file = "NUL")
                                      if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                        capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                       file = "NUL")
                                        capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                       file = "NUL")
                                        current_wells$modification_l1(modification = "$path$wellsets <- NA")
                                      }
                                    } else {
                                      for (partial_trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                        current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                        if (current_trip$.__enclos_env__$private$vessel_type == "Senneur") {
                                          if (length(current_trip$.__enclos_env__$private$wells) != 0) {
                                            capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                           file = "NUL")
                                            capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                           file = "NUL")
                                            wells_activities_samples_id <- vector(mode = "list",
                                                                                  length = current_wells$count())
                                            for (well_id in seq_len(length.out = current_wells$count())) {
                                              current_well <- current_wells$extract(id = well_id)[[1]]
                                              if (length(current_well$.__enclos_env__$private$wellplan) != 0) {
                                                capture.output(current_well_plans <- t3::object_r6(class_name = "elementarywellplans"),
                                                               file = "NUL")
                                                capture.output(current_well_plans$add(new_item = current_well$.__enclos_env__$private$wellplan),
                                                               file = "NUL")
                                                activities_id <- unique(unlist(current_well_plans$extract_l1_element_value(element = "activity_id")))
                                                wells_activities_samples_id[[well_id]][[1]] <- activities_id
                                              } else {
                                                wells_activities_samples_id[[well_id]][[1]] <- "no_well_plan_available"
                                              }
                                              if (length(current_well$.__enclos_env__$private$elementarysampleraw) != 0) {
                                                capture.output(current_elementarysamplesraw <- t3::object_r6(class_name = "elementarysamplesraw"),
                                                               file = "NUL")
                                                capture.output(current_elementarysamplesraw$add(new_item = unlist(current_well$.__enclos_env__$private$elementarysampleraw)),
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
                                              if (length(current_well$.__enclos_env__$private$wellplan) != 0) {
                                                # yes
                                                capture.output(current_well_plans <- t3::object_r6(class_name = "elementarywellplans"),
                                                               file = "NUL")
                                                capture.output(current_well_plans$add(new_item = current_well$.__enclos_env__$private$wellplan),
                                                               file = "NUL")
                                                # calcul of proportion of minus and plus 10 kg
                                                current_wellplan_weigth_category <- unique(unlist(current_well_plans$extract_l1_element_value(element = "wellplan_weigth_category_label")))
                                                well_prop_minus10_weigth <- 0
                                                well_prop_plus10_weigth <- 0
                                                well_prop_global_weigth <- 0
                                                if (! any(current_wellplan_weigth_category %in% c("inconnu",
                                                                                                  "mélange"))) {
                                                  for (well_plan_id in seq_len(length.out = current_well_plans$count())) {
                                                    current_well_plan <- current_well_plans$extract(id = well_plan_id)[[1]]
                                                    if (current_well_plan$.__enclos_env__$private$wellplan_weigth_category_label == "- 10 kg") {
                                                      well_prop_minus10_weigth <- well_prop_minus10_weigth + current_well_plan$.__enclos_env__$private$wellplan_weight
                                                      well_prop_global_weigth <- well_prop_global_weigth + current_well_plan$.__enclos_env__$private$wellplan_weight
                                                    } else if (current_well_plan$.__enclos_env__$private$wellplan_weigth_category_label == "+ 10 kg") {
                                                      well_prop_plus10_weigth <- well_prop_plus10_weigth + current_well_plan$.__enclos_env__$private$wellplan_weight
                                                      well_prop_global_weigth <- well_prop_global_weigth + current_well_plan$.__enclos_env__$private$wellplan_weight
                                                    } else {
                                                      cat(format(Sys.time(),
                                                                 "%Y-%m-%d %H:%M:%S"),
                                                          " - Error: ",
                                                          "Well plan weight category unknown.\n",
                                                          "[trip: ",
                                                          current_well$.__enclos_env__$private$trip_id,
                                                          ", well: ",
                                                          current_well$.__enclos_env__$private$well_id,
                                                          "]\n",
                                                          sep = "")
                                                      stop()
                                                    }
                                                  }
                                                } else {
                                                  well_prop_minus10_weigth <- NA
                                                  well_prop_plus10_weigth <- NA
                                                  for (well_plan_id in seq_len(length.out = current_well_plans$count())) {
                                                    current_well_plan <- current_well_plans$extract(id = well_plan_id)[[1]]
                                                    well_prop_global_weigth <- well_prop_global_weigth + current_well_plan$.__enclos_env__$private$wellplan_weight
                                                  }
                                                }
                                                current_well$.__enclos_env__$private$well_prop_minus10_weigth <- well_prop_minus10_weigth / well_prop_global_weigth
                                                current_well$.__enclos_env__$private$well_prop_plus10_weigth <- well_prop_plus10_weigth / well_prop_global_weigth
                                                capture.output(current_well_sets <- t3::object_r6(class_name = "wellsets"),
                                                               file = "NUL")
                                                # do we have more than one well associated to the trip ?
                                                if (length(wells_activities_samples_id) == 1) {
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
                                                    for (other_well_id in seq_len(length.out = length(wells_activities_samples_id))[seq_len(length.out = length(wells_activities_samples_id)) != well_id]) {
                                                      if (current_well_activitie_id %in% wells_activities_samples_id[[other_well_id]][[1]]) {
                                                        wells_associated <- append(wells_associated,
                                                                                   other_well_id)
                                                      }
                                                    }
                                                    # do we have at least one activity of the current well store in one or more other well(s) ?
                                                    if (length(wells_associated) != 0) {
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
                                                            for (elementarywellplan_id in seq_len(length.out = length(current_well_plan_tmp))) {
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
                                                cat(format(Sys.time(),
                                                           "%Y-%m-%d %H:%M:%S"),
                                                    " - Warning: ",
                                                    " No well plan availabe for this well.\n",
                                                    "[trip: ",
                                                    current_well$.__enclos_env__$private$trip_id,
                                                    ", well: ",
                                                    current_well$.__enclos_env__$private$well_id,
                                                    "]\n",
                                                    sep = "")
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
                                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                      " - Warning: ",
                                                      " No weighted weight availabe for this well in the database.\n",
                                                      "[trip: ",
                                                      current_well$.__enclos_env__$private$trip_id,
                                                      ", well: ",
                                                      current_well$.__enclos_env__$private$well_id,
                                                      "]\n",
                                                      sep = "")
                                                  current_well$.__enclos_env__$private$wellsets <- NA
                                                } else {
                                                  capture.output(current_well_sets <- t3::object_r6(class_name = "wellsets"),
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
                                          cat(format(Sys.time(),
                                                     "%Y-%m-%d %H:%M:%S"),
                                              " - Error: process not available for this vessel type.\n",
                                              "[trip: ",
                                              current_trip$.__enclos_env__$private$trip_id,
                                              "]\n",
                                              sep = "")
                                          stop()
                                        }
                                      }
                                    }
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Process 2.4 successfull on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    if (full_trip_id == length(private$data_selected)) {
                                      cat(format(Sys.time(),
                                                 "%Y-%m-%d %H:%M:%S"),
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
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.5: standardised sample creation.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                      capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                     file = "NUL")
                                      capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                     file = "NUL")
                                      current_wells$modification_l1(modification = "$path$standardisedsample <- NA")
                                    }
                                  } else {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Ongoing process 2.5 on item \"",
                                        names(private$data_selected)[full_trip_id],
                                        "\".\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    for (partial_trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                      if (length(current_trip$.__enclos_env__$private$wells) != 0) {
                                        capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                       file = "NUL")
                                        capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                       file = "NUL")
                                        for (well_id in seq_len(length.out = current_wells$count())) {
                                          current_well <- current_wells$extract(id = well_id)[[1]]
                                          if (is.null(current_well$.__enclos_env__$private$elementarysample)) {
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Error: the object elementarysample is NULL, please run processes 2.1 to 2.4 before this one.\n",
                                                "[trip_id: ",
                                                current_well$.__enclos_env__$private$trip_id,
                                                ", well_id: ",
                                                current_well$.__enclos_env__$private$well_id,
                                                "]\n",
                                                sep = "")
                                            stop()
                                          }
                                          if (paste0(class(x = current_well$.__enclos_env__$private$elementarysample),
                                                     collapse = "_") == "elementarysamples_list_t3_R6") {
                                            capture.output(current_standardisedsamples <- t3::object_r6(class_name = "standardisedsamples"),
                                                           file = "NUL")
                                            capture.output(current_elementarysamples <- t3::object_r6(class_name = "elementarysamples"),
                                                           file = "NUL")
                                            capture.output(current_elementarysamples$add(new_item = current_well$.__enclos_env__$private$elementarysample$.__enclos_env__$private$data),
                                                           file = "NUL")
                                            current_elementarysamples_species <- unique(paste(unlist(current_elementarysamples$extract_l1_element_value(element = "specie_code")),
                                                                                              unlist(current_elementarysamples$extract_l1_element_value(element = "specie_code3l")),
                                                                                              sep = "_"))
                                            for (elementarysamples_species_id in current_elementarysamples_species) {
                                              capture.output(current_elementarysamples_specie <- t3::object_r6(class_name = "elementarysamples"),
                                                             file = "NUL")
                                              capture.output(current_elementarysamples_specie$add(new_item = current_elementarysamples$filter_l1(filter = paste0("$path$specie_code == ",
                                                                                                                                                                 as.integer(unlist(strsplit(elementarysamples_species_id, "_"))[1])))),
                                                             file = "NUL")
                                              current_elementarysamples_specie_classes <- unique(unlist(current_elementarysamples_specie$extract_l1_element_value(element = "sample_standardised_length_class_lf")))
                                              for (current_elementarysamples_specie_class_id in current_elementarysamples_specie_classes) {
                                                capture.output(current_elementarysamples_specie_class <- t3::object_r6(class_name = "elementarysamples"),
                                                               file = "NUL")
                                                capture.output(current_elementarysamples_specie_class$add(new_item = current_elementarysamples_specie$filter_l1(filter = paste0("$path$sample_standardised_length_class_lf == ",
                                                                                                                                                                                current_elementarysamples_specie_class_id))),
                                                               file = "NUL")
                                                current_elementarysamples_sample_types <- unique(unlist(current_elementarysamples_specie_class$extract_l1_element_value(element = "sample_type")))
                                                for (current_elementarysamples_sample_type_id in current_elementarysamples_sample_types) {
                                                  capture.output(current_elementarysamples_sample_type <- t3::object_r6(class_name = "elementarysamples"),
                                                                 file = "NUL")
                                                  capture.output(current_elementarysamples_sample_type$add(new_item = current_elementarysamples_specie_class$filter_l1(filter = paste0("$path$sample_type == ",
                                                                                                                                                                                       current_elementarysamples_sample_type_id))),
                                                                 file = "NUL")
                                                  current_elementarysamples_sample_qualities <- unique(unlist(current_elementarysamples_sample_type$extract_l1_element_value(element = "sample_quality")))
                                                  for (current_elementarysamples_sample_quality_id in current_elementarysamples_sample_qualities) {
                                                    capture.output(current_elementarysamples_sample_quality <- t3::object_r6(class_name = "elementarysamples"),
                                                                   file = "NUL")
                                                    capture.output(current_elementarysamples_sample_quality$add(new_item = current_elementarysamples_sample_type$filter_l1(filter = paste0("$path$sample_quality == ",
                                                                                                                                                                                           current_elementarysamples_sample_quality_id))),
                                                                   file = "NUL")
                                                    current_standardisedsample <- standardisedsample$new(trip_id = current_well$.__enclos_env__$private$trip_id,
                                                                                                         well_id = current_well$.__enclos_env__$private$well_id,
                                                                                                         sample_id = unique(unlist(current_elementarysamples_sample_quality$extract_l1_element_value(element = "sample_id"))),
                                                                                                         sample_quality = as.integer(current_elementarysamples_sample_quality_id),
                                                                                                         sample_type = as.integer(current_elementarysamples_sample_type_id),
                                                                                                         specie_code = as.integer(unlist(strsplit(elementarysamples_species_id, "_"))[1]),
                                                                                                         specie_code3l = unlist(strsplit(elementarysamples_species_id, "_"))[2],
                                                                                                         sample_standardised_length_class_lf = as.integer(current_elementarysamples_specie_class_id),
                                                                                                         sample_number_measured_extrapolated_lf = sum(unlist(current_elementarysamples_sample_quality$extract_l1_element_value(element = "sample_number_measured_extrapolated_lf"))),
                                                                                                         sample_total_count = sum(unlist(current_elementarysamples_sample_quality$extract_l1_element_value(element = "sample_total_count"))),
                                                                                                         elementarysample = current_elementarysamples_sample_quality)
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
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Process 2.5 successfull on item \"",
                                      names(private$data_selected)[full_trip_id],
                                      "\".\n",
                                      "[trip: ",
                                      private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (full_trip_id == length(private$data_selected)) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - End process 2.5 standardised sample creation.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 2.6: standardised_sample_set_creation ----
                            #' @description R6 object standardised sample set creation.
                            #' @param length_weight_relationship_data Object of type \code{\link[base]{data.frame}} expected. Data frame object with parameters for length weight relationship.
                            standardised_sample_set_creation = function(length_weight_relationship_data) {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 2.6 (standardised sample set creation) cancelled.\n",
                                    sep = "")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.6: standardised sample set creation.\n",
                                        sep = "")
                                  }
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 2.6 on item \"",
                                      names(private$data_selected)[full_trip_id],
                                      "\".\n",
                                      "[trip: ",
                                      private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (names(private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                      capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                     file = "NUL")
                                      capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                     file = "NUL")
                                      current_wells$modification_l1(modification = "$path$standardisedsampleset <- NA")
                                    }
                                  } else {
                                    for (partial_trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                      capture.output(current_activities <- t3::object_r6(class_name = "activities"),
                                                     file = "NUL")
                                      capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                                                     file = "NUL")
                                      if (length(current_trip$.__enclos_env__$private$wells) != 0) {
                                        capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                       file = "NUL")
                                        capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                       file = "NUL")
                                        for (well_id in seq_len(length.out = current_wells$count())) {
                                          current_well <- current_wells$extract(id = well_id)[[1]]
                                          current_wells_sets <- current_well$.__enclos_env__$private$wellsets
                                          current_standardised_samples <- current_well$.__enclos_env__$private$standardisedsample
                                          if (all(class(current_wells_sets) == c("wellsets",
                                                                                 "list_t3",
                                                                                 "R6"))
                                              && all(class(current_standardised_samples) == c("standardisedsamples",
                                                                                              "list_t3",
                                                                                              "R6"))) {
                                            capture.output(standardised_samples_sets <- t3::object_r6(class_name = "standardisedsamplesets"),
                                                           file = "NUL")
                                            for (well_set_id in seq_len(length.out = current_wells_sets$count())) {
                                              current_well_set <- current_wells_sets$extract(id = well_set_id)[[1]]
                                              current_activity <- current_well_set$.__enclos_env__$private$activity_id
                                              current_ocean <- current_activities$filter_l1(filter = paste0("$path$activity_id == \"",
                                                                                                            current_activity,
                                                                                                            "\""))[[1]]$.__enclos_env__$private$ocean
                                              if (is.null(current_ocean)) {
                                                cat(format(Sys.time(),
                                                           "%Y-%m-%d %H:%M:%S"),
                                                    " - Error: sample activity missing from trip activities.\n",
                                                    "[trip: ",
                                                    private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                                    ", sample activity: ",
                                                    current_activity,
                                                    "]\n",
                                                    sep = "")
                                                stop()
                                              }
                                              for (standardisedsample_id in seq_len(length.out = current_standardised_samples$count())) {
                                                current_standardised_sample <- current_standardised_samples$extract(id = standardisedsample_id)[[1]]
                                                current_length_weight_relationship <- dplyr::filter(.data = length_weight_relationship_data,
                                                                                                    (ocean == current_ocean
                                                                                                     & specie_code3l == current_standardised_sample$.__enclos_env__$private$specie_code3l))[4:5]
                                                if (nrow(current_length_weight_relationship) == 1) {
                                                  coef_a <- current_length_weight_relationship[1, 1]
                                                  coef_b <- current_length_weight_relationship[1, 2]
                                                  if (current_standardised_sample$.__enclos_env__$private$specie_code3l %in% c("SKJ",
                                                                                                                               "LTA",
                                                                                                                               "FRI")) {
                                                    # step of 1 cm
                                                    length_class_lf <- current_standardised_sample$.__enclos_env__$private$sample_standardised_length_class_lf + 0.5
                                                  } else if (current_standardised_sample$.__enclos_env__$private$specie_code3l %in% c("YFT",
                                                                                                                                      "BET",
                                                                                                                                      "ALB")) {
                                                    # step of 2 cm
                                                    length_class_lf <- current_standardised_sample$.__enclos_env__$private$sample_standardised_length_class_lf + 1
                                                  } else {
                                                    length_class_lf <- current_standardised_sample$.__enclos_env__$private$sample_standardised_length_class_lf
                                                  }
                                                  lwr <- coef_a * length_class_lf ^ coef_b
                                                } else {
                                                  lwr <- NA
                                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                      " - Warning: length to weight conversion impossible.\n",
                                                      "[trip: ",
                                                      current_well$.__enclos_env__$private$trip_id,
                                                      ", well_id: ",
                                                      current_well$.__enclos_env__$private$well_id,
                                                      ", sample(s): ",
                                                      paste0(current_standardised_sample$.__enclos_env__$private$sample_id,
                                                             collapse = " - "),
                                                      "]\n",
                                                      sep = "")
                                                }
                                                current_standardised_samples_sets <- standardisedsampleset$new(trip_id = current_well_set$.__enclos_env__$private$trip_id,
                                                                                                               activity_id = current_well_set$.__enclos_env__$private$activity_id,
                                                                                                               well_id = current_well_set$.__enclos_env__$private$well_id,
                                                                                                               sample_id = current_standardised_sample$.__enclos_env__$private$sample_id,
                                                                                                               sample_quality = current_standardised_sample$.__enclos_env__$private$sample_quality,
                                                                                                               sample_type = current_standardised_sample$.__enclos_env__$private$sample_type,
                                                                                                               specie_code = current_standardised_sample$.__enclos_env__$private$specie_code,
                                                                                                               specie_code3l = current_standardised_sample$.__enclos_env__$private$specie_code3l,
                                                                                                               sample_standardised_length_class_lf = current_standardised_sample$.__enclos_env__$private$sample_standardised_length_class_lf,
                                                                                                               sample_number_weighted = current_standardised_sample$.__enclos_env__$private$sample_number_measured_extrapolated_lf * current_well_set$.__enclos_env__$private$prop_weighted_weight,
                                                                                                               sample_weigth = (current_standardised_sample$.__enclos_env__$private$sample_number_measured_extrapolated_lf * current_well_set$.__enclos_env__$private$prop_weighted_weight) * lwr,
                                                                                                               sample_weight_unit = lwr,
                                                                                                               sample_category = ifelse(lwr <= 10,
                                                                                                                                        "- 10kg",
                                                                                                                                        "+ 10kg"),
                                                                                                               standardisedsample = current_standardised_sample)
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
                                  }
                                  cat(format(Sys.time(),
                                             "%Y-%m-%d %H:%M:%S"),
                                      " - Process 2.6 successfull on item \"",
                                      names(private$data_selected)[full_trip_id],
                                      "\".\n",
                                      "[trip: ",
                                      private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (full_trip_id == length(private$data_selected)) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - End process 2.6: standardised sample set creation.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # process 2.7: raised_factors_determination ----
                            #' @description Raised factors determination for weigth sample set to set.
                            #' @param threshold_rf_minus10 Object of type \code{\link[base]{integer}} expected. Threshold limite value for raising factor on individuals category minus 10. By default 500.
                            #' @param threshold_rf_plus10 Object of type \code{\link[base]{integer}} expected. Threshold limite value for raising factor on individuals category plus 10. By default 500.
                            #' @param threshold_frequency_rf_minus10 Object of type \code{\link[base]{integer}} expected. Threshold limite frequency value for raising factor on individuals category minus 10. By default 75.
                            #' @param threshold_frequency_rf_plus10 Object of type \code{\link[base]{integer}} expected. Threshold limite frequency value for raising factor on individuals category plus 10. By default 75.
                            #' @param threshold_rf_total Object of type \code{\link[base]{integer}} expected. Threshold limite value for raising factor (all categories). By default 250.
                            raised_factors_determination = function(threshold_rf_minus10 = as.integer(500),
                                                                    threshold_rf_plus10 = as.integer(500),
                                                                    threshold_frequency_rf_minus10 = as.integer(75),
                                                                    threshold_frequency_rf_plus10 = as.integer(75),
                                                                    threshold_rf_total = as.integer(250)) {
                              if (is.null(private$data_selected)) {
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 2.7 (raised factors determination) cancelled.\n",
                                    sep = "")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.7: raised factors determination.\n",
                                        sep = "")
                                  }
                                  cat(format(Sys.time(),
                                             "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 2.7 on item \"",
                                      names(private$data_selected)[full_trip_id],
                                      "\".\n",
                                      "[trip: ",
                                      private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (names(private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")

                                    capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                      capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                     file = "NUL")
                                      capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                     file = "NUL")
                                      if (length(x = current_wells$filter_l1(filter = "all(class($path$wellsets) == c(\"wellsets\", \"list_t3\", \"R6\"))")) != 0) {
                                        capture.output(current_wells_bis <- t3::object_r6(class_name = "wells"),
                                                       file = "NUL")
                                        capture.output(current_wells_bis$add(new_item = current_wells$filter_l1(filter = "all(class($path$wellsets) == c(\"wellsets\", \"list_t3\", \"R6\"))")),
                                                       file = "NUL")
                                        capture.output(current_wellsets <- t3::object_r6(class_name = "wellsets"),
                                                       file = "NUL")
                                        capture.output(current_wellsets$add(new_item = current_wells_bis$extract_l1_element_value(element = "wellsets")),
                                                       file = "NUL")
                                        capture.output(current_wellsets_bis <- t3::object_r6(class_name = "wellsets"),
                                                       file = "NUL")
                                        capture.output(current_wellsets_bis$add(new_item = unlist(current_wellsets$extract_l1_element_value(element = "data"))),
                                                       file = "NUL")
                                        current_wellsets_bis$modification_l1(modification = "$path$rf_validation <- NA")
                                      }
                                    }
                                  } else {
                                    for (partial_trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                      if (length(current_trip$.__enclos_env__$private$wells) != 0) {
                                        capture.output(current_wells <- t3::object_r6(class_name = "wells"),
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
                                                capture.output(current_standardised_samples_sets <- t3::object_r6(class_name = "standardisedsamplesets"),
                                                               file = "NUL")
                                                capture.output(current_standardised_samples_sets$add(new_item = current_well$.__enclos_env__$private$standardisedsampleset$filter_l1(filter = paste0("$path$activity_id == \"",
                                                                                                                                                                                                     current_well_set$.__enclos_env__$private$activity_id,
                                                                                                                                                                                                     "\""))),
                                                               file = "NUL")
                                                if (length(x = current_standardised_samples_sets$filter_l1(filter = "$path$sample_weight_unit <= 10")) != 0) {
                                                  capture.output(current_standardised_samples_sets_minus10 <- t3::object_r6(class_name = "standardisedsamplesets"),
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
                                                  capture.output(current_standardised_samples_sets_plus10 <- t3::object_r6(class_name = "standardisedsamplesets"),
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
                                                  current_well_set$.__enclos_env__$private$rf_validation <- 1
                                                  cat(format(Sys.time(),
                                                             "%Y-%m-%d %H:%M:%S"),
                                                      " - Warning: well-set avoided because weighted samples total value egal to zero.\n",
                                                      "[trip: ,",
                                                      current_well_set$.__enclos_env__$private$trip_id,
                                                      ", activity: ",
                                                      current_well_set$.__enclos_env__$private$activity_id,
                                                      ", well: ",
                                                      current_well_set$.__enclos_env__$private$well_id,
                                                      ", sample(s): ",
                                                      paste0(current_well_set$.__enclos_env__$private$sample_id,
                                                             collapse = " - "),
                                                      "]\n",
                                                      sep = "")
                                                } else if (is.na(current_well_set$.__enclos_env__$private$weighted_weight)
                                                           || current_well_set$.__enclos_env__$private$weighted_weight == 0) {
                                                  # scenario 2
                                                  current_well_set$.__enclos_env__$private$rf_validation <- 2
                                                  cat(format(Sys.time(),
                                                             "%Y-%m-%d %H:%M:%S"),
                                                      " - Warning: well-set avoided because invalid weighted weigth.\n",
                                                      "[trip: ,",
                                                      current_well_set$.__enclos_env__$private$trip_id,
                                                      ", activity: ",
                                                      current_well_set$.__enclos_env__$private$activity_id,
                                                      ", well: ",
                                                      current_well_set$.__enclos_env__$private$well_id,
                                                      ", sample(s): ",
                                                      paste0(current_well_set$.__enclos_env__$private$sample_id,
                                                             collapse = " - "),
                                                      "]\n",
                                                      sep = "")
                                                } else {
                                                  if (current_well_set$.__enclos_env__$private$weighted_samples_minus10 == 0
                                                      || current_well_set$.__enclos_env__$private$weighted_samples_plus10 == 0) {
                                                    # scenario 3
                                                    current_well_set$.__enclos_env__$private$rf_validation <- 3
                                                    current_well_set$.__enclos_env__$private$rf_total <- current_well_set$.__enclos_env__$private$weighted_weight / current_well_set$.__enclos_env__$private$weighted_samples_total
                                                  } else {
                                                    current_well_set$.__enclos_env__$private$rf_minus10 <- current_well_set$.__enclos_env__$private$weighted_weight_minus10 / current_well_set$.__enclos_env__$private$weighted_samples_minus10
                                                    current_well_set$.__enclos_env__$private$rf_plus10 <- current_well_set$.__enclos_env__$private$weighted_weight_plus10 / current_well_set$.__enclos_env__$private$weighted_samples_plus10
                                                    if (is.na(current_well_set$.__enclos_env__$private$rf_minus10)
                                                        || is.na(current_well_set$.__enclos_env__$private$rf_plus10)
                                                        || current_well_set$.__enclos_env__$private$rf_minus10 > threshold_rf_minus10
                                                        || current_well_set$.__enclos_env__$private$rf_plus10 > threshold_rf_plus10
                                                        || current_standardised_samples_sets_minus10_nb > threshold_frequency_rf_minus10
                                                        || current_standardised_samples_sets_plus10_nb > threshold_frequency_rf_plus10) {
                                                      # scenario 4
                                                      current_well_set$.__enclos_env__$private$rf_validation <- 4
                                                      current_well_set$.__enclos_env__$private$rf_total <- current_well_set$.__enclos_env__$private$weighted_weight / current_well_set$.__enclos_env__$private$weighted_samples_total
                                                    } else {
                                                      # scenario 5
                                                      current_well_set$.__enclos_env__$private$rf_validation <- 5
                                                    }
                                                  }
                                                }
                                                if (current_well_set$.__enclos_env__$private$rf_validation %in% c(4, 3)
                                                    && current_well_set$.__enclos_env__$private$rf_total > threshold_rf_total) {
                                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                      " - Warning: well-set \"rf_total\" argument superior to ",
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
                                                      "]\n",
                                                      sep = "")
                                                }
                                              } else {
                                                current_well_set$.__enclos_env__$private$rf_validation <- NA
                                              }
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
                                      "]\n",
                                      sep = "")
                                  if (full_trip_id == length(private$data_selected)) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
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
                                cat(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Empty data selected in the R6 object.\n",
                                    " - Process 2.8 (raised standardised sample set) cancelled.\n",
                                    sep = "")
                              } else {
                                for (full_trip_id in seq_len(length.out = length(private$data_selected))) {
                                  if (full_trip_id == 1) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Start process 2.8: raised standardised sample set.\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[full_trip_id] %in% private$id_not_full_trip_retained) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Warning: full trip avoided because a least one trip inside is missing.\n",
                                        "[trip: ",
                                        private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                        "]\n",
                                        sep = "")
                                    capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                                                   file = "NUL")
                                    capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                                   file = "NUL")
                                    if (length(x = unlist(current_trips$extract_l1_element_value(element = "wells"))) != 0) {
                                      capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                     file = "NUL")
                                      capture.output(current_wells$add(new_item = unlist(current_trips$extract_l1_element_value(element = "wells"))),
                                                     file = "NUL")
                                      if (length(x = current_wells$filter_l1(filter = "all(class($path$standardisedsampleset) == c(\"standardisedsamplesets\", \"list_t3\", \"R6\"))")) != 0) {
                                        capture.output(current_standardisedsamplesets <- t3::object_r6(class_name = "standardisedsamplesets"),
                                                       file = "NUL")
                                        capture.output(current_standardisedsamplesets$add(new_item = current_wells$extract_l1_element_value(element = "standardisedsampleset")),
                                                       file = "NUL")
                                        capture.output(current_standardisedsamplesets_bis <- t3::object_r6(class_name = "standardisedsamplesets"),
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
                                        "]\n",
                                        sep = "")
                                    for (partial_trip_id in seq_len(length.out = length(private$data_selected[[full_trip_id]]))) {
                                      current_trip <- private$data_selected[[full_trip_id]][[partial_trip_id]]
                                      if (length(current_trip$.__enclos_env__$private$wells) != 0) {
                                        capture.output(current_wells <- t3::object_r6(class_name = "wells"),
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
                                                capture.output(current_standardised_samples_sets <- t3::object_r6(class_name = "standardisedsamplesets"),
                                                               file = "NUL")
                                                capture.output(current_standardised_samples_sets$add(new_item = current_well$.__enclos_env__$private$standardisedsampleset$filter_l1(filter = paste0("$path$activity_id == \"",
                                                                                                                                                                                                     current_well_set$.__enclos_env__$private$activity_id,
                                                                                                                                                                                                     "\""))),
                                                               file = "NUL")
                                                if (current_well_set$.__enclos_env__$private$rf_validation %in% c(1, 2)
                                                    | is.na(current_well_set$.__enclos_env__$private$rf_validation)) {
                                                  cat(format(Sys.time(),
                                                             "%Y-%m-%d %H:%M:%S"),
                                                      " - Warning: raised factors not available for this well-set.\n",
                                                      "[trip: ,",
                                                      current_well_set$.__enclos_env__$private$trip_id,
                                                      ", activity: ",
                                                      current_well_set$.__enclos_env__$private$activity_id,
                                                      ", well: ",
                                                      current_well_set$.__enclos_env__$private$well_id,
                                                      ", sample(s): ",
                                                      paste0(current_well_set$.__enclos_env__$private$sample_id,
                                                             collapse = " - "),
                                                      "]\n",
                                                      sep = "")
                                                  current_standardised_samples_sets$modification_l1(modification = "$path$sample_number_weighted_set <- NA")
                                                  current_standardised_samples_sets$modification_l1(modification = "$path$sample_weigth_set <- NA")
                                                } else if (current_well_set$.__enclos_env__$private$rf_validation %in% c(3, 4)) {
                                                  current_rf_total <- current_well_set$.__enclos_env__$private$rf_total
                                                  current_standardised_samples_sets$modification_l1(modification = paste0("$path$sample_number_weighted_set <- $path$sample_number_weighted * ",
                                                                                                                          current_rf_total))
                                                  current_standardised_samples_sets$modification_l1(modification = "$path$sample_weigth_set <- $path$sample_weight_unit * $path$sample_number_weighted_set / 1000")
                                                } else if (current_well_set$.__enclos_env__$private$rf_validation == 5) {
                                                  current_rf_minus10 <- current_well_set$.__enclos_env__$private$rf_minus10
                                                  current_rf_plus10 <- current_well_set$.__enclos_env__$private$rf_plus10
                                                  capture.output(current_standardised_samples_sets_minus10 <- t3::object_r6(class_name = "standardisedsamplesets"),
                                                                 file = "NUL")
                                                  capture.output(current_standardised_samples_sets_minus10$add(new_item = current_standardised_samples_sets$filter_l1(filter = "$path$sample_weight_unit <= 10")),
                                                                 file = "NUL")
                                                  current_standardised_samples_sets_minus10$modification_l1(modification = paste0("$path$sample_number_weighted_set <- $path$sample_number_weighted * ",
                                                                                                                                  current_rf_minus10))
                                                  capture.output(current_standardised_samples_sets_plus10 <- t3::object_r6(class_name = "standardisedsamplesets"),
                                                                 file = "NUL")
                                                  capture.output(current_standardised_samples_sets_plus10$add(new_item = current_standardised_samples_sets$filter_l1(filter = "$path$sample_weight_unit > 10")),
                                                                 file = "NUL")
                                                  current_standardised_samples_sets_plus10$modification_l1(modification = paste0("$path$sample_number_weighted_set <- $path$sample_number_weighted * ",
                                                                                                                                 current_rf_plus10))
                                                  current_standardised_samples_sets$modification_l1(modification = "$path$sample_weigth_set <- $path$sample_weight_unit * $path$sample_number_weighted_set / 1000")
                                                } else {
                                                  cat(format(Sys.time(),
                                                             "%Y-%m-%d %H:%M:%S"),
                                                      " - Error: raised factors verifications is not valide.\n",
                                                      "[trip: ,",
                                                      current_well_set$.__enclos_env__$private$trip_id,
                                                      ", activity: ",
                                                      current_well_set$.__enclos_env__$private$activity_id,
                                                      ", well: ",
                                                      current_well_set$.__enclos_env__$private$well_id,
                                                      ", sample(s): ",
                                                      paste0(current_well_set$.__enclos_env__$private$sample_id,
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
                                    }
                                  }
                                  cat(format(Sys.time(),
                                             "%Y-%m-%d %H:%M:%S"),
                                      " - Process 2.8 successfull on item \"",
                                      names(private$data_selected)[full_trip_id],
                                      "\".\n",
                                      "[trip: ",
                                      private$data_selected[[full_trip_id]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  if (full_trip_id == length(private$data_selected)) {
                                    cat(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - End 2.8 process: raised standardised sample set.\n",
                                        sep = "")
                                  }
                                }
                              }
                            },
                            # path to level 3 ----
                            #' @description Temporary link to the R object model with Antoine D. modelisation process.
                            path_to_level3 = function() {
                              cat(format(Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - Start path creation for level 3.\n",
                                  sep = "")
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
                                capture.output(current_trips <- t3::object_r6(class_name = "trips"),
                                               file = "NUL")
                                capture.output(current_trips$add(new_item = private$data_selected[[full_trip_id]]),
                                               file = "NUL")
                                for (partial_trip_id in seq_len(length.out = current_trips$count())) {
                                  current_trip <- current_trips$extract(id = partial_trip_id)[[1]]
                                  if (length(x = current_trip$.__enclos_env__$private$activities) != 0) {
                                    capture.output(current_activities <- t3::object_r6(class_name = "activities"),
                                                   file = "NUL")
                                    capture.output(current_activities$add(new_item = current_trip$.__enclos_env__$private$activities),
                                                   file = "NUL")
                                    tmp_activity <- list(id_act = unlist(current_activities$extract_l1_element_value(element = "activity_id")),
                                                         lat = unlist(current_activities$extract_l1_element_value(element = "activity_latitude")),
                                                         lon = unlist(current_activities$extract_l1_element_value(element = "activity_longitude")),
                                                         fmod = unlist(current_activities$extract_l1_element_value(element = "school_type")),
                                                         date_act = do.call("c", current_activities$extract_l1_element_value(element = "activity_date")),
                                                         vessel = rep(x = current_trip$.__enclos_env__$private$vessel_id,
                                                                      current_activities$count()),
                                                         id_trip = unlist(current_activities$extract_l1_element_value(element = "trip_id")),
                                                         landingdate = unlist(current_activities$extract_l1_element_value(element = "landing_date")),
                                                         ocean = unlist(current_activities$extract_l1_element_value(element = "ocean")),
                                                         code_act_type = unlist(current_activities$extract_l1_element_value(element = "activity_code")))
                                    tmp_activity <- dplyr::bind_rows(tmp_activity)
                                    act <- rbind(act,
                                                 tmp_activity)
                                    if (length(x = unlist(current_activities$extract_l1_element_value(element = "elementarycatches"))) != 0) {
                                      capture.output(current_elementarycatches <- t3::object_r6(class_name = "elementarycatches"),
                                                     file = "NUL")
                                      capture.output(current_elementarycatches$add(new_item = unlist(current_activities$extract_l1_element_value(element = "elementarycatches"))),
                                                     file = "NUL")
                                      tmp_elementarycatch <- list(id_act = unlist(current_elementarycatches$extract_l1_element_value(element = "activity_id")),
                                                                  w_lb_t3 = unlist(current_elementarycatches$extract_l1_element_value(element = "catch_weight_category_corrected")),
                                                                  sp_code = unlist(current_elementarycatches$extract_l1_element_value(element = "specie_code")),
                                                                  sp = unlist(current_elementarycatches$extract_l1_element_value(element = "specie_code3l")),
                                                                  wcat = unlist(current_elementarycatches$extract_l1_element_value(element = "corrected_logbook_category")))
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
                                    capture.output(current_wells <- t3::object_r6(class_name = "wells"),
                                                   file = "NUL")
                                    capture.output(current_wells$add(new_item = current_trip$.__enclos_env__$private$wells),
                                                   file = "NUL")
                                    if (any(! is.na(current_wells$extract_l1_element_value(element = "standardisedsampleset")))) {
                                      standardisedsampleset_not_na <- which(! is.na(current_wells$extract_l1_element_value(element = "standardisedsampleset")))
                                      capture.output(current_standardisedsamplesets <- t3::object_r6(class_name = "list_t3"),
                                                     file = "NUL")
                                      capture.output(current_standardisedsamplesets$add(new_item = lapply(X = standardisedsampleset_not_na,
                                                                                                          FUN = function(a) {
                                                                                                            current_wells$extract_l1_element_value(element = "standardisedsampleset")[[a]]
                                                                                                          })),
                                                     file = "NUL")
                                      capture.output(current_standardisedsamplesets_data <- t3::object_r6(class_name = "standardisedsamplesets"),
                                                     file = "NUL")
                                      capture.output(current_standardisedsamplesets_data$add(new_item = unlist(current_standardisedsamplesets$extract_l1_element_value(element = "data"))),
                                                     file = "NUL")
                                      tmp_standardisedsampleset <- list(id_act = unlist(current_standardisedsamplesets_data$extract_l1_element_value(element = "activity_id")),
                                                                        sp_code = unlist(current_standardisedsamplesets_data$extract_l1_element_value(element = "specie_code")),
                                                                        sp = unlist(current_standardisedsamplesets_data$extract_l1_element_value(element = "specie_code3l")),
                                                                        wcat = unlist(current_standardisedsamplesets_data$extract_l1_element_value(element = "sample_category")),
                                                                        w_fit_t3 = unlist(current_standardisedsamplesets_data$extract_l1_element_value(element = "sample_weigth_set")))
                                      tmp_standardisedsampleset <- dplyr::bind_rows(tmp_standardisedsampleset)
                                      samw <- rbind(samw,
                                                    tmp_standardisedsampleset)
                                      if (length(x = current_standardisedsamplesets_data$filter_l1(filter = "length($path$sample_id) == 1")) != 0) {
                                        capture.output(current_standardisedsamplesets_data_one_sample <- t3::object_r6(class_name = "standardisedsamplesets"),
                                                       file = "NUL")
                                        capture.output(current_standardisedsamplesets_data_one_sample$add(new_item = current_standardisedsamplesets_data$filter_l1(filter = "length($path$sample_id) == 1")),
                                                       file = "NUL")
                                        tmp_standardisedsampleset_one_sample_qt <- list(id_act = unlist(current_standardisedsamplesets_data_one_sample$extract_l1_element_value(element = "activity_id")),
                                                                                        id_sample = unlist(current_standardisedsamplesets_data_one_sample$extract_l1_element_value(element = "sample_id")),
                                                                                        quality = unlist(current_standardisedsamplesets_data_one_sample$extract_l1_element_value(element = "sample_quality")),
                                                                                        type = unlist(current_standardisedsamplesets_data_one_sample$extract_l1_element_value(element = "sample_type")))
                                      }
                                      if (length(x = current_standardisedsamplesets_data$filter_l1(filter = "length($path$sample_id) != 1")) != 0) {
                                        capture.output(current_standardisedsamplesets_data_multi_samples <- t3::object_r6(class_name = "standardisedsamplesets"),
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
                                                                                                                                                        quality = rep(x = current_standardisedsampleset$.__enclos_env__$private$sample_quality,
                                                                                                                                                                      current_number_samples),
                                                                                                                                                        type = rep(x = current_standardisedsampleset$.__enclos_env__$private$sample_type,
                                                                                                                                                                   current_number_samples))
                                                                                                   })
                                      }
                                      tmp_standardisedsampleset_qt <- dplyr::as_tibble()
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
                                      capture.output(current_wellplans <- t3::object_r6(class_name = "elementarywellplans"),
                                                     file = "NUL")
                                      capture.output(current_wellplans$add(new_item = unlist(current_wells$extract_l1_element_value(element = "wellplan"))),
                                                     file = "NUL")
                                      tmp_elementarywellplan <- list(id_well = unlist(current_wellplans$extract_l1_element_value(element = "well_id")),
                                                                     id_act = unlist(current_wellplans$extract_l1_element_value(element = "activity_id")),
                                                                     id_sample = unlist(current_wellplans$extract_l1_element_value(element = "sample_id")),
                                                                     sp_code = unlist(current_wellplans$extract_l1_element_value(element = "specie_code")),
                                                                     code3l = unlist(current_wellplans$extract_l1_element_value(element = "specie_code3l")),
                                                                     weight = unlist(current_wellplans$extract_l1_element_value(element = "wellplan_weight")),
                                                                     wcat_well = unlist(current_wellplans$extract_l1_element_value(element = "wellplan_weigth_category_label")))
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
                                                                                      sp_code,
                                                                                      sp,
                                                                                      wcat) %>%
                                                                        dplyr::summarise(w_fit_t3 = sum(w_fit_t3)) %>%
                                                                        dplyr::ungroup())
                              raw_inputs_level3[[4]] <- sset
                              raw_inputs_level3[[5]] <- wp
                              cat(format(Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - End path creation for level 3.\n",
                                  sep = "")
                              data_level3 <- append(data_level3,
                                                    list(raw_inputs_level3))
                              names(data_level3)[length(data_level3)] <- "raw_inputs_level3"
                              return(data_level3)
                            },
                            # process 3.1: data preparatory ----
                            #' @description Data preparatory for the t3 modelling process (level 3).
                            #' @param inputs_level3 Object of type \code{\link[base]{data.frame}} expected. Inputs of levels 3 (see function path to level 3).
                            #' @param inputs_level3_path Object of type \code{\link[base]{character}} expected. Path to the folder containing yearly data output of the level 1 and 2 (output of the function the path to level 3). If provide, replace the inputs_level3 object.
                            #' @param outputs_directory Object of type \code{\link[base]{character}} expected. Path of the t3 processes outputs directory.
                            #' @param periode_reference Object of type \code{\link[base]{integer}} expected. Year(s) period of reference for modelling estimation.
                            #' @param target_year Object of type \code{\link[base]{integer}} expected. Year of interest for the model estimation and prediction.Default value is current year -1.
                            #' @param period_duration Object of type \code{\link[base]{integer}} expected. number of years use for the modelling. The default value is 5
                            #' @param distance_maximum Object of type \code{\link[base]{integer}} expected. Maximum distance between all sets of a sampled well. By default 5.
                            #' @param number_sets_maximum Object of type \code{\link[base]{integer}} expected. Maximum number of sets allowed in mixture. By default 5.
                            #' @param set_weight_minimum Object of type \code{\link[base]{integer}} expected. Minimum set size considered. Remove smallest set for which sample could not be representative. By default 6 t.
                            #' @param minimum_set_frequency Object of type \code{\link[base]{numeric}} expected. Minimum threshold proportion of set in awell to be used for model training in the process. By default 0.1.
                            #' @param vessel_id_ignored Object of type \code{\link[base]{integer}} expected. Specify list of vessel(s) id(s) to be ignored in the model estimation and prediction .By default NULL.
                            data_preparatory = function(inputs_level3 = NULL,
                                                        inputs_level3_path = NULL,
                                                        outputs_directory,
                                                        periode_reference = NULL,
                                                        target_year = as.integer(lubridate::year(Sys.time()-1)),
                                                        period_duration = 4L,
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
                              } else if (class(target_year) != "integer"
                                         || length(target_year) != 1
                                         || nchar(target_year) != 4) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"target_year\" argument, one value of class integer expected with a format on 4 digits.\n",
                                    sep = "")
                                stop()
                              } else if (class(period_duration) != "integer"
                                         || length(period_duration) != 1
                                         || period_duration > 99) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"period_duration\" argument, one value of class integer expected with  maximum value 99.\n",
                                    sep = "")
                                stop()
                              } else if (!is.null(periode_reference)
                                         && class(periode_reference) != "integer") {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"periode_reference\" argument, class integer expected.\n",
                                    sep = "")
                                stop()
                              } else if (class(distance_maximum) != "integer"
                                         || length(distance_maximum) != 1) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"distance_maximum\" argument, one value of class integer expected.\n",
                                    sep = "")
                                stop()
                              } else if (class(number_sets_maximum) != "integer"
                                         || length(number_sets_maximum) != 1) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"number_sets_maximum\" argument, one value of class integer expected.\n",
                                    sep = "")
                                stop()
                              } else if (class(set_weight_minimum) != "integer"
                                         || length(set_weight_minimum) != 1) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"set_weight_minimum\" argument, one value of class integer expected.\n",
                                    sep = "")
                                stop()
                              } else if (class(minimum_set_frequency) != "numeric"
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
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Start process 3.1: data preparatory.\n",
                                    sep = "")
                                outputs_directory_name <- format(Sys.time(),
                                                                 "%Y%m%d_%H%M%S_t3_level3_outputs")
                                dir.create(path = file.path(outputs_directory,
                                                            outputs_directory_name))
                                for (directory in c("data",
                                                    "figures",
                                                    "tables",
                                                    "functions",
                                                    "outputs")) {
                                  dir.create(path = file.path(outputs_directory,
                                                              outputs_directory_name,
                                                              directory))
                                }
                                if (is.null(periode_reference)) {
                                  periode_reference <- seq.int(from = target_year,
                                                               to = target_year - period_duration)
                                }
                                if (! is.null(inputs_level3_path)) {
                                  # load from t3 levels 1 and 2 outputs and merge accordingly to the target_year ----
                                  file_available <- list.files(path = inputs_level3_path,
                                                               pattern = "inputs_level3_")
                                  file_year <-  as.numeric(do.call(rbind,
                                                                   strsplit(x = file_available,
                                                                            split = "_"))[,3])
                                  target_file <- file_available[file_year %in% target_year:(target_year-period_duration)]
                                  dataset_target <- vector("list",
                                                           length = 5)
                                  names(dataset_target) <- c("act_chr",
                                                             "catch_set_lb",
                                                             "samw",
                                                             "sset",
                                                             "wp")
                                  dataset_target<- lapply(dataset_target,
                                                          function(x){
                                                            x <- vector("list",
                                                                        length = length(target_file))
                                                          })
                                  for(x in seq_len(length.out = length(target_file))){
                                    load(file.path(inputs_level3_path,
                                                   target_file[x],
                                                   fsep ="/"))
                                    # sets characteristics
                                    dataset_target$act_chr[[x]] <- data_level3[[1]][[1]]
                                    # catch by set, species and categories from logbook (t3 level 1)
                                    dataset_target$catch_set_lb[[x]] <- data_level3[[1]][[2]]
                                    # catch by set, species and categories (t3 level 2)
                                    dataset_target$samw[[x]] <- data_level3[[1]][[3]]
                                    # link between sample and set, + sample quality and type
                                    dataset_target$sset[[x]] <- data_level3[[1]][[4]]
                                    # well plan
                                    dataset_target$wp[[x]] <- data_level3[[1]][[5]]
                                  }
                                  dataset_target <- lapply(X = dataset_target,
                                                           FUN = function(x){
                                                             return(unique(do.call(rbind,x)))
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
                                # catch_set_lb<-catch_set_lb[catch_set_lb$year > first_year & catch_set_lb$year <= target_year, ]
                                catch_set_lb<-catch_set_lb[catch_set_lb$year %in% periode_reference,]
                                act_chr$year <- lubridate::year(x = act_chr$date_act)
                                act_chr<-act_chr[act_chr$year %in% periode_reference, ]
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
                                # compute set weight in each sample to detect non representiveness of the sample
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
                                # homogeneous fishing mode in sample
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
                                                                                                  "vessel")],
                                                              by = c("id_act", "year"))
                                data_lb_sample_screened = list(data4mod = data4mod)
                                # export to global environment
                                outputs_level3_process1 <- list(sample_set_char = sample_set_char,
                                                                data_selected = data_selected,
                                                                data_sample_extract = data_sample_extract,
                                                                data_lb_sample_screened = data_lb_sample_screened)
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - End process 3.1: data preparatory.\n",
                                    sep = "")
                                return(list("raw_inputs_level3" = inputs_level3,
                                            "outputs_directory" = file.path(outputs_directory,
                                                                            outputs_directory_name),
                                            "outputs_level3_process1" = outputs_level3_process1))
                              }
                            },
                            # process 3.2: random forest models ----
                            #' @description Modelling proportions in sets througth random forest models.
                            #' @param outputs_level3_process1 Object of type \code{\link[base]{data.frame}} expected. Output table data_lb_sample_screened from process 3.1.
                            #' @param num.trees Object of type \code{\link[base]{integer}} expected. Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times. The default value is 1000.
                            #' @param mtry Object of type \code{\link[base]{integer}} expected. Number of variables randomly sampled as candidates at each split. The default value is 2.
                            #' @param min.node.size Object of type \code{\link[base]{numeric}} expected. Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time).The default value is 5.
                            #' @param seed_number Object of type \code{\link[base]{integer}} expected. Set the initial seed for the modelling. The default value is 7.
                            #' @param small_fish_only Object of type \code{\link[base]{logical}} expected. Whether the model estimate proportion for small fish only (< 10 kg).
                            random_forest_models = function(outputs_level3_process1,
                                                            num.trees = 1000L,
                                                            mtry = 2L,
                                                            min.node.size = 5,
                                                            seed_number = 7L,
                                                            small_fish_only = F) {
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
                              # select for small fish catch only if parameter = T
                              if(small_fish_only == F){
                                data4mod <- data4mod %>% dplyr::group_by(id_act,date_act, year, mon, lat, lon, sp, fmod, ocean, vessel, wtot_lb_t3) %>%
                                  dplyr::summarise(prop_lb = sum(prop_lb),
                                                   prop_t3 = sum(prop_t3),
                                                   w_lb_t3 = sum(w_lb_t3)) %>%
                                  dplyr::ungroup()

                                # aggregate(formula = cbind(prop_lb, prop_t3, w_lb_t3) ~ id_act + date_act + year + mon + lat + lon + sp + fmod + ocean + vessel + wtot_lb_t3,
                                #                     data = data4mod,
                                # FUN = sum)
                              } else {
                                data4mod <- data4mod %>% dplyr::mutate(prop_lb = replace (prop_lb, wcat == "p10", value = 0),
                                                                       prop_t3 = replace (prop_t3, wcat == "p10", value = 0)) %>%
                                  dplyr::group_by(id_act,date_act, year, mon, lat, lon, sp, fmod, ocean, vessel, wtot_lb_t3) %>%
                                  dplyr::summarise(prop_lb = sum(prop_lb),
                                                   prop_t3 = sum(prop_t3),
                                                   w_lb_t3 = sum(w_lb_t3)) %>%
                                  dplyr::ungroup()
                              }
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
                              #####################
                              return(outputs_level3_process2)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.2: random forest models.\n",
                                  sep = "")
                              #################

                              # if (exists(x = "data_level3",
                              #            envir = .GlobalEnv)) {
                              #   data_level3 <- get(x = "data_level3",
                              #                      envir = .GlobalEnv)
                              #   data_level3 <- append(data_level3,
                              #                         list(outputs_level3_process2))
                              #   names(data_level3)[length(data_level3)] <- "outputs_level3_process2"
                              # } else {
                              #   data_level3 <- list("outputs_level3_process1" = outputs_level3_process1,
                              #                       "outputs_level3_process2" = outputs_level3_process2)
                              # }
                              # assign(x = "data_level3",
                              #        value = data_level3,
                              #        envir = .GlobalEnv)
                              # cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                              #     " - End process 3.2: random forest models.\n",
                              #     sep = "")
                            },
                            # process 3.3: models checking ----
                            #' @description Load each full model and compute figures and tables to check the model quality. Furthermore, create a map of samples used for each model and relationship between logbook reports and samples.
                            #' @param outputs_level3_process2 Object of type \code{\link[base]{list}} expected. Outputs models and data from process 3.2.
                            #' @param outputs_path Object of type \code{\link[base]{character}} expected. Outputs directory path.
                            #' @param plot_sample \code{\link[base]{logical}}. Whether the sample figure is computed. Default value = F
                            #' @param avdth_patch_coord parameter waiting for coordinate conversion patch from avdth database
                            models_checking = function(outputs_level3_process2,
                                                       outputs_path,
                                                       plot_sample = F,
                                                       avdth_patch_coord = F) {
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
                                # check data subset for modeling ----
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
                                if(plot_sample == T){
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
                                }
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
                                if(avdth_patch_coord == T){
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
                                  correlogram_resp <- forecast::ggAcf(current_model_data$resp[order(current_model_data$date_act)], lag.max = 300)
                                  # correlogram_resp <- furdeb::ggplot_corr(data = current_model_data$resp[order(current_model_data$date_act)],
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
                                  # correlogram_res <- furdeb::ggplot_corr(data = current_model_data$res[order(current_model_data$date_act)],
                                  #                                        lag_max = 300)
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
                                }
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

                                  test$fit <- predict(model,data = test)$predictions

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
                              # if (exists(x = "data_level3",
                              #            envir = .GlobalEnv)) {
                              #   data_level3 <- get(x = "data_level3",
                              #                      envir = .GlobalEnv)
                              #   data_level3 <- append(data_level3,
                              #                         list(outputs_level3_process3))
                              #   names(data_level3)[length(data_level3)] <- "outputs_level3_process3"
                              # } else {
                              #   data_level3 <- list("outputs_level3_process2" = outputs_level3_process2,
                              #                       "outputs_level3_process3" = outputs_level3_process3)
                              # }
                              # assign(x = "data_level3",
                              #        value = data_level3,
                              #        envir = .GlobalEnv)
                              return(outputs_level3_process3)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.3: models checking.\n",
                                  sep = "")
                            },
                            # process 3.4: data formatting for predictions ----
                            #' @description Formatting data for model predictions.
                            #' @param inputs_level3 Object of type \code{\link[base]{data.frame}} expected. Inputs of levels 3 (see function path to level 3).
                            #' @param outputs_level3_process1 Object of type \code{\link[base]{data.frame}} expected. Output table data_lb_sample_screened from process 3.1.
                            #' @param target_year Object of type \code{\link[base]{integer}} expected. The year of interest for the model estimation and prediction.
                            #' @param vessel_id_ignored Object of type \code{\link[base]{integer}} expected. Specify here vessel(s) id(s) if you want to ignore it in the model estimation and prediction .By default NULL.
                            #' @param small_fish_only Object of type \code{\link[base]{logical}} expected. Whether the model estimate proportion for small fish only (< 10 kg).

                            data_formatting_for_predictions = function(inputs_level3,
                                                                       outputs_level3_process1,
                                                                       target_year,
                                                                       vessel_id_ignored = NULL,
                                                                       small_fish_only = F) {
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

                              # catches keep onboard only = set
                              catch_set_lb <- catch_set_lb[catch_set_lb$sp_code %in% c(1, 2, 3), ]
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
                                dplyr::group_by(id_act, date_act, sp, wcat, code_act_type) %>%
                                dplyr::summarise(w_lb_t3 = sum(w_lb_t3)) %>% ungroup()

                              # set use for modeling to remove for prediction
                              data4mod <- outputs_level3_process1
                              sampleset <- unique(data4mod[, c("id_act",
                                                               "fmod",
                                                               "ocean",
                                                               "year")])
                              act_chr$yr <- lubridate::year(x = act_chr$date_act)
                              act_chr$mon <- lubridate::month(x = act_chr$date_act)
                              act_chr$fmod <- as.factor(act_chr$fmod)
                              act_chr$vessel <- as.factor(act_chr$vessel)
                              # non sampled set
                              # reduce data to the period considered in the modeling and check data availability
                              act_chr <- act_chr[act_chr$yr %in% target_year, ]
                              if (nrow(act_chr) == 0) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: NO data available for the selected target_year.\n",
                                    sep = "")
                                stop()
                              }
                              # add the weight by categories, species from logbook (corrected by t3 level 1)
                              sets <- dplyr::inner_join(act_chr,
                                                        catch_set_lb,
                                                        by = c("id_act", "date_act", "code_act_type"))
                              # catches keep onboard only = set
                              sets <- sets[sets$code_act_type %in% c(0,1,2), ]
                              sets$sp <- factor(sets$sp)
                              sets$ocean <- factor(sets$ocean)
                              sets <- sets[! sets$vessel %in% vessel_id_ignored, ]
                              sets$sp_cat <- factor(paste(sets$sp,
                                                          sets$wcat,
                                                          sep = "_"))
                              sets$sp <- NULL
                              sets$wcat <- NULL

                              # calculate proportion of weight from t3 level 1
                              sets_wide <- tidyr::spread(data = sets,
                                                         key = sp_cat,
                                                         value = w_lb_t3,
                                                         fill = 0)
                              sets_wide$wtot_lb_t3 <- rowSums(sets_wide[, c("YFT_p10", "BET_p10", "SKJ_m10", "YFT_m10", "BET_m10")])
                              # remove activity with no catch
                              sets_wide <- sets_wide[sets_wide$wtot_lb_t3 > 0, ]

                              tmp <- sets_wide[, names(sets_wide) %in% levels(sets$sp_cat)]
                              tmp <- prop.table(as.matrix(tmp), 1)
                              sets_wide_tmp <- sets_wide
                              sets_wide_tmp[, names(sets_wide_tmp) %in% colnames(tmp)] <- tmp
                              sets_long <- tidyr::gather(data = sets_wide_tmp,
                                                         key = "sp_cat",
                                                         value = "prop_lb",
                                                         "BET_m10",
                                                         "BET_p10",
                                                         "SKJ_m10",
                                                         "YFT_m10",
                                                         "YFT_p10")
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
                                test$fmod2 <- predict(rfg,
                                                      data = test)$predictions
                                tmp <- dplyr::left_join(sets_long,
                                                        test[, c("id_act","fmod2")],
                                                        by = "id_act")
                                tmp$fmod[tmp$fmod == 3] <- tmp$fmod2[tmp$fmod == 3]
                                tmp$fmod2 <- NULL
                                sets_long <- droplevels(tmp)
                              }
                              sets_long <- tidyr::separate(data = sets_long,
                                                           col = sp_cat,
                                                           into = c("sp","wcat"),
                                                           sep = "_")
                              # filter data for small fish catch estimation only
                              if(small_fish_only == F){
                                sets_long <- sets_long %>% dplyr::group_by(id_act, id_trip, date_act, yr, mon, lat, lon, sp, fmod, ocean, vessel, wtot_lb_t3) %>%
                                  dplyr::summarise(prop_lb = sum(prop_lb)) %>%
                                  dplyr::ungroup()

                              } else {
                                sets_long <- sets_long %>% dplyr::mutate(prop_lb = replace (prop_lb, wcat == "p10", value = 0)) %>%
                                  dplyr::group_by(id_act, id_trip, date_act, yr, mon, lat, lon, sp, fmod, ocean, vessel, wtot_lb_t3) %>%
                                  dplyr::summarise(prop_lb = sum(prop_lb)) %>%
                                  dplyr::ungroup()
                              }
                              # test unitaire total catch equal
                              # sets_long %>% dplyr::filter(!duplicated(id_act)) %>% dplyr::summarise(tot = sum(wtot_lb_t3))
                              #   dplyr::summarise(tot = sum(wtot_lb_t3))
                              # tmp %>% dplyr::filter(!duplicated(id_act)) %>% dplyr::summarise(tot = sum(wtot_lb_t3))
                              # tmp2 %>% dplyr::filter(!duplicated(id_act)) %>% dplyr::summarise(tot = sum(wtot_lb_t3))
                              outputs_level3_process4 <- append(outputs_level3_process4,
                                                                list(list("sets_long" = sets_long,
                                                                          "sets_wide" = sets_wide)))
                              names(outputs_level3_process4)[length(outputs_level3_process4)] <- "nonsampled_sets"

                              # if (exists(x = "data_level3",
                              #            envir = .GlobalEnv)) {
                              #   data_level3 <- get(x = "data_level3",
                              #                      envir = .GlobalEnv)
                              #   data_level3 <- append(data_level3,
                              #                         list(outputs_level3_process4))
                              #   names(data_level3)[length(data_level3)] <- "outputs_level3_process4"
                              # } else {
                              #   data_level3 <- list("outputs_level3_process1" = outputs_level3_process2,
                              #                       "outputs_level3_process4" = outputs_level3_process4)
                              # }
                              # assign(x = "data_level3",
                              #        value = data_level3,
                              #        envir = .GlobalEnv)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.4: data formatting for predictions.\n",
                                  sep = "")
                              return(outputs_level3_process4)
                            },
                            # process 3.5: model predictions ----
                            #' @description Model predictions for the species composition and computing of catches.
                            #' @param outputs_level3_process2 Object of type \code{\link[base]{list}} expected. Outputs from level 3 process 2 (random forest models).
                            #' @param outputs_level3_process4 Object of type \code{\link[base]{list}} expected. Outputs from level 3 process 4 (data formatting for predictions).
                            #' @param outputs_path Object of type \code{\link[base]{character}} expected. Outputs directory path.
                            #' @param ci Object of type \code{\link[base]{logical}} expected. Logical indicating whether confidence interval is computed. The default value is FALSE as it is a time consuming step.
                            #' @param ci_type Type of confidence interval to compute. The default value is "all". Other options are "set" for ci on each set, "t1" for ci on nominal catch by species, "t1-fmod" for ci on nominal catch by species and fishing mode "t2" and "t2-fmod" for ci by 1 degree square and month. A vector of several ci option can be provided. ci_type are computed only if  the ci parameter is TRUE.
                            #' @param Nboot Object of type \code{\link[base]{numeric}} expected. The number of bootstrap samples desired for the ci computation. The default value is 10.
                            #' @param plot_predict Object of type \code{\link[base]{logical}} expected. Logical indicating whether maps of catch at size have to be done.
                            model_predictions = function(outputs_level3_process2,
                                                         outputs_level3_process4,
                                                         outputs_path,
                                                         ci = FALSE,
                                                         ci_type = "all",
                                                         Nboot = 50,
                                                         plot_predict = FALSE) {
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.5: model predictions.\n",
                                  sep = "")
                              figures_directory <- file.path(outputs_path,
                                                             "figures")
                              names(figures_directory) <- "figures"
                              tables_directory <- file.path(outputs_path,
                                                            "tables")
                              names(tables_directory) <- "tables"
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
                              # Compute estimates for SKJ and YFT ----
                              outputs_level3_process5 <- vector(mode = "list",
                                                                length = 5)
                              names(outputs_level3_process5) <- c("Estimated_catch",
                                                                  "Estimated_catch_ST",
                                                                  "Boot_output_list",
                                                                  "Boot_output_list_ST",
                                                                  "Final_output")
                              sets_long <- outputs_level3_process4[[1]][[1]]
                              ocean_level <- unique(do.call(what = rbind,
                                                            args = strsplit(names(outputs_level3_process2),
                                                                            split = "_"))[,1])
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
                                    sets_long_specie <- sets_long_ocean[sets_long_ocean$sp == species, ]
                                    for (fishing_mode in unique(sets_long_specie$fmod)) {
                                      sets_long_fishing_mode <- sets_long_specie[sets_long_specie$fmod == fishing_mode, ]
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - Ongoing process 3.5 (Predictions step) for ocean \"",
                                          ocean,
                                          "\", species \"",
                                          species,
                                          "\" and fishing mode \"",
                                          fishing_mode,
                                          "\"",
                                          ".\n",
                                          sep = "")
                                      if(nrow(sets_long_fishing_mode) > 0){
                                        # models
                                        current_outputs_level3_process2 <- outputs_level3_process2[[paste(ocean,
                                                                                                          species,
                                                                                                          fishing_mode,
                                                                                                          sep = "_")]]
                                        res <- tunapredict(sample_data = current_outputs_level3_process2[[1]],
                                                           allset_data = sets_long_fishing_mode,
                                                           Ntree = 1000,
                                                           Nmtry = 2,
                                                           Nseed = 7)

                                        outputs_level3_process5[[1]] <- append(outputs_level3_process5[[1]],
                                                                               list(res))
                                        names(outputs_level3_process5[[1]])[length(outputs_level3_process5[[1]])] <- paste(ocean,
                                                                                                                           species,
                                                                                                                           fishing_mode,
                                                                                                                           sep = "_")
                                        cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
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
                              # Standardize SKJ and YFT 'Estimated catch' and compute BET estimated catch ----
                              for (ocean in ocean_level) {
                                outputs_level3_process5_ocean <- outputs_level3_process5[[1]][grep(pattern = paste(ocean,"_", sep = ""),
                                                                                                   x = names(outputs_level3_process5[[1]]))]
                                boot_tmp_element <- dplyr::bind_rows(outputs_level3_process5_ocean)
                                boot_tmp_element_wide <- tidyr::spread(data = boot_tmp_element[,!names(boot_tmp_element) %in%  c("w_lb_t3","prop_lb","tlb","year","resp")],
                                                                       key = "sp",
                                                                       value = fit_prop)
                                boot_tmp_element_wide$S <- boot_tmp_element_wide$SKJ + boot_tmp_element_wide$YFT
                                boot_tmp_element_wide$SKJ <- base::ifelse(test = boot_tmp_element_wide$S > 1,
                                                                          yes = boot_tmp_element_wide$SKJ/boot_tmp_element_wide$S,
                                                                          no = boot_tmp_element_wide$SKJ)
                                boot_tmp_element_wide$YFT <- base::ifelse(test = boot_tmp_element_wide$S > 1,
                                                                          yes = boot_tmp_element_wide$YFT/boot_tmp_element_wide$S,
                                                                          no = boot_tmp_element_wide$YFT)
                                boot_tmp_element_wide$BET <- 1 - (boot_tmp_element_wide$SKJ + boot_tmp_element_wide$YFT)

                                boot_tmp_element_long <- tidyr::gather(data = boot_tmp_element_wide,
                                                                       key = "sp",
                                                                       value = "fit_prop_t3_ST",
                                                                       "BET", "SKJ", "YFT")
                                boot_tmp_element <- dplyr::left_join(boot_tmp_element_long, boot_tmp_element, by = c("id_act", "date_act", "lat", "lon", "fmod",  "vessel", "id_trip", "ocean", "yr", "mon", "wtot_lb_t3", "sp","data_source"))

                                boot_tmp_element$catch_set_fit <- boot_tmp_element$wtot_lb_t3 * boot_tmp_element$fit_prop_t3_ST

                                outputs_level3_process5[[2]] <- append(outputs_level3_process5[[2]],
                                                                       list(boot_tmp_element))
                                names(outputs_level3_process5[[2]])[length(outputs_level3_process5[[2]])] <- paste("ocean",
                                                                                                                   ocean,
                                                                                                                   sep = "_")
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
                                        cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                            " - Ongoing process 3.5 (Bootstrap step) for ocean \"",
                                            ocean,
                                            "\", species \"",
                                            species,
                                            "\" and fishing mode \"",
                                            fishing_mode,
                                            "\"",
                                            ".\n",
                                            sep = "")
                                        if(nrow(sets_long_fishing_mode) > 0){
                                          current_outputs_level3_process2 <- outputs_level3_process2[[paste(ocean,
                                                                                                            species,
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
                                                                                                                             species,
                                                                                                                             fishing_mode,
                                                                                                                             sep = "_")
                                          cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
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
                                outputs_level3_process5_ocean <- outputs_level3_process5[[3]][grep(pattern = paste(ocean,"_", sep = ""),
                                                                                                   x = names(outputs_level3_process5[[3]]))]

                                list_boot_ST_ocean <- vector("list", length = length(outputs_level3_process5_ocean[[1]]))

                                for(element in (seq.int(from = 1,
                                                        to = length(outputs_level3_process5_ocean[[1]])))){
                                  boot_tmp_element <- lapply(outputs_level3_process5_ocean, function(l) l[[element]])
                                  boot_tmp_element <- dplyr::bind_rows(boot_tmp_element)
                                  boot_tmp_element_wide <- tidyr::spread(data = boot_tmp_element[,!names(boot_tmp_element) %in%  c("w_lb_t3","prop_lb","tlb","year","resp")],
                                                                         key = "sp",
                                                                         value = fit_prop)
                                  boot_tmp_element_wide$S <- boot_tmp_element_wide$SKJ + boot_tmp_element_wide$YFT
                                  boot_tmp_element_wide$SKJ <- base::ifelse(test = boot_tmp_element_wide$S > 1,
                                                                            yes = boot_tmp_element_wide$SKJ/boot_tmp_element_wide$S,
                                                                            no = boot_tmp_element_wide$SKJ)
                                  boot_tmp_element_wide$YFT <- base::ifelse(test = boot_tmp_element_wide$S > 1,
                                                                            yes = boot_tmp_element_wide$YFT/boot_tmp_element_wide$S,
                                                                            no = boot_tmp_element_wide$YFT)
                                  boot_tmp_element_wide$BET <- 1 - (boot_tmp_element_wide$SKJ + boot_tmp_element_wide$YFT)

                                  boot_tmp_element_long <- tidyr::gather(data = boot_tmp_element_wide,
                                                                         key = "sp",
                                                                         value = "fit_prop_t3_ST",
                                                                         "BET", "SKJ", "YFT")
                                  boot_tmp_element <- dplyr::left_join(boot_tmp_element_long, boot_tmp_element, by = c("id_act", "date_act", "lat", "lon", "fmod",  "vessel", "id_trip", "ocean", "yr", "mon", "wtot_lb_t3", "sp","data_source"))

                                  boot_tmp_element$catch_set_fit <- boot_tmp_element$wtot_lb_t3 * boot_tmp_element$fit_prop_t3_ST
                                  list_boot_ST_ocean[[element]] <- boot_tmp_element
                                }
                                outputs_level3_process5[[4]] <- append(outputs_level3_process5[[4]],
                                                                       list(list_boot_ST_ocean))
                                names(outputs_level3_process5[[4]])[length(outputs_level3_process5[[4]])] <- paste("ocean",
                                                                                                                   ocean,
                                                                                                                   sep = "_")
                              }
                              }
                              # bootstrap step 3 - compute confident intervals ----
                              # Compute CI by set - export catch by set
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.5: set catch estimations.\n",
                                  sep = "")
                              set_all <- dplyr::bind_rows(outputs_level3_process5$Estimated_catch_ST)
                              if(ci == TRUE && (length(which(ci_type == "all")) > 0 || length(which(ci_type == "set")) > 0 )){
                                set_all_boot <- lapply(outputs_level3_process5$Boot_output_list_ST, function(x){
                                  set_all_boot_tmp <- dplyr::bind_rows(x)
                                  set_all_boot_tmp$loop <- rep(1:Nboot, each = nrow(set_all_boot_tmp)/Nboot)
                                  return(set_all_boot_tmp)
                                })
                                # compute final CI
                                set_all_final_ocean_list <- vector("list", length = length(outputs_level3_process5$Estimated_catch_ST))
                                names(set_all_final_ocean_list) <- names(outputs_level3_process5$Estimated_catch_ST)

                                for (o in names(outputs_level3_process5$Estimated_catch_ST)){
                                  set_all_final_ocean_list[[o]] <- catch_ci_calculator(fit_data = outputs_level3_process5$Estimated_catch_ST[[o]],
                                                                                       boot_data = set_all_boot[[o]])
                                }

                                set_all_final_ocean <- do.call(rbind, set_all_final_ocean_list)
                                set_all_final_ocean[, names(set_all_final_ocean) %in% c("catch_set_fit",
                                                                                        "ci_inf",
                                                                                        "ci_sup")] <- round(set_all_final_ocean[, names(set_all_final_ocean) %in% c("catch_set_fit","ci_inf","ci_sup")],
                                                                                                            digits = 2)
                                set_all <- set_all_final_ocean
                              }
                              write.csv2(x = set_all,
                                         file = file.path(tables_directory,
                                                          paste("set_all_ocean_",
                                                                paste(unique(set_all$ocean),
                                                                      collapse = "-"),
                                                                "_",
                                                                paste(unique(set_all$yr),
                                                                      collapse = "-"),
                                                                ".csv",
                                                                sep = "")),
                                         row.names = FALSE)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.5: set catch estimations.\n",
                                  sep = "")
                              # nominal catch by species (task 1)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.5: t1 catch estimations.\n",
                                  sep = "")
                              t1_all <- do.call(rbind,lapply(outputs_level3_process5$Estimated_catch_ST,
                                                             function(x){
                                                               t1_tmp_element <-aggregate(cbind(catch_set_fit) ~ yr + sp + ocean ,
                                                                                          data = x,
                                                                                          FUN = sum)
                                                               return(t1_tmp_element)
                                                             }))
                              # compute final CI
                              if(ci == TRUE && (length(which(ci_type == "all")) > 0 || length(which(ci_type == "t1")) > 0 )){
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
                                t1_all_final_ocean_list <- vector("list", length = length(levels(t1_all$ocean)))

                                for (o in levels(t1_all$ocean)){
                                  t1_all_final_ocean_list[[as.numeric(o)]] <- catch_ci_calculator(fit_data = t1_all[t1_all$ocean == o,],
                                                                                                  boot_data = t1_all_boot[t1_all_boot$ocean == o,])
                                }

                                t1_all_final_ocean <- do.call(rbind, t1_all_final_ocean_list)
                                t1_all_final_ocean[, names(t1_all_final_ocean) %in% c("catch_set_fit",
                                                                                      "ci_inf",
                                                                                      "ci_sup")] <- round(t1_all_final_ocean[, names(t1_all_final_ocean) %in% c("catch_set_fit","ci_inf","ci_sup")],
                                                                                                          digits = 2)
                                outputs_level3_process5[[5]] <- append(outputs_level3_process5[[5]],
                                                                       list(t1_all_final_ocean))
                                names(outputs_level3_process5[[5]])[length(outputs_level3_process5[[5]])] <- "Nominal_catch_species"
                                t1_all <- t1_all_final_ocean
                              }
                              write.csv2(x = t1_all,
                                         file = file.path(tables_directory,
                                                          paste("t1_all_ocean_",
                                                                paste(unique(t1_all$ocean),
                                                                      collapse = "-"),
                                                                "_",
                                                                paste(unique(t1_all$yr),
                                                                      collapse = "-"),
                                                                ".csv",
                                                                sep = "")),
                                         row.names = FALSE)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.5: t1 catch estimations.\n",
                                  sep = "")
                              # nominal catch by species and fishing mode (task 1 by fishing mode)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.5: t1-fmod catch estimations.\n",
                                  sep = "")
                              t1_fmod <- do.call(rbind,lapply(outputs_level3_process5$Estimated_catch_ST, function(x){
                                boot_tmp_subelement <- aggregate(cbind(catch_set_fit) ~ yr + fmod + sp + ocean ,data=x, sum)
                                return(boot_tmp_subelement)
                              }))

                              # bootstrap distribution
                              if(ci == TRUE && (length(which(ci_type == "all")) > 0 || length(which(ci_type == "t1-fmod")) > 0 )){
                                t1_fmod_boot <- do.call(rbind,lapply(outputs_level3_process5$Boot_output_list_ST,
                                                                     FUN = function(x){
                                                                       boot_tmp_element <-do.call(rbind,
                                                                                                  lapply(seq.int(1:length(x)),
                                                                                                         function(i){
                                                                                                           boot_tmp_subelement <- aggregate(cbind(catch_set_fit) ~ yr + fmod + sp + ocean,
                                                                                                                                            data=x[[i]],
                                                                                                                                            FUN = sum)
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

                                t1_fmod_final_ocean[, names(t1_fmod_final_ocean) %in% c("catch_set_fit",
                                                                                        "ci_inf",
                                                                                        "ci_sup")] <- round(t1_fmod_final_ocean[, names(t1_fmod_final_ocean) %in% c("catch_set_fit","ci_inf","ci_sup")],
                                                                                                            digits = 2)
                                outputs_level3_process5[[5]] <- append(outputs_level3_process5[[5]],
                                                                       list(t1_fmod_final_ocean))
                                names(outputs_level3_process5[[5]])[length(outputs_level3_process5[[5]])] <- "Nominal_catch_fishing_mode"
                                t1_fmod <- t1_fmod_final_ocean
                              }
                              write.csv2(x = t1_fmod,
                                         file = file.path(tables_directory,
                                                          paste("t1_fmod_ocean_",
                                                                paste(unique(t1_fmod$ocean),
                                                                      collapse = "-"),
                                                                "_",
                                                                paste(unique(t1_fmod$yr),
                                                                      collapse = "-"),
                                                                ".csv",
                                                                sep = "")),
                                         row.names = FALSE)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.5: t1-fmod catch estimations.\n",
                                  sep = "")
                              ## catch effort (task2)
                              # function for rounding, rounding up and down to a specific base
                              mtrunc <- function(x,base){
                                base*trunc(x/base)
                              }

                              mroundup <- function(x, base)
                              {
                                base*(x%/%base + as.logical(x%%base))
                              }

                              # assign coordinates to cwp with a specific base
                              latlon2cwp<-function(lat,lon,base){
                                quad<-ifelse(lon>=0,ifelse(lat>=0,1,2),ifelse(lat>=0,4,3)) # define quadrant
                                lat_tmp<-ifelse(quad %in% c(1,4),sprintf("%02d",abs(mtrunc(lat,base))),sprintf("%02d",abs(mroundup(lat,base))))
                                lon_tmp<-ifelse(quad %in% c(1,2),sprintf("%03d",abs(mtrunc(lon,base))),sprintf("%03d",abs(mroundup(lon,base))))
                                return(paste(quad,lat_tmp,lon_tmp,sep=""))

                              }

                              # nominal catch by species and cwp (task 2 - catch Effort)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.5: t2 catch estimations.\n",
                                  sep = "")
                              t2_all <- do.call(rbind,lapply(outputs_level3_process5$Estimated_catch_ST, function(x){
                                x$cwp <- latlon2cwp(lat = x$lat,
                                                    lon = x$lon,
                                                    base = 1)
                                # cwp <- furdeb::lat_lon_cwp_manipulation(manipulation_process = "lat_lon_to_cwp",
                                #                                         data_longitude = as.character(x$lon),
                                #                                         data_latitude = as.character(x$lat),
                                #                                         input_degree_format = "decimal_degree",
                                #                                         cwp_resolution = "1deg_x_1deg")
                                boot_tmp_subelement <- x %>%
                                  group_by(yr, mon, sp, ocean, cwp) %>%
                                  summarise(catch_set_fit = sum(catch_set_fit))
                                return(boot_tmp_subelement)
                              }))

                              # bootstrap distribution
                              if(ci == TRUE && (length(which(ci_type == "all")) > 0 || length(which(ci_type == "t2")) > 0 )){
                                t2_all_boot <- do.call(rbind,lapply(outputs_level3_process5$Boot_output_list_ST,
                                                                    FUN = function(x){
                                                                      boot_tmp_element <-do.call(rbind,
                                                                                                 lapply(seq.int(1:length(x)),
                                                                                                        function(i){
                                                                                                          x[[i]]$cwp <- latlon2cwp(lat = x[[i]]$lat,
                                                                                                                                   lon = x[[i]]$lon,
                                                                                                                                   base = 1)
                                                                                                          boot_tmp_subelement <- x[[i]] %>%
                                                                                                            group_by(yr, mon, sp, cwp, ocean) %>%
                                                                                                            summarise(catch_set_fit = sum(catch_set_fit))
                                                                                                          boot_tmp_subelement$loop <- i
                                                                                                          return(boot_tmp_subelement)
                                                                                                        }))
                                                                      return(boot_tmp_element)
                                                                    }))
                                # compute final CI
                                t2_all_final_ocean_list <- vector("list", length = length(levels(t2_all$ocean)))

                                for (o in as.numeric(levels(t2_all$ocean))){
                                  t2_all_final_ocean_list[[o]] <- catch_ci_calculator(fit_data = t2_all[t2_all$ocean == o,],
                                                                                      boot_data = t2_all_boot[t2_all_boot$ocean == o,])
                                }
                                t2_all_final_ocean <- do.call(rbind, t2_all_final_ocean_list)
                                t2_all_final_ocean[, names(t2_all_final_ocean) %in% c("catch_set_fit",
                                                                                      "ci_inf",
                                                                                      "ci_sup")] <- round(t2_all_final_ocean[, names(t2_all_final_ocean) %in% c("catch_set_fit","ci_inf","ci_sup")],
                                                                                                          digits = 2)
                                t2_all <- t2_all_final_ocean
                              }
                              write.csv2(x = t2_all,
                                         file = file.path(tables_directory,
                                                          paste("t2_all_ocean_",
                                                                paste(unique(t2_all$ocean),
                                                                      collapse = "-"),
                                                                "_",
                                                                paste(unique(t2_all$yr),
                                                                      collapse = "-"),
                                                                ".csv",
                                                                sep = "")),
                                         row.names = FALSE)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.5: t2 catch estimations.\n",
                                  sep = "")
                              # nominal catch by species and cwp and fishing mode (task 2 by fishing mode)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Start process 3.5: t2-fmod catch estimations.\n",
                                  sep = "")
                              t2_fmod <- do.call(rbind,lapply(outputs_level3_process5$Estimated_catch_ST, function(x){
                                x$cwp <- latlon2cwp(lat = x$lat,
                                                    lon = x$lon,
                                                    base = 1)
                                # cwp <- furdeb::lat_lon_cwp_manipulation(manipulation_process = "lat_lon_to_cwp",
                                #                                         data_longitude = as.character(x$lon),
                                #                                         data_latitude = as.character(x$lat),
                                #                                         input_degree_format = "decimal_degree",
                                #                                         cwp_resolution = "1deg_x_1deg")
                                boot_tmp_subelement <- x %>%
                                  group_by(yr, mon, fmod, sp, ocean, cwp) %>%
                                  summarise(catch_set_fit = sum(catch_set_fit))
                                return(boot_tmp_subelement)
                              }))
                              # bootstrap distribution
                              if(ci == TRUE && (length(which(ci_type == "all")) > 0 || length(which(ci_type == "t2-fmod")) > 0 )){
                                t2_fmod_boot <- do.call(rbind,lapply(outputs_level3_process5$Boot_output_list_ST,
                                                                     FUN = function(x){
                                                                       boot_tmp_element <-do.call(rbind,
                                                                                                  lapply(seq.int(1:length(x)),
                                                                                                         function(i){
                                                                                                           x[[i]]$cwp <- latlon2cwp(lat = x[[i]]$lat,
                                                                                                                                    lon = x[[i]]$lon,
                                                                                                                                    base = 1)
                                                                                                           boot_tmp_subelement <- aggregate(cbind(catch_set_fit) ~ yr + mon + fmod + sp + cwp + ocean,
                                                                                                                                            data=x[[i]],
                                                                                                                                            FUN = sum)
                                                                                                           boot_tmp_subelement$loop <- i
                                                                                                           return(boot_tmp_subelement)
                                                                                                         }))
                                                                       return(boot_tmp_element)
                                                                     }))
                                # compute final CI
                                t2_fmod_final_ocean_list <- vector("list", length = length(levels(t2_fmod$ocean)))
                                for (o in as.numeric(levels(t2_fmod$ocean))){
                                  t2_fmod_final_ocean_list[[o]] <- catch_ci_calculator(fit_data = t2_fmod[t2_fmod$ocean == o,],
                                                                                       boot_data = t2_fmod_boot[t2_fmod_boot$ocean == o,])
                                }
                                t2_fmod_final_ocean <- do.call(rbind, t2_fmod_final_ocean_list)
                                t2_fmod_final_ocean[, names(t2_fmod_final_ocean) %in% c("catch_set_fit",
                                                                                        "ci_inf",
                                                                                        "ci_sup")] <- round(t2_fmod_final_ocean[, names(t2_fmod_final_ocean) %in% c("catch_set_fit","ci_inf","ci_sup")],
                                                                                                            digits = 2)
                                t2_fmod <- t2_fmod_final_ocean
                              }
                              write.csv2(x = t2_fmod,
                                         file = file.path(tables_directory,
                                                          paste("t2_fmod_ocean_",
                                                                paste(unique(t2_fmod$ocean),
                                                                      collapse = "-"),
                                                                "_",
                                                                paste(unique(t2_fmod$yr),
                                                                      collapse = "-"),
                                                                ".csv",
                                                                sep = "")),
                                         row.names = FALSE)
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End process 3.5: t2-fmod catch estimations.\n",
                                  sep = "")

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

                              if(plot_predict == T){
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
                              }# link to if(plot_predict)
                              # if (exists(x = "data_level3",
                              #            envir = .GlobalEnv)) {
                              #   data_level3 <- get(x = "data_level3",
                              #                      envir = .GlobalEnv)
                              #   data_level3 <- append(data_level3,
                              #                         list(outputs_level3_process5))
                              #   names(data_level3)[length(data_level3)] <- "outputs_level3_process5"
                              # } else {
                              #   data_level3 <- list("outputs_level3_process5" = outputs_level3_process5)
                              # }
                              # assign(x = "data_level3",
                              #        value = data_level3,
                              #        envir = .GlobalEnv)
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
                            data_selected = NULL
                          ))

