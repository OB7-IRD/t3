#' @name full_trips
#' @title R6 class full_trips creation
#' @description Create R6 reference object class full_trips
#' @importFrom R6 R6Class
full_trips <- R6::R6Class(classname = "full_trips",
                          inherit = t3:::list_t3,
                          public = list(
                            # full trips creation ----
                            create_full_trips = function(object_trips) {
                              if (! any(class(object_trips) == "R6") | ! any(class(object_trips) == "trips")) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"object_trips\" argument\nClass R6 and trips expected\n",
                                    sep = "")
                                stop()
                              }
                              full_trips <- list()
                              full_trips_tmp <- list()
                              full_trip_warning <- 0
                              i <- 1
                              while(i <= object_trips$count()) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start full trips creation\n",
                                      sep = "")
                                }
                                if (object_trips$view(i)[[1]]$.__enclos_env__$private$fish_hold_empty == 1) {
                                  full_trips <- append(full_trips, list(list(object_trips$view(i)[[1]])))
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
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End of full trips creation\n",
                                  sep = "")
                            },
                            # filter full trips by periode_reference ----
                            filter_by_periode = function(periode_reference) {
                              if (length(class(periode_reference)) == 1 && class(periode_reference) != "numeric") {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"periode_reference\" argument\nclass numeric expected\n",
                                    sep = "")
                                stop()
                              } else if (nchar(periode_reference) != 4) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"periode_reference\" argument\nyear format on 4 digits expected\n",
                                    sep = "")
                                stop()
                              } else {
                                for (i in 1:length(private$data)) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start of full trips filtering (by periode reference)\n",
                                        sep = "")
                                  }
                                  full_trips_tmp <- private$data[[i]]
                                  year_full_trips <- vector(mode = "numeric")
                                  for (j in 1:length(full_trips_tmp)) {
                                    full_trips_tmp_bis <- full_trips_tmp[[j]]
                                    year_full_trips <- append(year_full_trips,
                                                              lubridate::year(x = full_trips_tmp_bis$.__enclos_env__$private$landing_date))
                                  }
                                  if (any(year_full_trips == periode_reference)) {
                                    private$data_selected <- append(private$data_selected, list(full_trips_tmp))
                                    names(private$data_selected)[length(private$data_selected)] <- names(private$data[i])
                                  }
                                }
                                if (any(private$id_not_full_trip %in% names(private$data_selected))) {
                                  warning("missing trip(s) in at least one full trip item\ncheck id(s): ",
                                          base::intersect(private$id_not_full_trip,
                                                          private$data_selected),
                                          call. = FALSE)
                                  private$id_not_full_trip_retained <- base::intersect(private$id_not_full_trip,
                                                                                       private$data_selected)
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - End of full trips filtering\n",
                                    sep = "")
                              }
                            },
                            # add activities ----
                            add_activities = function(object_activities) {
                              if (length(private$data_selected) == 0) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: argument \"data_selected\" empty\nlaunch selection data before\n",
                                    sep = "")
                                stop()
                              } else if (! any(class(object_activities) == "activities")) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"object_activities\" argument\nclass activities expected\n",
                                    sep = "")
                                stop()
                              } else {
                                for (i in 1:length(private$data_selected)) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start of add activity\n",
                                        sep = "")
                                  }
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    trip_id <- private$data_selected[[i]][[j]]$.__enclos_env__$private$trip_id
                                    activities_tmp <- object_activities$filter_by_trip(trip_id = trip_id)
                                    private$data_selected[[i]][[j]]$.__enclos_env__$private$activities <- activities_tmp
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - End of add activity\n",
                                    sep = "")
                              }
                            },
                            # add elementary catches ----
                            add_elementarycatches = function(object_elementarycatches) {
                              if (length(private$data_selected) == 0) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: argument \"data_selected\" empty\nlaunch selection data before\n",
                                    sep = "")
                                stop()
                              } else if (! any(class(object_elementarycatches) == "elementarycatches")) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"object_elementarycatches\" argument\nclass elementarycatches expected\n",
                                    sep = "")
                                stop()
                              } else {
                                for (i in 1:length(private$data_selected)) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start of add elementary catches\n",
                                        sep = "")
                                  }
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    if (length(private$data_selected[[i]][[j]]$.__enclos_env__$private$activities) != 0) {
                                      for (k in 1:length(private$data_selected[[i]][[j]]$.__enclos_env__$private$activities)) {
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
                                    " - End of add elementary catches\n",
                                    sep = "")
                              }
                            },
                            # add elementary landings ----
                            add_elementarylandings = function(object_elementarylandings) {
                              if (length(private$data_selected) == 0) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: argument \"data_selecetd\" empty\nlaunch selection data before\n",
                                    sep = "")
                                stop()
                              } else if (! any(class(object_elementarylandings) == "elementarylandings")) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"object_elementarylandings\" argument\nclass elementarylandings expected\n",
                                    sep = "")
                                stop()
                              } else {
                                for (i in 1:length(private$data_selected)) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start of add elementary landings\n",
                                        sep = "")
                                  }
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    trip_id <- private$data_selected[[i]][[j]]$.__enclos_env__$private$trip_id
                                    elementarylandings_tmp <- object_elementarylandings$filter_by_trip(trip_id = trip_id)
                                    private$data_selected[[i]][[j]]$.__enclos_env__$private$elementarylandings <- elementarylandings_tmp
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - End of add elementary landings\n",
                                    sep = "")
                              }
                            },
                            # add wells and samples ----
                            add_wells_samples = function(object_wells) {
                              if (length(private$data_selected) == 0) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: argument \"data_selecetd\" empty\nlaunch selection data before\n",
                                    sep = "")
                                stop()
                              } else if (! any(class(object_wells) == "wells")) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"object_wells\" argument\nclass wells expected\n",
                                    sep = "")
                                stop()
                              } else {
                                for (i in 1:length(private$data_selected)) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start of add well(s) - sample(s)\n",
                                        sep = "")
                                  }
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    trip_id <- private$data_selected[[i]][[j]]$.__enclos_env__$private$trip_id
                                    trip_wells <- object_wells$filter_by_trip(trip_id = trip_id)
                                    private$data_selected[[i]][[j]]$.__enclos_env__$private$wells <- trip_wells
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - End of add well(s) - sample(s)\n",
                                    sep = "")
                              }
                            },
                            # raising factor level 1 ----
                            rf1 = function(species_rf1) {
                              if (length(class(species_rf1)) != 1 || class(species_rf1) != "character") {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"species_rf1\" argument\nclass character expected\n",
                                    sep = "")
                                stop()
                              } else {
                                for (i in 1:length(private$data_selected)) {
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start of raising factor process 1\n",
                                        sep = "")
                                  }
                                  if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                    # case 1: at least on trip is missing in the full trip item
                                    # check if functions for selection of elementary catches and elementary landings ran before
                                    warning("missing trip(s) in full trip ", i)
                                    stop <- 0
                                    for (k in 1:length(private$data_selected[[i]])) {
                                      # case 1.1: at least one logbook is missing in not complet full trip item
                                      if (k == 1) {
                                        logbook_availability <- vector(mode = "integer")
                                      }
                                      current_trip <- private$data_selected[[i]][[k]]
                                      logbook_availability <- append(logbook_availability,
                                                                     current_trip$.__enclos_env__$private$logbook_availability)
                                      if (k == length(private$data_selected[[i]])) {
                                        if (any(logbook_availability) == 0) {
                                          warning("missing logbook",
                                                  "\ncheck item ", i,
                                                  call. = FALSE)
                                          for (l in 1:length(private$data_selected[[i]])) {
                                            current_trip <- private$data_selected[[i]][[l]]
                                            current_trip$.__enclos_env__$private$rf1 <- 1
                                            current_trip$.__enclos_env__$private$statut_rf1 <- 1.1
                                          }
                                          stop <- 1
                                        }
                                      }
                                    }
                                    if (stop != 1) {
                                      for (m in 1:length(private$data_selected[[i]])) {
                                        if (m == 1) {
                                          current_elementarycatches <- NULL
                                        }
                                        current_trip <- private$data_selected[[i]][[m]]
                                        if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                          for (w in 1:length(current_trip$.__enclos_env__$private$activities)) {
                                            current_elementarycatches <- append(current_elementarycatches,
                                                                                current_trip$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches)
                                          }
                                        }
                                      }
                                      if (is.null(current_elementarycatches)) {
                                        # case 1.2: trips with no catches (for example route or support) in not complet full trip item
                                        for (n in 1:length(private$data_selected[[i]])) {
                                          current_trip <- private$data_selected[[i]][[n]]
                                          current_trip$.__enclos_env__$private$rf1 <- 1
                                          current_trip$.__enclos_env__$private$statut_rf1 <- 2.2
                                        }
                                      } else {
                                        for (p in 1:length(private$data_selected[[i]])) {
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
                                            warning("missing elementary landing",
                                                    "\nitem ", i,
                                                    call. = FALSE)
                                            for (q in 1:length(private$data_selected[[i]])) {
                                              current_trip <- private$data_selected[[i]][[q]]
                                              current_trip$.__enclos_env__$private$rf1 <- 1
                                              current_trip$.__enclos_env__$private$statut_rf1 <- 1.3
                                            }
                                          } else {
                                            # case 1.4: almost rocks dude ! (not complet full trip item)
                                            for (s in 1:length(private$data_selected[[i]])) {
                                              current_trip <- private$data_selected[[i]][[s]]
                                              current_trip$.__enclos_env__$private$rf1 <- 1
                                              current_trip$.__enclos_env__$private$statut_rf1 <- 1.4
                                              if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                                current_elementarycatches <- NULL
                                                for (z in 1:length(current_trip$.__enclos_env__$private$activities)) {
                                                  current_elementarycatches <- append(current_elementarycatches,
                                                                                      current_trip$.__enclos_env__$private$activities[[z]]$.__enclos_env__$private$elementarycatches)
                                                }
                                              }
                                              if (! is.null(current_elementarycatches)) {
                                                for (t in 1:length(current_elementarycatches)) {
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
                                    for (k in 1:length(private$data_selected[[i]])) {
                                      if (k == 1) {
                                        logbook_availability <- vector(mode = "integer")
                                      }
                                      current_trip <- private$data_selected[[i]][[k]]
                                      logbook_availability <- append(logbook_availability,
                                                                     current_trip$.__enclos_env__$private$logbook_availability)
                                      if (k == length(private$data_selected[[i]])) {
                                        if (any(logbook_availability) == 0) {
                                          warning("missing logbook",
                                                  "\ncheck item ", i,
                                                  call. = FALSE)
                                          for (l in 1:length(private$data_selected[[i]])) {
                                            current_trip <- private$data_selected[[i]][[l]]
                                            current_trip$.__enclos_env__$private$rf1 <- 1
                                            current_trip$.__enclos_env__$private$statut_rf1 <- 2.1
                                          }
                                          stop <- 1
                                        }
                                      }
                                    }
                                    if (stop != 1) {
                                      for (m in 1:length(private$data_selected[[i]])) {
                                        if (m == 1) {
                                          current_elementarycatches <- NULL
                                        }
                                        current_trip <- private$data_selected[[i]][[m]]
                                        if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                          for (w in 1:length(current_trip$.__enclos_env__$private$activities)) {
                                            current_elementarycatches <- append(current_elementarycatches,
                                                                                current_trip$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches)
                                          }
                                        }
                                      }
                                      if (is.null(current_elementarycatches)) {
                                        # case 2.2: trips with no catches (for example route or support) in complet full trip item
                                        for (n in 1:length(private$data_selected[[i]])) {
                                          current_trip <- private$data_selected[[i]][[n]]
                                          current_trip$.__enclos_env__$private$rf1 <- 1
                                          current_trip$.__enclos_env__$private$statut_rf1 <- 2.2
                                        }
                                      } else {
                                        current_elementarycatches_weight <- vector(mode = "numeric")
                                        for (o in 1:length(current_elementarycatches)) {
                                          if (current_elementarycatches[[o]]$.__enclos_env__$private$specie_code3l %in% species_rf1) {
                                            current_elementarycatches_weight <- append(current_elementarycatches_weight,
                                                                                       current_elementarycatches[[o]]$.__enclos_env__$private$catch_weight)
                                          }
                                        }
                                        for (p in 1:length(private$data_selected[[i]])) {
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
                                            # case 2.3: at least one elementary landing is missing in complet full trip item
                                            warning("missing elementary landing",
                                                    "\nitem ", i,
                                                    call. = FALSE)
                                            for (q in 1:length(private$data_selected[[i]])) {
                                              current_trip <- private$data_selected[[i]][[q]]
                                              current_trip$.__enclos_env__$private$rf1 <- 1
                                              current_trip$.__enclos_env__$private$statut_rf1 <- 2.3
                                            }
                                          } else {
                                            # case 2.4: everything rocks dude !
                                            current_elementarylandings_weight <- vector(mode = "numeric")
                                            for (r in 1:length(current_elementarylandings)) {
                                              if (current_elementarylandings[[r]]$.__enclos_env__$private$specie_code3l %in% species_rf1) {
                                                current_elementarylandings_weight <- append(current_elementarylandings_weight,
                                                                                            current_elementarylandings[[r]]$.__enclos_env__$private$landing_weight)
                                              }
                                            }
                                            current_rf1 <- sum(current_elementarylandings_weight) / sum(current_elementarycatches_weight)
                                            if (current_rf1 < 0.8 | current_rf1 > 1.2) {
                                              warning("Be carefull ! RF1 value out of theorical boundaries: ", round(current_rf1, 3),
                                                      "\nitem ", i,
                                                      call. = FALSE)
                                            }
                                            for (s in 1:length(private$data_selected[[i]])) {
                                              current_trip <- private$data_selected[[i]][[s]]
                                              current_trip$.__enclos_env__$private$rf1 <- current_rf1
                                              current_trip$.__enclos_env__$private$statut_rf1 <- 2.4
                                            }
                                          }
                                        }
                                      }
                                    }
                                    # assign rf1 to elementary catches
                                    for (u in 1:length(private$data_selected[[i]])) {
                                      current_trip <- private$data_selected[[i]][[u]]
                                      current_rf1 <- current_trip$.__enclos_env__$private$rf1
                                      if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                        current_elementarycatches <- NULL
                                        for (x in 1:length(current_trip$.__enclos_env__$private$activities)) {
                                          current_elementarycatches <- append(current_elementarycatches,
                                                                              current_trip$.__enclos_env__$private$activities[[x]]$.__enclos_env__$private$elementarycatches)
                                        }
                                      }
                                      if (! is.null(current_elementarycatches)) {
                                        for (v in 1:length(current_elementarycatches)) {
                                          current_elementarycatches[[v]]$.__enclos_env__$private$catch_weight_rf1 <- current_elementarycatches[[v]]$.__enclos_env__$private$catch_weight * current_rf1
                                        }
                                      }
                                    }
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - \n",
                                    sep = "")
                              }
                            },
                            # raising factor level 2 ----
                            rf2 = function() {
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start raising factor process 2\n",
                                      sep = "")
                                } else if (i == length(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End of raising factor process 2\n",
                                      sep = "")
                                }
                                if (private$data_selected[[i]][[1]]$.__enclos_env__$private$statut_rf1 == 2.1) {
                                  # case 1: rf2 calculated
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Error: rf2 not developped yet\n",
                                      sep = "")
                                  stop()
                                } else if (private$data_selected[[i]][[1]]$.__enclos_env__$private$statut_rf1 %in% c(2.2, 2.3, 2.4)) {
                                  # case 2: rf2 not need to be calculated
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    current_rf2 <- 1
                                    current_trip$.__enclos_env__$private$rf2 <- current_rf2
                                    current_trip$.__enclos_env__$private$statut_rf2 <- 2
                                    if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                      current_elementarycatches <- NULL
                                      for (m in 1:length(current_trip$.__enclos_env__$private$activities)) {
                                        current_elementarycatches <- append(current_elementarycatches,
                                                                            current_trip$.__enclos_env__$private$activities[[m]]$.__enclos_env__$private$elementarycatches)
                                      }
                                    }
                                    if (length(current_elementarycatches) != 0) {
                                      for (k in 1:length(current_elementarycatches)) {
                                        current_elementarycatches[[k]]$.__enclos_env__$private$catch_weight_rf2 <- current_elementarycatches[[k]]$.__enclos_env__$private$catch_weight_rf1 * current_rf2
                                      }
                                    }
                                  }
                                } else {
                                  # case 3: full trip not complet
                                  for (l in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[l]]
                                    current_rf2 <- 1
                                    current_trip$.__enclos_env__$private$rf2 <- current_rf2
                                    current_trip$.__enclos_env__$private$statut_rf2 <- 3
                                  }
                                }
                              }
                            },
                            # logbook weigth categories conversion ----
                            conversion_weigth_category = function () {
                              category_1 <- "<10kg"
                              category_2 <- "10-30kg"
                              category_3 <- ">30kg"
                              category_4 <- ">10kg"
                              category_5 <- "unknown"
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start process 1.3: logbook weight categories conversion\n",
                                      sep = "")
                                }
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: trip avoided because not associated to a full trip\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  next()
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 1.3 on item ",
                                      i,
                                      "\n[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  # first stage: conversion of all categories except for unknown (category 9)
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                      for (w in 1:length(current_trip$.__enclos_env__$private$activities)) {
                                        if (current_trip$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$activity_code %in% c(0, 1, 2, 14)) {
                                          current_elementarycatches <- current_trip$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches
                                          if (length(current_elementarycatches) != 0) {
                                            ocean_activity <- current_trip$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$ocean
                                            school_type_activity <- current_trip$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$school_type
                                            for (k in 1:length(current_elementarycatches)) {
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
                                                    }
                                                  } else if (current_elementarycatch$.__enclos_env__$private$specie_code3l == "SKJ") {
                                                    if (current_elementarycatch$.__enclos_env__$private$logbook_category != 9) {
                                                      current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
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
                                                    }
                                                  } else if (current_elementarycatch$.__enclos_env__$private$specie_code3l == "SKJ") {
                                                    if (current_elementarycatch$.__enclos_env__$private$logbook_category != 9) {
                                                      current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
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
                                                    }
                                                  } else if (current_elementarycatch$.__enclos_env__$private$specie_code3l == "SKJ") {
                                                    if (current_elementarycatch$.__enclos_env__$private$logbook_category != 9) {
                                                      current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                      current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                    }
                                                  } else {
                                                    current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_5
                                                    current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                  }
                                                } else {
                                                  # for floating object school
                                                  if (current_elementarycatch$.__enclos_env__$private$specie_code3l %in% c("YFT", "BET", "ALB")) {
                                                    if (current_elementarycatch$.__enclos_env__$private$specie_code3l %in% c(1, 2, 10)) {
                                                      current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                      current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                    } else if (current_elementarycatch$.__enclos_env__$private$specie_code3l == 4) {
                                                      current_elementarycatch_tmp <- current_elementarycatch$clone()
                                                      current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                      current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2 * 0.2
                                                      current_elementarycatch_tmp$.__enclos_env__$private$corrected_logbook_category <- category_4
                                                      current_elementarycatch_tmp$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch_tmp$.__enclos_env__$private$catch_weight_rf2 * 0.8
                                                      private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches <- append(private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[w]]$.__enclos_env__$private$elementarycatches,
                                                                                                                                                                                  current_elementarycatch_tmp)
                                                    } else if (current_elementarycatch$.__enclos_env__$private$specie_code3l %in% c(3, 12, 5, 7, 8, 13, 6, 11)) {
                                                      current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_4
                                                      current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                    }
                                                  } else if (current_elementarycatch$.__enclos_env__$private$specie_code3l == "SKJ") {
                                                    if (current_elementarycatch$.__enclos_env__$private$logbook_category != 9) {
                                                      current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_1
                                                      current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                    }
                                                  } else {
                                                    current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_5
                                                    current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                                  }
                                                }
                                              } else {
                                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                    " - Error: algorithm not calculated for the ocean number ",
                                                    ocean_activity,
                                                    "\n",
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
                                  for (l in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[l]]
                                    current_elementarycatches <- vector(mode = "list")
                                    if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                      for (x in 1:length(current_trip$.__enclos_env__$private$activities)) {
                                        current_elementarycatches <- append(current_elementarycatches,
                                                                            current_trip$.__enclos_env__$private$activities[[x]]$.__enclos_env__$private$elementarycatches)
                                      }
                                    }
                                    if (length(current_elementarycatches) != 0) {
                                      category_9 <- FALSE
                                      names(category_9) <- 0
                                      other_category <- FALSE
                                      names(other_category) <- 0
                                      for (n in 1:length(current_elementarycatches)) {
                                        if (current_elementarycatches[[n]]$.__enclos_env__$private$logbook_category == 9 & current_elementarycatches[[n]]$.__enclos_env__$private$specie_code3l %in% c("YFT", "BET", "ALB", "SKJ")) {
                                          category_9 = append(category_9, TRUE)
                                          names(category_9)[length(category_9)] <- n
                                        } else {
                                          other_category = append(other_category, TRUE)
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
                                              total_catch_weight_category_corrected <- sum(sapply(1:length(current_other_category),
                                                                                                  function(i) {
                                                                                                    current_other_category[[i]]$.__enclos_env__$private$catch_weight_category_corrected
                                                                                                  }))
                                              other_category_names <- unique(sapply(X = 1:length(current_other_category),
                                                                                    FUN = function(i) {
                                                                                      current_other_category[[i]]$.__enclos_env__$private$corrected_logbook_category
                                                                                    }))
                                              proportion <- vector(mode = "numeric")
                                              for (s in other_category_names) {
                                                weight_category_corrected <- sum(sapply(X = 1:length(current_other_category),
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
                                              for (t in 1:length(current_category_9)) {
                                                for (u in 1:length(proportion)) {
                                                  if (u == length(proportion)) {
                                                    current_category_9[[t]]$.__enclos_env__$private$corrected_logbook_category = names(proportion)[u]
                                                    current_category_9[[t]]$.__enclos_env__$private$catch_weight_category_corrected = current_category_9[[t]]$.__enclos_env__$private$catch_weight_rf2 * as.numeric(proportion[u])
                                                  } else {
                                                    current_category_9_tmp <- current_category_9[[t]]$clone()
                                                    current_category_9_tmp$.__enclos_env__$private$corrected_logbook_category = names(proportion)[u]
                                                    current_category_9_tmp$.__enclos_env__$private$catch_weight_category_corrected = current_category_9_tmp$.__enclos_env__$private$catch_weight_rf2 * as.numeric(proportion[u])
                                                    for (y in 1:length(private$data_selected[[i]][[l]]$.__enclos_env__$private$activities)) {
                                                      if (private$data_selected[[i]][[l]]$.__enclos_env__$private$activities[[y]]$.__enclos_env__$private$activity_id == current_category_9_tmp$.__enclos_env__$private$activity_id) {
                                                        private$data_selected[[i]][[l]]$.__enclos_env__$private$activities[[y]]$.__enclos_env__$private$elementarycatches <- append(private$data_selected[[i]][[l]]$.__enclos_env__$private$activities[[y]]$.__enclos_env__$private$elementarycatches,
                                                                                                                                                                                    current_category_9_tmp)
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            } else {
                                              for (v in as.numeric(names(category_9))) {
                                                if (current_elementarycatches[[v]]$.__enclos_env__$private$school_type == school_type &
                                                    current_elementarycatches[[v]]$.__enclos_env__$private$ocean == ocean &
                                                    current_elementarycatches[[v]]$.__enclos_env__$private$specie_code3l == specie) {
                                                  current_elementarycatches[[v]]$.__enclos_env__$private$corrected_logbook_category <- category_5
                                                  current_elementarycatches[[v]]$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatches[[v]]$.__enclos_env__$private$catch_weight_rf2
                                                }
                                              }
                                            }
                                          }
                                        } else {
                                          for (m in 1:length(current_elementarycatches)) {
                                            current_elementarycatch <- current_elementarycatches[[m]]
                                            current_elementarycatch$.__enclos_env__$private$corrected_logbook_category <- category_5
                                            current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected <- current_elementarycatch$.__enclos_env__$private$catch_weight_rf2
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Process 1.3 successfull on item ",
                                    i,
                                    "\n[trip: ",
                                    private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                    "]\n",
                                    sep = "")
                                if (i == length(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End process 1.3: logbook weight categories conversion\n",
                                      sep = "")
                                }
                              }
                            },
                            # set_count ----
                            set_count = function () {
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start process 1.4: set count\n",
                                      sep = "")
                                }
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: trip avoided because not associated to a full trip\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  next()
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 1.4 on item ",
                                      i,
                                      "\n[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                      for (k in 1:length(current_trip$.__enclos_env__$private$activities)) {
                                        if (current_trip$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$activity_code %in% c(0, 1, 2, 14)) {
                                          current_activity <- current_trip$.__enclos_env__$private$activities[[k]]
                                          current_elementarycatches <- current_activity$.__enclos_env__$private$elementarycatches
                                          if (length(current_elementarycatches) != 0) {
                                            catch_weight_category_corrected <- sum(sapply(X = 1:length(current_elementarycatches),
                                                                                          FUN = function(l) {
                                                                                            if (is.null(current_elementarycatches[[l]]$.__enclos_env__$private$catch_weight_category_corrected)) {
                                                                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                                                  " - Error: run function for logbook weigth categories conversion before that one",
                                                                                                  "\n",
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
                                    "\n[trip: ",
                                    private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                    "]\n",
                                    sep = "")
                                if (i == length(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End process 1.4: set count\n",
                                      sep = "")
                                }
                              }
                            },
                            # set_duration ----
                            set_duration = function(set_duration_ref) {
                              if (class(set_duration_ref) != "data.frame" || dim(set_duration_ref)[2] != 7 || dim(set_duration_ref)[1] < 1) {
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"set_duration_ref\" argument\n",
                                    "class \"data.frame\" expected with 7 columns and at least 1 row",
                                    sep = "")
                                stop()
                              }
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start process 1.5: set duration calculation\n",
                                      sep = "")
                                }
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: trip avoided because not associated to a full trip\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  next()
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 1.5 on item ",
                                      i,
                                      "\n[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                      for (k in 1:length(current_trip$.__enclos_env__$private$activities)) {
                                        if (current_trip$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$activity_code %in% c(0, 1, 2, 14)) {
                                          current_activity <- current_trip$.__enclos_env__$private$activities[[k]]
                                          if (current_activity$.__enclos_env__$private$activity_code %in% c(0, 2, 14)) {
                                            # for a set declared as null set (0), unknown set (2) or pocket capsizing (14)
                                            current_elementarycatches <- current_activity$.__enclos_env__$private$elementarycatches
                                            if (dim(set_duration_ref[set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                     & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type, ])[1] != 1) {
                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                  " - Error: invalid \"set_duration_ref\" argument",
                                                  "\n",
                                                  sep = "")
                                              stop()
                                            }
                                            if (length(current_elementarycatches) != 0) {
                                              catch_weight_category_corrected <- sum(sapply(X = 1:length(current_elementarycatches),
                                                                                            FUN = function(l) {
                                                                                              if (is.null(current_elementarycatches[[l]]$.__enclos_env__$private$catch_weight_category_corrected)) {
                                                                                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                                                    " - Error: run function for logbook weigth categories conversion before that one",
                                                                                                    "\n",
                                                                                                    "identification: ",
                                                                                                    i,
                                                                                                    " - ",
                                                                                                    j,
                                                                                                    " - ",
                                                                                                    k,
                                                                                                    " - ",
                                                                                                    l,
                                                                                                    " (full trip - trip - activity - elementary catch)\n",
                                                                                                    sep = "")
                                                                                                stop()
                                                                                              } else {
                                                                                                current_elementarycatches[[l]]$.__enclos_env__$private$catch_weight_category_corrected
                                                                                              }
                                                                                            }))
                                              parameter_a <- set_duration_ref[set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                              & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type, "parameter_a"]
                                              parameter_b <- set_duration_ref[set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                              & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type, "parameter_b"]
                                              current_activity$.__enclos_env__$private$set_duration <- parameter_a * catch_weight_category_corrected + parameter_b
                                            } else {
                                              current_activity$.__enclos_env__$private$set_duration <- set_duration_ref[set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                                                                        & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type, "null_set_value"]
                                            }
                                          } else if (current_activity$.__enclos_env__$private$activity_code == 1) {
                                            # for a set declared as positive (1)
                                            current_elementarycatches <- current_activity$.__enclos_env__$private$elementarycatches
                                            if (dim(set_duration_ref[set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                     & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type, ])[1] != 1) {
                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                  " - Error: invalid \"set_duration_ref\" argument",
                                                  "\n",
                                                  sep = "")
                                              stop()
                                            }
                                            if (length(current_elementarycatches) != 0) {
                                              catch_weight_category_corrected <- sum(sapply(X = 1:length(current_elementarycatches),
                                                                                            FUN = function(l) {
                                                                                              if (is.null(current_elementarycatches[[l]]$.__enclos_env__$private$catch_weight_category_corrected)) {
                                                                                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                                                    " - Error: run function for logbook weigth categories conversion before that one",
                                                                                                    "\n",
                                                                                                    "identification: ",
                                                                                                    i,
                                                                                                    " - ",
                                                                                                    j,
                                                                                                    " - ",
                                                                                                    k,
                                                                                                    " - ",
                                                                                                    l,
                                                                                                    " (full trip - trip - activity - elementary catch)\n",
                                                                                                    sep = "")
                                                                                                stop()
                                                                                              } else {
                                                                                                current_elementarycatches[[l]]$.__enclos_env__$private$catch_weight_category_corrected
                                                                                              }
                                                                                            }))
                                              parameter_a <- set_duration_ref[set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                              & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type, "parameter_a"]
                                              parameter_b <- set_duration_ref[set_duration_ref$ocean == current_activity$.__enclos_env__$private$ocean
                                                                              & set_duration_ref$school_type == current_activity$.__enclos_env__$private$school_type, "parameter_b"]
                                              current_activity$.__enclos_env__$private$set_duration <- parameter_a * catch_weight_category_corrected + parameter_b
                                            } else {
                                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                  " - Error: set declared as positive but without elementary catch",
                                                  "\n",
                                                  "identification: ",
                                                  i,
                                                  " - ",
                                                  j,
                                                  " - ",
                                                  k,
                                                  " (full trip - trip - activity)\n",
                                                  sep = "")
                                              stop()
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Process 1.5 successfull on item ",
                                    i,
                                    "\n[trip: ",
                                    private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                    "]\n",
                                    sep = "")
                                if (i == length(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End process 1.5: set duration calculation\n",
                                      sep = "")
                                }
                              }
                            },
                            # time at sea ----
                            time_at_sea = function() {
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start process 1.6: time at sea calculation\n",
                                      sep = "")
                                }
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: trip avoided because not associated to a full trip\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  next()
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 1.6 on item ",
                                      i,
                                      "\n[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  for (j in 1:length(private$data_selected[[i]])) {
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
                                        for (k in 1:length(current_trip$.__enclos_env__$private$activities)) {
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
                                          for (l in 1:length(current_activities_departure_date)) {
                                            current_activities_departure_date_time_at_sea <- current_activities_departure_date_time_at_sea + current_activities_departure_date[[l]]$.__enclos_env__$private$time_at_sea
                                          }
                                        }
                                        if (length(current_activities_landing_date) != 0) {
                                          for (m in 1:length(current_activities_landing_date)) {
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
                                    "\n[trip: ",
                                    private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                    "]\n",
                                    sep = "")
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End process 1.6: time at sea calculation\n",
                                      sep = "")
                                }
                              }
                            },
                            # fishing_time ----
                            fishing_time = function() {
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start process 1.7: fishing time calculation\n",
                                      sep = "")
                                }
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: trip avoided because not associated to a full trip\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  next()
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 1.7 on item ",
                                      i,
                                      "\n[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    fishing_time <- 0
                                    if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                      activities_dates <- vector(mode = "list")
                                      for (k in 1:length(current_trip$.__enclos_env__$private$activities)) {
                                        current_activity_date <- current_trip$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$activity_date
                                        activities_dates <- append(activities_dates,
                                                                   current_activity_date)
                                      }
                                      activities_dates <- sort(x = unique(lubridate::date(activities_dates)))
                                      for (l in activities_dates) {
                                        fishing_time_tmp <- 0
                                        current_activities_code <- unique(sapply(X = 1:length(current_trip$.__enclos_env__$private$activities),
                                                                                 FUN = function(m) {
                                                                                   if (current_trip$.__enclos_env__$private$activities[[m]]$.__enclos_env__$private$activity_date == l) {
                                                                                     current_trip$.__enclos_env__$private$activities[[m]]$.__enclos_env__$private$activity_code
                                                                                   } else {
                                                                                     NA
                                                                                   }
                                                                                 }))
                                        current_activities_code <- current_activities_code[!is.na(current_activities_code)]
                                        if (any(! current_activities_code %in% c(4, 7, 10, 15, 100))) {
                                          current_activities_location <- unique(sapply(X = 1:length(current_trip$.__enclos_env__$private$activities),
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
                                          latitude_mean <- mean(sapply(X = 1:length(current_activities_location),
                                                                       FUN = function(o) {
                                                                         as.numeric(unlist(strsplit(current_activities_location[o],
                                                                                                    '_'))[1])
                                                                       }))
                                          longitude_mean <- mean(sapply(X = 1:length(current_activities_location),
                                                                        FUN = function(o) {
                                                                          as.numeric(unlist(strsplit(current_activities_location[o],
                                                                                                     '_'))[2])
                                                                        }))
                                          # sunrise (top edge of the sun appears on the horizon)
                                          current_sunrise <- suncalc::getSunlightTimes(date = lubridate::as_date(l),
                                                                                       lat = latitude_mean,
                                                                                       lon = longitude_mean)$sunrise
                                          # sunset (sun disappears below the horizon, evening civil twilight starts)
                                          current_sunset <- suncalc::getSunlightTimes(date = lubridate::as_date(l),
                                                                                      lat = latitude_mean,
                                                                                      lon = longitude_mean)$sunset
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
                                    "\n[trip: ",
                                    private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                    "]\n",
                                    sep = "")
                                if (i == length(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End process 1.7: fishing time calculation\n",
                                      sep = "")
                                }
                              }
                            },
                            # searching_time ----
                            searching_time = function() {
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start process 1.8: searching time calculation\n",
                                      sep = "")
                                }
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: trip avoided because not associated to a full trip\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  next()
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 1.8 on item ",
                                      i,
                                      "\n[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                      activities_set_duration <- sum(sapply(X = 1:length(current_trip$.__enclos_env__$private$activities),
                                                                            FUN = function(k) {
                                                                              if (! is.null(current_trip$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$set_duration)) {
                                                                                current_trip$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$set_duration
                                                                              } else {
                                                                                0
                                                                              }
                                                                            })) / 60
                                      if (is.null(current_trip$.__enclos_env__$private$fishing_time)) {
                                        cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                            " - Error: run fishing time calculation before searching time calculation\n",
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
                                    "\n[trip: ",
                                    private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                    "]\n",
                                    sep = "")
                                if (i == length(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End process 1.8: searching time calculation\n",
                                      sep = "")
                                }
                              }
                            },
                            # sample_number_measured_extrapolation ----
                            sample_number_measured_extrapolation = function() {
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start process 2.1: sample number measured extrapolation\n",
                                      sep = "")
                                }
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: trip avoided because not associated to a full trip\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  next()
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 2.1 on item ",
                                      i,
                                      "\n[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    current_well <- current_trip$.__enclos_env__$private$wells
                                    if (length(current_well) != 0) {
                                      for (q in 1:length(current_well)) {
                                        current_samples <- current_well[[q]]$.__enclos_env__$private$elementarysampleraw
                                        if (length(current_samples) != 0) {
                                          for (k in 1:length(current_samples)) {
                                            current_sample <- current_samples[[k]]
                                            sample_specie <- vector(mode = "character")
                                            for (l in 1:length(current_sample)) {
                                              sample_specie <- append(sample_specie,
                                                                      current_sample[[l]]$.__enclos_env__$private$specie_code3l)
                                            }
                                            sample_specie <- unique(sample_specie)
                                            for (m in sample_specie) {
                                              sum_sample_number_measured <- 0
                                              sample_total_count_tmp <- vector(mode = "character")
                                              current_sample_tmp <- vector(mode = "list")
                                              for (n in 1:length(current_sample)) {
                                                if (current_sample[[n]]$.__enclos_env__$private$specie_code3l == m) {
                                                  sum_sample_number_measured <- sum_sample_number_measured + current_sample[[n]]$.__enclos_env__$private$sample_number_measured
                                                  sample_total_count_tmp <- append(sample_total_count_tmp,
                                                                                   paste(current_sample[[n]]$.__enclos_env__$private$sub_sample_id,
                                                                                         current_sample[[n]]$.__enclos_env__$private$length_type,
                                                                                         current_sample[[n]]$.__enclos_env__$private$sample_total_count,
                                                                                         sep = "_"))
                                                  current_sample_tmp <- append(current_sample_tmp,
                                                                               current_sample[[n]])
                                                }
                                              }
                                              sample_total_count_tmp <- unique(sample_total_count_tmp)
                                              sum_sample_total_count <- sum(sapply(X = 1:length(sample_total_count_tmp),
                                                                                   FUN = function(o) {
                                                                                     as.numeric(unlist(strsplit(sample_total_count_tmp[o],
                                                                                                                '_'))[3])
                                                                                   }))
                                              rf4 <- sum_sample_number_measured / sum_sample_total_count
                                              for (p in 1:length(current_sample_tmp)) {
                                                current_sample_tmp[[p]]$.__enclos_env__$private$rf4 <- rf4
                                                current_sample_tmp[[p]]$.__enclos_env__$private$sample_number_measured_extrapolated <- current_sample_tmp[[p]]$.__enclos_env__$private$sample_number_measured * rf4
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
                                    "\n[trip: ",
                                    private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                    "]\n",
                                    sep = "")
                                if (i == length(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End process 2.1: sample number measured extrapolation\n",
                                      sep = "")
                                }
                              }
                            },
                            # sample_length_class_ld1_to_lf ----
                            sample_length_class_ld1_to_lf =  function(length_step) {
                              length_step_count <- length_step %>%
                                dplyr::group_by(ocean, specie_code3l, ld1_class) %>%
                                dplyr::summarise(nb = n())
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start process 2.2: sample length class conversion ld1 to lf\n",
                                      sep = "")
                                }
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: trip avoided because not associated to a full trip\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  next()
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 2.2 on item ",
                                      i,
                                      "\n[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    current_wells <- current_trip$.__enclos_env__$private$wells
                                    if (length(current_wells) != 0) {
                                      current_well_samples <- lapply(X = 1:length(current_wells),
                                                                FUN = function(o) {
                                                                  if (length(current_wells[[o]]$.__enclos_env__$private$elementarysampleraw) != 0) {
                                                                    current_wells[[o]]$.__enclos_env__$private$elementarysampleraw
                                                                  } else {
                                                                    next()
                                                                  }
                                                                })
                                      for (k in 1:length(current_well_samples)) {
                                        current_samples <- current_well_samples[[k]]
                                        for (p in 1:length(current_samples)) {
                                          current_sample <- unlist(current_samples[[p]])
                                          for (l in 1:length(current_sample)) {
                                            current_elementary_sample <- current_sample[[l]]
                                            if (current_elementary_sample$.__enclos_env__$private$length_type == 2) {
                                              current_elementary_sample$.__enclos_env__$private$sample_length_class_lf <- current_elementary_sample$.__enclos_env__$private$sample_length_class
                                              current_elementary_sample$.__enclos_env__$private$sample_number_measured_extrapolated_lf <- current_elementary_sample$.__enclos_env__$private$sample_number_measured_extrapolated
                                            } else {
                                              current_activities <- current_trip$.__enclos_env__$private$activities
                                              if (length(current_activities) != 0) {
                                                ocean_activities <- vector(mode = "integer")
                                                for (m in 1:length(current_activities)) {
                                                  ocean_activities <- append(ocean_activities,
                                                                             current_activities[[m]]$.__enclos_env__$private$ocean)
                                                }
                                                ocean_activities <- unique(ocean_activities)
                                                if (length(ocean_activities) != 1) {
                                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                      " - Error: activites associated to sample in more than one ocean\n",
                                                      "[trip_id: ",
                                                      current_elementary_sample$.__enclos_env__$private$trip_id,
                                                      "\nwell_id: ",
                                                      current_elementary_sample$.__enclos_env__$private$well_id,
                                                      "\nsample_id: ",
                                                      current_elementary_sample$.__enclos_env__$private$sample_id,
                                                      "]",
                                                      sep = "")
                                                  stop()
                                                }
                                              } else {
                                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                    " - Error: sample detected without any activity associated\n",
                                                    "[trip_id: ",
                                                    current_elementary_sample$.__enclos_env__$private$trip_id,
                                                    "\nwell_id: ",
                                                    current_elementary_sample$.__enclos_env__$private$well_id,
                                                    "\nsample_id: ",
                                                    current_elementary_sample$.__enclos_env__$private$sample_id,
                                                    "]",
                                                    sep = "")

                                                stop()
                                              }
                                              current_length_step_count <- as.numeric(length_step_count[length_step_count$ocean == ocean_activities
                                                                                                        & length_step_count$specie_code3l == current_elementary_sample$.__enclos_env__$private$specie_code3l
                                                                                                        & length_step_count$ld1_class == current_elementary_sample$.__enclos_env__$private$sample_length_class, "nb"])
                                              if (is.na(current_length_step_count)) {
                                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                    " - Error: no correspondance between sample length class and ld1-lf reference table\n",
                                                    "[trip_id: ",
                                                    current_elementary_sample$.__enclos_env__$private$trip_id,
                                                    "\nwell_id: ",
                                                    current_elementary_sample$.__enclos_env__$private$well_id,
                                                    "\nsample_id: ",
                                                    current_elementary_sample$.__enclos_env__$private$sample_id,
                                                    "]",
                                                    sep = "")
                                                stop()
                                              }
                                              current_length_step <- length_step[length_step$ocean == ocean_activities
                                                                                 & length_step$specie_code3l == current_elementary_sample$.__enclos_env__$private$specie_code3l
                                                                                 & length_step$ld1_class == current_elementary_sample$.__enclos_env__$private$sample_length_class, ]
                                              current_elementary_sample_tmp <- vector(mode = "list")
                                              for (n in 1:current_length_step_count) {
                                                if (n == current_length_step_count) {
                                                  current_elementary_sample$.__enclos_env__$private$length_type <- 2
                                                  current_elementary_sample$.__enclos_env__$private$sample_length_class_lf <- current_length_step[n, "lf_class"]
                                                  current_elementary_sample$.__enclos_env__$private$sample_number_measured_extrapolated_lf <- current_length_step[n, "ratio"] * 10^-2 * current_elementary_sample_tmpbis$.__enclos_env__$private$sample_number_measured_extrapolated
                                                } else {
                                                  current_elementary_sample_tmpbis <- current_elementary_sample$clone()
                                                  current_elementary_sample_tmpbis$.__enclos_env__$private$length_type <- 2
                                                  current_elementary_sample_tmpbis$.__enclos_env__$private$sample_length_class_lf <- current_length_step[n, "lf_class"]
                                                  current_elementary_sample_tmpbis$.__enclos_env__$private$sample_number_measured_extrapolated_lf <- current_length_step[n, "ratio"] * 10^-2 * current_elementary_sample_tmpbis$.__enclos_env__$private$sample_number_measured_extrapolated
                                                  current_elementary_sample_tmp <- append(current_elementary_sample_tmp, current_elementary_sample_tmpbis)
                                                  if (n == current_length_step_count - 1) {
                                                    private$data_selected[[i]][[j]]$.__enclos_env__$private$wells[[k]]$.__enclos_env__$private$elementarysampleraw[[p]] <- append(private$data_selected[[i]][[j]]$.__enclos_env__$private$wells[[k]]$.__enclos_env__$private$elementarysampleraw[[p]],
                                                                                                                                                                                  current_elementary_sample_tmp)
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
                                    " - Process 2.2 successfull on item ",
                                    i,
                                    "\n[trip: ",
                                    private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                    "]\n",
                                    sep = "")
                                if (i == length(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End process 2.2 sample length class conversion ld1 to lf\n",
                                      sep = "")
                                }
                              }
                            },
                            # sample_length_class_step_standardisation ----
                            sample_length_class_step_standardisation = function() {
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start process 2.3: sample length class step standardisation\n",
                                      sep = "")
                                }
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  # full trip is not complet (missing at least one trip)
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: trip avoided because not associated to a full trip\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  next()
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 2.3 on item ",
                                      i,
                                      "\n[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    current_wells <- current_trip$.__enclos_env__$private$wells
                                    if (length(current_wells) != 0) {
                                      current_wells_samples <- lapply(X = 1:length(current_wells),
                                                                      FUN = function(y) {
                                                                        if (length(current_wells[[y]]$.__enclos_env__$private$elementarysampleraw) != 0) {
                                                                          current_wells[[y]]$.__enclos_env__$private$elementarysampleraw
                                                                        } else {
                                                                          next()
                                                                        }
                                                                      })
                                      for (x in 1:length(current_wells_samples)) {
                                        current_well <- current_wells_samples[[x]]
                                        current_wells[[x]]$.__enclos_env__$private$elementarysample <- vector(mode = "list",
                                                                                                              length = length(current_well))
                                        for (k in 1:length(current_well)) {
                                          current_sample <- unlist(current_well[[k]])
                                          sample_species <- unique(sapply(X = 1:length(current_sample),
                                                                          FUN = function(l) {
                                                                            current_sample[[l]]$.__enclos_env__$private$specie_code3l
                                                                          }))
                                          current_sample_by_species <- vector(mode = "list", length = length(sample_species))
                                          for (m in 1:length(current_sample)) {
                                            for (n in 1:length(current_sample_by_species)) {
                                              if (current_sample[[m]]$.__enclos_env__$private$specie_code3l == sample_species[n]) {
                                                current_sample_by_species[[n]] <- append(current_sample_by_species[[n]],
                                                                                         current_sample[[m]])
                                              }
                                            }
                                          }
                                          current_sample_standardised <- vector(mode = "list")
                                          for (o in 1:length(current_sample_by_species)) {
                                            current_sample_specie <- current_sample_by_species[[o]]
                                            sample_length_class_lf <- sort(unique(sapply(X = 1:length(current_sample_specie),
                                                                                         FUN = function(p) {
                                                                                           current_sample_specie[[p]]$.__enclos_env__$private$sample_length_class_lf
                                                                                         })))
                                            max_lf_class <- 500
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
                                              for (w in 1:length(current_sample_specie)) {
                                                object_elementarysample <- elementarysample$new(trip_id = current_sample_specie[[w]]$.__enclos_env__$private$trip_id,
                                                                                                well_id = current_sample_specie[[w]]$.__enclos_env__$private$well_id,
                                                                                                sample_id = current_sample_specie[[w]]$.__enclos_env__$private$sample_id,
                                                                                                sub_sample_id = current_sample_specie[[w]]$.__enclos_env__$private$sub_sample_id,
                                                                                                sample_quality = current_sample_specie[[w]]$.__enclos_env__$private$sample_quality,
                                                                                                sample_type = current_sample_specie[[w]]$.__enclos_env__$private$sample_type,
                                                                                                specie_code3l = current_sample_specie[[w]]$.__enclos_env__$private$specie_code3l,
                                                                                                sample_standardised_length_class_lf = current_sample_specie[[w]]$.__enclos_env__$private$sample_length_class_lf,
                                                                                                sample_number_measured_extrapolated_lf = as.numeric(current_sample_specie[[w]]$.__enclos_env__$private$sample_number_measured_extrapolated_lf),
                                                                                                sample_total_count = as.integer(current_sample_specie[[w]]$.__enclos_env__$private$sample_total_count),
                                                                                                elementarysampleraw = current_sample_specie[[w]])
                                                current_wells[[x]]$.__enclos_env__$private$elementarysample[[k]] <- append(current_wells[[x]]$.__enclos_env__$private$elementarysample[[k]],
                                                                                                                           list(object_elementarysample))
                                              }
                                            } else {
                                              lower_border_reference <- seq(from = 0, to = max_lf_class - 1, by = step)
                                              upper_border_reference <- seq(from = step, to = max_lf_class, by = step)
                                              q <- 1
                                              while(q <= length(sample_length_class_lf)) {
                                                lower_border <- data.table::last(x = lower_border_reference[which(lower_border_reference <= trunc(sample_length_class_lf[q]))])
                                                upper_border <- data.table::first(x = upper_border_reference[which(upper_border_reference > trunc(sample_length_class_lf[q]))])
                                                sample_length_class_lf_for_merge <- sample_length_class_lf[which(sample_length_class_lf >= lower_border
                                                                                                                 & sample_length_class_lf < upper_border)]
                                                current_sample_specie_by_step <- unlist(lapply(X = 1:length(current_sample_specie),
                                                                                               FUN = function(r) {
                                                                                                 if (current_sample_specie[[r]]$.__enclos_env__$private$sample_length_class_lf %in% sample_length_class_lf_for_merge) {
                                                                                                   current_sample_specie[[r]]
                                                                                                 }
                                                                                               }))
                                                current_sample_specie_by_step_subid <- unique(sapply(X = 1:length(current_sample_specie_by_step),
                                                                                                     FUN = function(s) {
                                                                                                       current_sample_specie_by_step[[s]]$.__enclos_env__$private$sub_sample_id
                                                                                                     }))
                                                for (t in current_sample_specie_by_step_subid) {
                                                  current_sample_specie_by_step_by_subid <- unlist(lapply(X = 1:length(current_sample_specie_by_step),
                                                                                                          FUN = function(u) {
                                                                                                            if (current_sample_specie_by_step[[u]]$.__enclos_env__$private$sub_sample_id == t) {
                                                                                                              current_sample_specie_by_step[[u]]
                                                                                                            }
                                                                                                          }))
                                                  object_elementarysample <- elementarysample$new(trip_id = current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$trip_id,
                                                                                                  well_id = current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$well_id,
                                                                                                  sample_id = current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$sample_id,
                                                                                                  sub_sample_id = current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$sub_sample_id,
                                                                                                  sample_quality = current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$sample_quality,
                                                                                                  sample_type = current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$sample_type,
                                                                                                  specie_code3l = current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$specie_code3l,
                                                                                                  sample_standardised_length_class_lf = as.integer(lower_border),
                                                                                                  sample_number_measured_extrapolated_lf = sum(sapply(X = 1:length(current_sample_specie_by_step_by_subid),
                                                                                                                                                      FUN = function(v) {
                                                                                                                                                        current_sample_specie_by_step_by_subid[[v]]$.__enclos_env__$private$sample_number_measured_extrapolated_lf
                                                                                                                                                      })),
                                                                                                  sample_total_count = as.integer(current_sample_specie_by_step_by_subid[[1]]$.__enclos_env__$private$sample_total_count),
                                                                                                  elementarysampleraw = current_sample_specie_by_step_by_subid)
                                                  current_wells[[x]]$.__enclos_env__$private$elementarysample[[k]] <- append(current_wells[[x]]$.__enclos_env__$private$elementarysample[[k]],
                                                                                                                             list(object_elementarysample))
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
                                    "\n[trip: ",
                                    private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                    "]\n",
                                    sep = "")
                                if (i == length(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End process 2.3: sample length class step standardisation\n",
                                      sep = "")
                                }
                              }
                            },
                            # well_set_weigth_categories ----
                            well_set_weigth_categories = function(sample_set) {
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start process 2.4: well-set weight categories definition\n",
                                      sep = "")
                                }
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: trip avoided because not associated to a full trip\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  next()
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 2.4 on item ",
                                      i,
                                      "\n[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    current_wells <- current_trip$.__enclos_env__$private$wells
                                    if (length(current_wells) != 0) {
                                      for (k in 1:length(current_wells)) {
                                        current_well <- current_wells[[k]]
                                        if (current_well$.__enclos_env__$private$well_minus10_weigth != 0 || current_well$.__enclos_env__$private$well_plus10_weigth != 0) {
                                          # case 1: we use proportion of -10/+10 in the well
                                          proportion_verification <- 1
                                        } else if (current_well$.__enclos_env__$private$well_global_weigth != 0) {
                                          # case 2: we don't know the proportion of -10/+10 in the well, we use global weight
                                          proportion_verification <- 2
                                        } else {
                                          # case 3: we know nothing... sample(s) in the well are not usable
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
                                                " samples detected in a single well.",
                                                " Only the first sample well set weighted weight will be considerated\n",
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
                                          current_well$.__enclos_env__$private$wellsets <- lapply(X = 1:nrow(x = sample_set_well),
                                                                                                  FUN = function(l) {
                                                                                                    t3:::wellset$new(trip_id = sample_set_well[l, 1],
                                                                                                                     activity_id = sample_set_well[l, 2],
                                                                                                                     well_id = sample_set_well[l, 3],
                                                                                                                     sample_id = sample_set_well[l, 4],
                                                                                                                     weighted_weight = sample_set_well[l, 5],
                                                                                                                     weighted_weight_minus10 =  sample_set_well[l, 5] * current_well$.__enclos_env__$private$well_prop_minus10_weigth,
                                                                                                                     weighted_weight_plus10 =  sample_set_well[l, 5] * current_well$.__enclos_env__$private$well_prop_plus10_weigth)
                                                                                                     })
                                          sum_weighted_weight <- sum(sapply(X = 1:length(current_well$.__enclos_env__$private$wellsets),
                                                                            FUN = function(m) {
                                                                              current_well$.__enclos_env__$private$wellsets[[m]]$.__enclos_env__$private$weighted_weight
                                                                            }))
                                          for (n in 1:length(current_well$.__enclos_env__$private$wellsets)) {
                                            current_well$.__enclos_env__$private$wellsets[[n]]$.__enclos_env__$private$prop_weighted_weight <- current_well$.__enclos_env__$private$wellsets[[n]]$.__enclos_env__$private$weighted_weight / sum_weighted_weight
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Process 2.4 successfull on item ",
                                    i,
                                    "\n[trip: ",
                                    private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                    "]\n",
                                    sep = "")
                                if (i == length(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End process 2.4 well-set weight categories definition\n",
                                      sep = "")
                                }
                              }
                            },
                            # standardised_sample_creation ----
                            standardised_sample_creation = function() {
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start process 2.5: standardised sample creation\n",
                                      sep = "")
                                }
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: trip avoided because not associated to a full trip\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  next()
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 2.5 on item ",
                                      i,
                                      "\n[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    current_wells <- current_trip$.__enclos_env__$private$wells
                                    if (length(current_wells) != 0) {
                                      for (k in 1:length(current_wells)) {
                                        current_well <- current_wells[[k]]
                                        current_elementarysamples <- unlist(current_well$.__enclos_env__$private$elementarysample)
                                        if (length(current_elementarysamples) != 0) {
                                          current_elementarysamples_species <- unique(sapply(X = 1:length(current_elementarysamples),
                                                                                             FUN = function(l) {
                                                                                               current_elementarysamples[[l]]$.__enclos_env__$private$specie_code3l
                                                                                             }))
                                          for (m in current_elementarysamples_species) {
                                            current_elementarysamples_specie <- Filter(Negate(is.null),
                                                                                       lapply(X = 1:length(current_elementarysamples),
                                                                                              FUN = function(n) {
                                                                                                if (current_elementarysamples[[n]]$.__enclos_env__$private$specie_code3l == m) {
                                                                                                  current_elementarysamples[[n]]
                                                                                                }
                                                                                              }))
                                            current_elementarysamples_specie_classes <- unique(sapply(X = 1:length(current_elementarysamples_specie),
                                                                                                      FUN = function(o) {
                                                                                                        current_elementarysamples_specie[[o]]$.__enclos_env__$private$sample_standardised_length_class_lf
                                                                                                      }))
                                            for (p in current_elementarysamples_specie_classes) {
                                              current_elementarysamples_specie_class <- Filter(Negate(is.null),
                                                                                               lapply(X = 1:length(current_elementarysamples_specie),
                                                                                                      FUN = function(q) {
                                                                                                        if (current_elementarysamples_specie[[q]]$.__enclos_env__$private$sample_standardised_length_class_lf == p) {
                                                                                                          current_elementarysamples_specie[[q]]
                                                                                                        }
                                                                                                      }))
                                              current_elementarysamples_sample_types <- unique(sapply(X = 1:length(current_elementarysamples_specie_class),
                                                                                                      FUN = function(u) {
                                                                                                        current_elementarysamples_specie_class[[u]]$.__enclos_env__$private$sample_type
                                                                                                      }))
                                              for (v in current_elementarysamples_sample_types) {
                                                current_elementarysamples_sample_type <- Filter(Negate(is.null),
                                                                                                lapply(X = 1:length(current_elementarysamples_specie_class),
                                                                                                       FUN = function(w) {
                                                                                                         if (current_elementarysamples_specie_class[[w]]$.__enclos_env__$private$sample_type == v) {
                                                                                                           current_elementarysamples_specie_class[[w]]
                                                                                                         }
                                                                                                       }))
                                                current_elementarysamples_sample_qualities <- unique(sapply(X = 1:length(current_elementarysamples_sample_type),
                                                                                                          FUN = function(x) {
                                                                                                            current_elementarysamples_sample_type[[x]]$.__enclos_env__$private$sample_quality
                                                                                                          }))
                                                for (y in current_elementarysamples_sample_qualities) {
                                                  current_elementarysamples_sample_quality <- Filter(Negate(is.null),
                                                                                                  lapply(X = 1:length(current_elementarysamples_sample_type),
                                                                                                         FUN = function(z) {
                                                                                                           if (current_elementarysamples_sample_type[[z]]$.__enclos_env__$private$sample_quality == y) {
                                                                                                             current_elementarysamples_sample_type[[z]]
                                                                                                           }
                                                                                                         }))
                                                  standardisedsample <- t3:::standardisedsample$new(trip_id = current_well$.__enclos_env__$private$trip_id,
                                                                                                    well_id = current_well$.__enclos_env__$private$well_id,
                                                                                                    sample_id = unique(sapply(X = 1:length(current_elementarysamples_sample_quality),
                                                                                                                              FUN = function(r) {
                                                                                                                                current_elementarysamples_sample_quality[[r]]$.__enclos_env__$private$sample_id
                                                                                                                              })),
                                                                                                    sample_quality = v,
                                                                                                    sample_type = y,
                                                                                                    specie_code3l = m,
                                                                                                    sample_standardised_length_class_lf = as.integer(p),
                                                                                                    sample_number_measured_extrapolated_lf = sum(sapply(X = 1:length(current_elementarysamples_sample_quality),
                                                                                                                                                        FUN = function(s) {
                                                                                                                                                          current_elementarysamples_sample_quality[[s]]$.__enclos_env__$private$sample_number_measured_extrapolated_lf
                                                                                                                                                        })),
                                                                                                    sample_total_count = sum(sapply(X = 1:length(current_elementarysamples_sample_quality),
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
                                    "\n[trip: ",
                                    private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                    "]\n",
                                    sep = "")
                                if (i == length(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End process 2.5 standardised sample creation\n",
                                      sep = "")
                                }
                              }
                            },
                            # standardised_sample_set_creation ----
                            standardised_sample_set_creation = function(length_weight_relationship_data) {
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start process 2.6: standardised sample set creation\n",
                                      sep = "")
                                }
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: trip avoided because not associated to a full trip\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  next()
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 2.6 on item ",
                                      i,
                                      "\n[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    current_wells <- current_trip$.__enclos_env__$private$wells
                                    if (length(current_wells) != 0) {
                                      for (k in 1:length(current_wells)) {
                                        current_well <- current_wells[[k]]
                                        current_wells_sets <- current_well$.__enclos_env__$private$wellsets
                                        current_standardised_samples <- current_well$.__enclos_env__$private$standardisedsample
                                        standardised_samples_sets <- vector(mode = "list",
                                                                            length = length(current_wells_sets))
                                        for (l in 1:length(current_wells_sets)) {
                                          current_well_sets <- current_wells_sets[[l]]
                                          current_activity <- current_well_sets$.__enclos_env__$private$activity_id
                                          current_ocean <- unlist(Filter(Negate(is.null), sapply(X = 1:length(current_trip$.__enclos_env__$private$activities),
                                                                                                 FUN = function(m) {
                                                                                                   if (current_trip$.__enclos_env__$private$activities[[m]]$.__enclos_env__$private$activity_id == current_activity) {
                                                                                                     current_trip$.__enclos_env__$private$activities[[m]]$.__enclos_env__$private$ocean
                                                                                                   }
                                                                                                 })))
                                          if (is.null(current_ocean)) {
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Error: sample activity missing from trip activities\n",
                                                "[trip: ",
                                                private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                                ", sample activity: ",
                                                current_activity,
                                                "]\n",
                                                sep = "")
                                            stop()
                                          }
                                          current_standardised_samples_sets <- lapply(X = 1:length(current_standardised_samples),
                                                                                      FUN = function(m) {
                                                                                        current_length_weight_relationship <- dplyr::filter(.data = length_weight_relationship_data,
                                                                                                                                            ocean == current_ocean & specie_code3l == current_standardised_samples[[m]]$.__enclos_env__$private$specie_code3l)[3:4]
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
                                                                                          lwr <- coef_a * (length_class_lf ^ coef_b)
                                                                                        } else {
                                                                                          lwr <- NA
                                                                                          cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                                                              " - Warning: length to weight conversion impossible\n",
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
                                    "\n[trip: ",
                                    private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                    "]\n",
                                    sep = "")
                                if (i == length(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End process 2.6: standardised sample set creation\n",
                                      sep = "")
                                }
                              }
                            },
                            # raised_factors_determination ----
                            raised_factors_determination = function(threshold_rf_minus10,
                                                                    threshold_rf_plus10,
                                                                    threshold_frequency_rf_minus10,
                                                                    threshold_frequency_rf_plus10,
                                                                    threshold_rf_total) {
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start process 2.7: raised factors determination\n",
                                      sep = "")
                                }
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: trip avoided because not associated to a full trip\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  next()
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 2.7 on item ",
                                      i,
                                      "\n[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    current_wells <- current_trip$.__enclos_env__$private$wells
                                    if (length(current_wells) != 0) {
                                      for (k in 1:length(current_wells)) {
                                        current_well <- current_wells[[k]]
                                        current_wells_sets <- current_well$.__enclos_env__$private$wellsets
                                        for (l in 1:length(current_wells_sets)) {
                                          current_well_sets <- current_wells_sets[[l]]
                                          current_well_standardisedsampleset <- current_well$.__enclos_env__$private$standardisedsampleset[[l]]
                                          current_well_sets$.__enclos_env__$private$weighted_samples_minus10 <- sum(unlist(lapply(X = 1:length(current_well_standardisedsampleset),
                                                                                                                                  FUN = function(m) {
                                                                                                                                    if (current_well_standardisedsampleset[[m]]$.__enclos_env__$private$sample_weight_unit <= 10) {
                                                                                                                                      current_well_standardisedsampleset[[m]]$.__enclos_env__$private$sample_weigth
                                                                                                                                    }
                                                                                                                                  }))) / 1000
                                          current_well_sets$.__enclos_env__$private$weighted_samples_plus10 <- sum(unlist(lapply(X = 1:length(current_well_standardisedsampleset),
                                                                                                                                  FUN = function(n) {
                                                                                                                                    if (current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_weight_unit > 10) {
                                                                                                                                      current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_weigth
                                                                                                                                    }
                                                                                                                                  }))) / 1000
                                          current_well_sets$.__enclos_env__$private$weighted_samples_total <- sum(unlist(lapply(X = 1:length(current_well_standardisedsampleset),
                                                                                                                                FUN = function(o) {
                                                                                                                                  current_well_standardisedsampleset[[o]]$.__enclos_env__$private$sample_weigth
                                                                                                                                }))) / 1000
                                          if (current_well_sets$.__enclos_env__$private$weighted_samples_total <= 0) {
                                            # scenario 1
                                            current_well_sets$.__enclos_env__$private$rf_validation <- 1
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Warning: well-set avoided because weighted samples total value egal to zero\n",
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
                                                " - Warning: well-set avoided because invalid weighted weigth\n",
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
                                                  || sum(unlist(sapply(X = 1:length(current_well_standardisedsampleset),
                                                                       FUN = function(p) {
                                                                         if (current_well_standardisedsampleset[[p]]$.__enclos_env__$private$sample_weight_unit <= 10) {
                                                                           current_well_standardisedsampleset[[p]]$.__enclos_env__$private$sample_number_weighted
                                                                         }
                                                                       }))) > threshold_frequency_rf_minus10
                                                  || sum(unlist(sapply(X = 1:length(current_well_standardisedsampleset),
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
                                                "\n[trip: ,",
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
                                    "\n[trip: ",
                                    private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                    "]\n",
                                    sep = "")
                                if (i == length(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End process 2.7: raised factors determination\n",
                                      sep = "")
                                }
                              }
                            },
                            # raised standardised sample set ----
                            raised_standardised_sample_set = function() {
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start process 2.8: raised standardised sample set\n",
                                      sep = "")
                                }
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Warning: trip avoided because not associated to a full trip\n",
                                      "[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  next()
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Ongoing process 2.8 on item ",
                                      i,
                                      "\n[trip: ",
                                      private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                      "]\n",
                                      sep = "")
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    current_wells <- current_trip$.__enclos_env__$private$wells
                                    if (length(current_wells) != 0) {
                                      for (k in 1:length(current_wells)) {
                                        current_well <- current_wells[[k]]
                                        current_wells_sets <- current_well$.__enclos_env__$private$wellsets
                                        for (l in 1:length(current_wells_sets)) {
                                          current_well_sets <- current_wells_sets[[l]]
                                          current_well_standardisedsampleset <- current_well$.__enclos_env__$private$standardisedsampleset[[l]]
                                          if (current_well_sets$.__enclos_env__$private$rf_validation %in% c(1, 2)) {
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Warning: raised factors not available for this well-set\n",
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
                                            for (m in 1:length(current_well_standardisedsampleset)) {
                                              current_well_standardisedsampleset[[m]]$.__enclos_env__$private$sample_number_weighted_set <- current_well_standardisedsampleset[[m]]$.__enclos_env__$private$sample_number_weighted * current_rf_total
                                              current_well_standardisedsampleset[[m]]$.__enclos_env__$private$sample_weigth_set <- current_well_standardisedsampleset[[m]]$.__enclos_env__$private$sample_weight_unit * current_well_standardisedsampleset[[m]]$.__enclos_env__$private$sample_number_weighted_set
                                            }
                                          } else if (current_well_sets$.__enclos_env__$private$rf_validation == 5) {
                                            current_rf_minus10 <- current_well_sets$.__enclos_env__$private$rf_minus10
                                            current_rf_plus10 <- current_well_sets$.__enclos_env__$private$rf_plus10
                                            for (n in 1:length(current_well_standardisedsampleset)) {
                                              if (current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_weight_unit <= 10) {
                                                current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_number_weighted_set <- current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_number_weighted * current_rf_minus10
                                              } else {
                                                current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_number_weighted_set <- current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_number_weighted * current_rf_plus10
                                              }
                                              current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_weigth_set <- current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_weight_unit * current_well_standardisedsampleset[[n]]$.__enclos_env__$private$sample_number_weighted_set
                                            }
                                          } else {
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Error: raised factors verifications is not valide\n",
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
                                    "\n[trip: ",
                                    private$data_selected[[i]][[1]]$.__enclos_env__$private$trip_id,
                                    "]\n",
                                    sep = "")
                                if (i == length(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End 2.8 process: raised standardised sample set\n",
                                      sep = "")
                                }
                              }
                            },
                            # path to level 3
                            path_to_level3 = function() {
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Start path creation for level 3\n",
                                  sep = "")
                              inputs_level3 <- vector(mode = "list",
                                                      length = 5)
                              names(inputs_level3) <- c("act", "act3", "samw", "sset", "wp")
                              act <- data.frame(stringsAsFactors = FALSE)
                              act3 <- data.frame(stringsAsFactors = FALSE)
                              samw <- data.frame(stringsAsFactors = FALSE)
                              sset <- data.frame(stringsAsFactors = FALSE)
                              wp <- data.frame(stringsAsFactors = FALSE)
                              for (i in 1:length(private$data_selected)) {
                                for (j in 1:length(private$data_selected[[i]])) {
                                  current_trip <- private$data_selected[[i]][[j]]
                                  current_activities <- current_trip$.__enclos_env__$private$activities
                                  current_wells <- current_trip$.__enclos_env__$private$wells
                                  if (length(current_activities) != 0) {
                                    for (k in 1:length(current_activities)) {
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
                                        for (l in 1:length(current_elementarycatches)) {
                                          current_elementarycatch <- current_elementarycatches[[l]]
                                          tmp_elementarycatch <- data.frame("id_act" = current_elementarycatch$.__enclos_env__$private$activity_id,
                                                                            "w_lb_t3" = current_elementarycatch$.__enclos_env__$private$catch_weight_category_corrected,
                                                                            "date_act" = current_activity$.__enclos_env__$private$activity_date,
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
                                    for (m in 1:length(current_wells)) {
                                      current_well <- current_wells[[m]]
                                      current_standardisedsamplesets <- unlist(current_well$.__enclos_env__$private$standardisedsampleset)
                                      current_wellplan <- current_well$.__enclos_env__$private$wellplan
                                      if (! is.null(current_standardisedsamplesets)) {
                                        for (n in 1:length(current_standardisedsamplesets)) {
                                          current_standardisedsampleset <- current_standardisedsamplesets[[n]]
                                          tmp_standardisedsampleset <- data.frame("id_act" = current_standardisedsampleset$.__enclos_env__$private$activity_id,
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
                                        for (o in 1:length(current_wellplan)) {
                                          current_elementarywellplan <- current_wellplan[[o]]
                                          tmp_elementarywellplan <- data.frame("id_well" = current_elementarywellplan$.__enclos_env__$private$well_id,
                                                                               "id_act" = current_elementarywellplan$.__enclos_env__$private$activity_id,
                                                                               "id_sample" = current_elementarywellplan$.__enclos_env__$private$sample_id,
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
                              inputs_level3[[1]] <- act
                              inputs_level3[[2]] <- act3
                              inputs_level3[[3]] <- data.frame(dplyr::group_by(.data = samw,
                                                                               id_act, sp, wcat) %>%
                                                                 dplyr::summarise(w_fit_t3 = sum(w_fit_t3)) %>%
                                                                 dplyr::ungroup())
                              inputs_level3[[4]] <- unique(sset)
                              inputs_level3[[5]] <- wp
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End path creation for level 3\n",
                                  sep = "")
                              inputs_level3 <<- inputs_level3
                            },
                            # browser
                            show_me_what_you_got = function() {
                              browser()
                            }),
                          private = list(
                            id_not_full_trip = NULL,
                            id_not_full_trip_retained = NULL,
                            data_selected = NULL
                          ))
