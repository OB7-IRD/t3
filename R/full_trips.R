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
                                    activities_tmp <- object_activities$filter(attribut_l1 = "data",
                                                                               filter = paste0("arg$trip_id == \"",
                                                                                               trip_id,
                                                                                               "\""))
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
                                  print(i)
                                  if (i == 1) {
                                    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                        " - Start of add elementary catches\n",
                                        sep = "")
                                  }
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    if (length(private$data_selected[[i]][[j]]$.__enclos_env__$private$activities) != 0) {
                                      for (k in 1:length(private$data_selected[[i]][[j]]$.__enclos_env__$private$activities)) {
                                        activity_id <- private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$activity_id
                                        elementarycatches_tmp <- object_elementarycatches$filter(attribut_l1 = "data",
                                                                                                 filter = paste0("arg$activity_id == \"",
                                                                                                                 activity_id,
                                                                                                                 "\""))
                                        private$data_selected[[i]][[j]]$.__enclos_env__$private$activities[[k]]$.__enclos_env__$private$elementarycatches <- elementarycatches_tmp
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
                                    elementarylandings_tmp <- object_elementarylandings$filter(attribut_l1 = "data",
                                                                                               filter = paste0("arg$trip_id == \"",
                                                                                                               trip_id,
                                                                                                               "\""))
                                    private$data_selected[[i]][[j]]$.__enclos_env__$private$elementarylandings <- elementarylandings_tmp
                                  }
                                }
                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - End of add elementary landings\n",
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
                                        " - Start of raising factor level 1\n",
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
                                    " - End of raising factor level 1\n",
                                    sep = "")
                              }
                            },
                            # raising factor level 2 ----
                            rf2 = function() {
                              for (i in 1:length(private$data_selected)) {
                                if (i == 1) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Start raising factor level 2\n",
                                      sep = "")
                                } else if (i == length(private$data_selected)) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - End of raising factor level 2\n",
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
                                    if (! is.null(current_elementarycatches)) {
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
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Start of logbook weight categories conversion\n",
                                  sep = "")
                              category_1 <- "<10kg"
                              category_2 <- "10-30kg"
                              category_3 <- ">30kg"
                              category_4 <- ">10kg"
                              category_5 <- "unknown"
                              for (i in 1:length(private$data_selected)) {
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  # full trip is not complet (missing at least one trip)
                                  next()
                                } else {
                                  # first stage: conversion of all categories except for unknown (category 9)
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                      for (w in 1:length(current_trip$.__enclos_env__$private$activities)) {
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
                                  # second stage: conversion of category unknow (category 9) if possible
                                  for (l in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[l]]
                                    current_elementarycatches <- NULL
                                    if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                      for (x in 1:length(current_trip$.__enclos_env__$private$activities)) {
                                        current_elementarycatches <- append(current_elementarycatches,
                                                                            current_trip$.__enclos_env__$private$activities[[x]]$.__enclos_env__$private$elementarycatches)
                                      }
                                    }
                                    if (! is.null(current_elementarycatches)) {
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
                              }
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - End of logbook weight categories conversion\n",
                                  sep = "")
                            },
                            # set_count ----
                            set_count = function () {
                              for (i in 1:length(private$data_selected)) {
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  # full trip is not complet (missing at least one trip)
                                  next()
                                } else {
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                      for (k in 1:length(current_trip$.__enclos_env__$private$activities)) {
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
                            },
                            # set_duration ----
                            set_duration = function(set_duration_ref) {
                              browser()
                              for (i in 1:length(private$data_selected)) {
                                if (names(private$data_selected)[i] %in% private$id_not_full_trip_retained) {
                                  # full trip is not complet (missing at least one trip)
                                  next()
                                } else {
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    current_trip <- private$data_selected[[i]][[j]]
                                    if (length(current_trip$.__enclos_env__$private$activities) != 0) {
                                      for (k in 1:length(current_trip$.__enclos_env__$private$activities)) {
                                        current_activity <- current_trip$.__enclos_env__$private$activities[[k]]
                                        if (current_activity$.__enclos_env__$private$activity_code == 0) {
                                          # for a set declared as null set
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
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }),
                          private = list(
                            id_not_full_trip = NULL,
                            id_not_full_trip_retained = NULL,
                            data_selected = NULL
                          ))
