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
                                stop("invalid \"object_trips\" argument\nClass R6 and trips expected")
                              }
                              full_trips <- list()
                              full_trips_tmp <- list()
                              full_trip_warning <- 0
                              i <- 1
                              while(i <= object_trips$count()) {
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
                            },
                            # filter full trips by periode_reference ----
                            filter_by_periode = function(periode_reference) {
                              if (length(class(periode_reference)) == 1 && class(periode_reference) != "numeric") {
                                stop("invalid \"periode_reference\" argument\nclass numeric expected")
                              } else if (nchar(periode_reference) != 4) {
                                stop("invalid \"periode_reference\" argument\nyear format on 4 digits expected")
                              } else {
                                for (i in 1:length(private$data)) {
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
                              }
                            },
                            # add elementary catches ----
                            add_elementarycatches = function(object_elementarycatches) {
                              if (length(private$data_selected) == 0) {
                                stop("argument \"data_selecetd\" empty\nlaunch selection data before")
                              } else if (! any(class(object_elementarycatches) == "elementarycatches")) {
                                stop("invalid \"object_elementarycatches\" argument\nclass elementarycatches expected")
                              } else {
                                for (i in 1:length(private$data_selected)) {
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    trip_id <- private$data_selected[[i]][[j]]$.__enclos_env__$private$trip_id
                                    elementarycatches_tmp <- list(object_elementarycatches$filter(attribut_l1 = "data",
                                                                                                  filter = paste0("arg$trip_id == \"",
                                                                                                                  trip_id,
                                                                                                                  "\"")))
                                    private$data_selected[[i]][[j]]$.__enclos_env__$private$elementarycatches <- elementarycatches_tmp
                                  }
                                }
                              }
                            },
                            # add landing catches ----
                            add_elementarylandings = function(object_elementarylandings) {
                              if (length(private$data_selected) == 0) {
                                stop("argument \"data_selecetd\" empty\nlaunch selection data before")
                              } else if (! any(class(object_elementarylandings) == "elementarylandings")) {
                                stop("invalid \"object_elementarylandings\" argument\nclass elementarylandings expected")
                              } else {
                                for (i in 1:length(private$data_selected)) {
                                  for (j in 1:length(private$data_selected[[i]])) {
                                    trip_id <- private$data_selected[[i]][[j]]$.__enclos_env__$private$trip_id
                                    elementarylandings_tmp <- list(object_elementarylandings$filter(attribut_l1 = "data",
                                                                                                    filter = paste0("arg$trip_id == \"",
                                                                                                                    trip_id,
                                                                                                                    "\"")))
                                    private$data_selected[[i]][[j]]$.__enclos_env__$private$elementarylandings <- elementarylandings_tmp
                                  }
                                }
                              }
                            },
                            # raising factor level 1 ----
                            rf1 = function(object_elementarycatches, object_elementarylandings, species_rf1) {
                              if (! any(class(object_elementarycatches) == "elementarycatches")) {
                                stop("invalid \"object_elementarycatches\" argument\nclass elementarycatches expected")
                              } else if (! any(class(object_elementarylandings) == "elementarylandings")) {
                                stop("invalid \"object_elementarylandings\" argument\nclass elementarycatches expected")
                              } else if (length(class(species_rf1)) != 1 || class(species_rf1) != "character") {
                                stop("invalid \"species_rf1\" argument\nclass character expected")
                              } else {
                                for (i in 1:length(private$data)) {
                                  current_full_trip <- private$data[[i]]
                                  trip_id <- vector(mode = "character")
                                  if (is.environment(current_full_trip)) {
                                    trip_id <- append(trip_id,
                                                      current_full_trip$.__enclos_env__$private$trip_id)
                                  } else {
                                    for (j in 1:length(current_full_trip)) {
                                      current_full_trip_tmp <- current_full_trip[[j]]
                                      trip_id <- append(trip_id,
                                                        current_full_trip_tmp$.__enclos_env__$private$trip_id)
                                    }
                                  }
                                  current_elementarycatches <- list()
                                  current_elementarycatches <- lapply(trip_id, function(x) {
                                    append(current_elementarycatches,
                                           object_elementarycatches$.__enclos_env__$private$data_selected[[x]])
                                  })
                                  current_elementarycatches <- unlist(current_elementarycatches)
                                  if (is.null(current_elementarycatches)) {
                                    if (length(trip_id) == 1) {
                                      if (current_full_trip$.__enclos_env__$private$logbook_availability == 1) {
                                        current_full_trip$.__enclos_env__$private$rf1 <- NA
                                      } else {
                                        warning("missing logbook\ntrip id: ",
                                                paste0("landing date ",
                                                       current_full_trip$.__enclos_env__$private$landing_date,
                                                       " and vessel ",
                                                       current_full_trip$.__enclos_env__$private$vessel_id),
                                                call. = FALSE)
                                        current_full_trip$.__enclos_env__$private$rf1 <- 1
                                      }
                                    } else {
                                      for (p in 1:length(trip_id)) {
                                        if (current_full_trip[[p]]$.__enclos_env__$private$logbook_availability == 1) {
                                          current_full_trip[[p]]$.__enclos_env__$private$rf1 <- NA
                                        } else {
                                          warning("missing logbook\ntrip id: ",
                                                  paste0("landing date ",
                                                         current_full_trip[[p]]$.__enclos_env__$private$landing_date,
                                                         " and vessel ",
                                                         current_full_trip[[p]]$.__enclos_env__$private$vessel_id),
                                                  call. = FALSE)
                                          current_full_trip[[p]]$.__enclos_env__$private$rf1 <- 1
                                        }
                                      }
                                    }
                                  } else {
                                    current_elementarycatches_weight <- vector(mode = "numeric")
                                    for (k in 1:length(current_elementarycatches)) {
                                      if (current_elementarycatches[[k]]$.__enclos_env__$private$specie_code3l %in% species_rf1) {
                                        current_elementarycatches_weight <- append(current_elementarycatches_weight,
                                                                                   current_elementarycatches[[k]]$.__enclos_env__$private$catch_weight)
                                      }
                                    }
                                    current_elementarylandings <- list()
                                    current_elementarylandings <- lapply(trip_id, function(x) {
                                      append(current_elementarylandings,
                                             object_elementarylandings$.__enclos_env__$private$data_selected[[x]])
                                    })
                                    current_elementarylandings <- unlist(current_elementarylandings)
                                    if (is.null(current_elementarylandings)) {
                                      if (length(trip_id) == 1) {
                                        warning("missing elementary landing\ntrip id: ",
                                                paste0("landing date ",
                                                       current_full_trip$.__enclos_env__$private$landing_date,
                                                       " and vessel ",
                                                       current_full_trip$.__enclos_env__$private$vessel_id),
                                                call. = FALSE)
                                        current_full_trip$.__enclos_env__$private$rf1 <- 1
                                      } else {
                                        for (z in 1:length(trip_id)) {
                                          warning("missing elementary landing\ntrip id: ",
                                                  paste0("landing date ",
                                                         current_full_trip[[z]]$.__enclos_env__$private$landing_date,
                                                         " and vessel ",
                                                         current_full_trip[[z]]$.__enclos_env__$private$vessel_id),
                                                  call. = FALSE)
                                          current_full_trip[[z]]$.__enclos_env__$private$rf1 <- 1
                                        }
                                      }
                                    } else {
                                      current_elementarylandings_weight <- vector(mode = "numeric")
                                      for (u in 1:length(current_elementarylandings)) {
                                        if (current_elementarylandings[[u]]$.__enclos_env__$private$specie_code3l %in% species_rf1) {
                                          current_elementarylandings_weight <- append(current_elementarylandings_weight,
                                                                                      current_elementarylandings[[u]]$.__enclos_env__$private$landing_weight)
                                        }
                                      }
                                      current_rf1 <- sum(current_elementarylandings_weight) / sum(current_elementarycatches_weight)
                                      if (length(trip_id) == 1) {
                                        current_full_trip$.__enclos_env__$private$rf1 <- current_rf1
                                      } else {
                                        for (w in 1:length(trip_id)) {
                                          current_full_trip[[w]]$.__enclos_env__$private$rf1 <- current_rf1
                                        }
                                      }
                                    }
                                  }
                                  browser()
                                }
                              }
                            }),
                          private = list(
                            id_not_full_trip = NULL,
                            id_not_full_trip_retained = NULL,
                            data_selected = NULL
                          ))
