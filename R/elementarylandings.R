#' @name elementarylandings
#' @title R6 class elementarylandings creation
#' @description Create R6 reference object class elementarylandings
#' @importFrom R6 R6Class
elementarylandings <- R6::R6Class(classname = "elementarylandings",
                                  inherit = t3:::list_t3,
                                  public = list(
                                    initialize = function(...) {
                                      arguments <- list(...)
                                      if (nargs() == 0) {
                                        super$initialize()
                                      } else {
                                        for (i in 1:nargs()) {
                                          if (length(class(arguments[[i]])) == 1 && class(arguments[[i]]) == "list") {
                                            for (i in length(arguments[[i]])) {
                                              if (length(class(arguments[[i]][[i]])) == 2 && (! any(class(arguments[[i]][[i]]) == "R6") & ! any(class(new_item[[i]]) == "elementarylanding"))) {
                                                stop("invalid \"data\" argument\nClass list or R6-elementarylanding expected")
                                              }
                                            }
                                            private$data <- append(private$data, arguments[[i]])
                                          } else if (length(class(arguments[[i]])) == 2 && (any(class(arguments[[i]]) == "R6") & any(class(arguments[[i]]) == "elementarylanding"))) {
                                            private$data <- append(private$data, arguments[[i]])
                                          } else {
                                            stop("invalid \"data\" argument\nClass list or R6-elementarylanding expected")
                                          }
                                        }
                                      }
                                    },
                                    # add new elementarylanding
                                    add = function(new_item) {
                                      if (length(class(new_item)) == 1 && class(new_item) == "list") {
                                        for (i in length(new_item)) {
                                          if (length(class(new_item[[i]])) == 2 && (! any(class(new_item[[i]]) == "R6") & ! any(class(new_item[[i]]) == "elementarylanding"))) {
                                            stop("invalid \"data\" argument\nClass list or R6-elementarylanding expected")
                                          }
                                        }
                                        super$add(new_item)
                                      } else if (length(class(new_item)) == 2 && (any(class(new_item) == "R6") & any(class(new_item) == "elementarylanding"))) {
                                        super$add(new_item)
                                      } else {
                                        stop("invalid \"data\" argument\nClass list or R6-elementarylanding expected")
                                      }
                                    },
                                    # Filter elementarylanding by trips selected
                                    filter_by_trips = function(trips_selected) {
                                      if (length(class(trips_selected)) != 1 || class(trips_selected) != "character") {
                                        stop("invalid \"trips_selected\" argument")
                                      } else {
                                        data_selected <- list()
                                        for (trip in trips_selected) {
                                          tmp1 <- list(super$filter(attribut_l1 = "data",
                                                                    filter = paste0("arg$trip_id == \"",
                                                                                    trip,
                                                                                    "\"")))
                                          names(tmp1) <- trip
                                          data_selected <- append(data_selected, tmp1)
                                        }
                                        private$data_selected <- data_selected
                                      }
                                    }))
