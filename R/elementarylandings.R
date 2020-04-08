#' @name elementarylandings
#' @title R6 class elementarylandings creation
#' @description Create R6 reference object class elementarylandings
#' @importFrom R6 R6Class
elementarylandings <- R6::R6Class(classname = "elementarylandings",
                                  inherit = t3:::list_t3,
                                  public = list(
                                    # initialize ----
                                    #' @description Initialize function for R6 elementarylandings class.
                                    #' @param ... (empty, list or R6-elementarylanding classes) Nothing, a list of object R6-elementarylanding classes or one object R6-elementarylanding classes.
                                    initialize = function(...) {
                                      arguments <- list(...)
                                      if (nargs() == 0) {
                                        super$initialize()
                                      } else {
                                        for (i in 1:nargs()) {
                                          if (length(class(arguments[[i]])) == 1 && class(arguments[[i]]) == "list") {
                                            for (i in length(arguments[[i]])) {
                                              if (length(class(arguments[[i]][[i]])) == 2 && (! any(class(arguments[[i]][[i]]) == "R6") & ! any(class(new_item[[i]]) == "elementarylanding"))) {
                                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                    " - Error: invalid \"data\" argument, class list or R6-elementarylanding expected.\n",
                                                    sep = "")
                                                stop()
                                              }
                                            }
                                            private$data <- append(private$data, arguments[[i]])
                                          } else if (length(class(arguments[[i]])) == 2 && (any(class(arguments[[i]]) == "R6") & any(class(arguments[[i]]) == "elementarylanding"))) {
                                            private$data <- append(private$data, arguments[[i]])
                                          } else {
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Error: invalid \"data\" argument, class list or R6-elementarylanding expected.\n",
                                                sep = "")
                                            stop()
                                          }
                                        }
                                      }
                                    },
                                    # add new elementarylanding ----
                                    #' @description Function for add a new elementarylanding in the object elementarylandings.
                                    #' @param new_item (list or R6-elementarylanding classes) A list of object R6-elementarylanding classes or one object R6-elementarylanding classes.
                                    add = function(new_item) {
                                      if (length(class(new_item)) == 1 && class(new_item) == "list") {
                                        for (i in length(new_item)) {
                                          if (length(class(new_item[[i]])) == 2 && (! any(class(new_item[[i]]) == "R6") & ! any(class(new_item[[i]]) == "elementarylanding"))) {
                                            cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                " - Error: invalid \"data\" argument, class list or R6-elementarylanding expected.\n",
                                                sep = "")
                                            stop()
                                          }
                                        }
                                        super$add(new_item)
                                      } else if (length(class(new_item)) == 2 && (any(class(new_item) == "R6") & any(class(new_item) == "elementarylanding"))) {
                                        super$add(new_item)
                                      } else {
                                        cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                            " - Error: invalid \"data\" argument, class list or R6-elementarylanding expected.\n",
                                            sep = "")
                                        stop()
                                      }
                                    },
                                    # filter by trip ----
                                    #' @description Function for filter elementarylandings by trip identification.
                                    #' @param trip_id (character) Trip identification.
                                    filter_by_trip = function(trip_id) {
                                      current_elementarylandings <- vector(mode = "list")
                                      for (i in seq_len(length(private[["data"]]))) {
                                        current_trip_id <- private[["data"]][[i]]$.__enclos_env__$private$trip_id
                                        if (trip_id == current_trip_id) {
                                          current_elementarylandings <- append(current_elementarylandings,
                                                                               list(private[["data"]][[i]]))
                                        }
                                      }
                                      return(current_elementarylandings)
                                    }))
