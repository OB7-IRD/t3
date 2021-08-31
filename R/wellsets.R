#' @name wellsets
#' @title R6 class wellsets
#' @description Create R6 reference object class wellsets
#' @importFrom R6 R6Class
wellsets <- R6::R6Class(classname = "wellsets",
                        inherit = t3:::list_t3,
                        public = list(
                          # initialize ----
                          #' @description Initialize function for R6 wellsets class.
                          #' @param ... (empty, list or R6-wellset classes) Nothing, a list of object R6-wellset classes or one object R6-wellset classes.
                          initialize = function(...) {
                            arguments <- list(...)
                            if (nargs() == 0) {
                              super$initialize()
                            } else {
                              for (i in 1:nargs()) {
                                if (length(class(arguments[[i]])) == 1
                                    && class(arguments[[i]]) == "list") {
                                  for (i in length(arguments[[i]])) {
                                    if (length(class(arguments[[i]][[i]])) == 2
                                        && (! any(class(arguments[[i]][[i]]) == "R6")
                                            & ! any(class(new_item[[i]]) == "wellset"))) {
                                      cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                          " - Error: invalid \"data\" argument, class list or R6-wellset expected.\n",
                                          sep = "")
                                      stop()
                                    }
                                  }
                                  private$data <- append(private$data, arguments[[i]])
                                } else if (length(class(arguments[[i]])) == 2
                                           && (any(class(arguments[[i]]) == "R6")
                                               & any(class(arguments[[i]]) == "wellset"))) {
                                  private$data <- append(private$data,
                                                         arguments[[i]])
                                } else {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Error: invalid \"data\" argument, class list or R6-wellset expected.\n",
                                      sep = "")
                                  stop()
                                }
                              }
                            }
                          },
                          # add new wellset ----
                          #' @description Function for add a new wellset in the object wellsets.
                          #' @param new_item (list or R6-wellset classes) A list of object R6-wellset classes or one object R6-wellset classes.
                          add = function(new_item) {
                            if (length(class(new_item)) == 1
                                && class(new_item) == "list") {
                              for (i in length(new_item)) {
                                if (length(class(new_item[[i]])) == 2
                                    && (! any(class(new_item[[i]]) == "R6")
                                        & ! any(class(new_item[[i]]) == "wellset"))) {
                                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      " - Error: invalid \"data\" argument, class list or R6-wellset expected.\n",
                                      sep = "")
                                  stop()
                                }
                              }
                              super$add(new_item)
                            } else if (length(class(new_item)) == 2
                                       && (any(class(new_item) == "R6")
                                           & any(class(new_item) == "wellset"))) {
                              super$add(new_item)
                            } else {
                              cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  " - Error: invalid \"data\" argument, class list or R6-wellset expected.\n",
                                  sep = "")
                              stop()
                            }
                          },
                          # filter by trip ----
                          #' @description Function for filter wellsets by trip identification.
                          #' @param trip_id (character) Trip identification.
                          filter_by_trip = function(trip_id) {
                            current_wellsets <- vector(mode = "list")
                            for (i in seq_len(length(private[["data"]]))) {
                              current_trip_id <- private[["data"]][[i]]$.__enclos_env__$private$trip_id
                              if (trip_id == current_trip_id) {
                                current_elementarylandings <- append(current_wellsets,
                                                                     list(private[["data"]][[i]]))
                              }
                            }
                            return(current_wellsets)
                          }))
