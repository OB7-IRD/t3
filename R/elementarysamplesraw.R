#' @name elementarysamplesraw
#' @title R6 class elementarysamplesraw
#' @description Create R6 reference object class elementarysamplesraw
#' @importFrom R6 R6Class
elementarysamplesraw <- R6::R6Class(classname = "elementarysamplesraw",
                                    inherit = list_t3,
                                    public = list(
                                      # initialize ----
                                      #' @description Initialize function for R6 elementarysamplesraw class.
                                      #' @param ... (empty, list or R6-elementarysampleraw classes) Nothing, a list of object R6-elementarysampleraw classes or one object R6-elementarysampleraw classes.
                                      initialize = function(...) {
                                        arguments <- list(...)
                                        if (nargs() == 0) {
                                          super$initialize()
                                        } else {
                                          for (i in 1:nargs()) {
                                            if (length(x = class(arguments[[i]])) == 1
                                                && inherits(x = arguments[[i]],
                                                            what = "list")) {
                                              for (i in length(arguments[[i]])) {
                                                if (length(x = class(x = arguments[[i]][[i]])) == 2
                                                    && (! any(class(arguments[[i]][[i]]) == "R6")
                                                        & ! any(class(new_item[[i]]) == "elementarysampleraw"))) {
                                                  stop(format(Sys.time(),
                                                              "%Y-%m-%d %H:%M:%S"),
                                                      " - Invalid \"data\" argument, class list or R6-elementarysampleraw expected.")
                                                }
                                              }
                                              private$data <- append(private$data, arguments[[i]])
                                            } else if (length(x = class(x = arguments[[i]])) == 2
                                                       && (any(class(x = arguments[[i]]) == "R6")
                                                           & any(class(x = arguments[[i]]) == "elementarysampleraw"))) {
                                              private$data <- append(x = private$data,
                                                                     values = arguments[[i]])
                                            } else {
                                              stop(format(Sys.time(),
                                                         "%Y-%m-%d %H:%M:%S"),
                                                  " - Invalid \"data\" argument, class list or R6-elementarysampleraw expected.")
                                            }
                                          }
                                        }
                                      },
                                      # add new elementarysampleraw ----
                                      #' @description Function for add a new elementarysampleraw in the object elementarysamplesraw.
                                      #' @param new_item (list or R6-elementarysampleraw classes) A list of object R6-elementarysampleraw classes or one object R6-elementarysampleraw classes.
                                      add = function(new_item) {
                                        if (length(x = class(x = new_item)) == 1
                                            && inherits(x = new_item,
                                                        what = "list")) {
                                          for (i in length(x = new_item)) {
                                            if (length(x = class(x = new_item[[i]])) == 2
                                                && (! any(class(x = new_item[[i]]) == "R6")
                                                    & ! any(class(x = new_item[[i]]) == "elementarysampleraw"))) {
                                              stop(format(Sys.time(),
                                                          "%Y-%m-%d %H:%M:%S"),
                                                  " - Invalid \"data\" argument, class list or R6-elementarysampleraw expected.")
                                            }
                                          }
                                          super$add(new_item)
                                        } else if (length(x = class(x = new_item)) == 2
                                                   && (any(... = class(x = new_item) == "R6")
                                                       & any(class(x = new_item) == "elementarysampleraw"))) {
                                          super$add(new_item)
                                        } else {
                                          stop(format(Sys.time(),
                                                      "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"data\" argument, class list or R6-elementarysampleraw expected.")
                                        }
                                      },
                                      # filter by trip ----
                                      #' @description Function for filter elementarysamplesraw by trip identification.
                                      #' @param trip_id (character) Trip identification.
                                      filter_by_trip = function(trip_id) {
                                        current_elementarysamplesraw <- vector(mode = "list")
                                        for (i in seq_len(length.out = length(x = private[["data"]]))) {
                                          current_trip_id <- private[["data"]][[i]]$.__enclos_env__$private$trip_id
                                          if (trip_id == current_trip_id) {
                                            current_elementarysamplesraw <- append(x = current_elementarysamplesraw,
                                                                                 values = list(private[["data"]][[i]]))
                                          }
                                        }
                                        return(current_elementarysamplesraw)
                                      }))
