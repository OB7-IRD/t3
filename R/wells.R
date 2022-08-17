#' @name wells
#' @title R6 class wells
#' @description Create R6 reference object class wells
#' @importFrom R6 R6Class
wells <- R6::R6Class(classname = "wells",
                                 inherit = list_t3,
                                 public = list(
                                   # initialize ----
                                   #' @description Initialize function for R6 wells class.
                                   #' @param ... (empty, list or R6-well classes) Nothing, a list of object R6-well classes or one object R6-well classes.
                                   initialize = function(...) {
                                     arguments <- list(...)
                                     if (nargs() == 0) {
                                       super$initialize()
                                     } else {
                                       for (i in 1:nargs()) {
                                         if (length(x = class(x = arguments[[i]])) == 1
                                             && inherits(x = arguments[[i]],
                                                         what = "list")) {
                                           for (i in length(x = arguments[[i]])) {
                                             if (length(x = class(x = arguments[[i]][[i]])) == 2
                                                 && (! any(class(x = arguments[[i]][[i]]) == "R6")
                                                     & ! any(class(x = new_item[[i]]) == "well"))) {
                                               cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                   " - Error: invalid \"data\" argument, class list or R6-well expected.\n",
                                                   sep = "")
                                               stop()
                                             }
                                           }
                                           private$data <- append(x = private$data,
                                                                  values = arguments[[i]])
                                         } else if (length(x = class(x = arguments[[i]])) == 2
                                                    && (any(class(x = arguments[[i]]) == "R6")
                                                        & any(class(x = arguments[[i]]) == "well"))) {
                                           private$data <- append(x = private$data,
                                                                  values = arguments[[i]])
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"data\" argument, class list or R6-well expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       }
                                     }
                                   },
                                   # add new well ----
                                   #' @description Function for add a new well in the object wells.
                                   #' @param new_item (list or R6-well classes) A list of object R6-well classes or one object R6-well classes.
                                   add = function(new_item) {
                                     if (length(x = class(x = new_item)) == 1
                                         && inherits(x = new_item,
                                                     what = "list")) {
                                       for (i in length(x = new_item)) {
                                         if (length(x = class(x = new_item[[i]])) == 2
                                             && (! any(class(x = new_item[[i]]) == "R6")
                                                 & ! any(class(x = new_item[[i]]) == "well"))) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"data\" argument, class list or R6-well expected.\n",
                                               sep = "")
                                           stop()
                                         }
                                       }
                                       super$add(new_item)
                                     } else if (length(x = class(x = new_item)) == 2
                                                && (any(class(x = new_item) == "R6")
                                                    & any(class(x = new_item) == "well"))) {
                                       super$add(new_item)
                                     } else {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data\" argument, class list or R6-well expected.\n",
                                           sep = "")
                                       stop()
                                     }
                                   },
                                   # filter by trip ----
                                   #' @description Function for filter well by trip identification.
                                   #' @param trip_id (character) Trip identification.
                                   filter_by_trip = function(trip_id) {
                                     current_wells <- vector(mode = "list")
                                     for (i in seq_len(length.out = length(private[["data"]]))) {
                                       current_trip_id <- private[["data"]][[i]]$.__enclos_env__$private$trip_id
                                       if (trip_id == current_trip_id) {
                                         current_wells <- append(x = current_wells,
                                                                 values = list(private[["data"]][[i]]))
                                       }
                                     }
                                     return(current_wells)
                                   }))
