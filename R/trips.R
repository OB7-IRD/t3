#' @name trips
#' @title R6 class trips
#' @description Create R6 reference object class trips
#' @importFrom R6 R6Class
trips <- R6::R6Class(classname = "trips",
                     inherit = list_t3,
                     public = list(
                       # initialize ----
                       #' @description Initialize function for R6 trips class.
                       #' @param ... (empty, list or R6-trip classes) Nothing, a list of object R6-trip classes or one object R6-trip classes.
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
                                         & ! any(class(x = new_item[[i]]) == "trip"))) {
                                   cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                       " - Error: invalid \"data\" argument, class list or R6-trip expected.\n",
                                       sep = "")
                                   stop()
                                 }
                               }
                               private$data <- append(x = private$data,
                                                      values = arguments[[i]])
                             } else if (length(x = class(x = arguments[[i]])) == 2
                                        && (any(class(x = arguments[[i]]) == "R6")
                                            & any(class(x = arguments[[i]]) == "trip"))) {
                               private$data <- append(x = private$data,
                                                      values = arguments[[i]])
                             } else {
                               cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                   " - Error: invalid \"data\" argument, class list or R6-trip expected.\n",
                                   sep = "")
                               stop()
                             }
                           }
                         }
                       },
                       # add new trip ----
                       #' @description Function for add a new trip in the object trips
                       #' @param new_item (list or R6-trip classes) A list of object R6-trip classes or one object R6-trip classes.
                       add = function(new_item) {
                         if (length(x = class(x = new_item)) == 1
                             && inherits(x = new_item,
                                         what = "list")) {
                           for (i in length(x = new_item)) {
                             if (length(x = class(x = new_item[[i]])) == 2
                                 && (! any(class(x = new_item[[i]]) == "R6")
                                     & ! any(class(x = new_item[[i]]) == "trip"))) {
                               cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                   " - Error: invalid \"data\" argument, class list or R6-trip expected.\n",
                                   sep = "")
                               stop()
                             }
                           }
                           super$add(new_item)
                         } else if (length(x = class(x = new_item)) == 2
                                    && (any(class(x = new_item) == "R6")
                                        & any(class(x = new_item) == "trip"))) {
                           super$add(new_item)
                         } else {
                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                               " - Error: invalid \"data\" argument, class list or R6-trip expected.\n",
                               sep = "")
                           stop()
                         }
                       }
                     ))
