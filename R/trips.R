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
                             if (length(class(arguments[[i]])) == 1 && class(arguments[[i]]) == "list") {
                               for (i in length(arguments[[i]])) {
                                 if (length(class(arguments[[i]][[i]])) == 2 && (! any(class(arguments[[i]][[i]]) == "R6") & ! any(class(new_item[[i]]) == "trip"))) {
                                   cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                       " - Error: invalid \"data\" argument, class list or R6-trip expected.\n",
                                       sep = "")
                                   stop()
                                 }
                               }
                               private$data <- append(private$data, arguments[[i]])
                             } else if (length(class(arguments[[i]])) == 2 && (any(class(arguments[[i]]) == "R6") & any(class(arguments[[i]]) == "trip"))) {
                               private$data <- append(private$data, arguments[[i]])
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
                         if (length(class(new_item)) == 1 && class(new_item) == "list") {
                           for (i in length(new_item)) {
                             if (length(class(new_item[[i]])) == 2 && (! any(class(new_item[[i]]) == "R6") & ! any(class(new_item[[i]]) == "trip"))) {
                               cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                   " - Error: invalid \"data\" argument, class list or R6-trip expected.\n",
                                   sep = "")
                               stop()
                             }
                           }
                           super$add(new_item)
                         } else if (length(class(new_item)) == 2 && (any(class(new_item) == "R6") & any(class(new_item) == "trip"))) {
                           super$add(new_item)
                         } else {
                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                               " - Error: invalid \"data\" argument, class list or R6-trip expected.\n",
                               sep = "")
                           stop()
                         }
                       }
                     ))
