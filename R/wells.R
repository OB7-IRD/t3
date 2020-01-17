#' @name wells
#' @title R6 class wells creation
#' @description Create R6 reference object class wells
#' @importFrom R6 R6Class
wells <- R6::R6Class(classname = "wells",
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
                                             if (length(class(arguments[[i]][[i]])) == 2 && (! any(class(arguments[[i]][[i]]) == "R6") & ! any(class(new_item[[i]]) == "well"))) {
                                               cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                   " - Error: invalid \"data\" argument\nClass list or R6-well expected\n",
                                                   sep = "")
                                               stop()
                                             }
                                           }
                                           private$data <- append(private$data, arguments[[i]])
                                         } else if (length(class(arguments[[i]])) == 2 && (any(class(arguments[[i]]) == "R6") & any(class(arguments[[i]]) == "well"))) {
                                           private$data <- append(private$data, arguments[[i]])
                                         } else {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"data\" argument\nClass list or R6-well expected\n",
                                               sep = "")
                                           stop()
                                         }
                                       }
                                     }
                                   },
                                   # add new well ----
                                   add = function(new_item) {
                                     if (length(class(new_item)) == 1 && class(new_item) == "list") {
                                       for (i in length(new_item)) {
                                         if (length(class(new_item[[i]])) == 2 && (! any(class(new_item[[i]]) == "R6") & ! any(class(new_item[[i]]) == "well"))) {
                                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                               " - Error: invalid \"data\" argument\nClass list or R6-well expected\n",
                                               sep = "")
                                           stop()
                                         }
                                       }
                                       super$add(new_item)
                                     } else if (length(class(new_item)) == 2 && (any(class(new_item) == "R6") & any(class(new_item) == "well"))) {
                                       super$add(new_item)
                                     } else {
                                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                           " - Error: invalid \"data\" argument\nClass list or R6-well expected\n",
                                           sep = "")
                                       stop()
                                     }
                                   },
                                   # filter_by_trip ----
                                   filter_by_trip = function(trip_id) {
                                     current_wells <- vector(mode = "list")
                                     for (i in 1:length(private[["data"]])) {
                                       current_trip_id <- private[["data"]][[i]][[1]]$.__enclos_env__$private$trip_id
                                       if (trip_id == current_trip_id) {
                                         current_wells <- append(current_wells,
                                                                 list(private[["data"]][[i]]))
                                       }
                                     }
                                     return(current_wells)
                                   }))
