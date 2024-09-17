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
                                   stop(format(Sys.time(),
                                               "%Y-%m-%d %H:%M:%S"),
                                        " - Invalid \"data\" argument, class list or R6-trip expected.")
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
                               stop(format(Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Invalid \"data\" argument, class list or R6-trip expected.")
                             }
                           }
                         }
                       },
                       # add new trip ----
                       #' @description Function for add a new trip in the object trips
                       #' @param new_item (list or R6-trip classes) A list of object R6-trip classes or one object R6-trip classes.
                       add = function(new_item) {
                         if (inherits(x = new_item,
                                      what = "list")) {
                           class_new_item <- unique(x = sapply(X = seq_len(length.out = length(x = new_item)),
                                                               FUN = function(new_item_id) {
                                                                 paste(class(x = new_item[[new_item_id]]),
                                                                       collapse = "_")
                                                               }))
                           if (length(x = class_new_item) != 1
                               || class_new_item != "trip_R6") {
                             stop(format(x = Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - Invalid \"data\" argument, class trip-R6 expected.")
                           } else {
                             super$add(new_item = new_item)
                           }
                         } else {
                           class_new_item <- paste(class(x = new_item),
                                                   collapse = "_")
                           if (class_new_item != "trip_R6") {
                             stop(format(x = Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                                  " - Invalid \"data\" argument, class trip-R6 expected.")
                           } else {
                             super$add(new_item = new_item)
                           }
                         }
                       }
                     ))
