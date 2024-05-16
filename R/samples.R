#' @name samples
#' @title R6 class samples
#' @description Create R6 reference object class samples
samples <- R6::R6Class(classname = "samples",
                       inherit = list_t3,
                       public = list(
                         # initialize ----
                         #' @description Initialize function for R6 samples class.
                         #' @param ... (empty, list or R6-elementarysample classes) Nothing, a list of object R6-elementarysample classes or one object R6-elementarysample classes.
                         initialize = function(...) {
                           arguments <- list(...)
                           if (nargs() == 0) {
                             super$initialize()
                           } else {
                             for (i in 1:nargs()) {
                               if (length(x = class(x = arguments[[i]])) == 1
                                   && inherits(x = arguments[[i]],
                                               what = "list")) {
                                 for (i in length(arguments[[i]])) {
                                   if (length(x = class(x = arguments[[i]][[i]])) == 2
                                       && (! any(class(x = arguments[[i]][[i]]) == "R6")
                                           & ! any(class(x = new_item[[i]]) == "elementarysample"))) {
                                     stop(format(Sys.time(),
                                                 "%Y-%m-%d %H:%M:%S"),
                                          " - Invalid \"data\" argument, class list or R6-elementarysample expected.")
                                   }
                                 }
                                 private$data <- append(private$data, arguments[[i]])
                               } else if (length(x = class(x = arguments[[i]])) == 2
                                          && (any(class(x = arguments[[i]]) == "R6")
                                              & any(class(x = arguments[[i]]) == "elementarysample"))) {
                                 private$data <- append(x = private$data,
                                                        values = arguments[[i]])
                               } else {
                                 stop(format(Sys.time(),
                                             "%Y-%m-%d %H:%M:%S"),
                                      " - Invalid \"data\" argument, class list or R6-elementarysample expected.")
                               }
                             }
                           }
                         },
                         # add new elementarysample ----
                         #' @description Function for add a new elementarysample in the object samples.
                         #' @param new_item (list or R6-elementarysample classes) A list of object R6-elementarysample classes or one object R6-elementarysample classes.
                         add = function(new_item) {
                           if (inherits(x = new_item,
                                        what = "list")) {
                             class_new_item <- unique(x = sapply(X = seq_len(length.out = length(x = new_item)),
                                                                 FUN = function(new_item_id) {
                                                                   paste(class(x = new_item[[new_item_id]]),
                                                                         collapse = "_")
                                                                 }))
                             if (length(x = class_new_item) != 1
                                 || class_new_item != "elementarysample_R6") {
                               stop(format(x = Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Invalid \"data\" argument, class elementarysample-R6 expected.")
                             } else {
                               super$add(new_item = new_item)
                             }
                           } else {
                             class_new_item <- paste(class(x = new_item),
                                                     collapse = "_")
                             if (class_new_item != "elementarysample_R6") {
                               stop(format(x = Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Invalid \"data\" argument, class elementarysample-R6 expected.")
                             } else {
                               super$add(new_item = new_item)
                             }
                           }
                         },
                         # filter by trip ----
                         #' @description Function for filter elementarysample by trip identification.
                         #' @param trip_id (character) Trip identification.
                         filter_by_trip = function(trip_id) {
                           current_samples <- vector(mode = "list")
                           for (i in seq_len(length.out = length(x = private[["data"]]))) {
                             current_trip_id <- private[["data"]][[i]][[1]]$.__enclos_env__$private$trip_id
                             if (trip_id == current_trip_id) {
                               current_samples <- append(x = current_samples,
                                                         values = list(private[["data"]][[i]]))
                             }
                           }
                           return(current_samples)
                         }))
