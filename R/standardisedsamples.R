#' @name standardisedsamples
#' @title R6 class standardisedsamples
#' @description Create R6 reference object class standardisedsamples
#' @importFrom R6 R6Class
standardisedsamples <- R6::R6Class(classname = "standardisedsamples",
                                   inherit = list_t3,
                                   public = list(
                                     # initialize ----
                                     #' @description Initialize function for R6 standardisedsamples class.
                                     #' @param ... (empty, list or R6-standardisedsample classes) Nothing, a list of object R6-standardisedsample classes or one object R6-standardisedsample classes.
                                     initialize = function(...) {
                                       arguments <- list(...)
                                       if (nargs() == 0) {
                                         super$initialize()
                                       } else {
                                         for (i in 1:nargs()) {
                                           if (length(class(arguments[[i]])) == 1
                                               && inherits(arguments[[i]],
                                                           what = "list")) {
                                             for (i in length(arguments[[i]])) {
                                               if (length(class(arguments[[i]][[i]])) == 2
                                                   && (! any(class(arguments[[i]][[i]]) == "R6")
                                                       & ! any(class(new_item[[i]]) == "standardisedsample"))) {
                                                 stop(format(Sys.time(),
                                                             "%Y-%m-%d %H:%M:%S"),
                                                      " - Invalid \"data\" argument, class list or R6-standardisedsample expected.")
                                               }
                                             }
                                             private$data <- append(private$data, arguments[[i]])
                                           } else if (length(class(arguments[[i]])) == 2
                                                      && (any(class(arguments[[i]]) == "R6")
                                                          & any(class(arguments[[i]]) == "standardisedsample"))) {
                                             private$data <- append(private$data,
                                                                    arguments[[i]])
                                           } else {
                                             stop(format(Sys.time(),
                                                         "%Y-%m-%d %H:%M:%S"),
                                                  " - Invalid \"data\" argument, class list or R6-standardisedsample expected.")
                                           }
                                         }
                                       }
                                     },
                                     # add new standardisedsample ----
                                     #' @description Function for add a new standardisedsample in the object standardisedsamples.
                                     #' @param new_item (list or R6-standardisedsample classes) A list of object R6-standardisedsample classes or one object R6-standardisedsample classes.
                                     add = function(new_item) {
                                       if (inherits(x = new_item,
                                                    what = "list")) {
                                         class_new_item <- unique(x = sapply(X = seq_len(length.out = length(x = new_item)),
                                                                             FUN = function(new_item_id) {
                                                                               paste(class(x = new_item[[new_item_id]]),
                                                                                     collapse = "_")
                                                                             }))
                                         if (length(x = class_new_item) != 1
                                             || class_new_item != "standardisedsample_R6") {
                                           stop(format(x = Sys.time(),
                                                       "%Y-%m-%d %H:%M:%S"),
                                                " - Invalid \"data\" argument, class standardisedsample-R6 expected.")
                                         } else {
                                           super$add(new_item = new_item)
                                         }
                                       } else {
                                         class_new_item <- paste(class(x = new_item),
                                                                 collapse = "_")
                                         if (class_new_item != "standardisedsample_R6") {
                                           stop(format(x = Sys.time(),
                                                       "%Y-%m-%d %H:%M:%S"),
                                                " - Invalid \"data\" argument, class standardisedsample-R6 expected.")
                                         } else {
                                           super$add(new_item = new_item)
                                         }
                                       }
                                     },
                                     # filter by trip ----
                                     #' @description Function for filter standardisedsamples by trip identification.
                                     #' @param trip_id (character) Trip identification.
                                     filter_by_trip = function(trip_id) {
                                       current_standardisedsamples <- vector(mode = "list")
                                       for (i in seq_len(length(private[["data"]]))) {
                                         current_trip_id <- private[["data"]][[i]]$.__enclos_env__$private$trip_id
                                         if (trip_id == current_trip_id) {
                                           current_elementarylandings <- append(current_standardisedsamples,
                                                                                list(private[["data"]][[i]]))
                                         }
                                       }
                                       return(current_standardisedsamples)
                                     }))
