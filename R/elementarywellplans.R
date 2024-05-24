#' @name elementarywellplans
#' @title R6 class elementarywellplans
#' @description Create R6 reference object class elementarywellplans
elementarywellplans <- R6::R6Class(classname = "elementarywellplans",
                                   inherit = list_t3,
                                   public = list(
                                     # initialize ----
                                     #' @description Initialize function for R6 elementarywellplans class.
                                     #' @param ... (empty, list or R6-elementarywellplan classes) Nothing, a list of object R6-elementarywellplan classes or one object R6-elementarywellplan classes.
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
                                                       & ! any(class(x = new_item[[i]]) == "elementarywellplan"))) {
                                                 stop(format(Sys.time(),
                                                             "%Y-%m-%d %H:%M:%S"),
                                                      " - Invalid \"data\" argument, class list or R6-elementarywellplan expected.")
                                               }
                                             }
                                             private$data <- append(private$data, arguments[[i]])
                                           } else if (length(x = class(x = arguments[[i]])) == 2
                                                      && (any(class(x = arguments[[i]]) == "R6")
                                                          & any(class(x = arguments[[i]]) == "elementarywellplan"))) {
                                             private$data <- append(x = private$data,
                                                                    values = arguments[[i]])
                                           } else {
                                             stop(format(Sys.time(),
                                                         "%Y-%m-%d %H:%M:%S"),
                                                  " - Invalid \"data\" argument, class list or R6-elementarywellplan expected.")
                                           }
                                         }
                                       }
                                     },
                                     # add new elementarywellplan ----
                                     #' @description Function for add a new elementarywellplan in the object elementarywellplans.
                                     #' @param new_item (list or R6-elementarywellplan classes) A list of object R6-elementarywellplan classes or one object R6-elementarywellplan classes.
                                     add = function(new_item) {
                                       if (inherits(x = new_item,
                                                    what = "list")) {
                                         class_new_item <- unique(x = sapply(X = seq_len(length.out = length(x = new_item)),
                                                                             FUN = function(new_item_id) {
                                                                               paste(class(x = new_item[[new_item_id]]),
                                                                                     collapse = "_")
                                                                             }))
                                         if (length(x = class_new_item) != 1
                                             || class_new_item != "elementarywellplan_R6") {
                                           stop(format(x = Sys.time(),
                                                       "%Y-%m-%d %H:%M:%S"),
                                                " - Invalid \"data\" argument, class elementarywellplan-R6 expected.")
                                         } else {
                                           super$add(new_item = new_item)
                                         }
                                       } else {
                                         class_new_item <- paste(class(x = new_item),
                                                                 collapse = "_")
                                         if (class_new_item != "elementarywellplan_R6") {
                                           stop(format(x = Sys.time(),
                                                       "%Y-%m-%d %H:%M:%S"),
                                                " - Invalid \"data\" argument, class elementarywellplan-R6 expected.")
                                         } else {
                                           super$add(new_item = new_item)
                                         }
                                       }
                                     },
                                     # filter by trip ----
                                     #' @description Function for filter elementarywellplans by trip identification.
                                     #' @param trip_id (character) Trip identification.
                                     filter_by_trip = function(trip_id) {
                                       current_elementarywellplans <- vector(mode = "list")
                                       for (i in seq_len(length(x = private[["data"]]))) {
                                         current_trip_id <- private[["data"]][[i]]$.__enclos_env__$private$trip_id
                                         if (trip_id == current_trip_id) {
                                           current_elementarylandings <- append(x = current_elementarywellplans,
                                                                                values = list(private[["data"]][[i]]))
                                         }
                                       }
                                       return(current_elementarywellplans)
                                     }))
