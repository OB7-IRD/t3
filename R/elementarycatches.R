#' @name elementarycatches
#' @title R6 class elementarycatches
#' @description Create R6 reference object class elementarycatches
elementarycatches <- R6::R6Class(classname = "elementarycatches",
                                 inherit = list_t3,
                                 public = list(
                                   # initialize ----
                                   #' @description Initialize function for R6 elementarycatches class.
                                   #' @param ... (empty, list or R6-elementarycatch classes) Nothing, a list of object R6-elementarycatch classes or one object R6-elementarycatch classes.
                                   initialize = function(...) {
                                     arguments <- list(...)
                                     if (nargs() == 0) {
                                       super$initialize()
                                     } else {
                                       for (i in 1:nargs()) {
                                         if (length(x = class(x = arguments[[i]])) == 1
                                             && inherits(arguments[[i]],
                                                         what = "list")) {
                                           for (i in length(arguments[[i]])) {
                                             if (length(class(arguments[[i]][[i]])) == 2 && (! any(class(arguments[[i]][[i]]) == "R6") & ! any(class(new_item[[i]]) == "elementarycatch"))) {
                                               stop(format(Sys.time(),
                                                           "%Y-%m-%d %H:%M:%S"),
                                                    " - Invalid \"data\" argument, class list or R6-elementarycatch expected.")
                                             }
                                           }
                                           private$data <- append(private$data, arguments[[i]])
                                         } else if (length(class(arguments[[i]])) == 2 && (any(class(arguments[[i]]) == "R6") & any(class(arguments[[i]]) == "elementarycatch"))) {
                                           private$data <- append(private$data, arguments[[i]])
                                         } else {
                                           stop(format(Sys.time(),
                                                       "%Y-%m-%d %H:%M:%S"),
                                                " - Invalid \"data\" argument, class list or R6-elementarycatch expected.")
                                         }
                                       }
                                     }
                                   },
                                   # add new elementarycatch ----
                                   #' @description Function for add a new elementarycatch in the object elementarycatches.
                                   #' @param new_item (list or R6-elementarycatch classes) A list of object R6-elementarycatch classes or one object R6-elementarycatch classes.
                                   add = function(new_item) {
                                     if (inherits(x = new_item,
                                                  what = "list")) {
                                       class_new_item <- unique(x = sapply(X = seq_len(length.out = length(x = new_item)),
                                                                           FUN = function(new_item_id) {
                                                                             paste(class(x = new_item[[new_item_id]]),
                                                                                   collapse = "_")
                                                                           }))
                                       if (length(x = class_new_item) != 1
                                           || class_new_item != "elementarycatch_R6") {
                                         stop(format(x = Sys.time(),
                                                     "%Y-%m-%d %H:%M:%S"),
                                              " - Invalid \"data\" argument, class elementarycatch-R6 expected.")
                                       } else {
                                         super$add(new_item = new_item)
                                       }
                                     } else {
                                       class_new_item <- paste(class(x = new_item),
                                                               collapse = "_")
                                       if (class_new_item != "elementarycatch_R6") {
                                         stop(format(x = Sys.time(),
                                                     "%Y-%m-%d %H:%M:%S"),
                                              " - Error: invalid \"data\" argument, class elementarycatch-R6 expected.")
                                       } else {
                                         super$add(new_item = new_item)
                                       }
                                     }
                                   },
                                   # filter by activity ----
                                   #' @description Function for filter elementarycatches by activity identification.
                                   #' @param activity_id (character) Activity identification.
                                   filter_by_activity = function(activity_id) {
                                     current_elementarycatches <- vector(mode = "list")
                                     for (i in seq_len(length(private[["data"]]))) {
                                       current_activity_id <- private[["data"]][[i]]$.__enclos_env__$private$activity_id
                                       if (activity_id == current_activity_id) {
                                         current_elementarycatches <- append(current_elementarycatches,
                                                                             list(private[["data"]][[i]]))
                                       }
                                     }
                                     return(current_elementarycatches)
                                   }))
