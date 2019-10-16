#' @name trips
#' @title R6 class trips creation
#' @description Create R6 reference object class trips
#' @importFrom R6 R6Class
# trips ----
trips <- R6::R6Class(classname = "trips",
                     inherit = t3:::list_t3,
                     public = list(
                       initialize = function(...) {
                         arguments <- list(...)
                         if (nargs() == 0) {
                           super$initialize()
                         } else {
                           for (i in 1:nargs()) {
                             if (! any(class(arguments[[i]]) == "R6") | ! any(class(arguments[[i]]) == "trip")) {
                               stop("invalid \"data\" argument\nClass R6 and trip expected")
                             } else {
                               private$data <- append(private$data, arguments[[i]])
                             }
                           }
                         }
                       },
                       # add new trip
                       add = function(new_item) {
                         if (! any(class(new_item) == "R6") | ! any(class(new_item) == "trip")) {
                           stop("invalid \"data\" argument\nClass R6 and trip expected")
                         }
                         super$add(new_item)
                       }
                     ))
