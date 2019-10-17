#' @name elementarylandings
#' @title R6 class elementarylandings creation
#' @description Create R6 reference object class elementarylandings
#' @importFrom R6 R6Class
elementarylandings <- R6::R6Class(classname = "elementarylandings",
                                  inherit = t3:::list_t3,
                                  public = list(
                                    initialize = function(...) {
                                      arguments <- list(...)
                                      if (nargs() == 0) {
                                        super$initialize()
                                      } else {
                                        for (i in 1:nargs()) {
                                          if (! any(class(arguments[[i]]) == "R6") | ! any(class(arguments[[i]]) == "elementarylanding")) {
                                            stop("invalid \"data\" argument\nClass R6 and elementarylanding expected")
                                          } else {
                                            private$data <- append(private$data, arguments[[i]])
                                          }
                                        }
                                      }
                                    },
                                    # add new elementary_catch
                                    add = function(new_item) {
                                      if (! any(class(new_item) == "R6") | ! any(class(new_item) == "elementarylanding")) {
                                        stop("invalid \"data\" argument\nClass R6 and elementarylanding expected")
                                      }
                                      super$add(new_item)
                                    }
                                  ))
