#' @name object_trips
#' @title R6 object trips creation
#' @description Creation object trips in relation with R6 reference object class trips
#' @param ... (empty or class R6 and trip) Leave empty if you want to initialise the object or provide at least one R6 reference object of class trip
#' @return A R6 reference object with data of one or more trips
#' @seealso \code{\link{trips}}
#' @export
object_trips <- function(...) {
  arguments <- list(...)
  if (nargs() == 0) {
    object_trips <- t3:::trips$new()
  } else {
    for (i in 1:nargs()) {
      if (i == 1) {
        object_trips <- t3:::trips$new(arguments[[i]])
      } else {
        object_trips$add(arguments[[i]])
      }
    }
  }
  return(object_trips)
}
