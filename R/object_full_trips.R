#' @name object_full_trips
#' @title R6 object full trips creation
#' @description Creation object full trips in relation with R6 reference object class full_trips
#' @param object_trips (class R6 and trips) A R6 reference object of class trips
#' @return A R6 reference object with data of trips grouped by full trips
#' @seealso \code{\link{full_trips}}
#' @export
object_full_trips <- function(object_trips) {
  object_full_trips <- t3:::full_trips$new()
  object_full_trips$create(object_trips)
  return(object_full_trips)
}
