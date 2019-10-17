#' @name object_elementarylandings
#' @title R6 object object_elementarylandings creation
#' @description Creation object object_elementarylandings in relation with R6 reference object class elementarylandings
#' @return A R6 reference object with data related to one or several elementary landing(s)
#' @seealso \code{\link{elementarylandings}}
#' @export
object_elementarylandings <- function(...) {
  arguments <- list(...)
  if (nargs() == 0) {
    object_elementarylandings <- t3:::elementarylandings$new()
  } else {
    for (i in 1:nargs()) {
      if (i == 1) {
        object_elementarylandings <- t3:::elementarylandings$new(arguments[[i]])
      } else {
        object_elementarylandings$add(arguments[[i]])
      }
    }
  }
  return(object_elementarylandings)
}
