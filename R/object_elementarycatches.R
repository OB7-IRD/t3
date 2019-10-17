#' @name object_elementarycatches
#' @title R6 object object_elementarycatches creation
#' @description Creation object object_elementarycatches in relation with R6 reference object class elementarycatches
#' @return A R6 reference object with data related to one or several elementary catches
#' @seealso \code{\link{elementarycatches}}
#' @export
object_elementarycatches <- function(...) {
  arguments <- list(...)
  if (nargs() == 0) {
    object_elementarycatches <- t3:::elementarycatches$new()
  } else {
    for (i in 1:nargs()) {
      if (i == 1) {
        object_elementarycatches <- t3:::elementarycatches$new(arguments[[i]])
      } else {
        object_elementarycatches$add(arguments[[i]])
      }
    }
  }
  return(object_elementarycatches)
}
