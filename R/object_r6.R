#' @name object_r6
#' @title R6 object creation
#' @description Creation object R6 in relation with R6 reference object class declared in function argument
#' @param class_name (character) Name of the R6 reference class
#' @param ... (empty, list or R6 object) Leave empty if you want to initialise the object or provide one or several item to add in the class
#' @return A R6 reference object
#' @export
object_r6 <- function(class_name, ...) {
  if (length(class(class_name)) != 1 || class(class_name) != "character") {
    stop("invalid \"class_name\" argument\nclass character expected")
  } else {
    arguments <- list(...)
    if (length(arguments) == 0) {
      expr <- paste0("t3:::",
                     class_name,
                     "$new()")
      object_r6 <- eval(parse(text = expr))
    } else {
      for (i in 1:length(arguments)) {
        if (i == 1) {
          expr <- paste0("t3:::",
                         class_name,
                         "$new(",
                         arguments[[i]],
                         ")")
          object_r6 <- eval(parse(text = expr))
        } else {
          expr <- paste0("object_r6$add(",
                         arguments[[i]],
                         ")")
          eval(parse(text = expr))
        }
      }
    }
  }
  return(object_r6)
}
