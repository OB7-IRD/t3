#' @name object_r6
#' @title R6 object
#' @description Creation object R6 in relation with R6 reference object class declared in function argument
#' @param class_name (character) Name of the R6 reference class.
#' @param silent {\link[base]{logical}} expected. By default TRUE Display or not information when you run the process.
#' @param ... (empty, list or R6 object) Leave empty if you want to initialize the object or provide one or several item to add in the class.
#' @return A R6 reference object.
#' @importFrom codama r_type_checking
#' @export
object_r6 <- function(class_name,
                      silent = TRUE,
                      ...) {
  # 1 - Arguments verifications ----
  codama::r_type_checking(r_object = class_name,
                          type = "character")
  codama::r_type_checking(r_object = silent,
                          type = "logical",
                          length = 1L)
  # 2 - Process ----
  if (silent != TRUE) {
    message(format(Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            " - Start creation object of class \"",
            class_name,
            "\"")
  }
  arguments <- list(...)
  if (length(x = arguments) == 0) {
    expr <- paste0(class_name,
                   "$new()")
    object_r6 <- eval(parse(text = expr))
  } else {
    for (i in seq_len(length.out = length(arguments))) {
      if (i == 1) {
        expr <- paste0(class_name,
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
  if (silent != TRUE) {
    message(format(Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            " - Successful creation object of class \"",
            class_name,
            "\"")
  }
  return(object_r6)
}
