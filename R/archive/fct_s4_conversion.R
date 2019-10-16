#' @name fct_s4_conversion
#' @title Conversion S4 object into a list or a data frame
#' @param object R object in S4 format.
#' @param type Type of output expected. You can choose between a "list" or a "data.frame". For a data frame, all attributs of the S4 object need to have the same length. By default, the output is a list.
#' @description Conversion of a S4 object into a list or a data frame.
#' @references \url{https://github.com/OB7-IRD/t3}
#' @details To do after dude !
#' @export
fct_s4_conversion <- function(object, type = "list") {
  # Arguments' checking ----
  if (mode(object) != "S4") {
    stop("Argument \"object\" is not a S4 object")
  }
  if (! type %in% c("list", "dataframe")) {
    stop("Argument \"type\" is not correct\n Please select between \"list\" and \"dataframe\"")
  }
  # Core function ----
  tmp <-vector("list", length(slotNames(object)))
  names(tmp) <- slotNames(object)
  for (a in slotNames(object)) {
    tmp[[a]] <- slot(object, a)
  }
  if (type == "dataframe") {
    if (length(unique(lengths(tmp))) != 1 || unique(lengths(tmp)) == 0) {
      warnings("Object's arguments have different number of rows or object(s) is empty\n The function return only a list")
    } else {
      tmp <- as.data.frame(tmp)
    }
  }
  return(tmp)
}
