#' @name object_well
#' @title R6 object object_well creation
#' @description Creation object object_well in relation with R6 reference object class well
#' @return A R6 reference object with data related to one well
#' @seealso \code{\link{well}}
#' @export
object_well <- function(well_id,
                        well_minus10_weigth,
                        well_plus10_weigth,
                        well_global_weigth) {
  t3:::well$new(well_id = well_id,
                well_minus10_weigth = well_minus10_weigth,
                well_plus10_weigth = well_plus10_weigth,
                well_global_weigth = well_global_weigth)
}
