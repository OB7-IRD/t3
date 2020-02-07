#' @name object_elementarywellplan
#' @title R6 object object_elementarywellplan creation
#' @description Creation object object_elementarywellplan in relation with R6 reference object class elementarywellplan
#' @return A R6 reference object with data related to one item in the well plan
#' @seealso \code{\link{elementarywellplan}}
#' @export
object_elementarywellplan <- function(wellplan_id,
                                      well_id,
                                      activity_id,
                                      specie_code3l,
                                      wellplan_weight,
                                      wellplan_number,
                                      wellplan_weigth_category_code,
                                      wellplan_weigth_category_label) {
  t3:::elementarywellplan$new(wellplan_id = wellplan_id,
                              well_id = well_id,
                              activity_id = activity_id,
                              specie_code3l = specie_code3l,
                              wellplan_weight = wellplan_weight,
                              wellplan_number = wellplan_number,
                              wellplan_weigth_category_code = wellplan_weigth_category_code,
                              wellplan_weigth_category_label = wellplan_weigth_category_label)
}
