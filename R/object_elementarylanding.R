#' @name object_elementarylanding
#' @title R6 object object_elementarylanding creation
#' @description Creation object object_elementarylanding in relation with R6 reference object class elementarylanding
#' @return A R6 reference object with data related to one elementary landing
#' @seealso \code{\link{elementarylanding}}
#' @export
object_elementarylanding <- function(trip_id,
                                     landing_date,
                                     vessel_id,
                                     landing_category,
                                     specie_code3l,
                                     landing_weight) {
  t3:::elementarylanding$new(trip_id = trip_id,
                             landing_date = landing_date,
                             vessel_id = vessel_id,
                             landing_category = landing_category,
                             specie_code3l = specie_code3l,
                             landing_weight = landing_weight)
}
