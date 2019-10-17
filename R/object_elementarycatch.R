#' @name object_elementarycatch
#' @title R6 object object_elementarycatch creation
#' @description Creation object object_elementarycatch in relation with R6 reference object class elementarycatch
#' @return A R6 reference object with data related to one elementary catch
#' @seealso \code{\link{elementarycatch}}
#' @export
object_elementarycatch <- function(trip_id,
                                   landing_date,
                                   activity_date,
                                   activity_number,
                                   logbook_category,
                                   vessel_id,
                                   specie_code3l,
                                   catch_weight) {
  t3:::elementarycatch$new(trip_id = trip_id,
                           landing_date = landing_date,
                           activity_date = activity_date,
                           activity_number = activity_number,
                           logbook_category = logbook_category,
                           vessel_id = vessel_id,
                           specie_code3l = specie_code3l,
                           catch_weight = catch_weight)
}
