#' @name object_elementarysampleraw
#' @title R6 object object_elementarysampleraw creation
#' @description Creation object object_elementarysampleraw in relation with R6 reference object class elementarysampleraw
#' @return A R6 reference object with data related to one elementary sample (raw data)
#' @seealso \code{\link{elementarysampleraw}}
#' @export
object_elementarysampleraw <- function(trip_id,
                                       sample_id,
                                       sub_sample_id,
                                       specie_code3l,
                                       length_type,
                                       sample_total_count,
                                       sample_number_measured,
                                       sample_length_class) {
  t3:::elementarysampleraw$new(trip_id = trip_id,
                               sample_id = sample_id,
                               sub_sample_id = sub_sample_id,
                               specie_code3l = specie_code3l,
                               length_type = length_type,
                               sample_total_count = sample_total_count,
                               sample_number_measured = sample_number_measured,
                               sample_length_class = sample_length_class)
}
