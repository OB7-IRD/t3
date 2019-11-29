#' @name object_elementarysample
#' @title R6 object object_elementarysample creation
#' @description Creation object object_elementarysample in relation with R6 reference object class elementarysample
#' @return A R6 reference object with data related to one elementary sample
#' @seealso \code{\link{elementarysample}}
#' @export
object_elementarysample <- function(trip_id,
                                    sample_id,
                                    sub_sample_id,
                                    specie_code3l,
                                    length_type,
                                    sample_total_count,
                                    sample_number_measured,
                                    sample_length_class) {
  t3:::elementarysample$new(trip_id = trip_id,
                            sample_id = sample_id,
                            sub_sample_id = sub_sample_id,
                            specie_code3l = specie_code3l,
                            length_type = length_type,
                            sample_total_count = sample_total_count,
                            sample_number_measured = sample_number_measured,
                            sample_length_class = sample_length_class)
}
