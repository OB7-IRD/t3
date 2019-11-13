#' @name object_activity
#' @title R6 object object_activity creation
#' @description Creation object object_activity in relation with R6 reference object class activity
#' @return A R6 reference object with data related to one activity
#' @seealso \code{\link{activity}}
#' @export
object_activity <- function(trip_id,
                            activity_id,
                            ocean,
                            activity_date,
                            activity_number,
                            set_count,
                            school_type,
                            activity_code,
                            activity_name) {
  t3:::activity$new(trip_id = trip_id,
                    activity_id = activity_id,
                    ocean = ocean,
                    activity_date = activity_date,
                    activity_number = activity_number,
                    set_count = set_count,
                    school_type = school_type,
                    activity_code = activity_code,
                    activity_name = activity_name)
}
