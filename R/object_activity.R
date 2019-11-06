#' @name object_activity
#' @title R6 object object_activity creation
#' @description Creation object object_activity in relation with R6 reference object class activity
#' @return A R6 reference object with data related to one activity
#' @seealso \code{\link{activity}}
#' @export
object_activity <- function(vessel_id,
                            landing_date,
                            activity_date,
                            activity_number,
                            set_count) {
  t3:::activity$new(vessel_id = vessel_id,
                    landing_date = landing_date,
                    activity_date = activity_date,
                    activity_number = activity_number,
                    set_count = set_count)
}
