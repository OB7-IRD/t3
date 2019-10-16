#' @name mtd_full_trip
#' @title To do
#' @param data To do
#' @references \url{https://github.com/OB7-IRD/t3}
#' @details To do after dude !
#' @export
mtd_full_trip <- function(object, year) {
  trip <- t3::fct_s4_conversion(object = object,
                                type = "dataframe")

  trip_year <- dplyr::filter(.data = trip,
                             format(landing_date, "%Y") == year)

  not_full_trip <- dplyr::filter(.data = trip_year,
                                 fish_hold_empty == 0)

  i = "fr.ird.t3.entities.reference.Vessel#1460000000000#0.164"

  for (i in not_full_trip$vessel_id) {
    tmp1 <- not_full_trip[not_full_trip$vessel_id == i, ] %>%
      dplyr::arrange(landing_date)
    tmp2 <- trip[trip$vessel_id == i, ] %>%
      dplyr::arrange(landing_date)

  }
}
