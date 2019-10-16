library(R6)
# trip ----
cover <- R6::R6Class(classname = "cover",
                public = list(trip_id = NULL,
                              landing_date = NULL,
                              fishing_time = NULL,
                              time_at_sea = NULL,
                              logbook_availability = NULL,
                              fish_hold_empty = NULL,
                              vessel_id = NULL,
                              landing_harbour_id = NULL,
                              departure_harbour_id = NULL,
                              initialize = function(trip_id,
                                                    landing_date,
                                                    fishing_time,
                                                    time_at_sea,
                                                    logbook_availability,
                                                    fish_hold_empty,
                                                    vessel_id,
                                                    landing_harbour_id,
                                                    departure_harbour_id) {
                                if (fishing_time < 0) {
                                  stop("Attribut \"fishing_time\" have at least one negative value")
                                }
                                # Attribut "time_at_sea" verification
                                if (time_at_sea < 0) {
                                  stop("Attribut \"time_at_sea\" have at least one negative value")
                                }
                                # Attribut "landing_date" verification
                                if (landing_date > Sys.Date()) {
                                  stop("At least one attribut \"landing_date\" is superior to the actual date")
                                }
                                self$trip_id <- trip_id
                                self$landing_date <- landing_date
                                self$fishing_time <- fishing_time
                                self$time_at_sea <- time_at_sea
                                self$logbook_availability <- logbook_availability
                                self$fish_hold_empty <- fish_hold_empty
                                self$vessel_id <- vessel_id
                                self$landing_harbour_id <- landing_harbour_id
                                self$departure_harbour_id <- departure_harbour_id},
                              cover = function(x) {
                                print(paste0(self$trip_id, x))
                                invisible(self)
                              }))

tmp1 <- cover$new(trip_id = trip_ori[1, 1],
                 landing_date = trip_ori[1, 2],
                 fishing_time = trip_ori[1, 3],
                 time_at_sea = trip_ori[1, 4],
                 logbook_availability = trip_ori[1, 5],
                 fish_hold_empty = trip_ori[1, 6],
                 vessel_id = trip_ori[1, 7],
                 landing_harbour_id = trip_ori[1, 8],
                 departure_harbour_id = trip_ori[1, 9])

tmp2 <- trip$new(trip_id = trip_ori[2, 1],
                 landing_date = trip_ori[2, 2],
                 fishing_time = trip_ori[2, 3],
                 time_at_sea = trip_ori[2, 4],
                 logbook_availability = trip_ori[2, 5],
                 fish_hold_empty = trip_ori[2, 6],
                 vessel_id = trip_ori[2, 7],
                 landing_harbour_id = trip_ori[2, 8],
                 departure_harbour_id = trip_ori[2, 9])

# trips ----
trips <- R6Class(classname = "trips",
                 public = list(trips = NULL,
                               # trips_add_trip ----
                               trips_add_trip = function(new_trip) {
                                 if (is.null(self$trips)) {
                                   self$trips <- list(new_trip)
                                   names(self$trips) <- new_trip$trip_id
                                   invisible(self)
                                 } else {
                                   names_tmp <- names(self$trips)
                                   self$trips <- c(self$trips, new_trip)
                                   names(self$trips) <- c(names_tmp, new_trip$trip_id)
                                   invisible(self)
                                 }
                               }))

tmp3 <- trips$new()
tmp3$trips_add_trip(tmp1)
tmp3$trips_add_trip(tmp2)
tmp3$trips_add_trip(tmp2)
tmp3$trips

tmp3 <- trips$new()
tmp3$trips_add_trip(tmp1)$
  trips_add_trip(tmp2)$
  trips_add_trip(tmp2)
tmp3$trips

Person <- R6::R6Class("Person",
                      public = list(
                        initialize = function(name, age = NA) {
                          stopifnot(is.character(name), length(name) == 1)
                          stopifnot(is.numeric(age), length(age) == 1)
                          private$name <- name
                          private$age <- age
                        }
                      ),
                      private = list(
                        name = NULL,
                        age = NA))
hadley <- Person$new(name = "Hadely", age = 38)
