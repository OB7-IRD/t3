#' @name t3_level1
#' @title T3 process level 1
#' @description Level 1 of t3 process (logbooks standardisation).
#' @param object_model_data Object of classes \code{\link[t3]{object_model_data}} and \code{\link[R6]{R6}} expected.
#' @param object_full_trips Object of classes \code{\link[t3]{full_trips}}, \code{\link[t3]{list_t3}} and \code{\link[R6]{R6}} expected.
#' @param rf1_computation Object of class \code{\link[base]{logical}} expected. If FALSE rf1 is not calculated (rf1=1 for all trips).
#' By default TRUE, the rf1 is calculated for each trip.
#' @param species_fao_codes_rf1 Object of type \code{\link[base]{character}} expected.Specie(s) FAO code(s) used for the RF1 process.
#' By default, use codes YFT (*Thunnus albacares*), SKJ (*Katsuwonus pelamis*), BET (*Thunnus obesus*), ALB (*Thunnus alalunga*),
#' LOT (*Thunnus tonggol*) and TUN/MIX (mix of tunas species in Observe/AVDTH database) (French and Mayotte fleets).
#' @param apply_rf1_on_bycatch Object of class \code{\link[base]{logical}} expected. By default FALSE, only the catch weights of species belonging to the species list, defined by the \code{species_fao_codes_rf1} argument are corrected, rf1 is not applied to by-catch species.
#' If TRUE, rf1 values will be applied to all the logbook catches associated to the trip, including by-catch species.
#' @param species_fate_codes_rf1 Object of type \code{\link[base]{integer}} expected. By default 6 ("Retained, presumably destined for the cannery"). Specie(s) fate code(s) used for the RF1 process.
#' @param vessel_type_codes_rf1 Object of type \code{\link[base]{integer}} expected. By default 4, 5 and 6. Vessel type(s).
#' @param rf1_lowest_limit Object of type \code{\link[base]{numeric}} expected. Verification value for the lowest limit of the RF1. By default 0.8.
#' @param rf1_highest_limit Object of type \code{\link[base]{numeric}} expected. Verification value for the highest limit of the RF1. By default 1.2.
#' @param fishing_effort_computation Object of class {\link[base]{logical}} expected. Calculation or not of fishing effort indicators (time at sea, fishing time, set duration and searching time). By default TRUE (yes).
#' @param sunrise_schema Object of class {\link[base]{character}} expected. Sunrise characteristic. By default "sunrise" (top edge of the sun appears on the horizon). See function fishing_time() for more details.
#' @param sunset_schema Object of class {\link[base]{character}} expected. Sunset characteristic. By default "sunset" (sun disappears below the horizon, evening civil twilight starts). See function fishing_time() for more details.
#' @param log_file Object of class {\link[base]{logical}} expected. Initiation or not for log file creation. By default FALSE (no).
#' @param log_path Object of class {\link[base]{character}} expected. Path of the log file directory. By default NULL.
#' @param log_name Object of class {\link[base]{character}} expected. Name of the log file. By default "t3_level1".
#' @param output_path Object of class \code{\link[base]{character}} expected. Outputs path directory. By default NULL.
#' @param new_directory Object of class \code{\link[base]{logical}} expected. Initiate a new outputs directory of use an existing one. By default NULL.
#' @param integrated_process Object of class \code{\link[base]{logical}} expected. Indicate if the process is integrated in another (like the one in the function "t3_process"). By default FALSE.
#' @param referential_template Object of class \code{\link[base]{character}} expected. By default "observe". Referential template selected (for example regarding the activity_code). You can switch to "avdth".
#' @return The function return a R6 reference object named "object_full_trips" of class \code{\link[t3]{full_trips}}.
#' @details
#' If a output_path is specified, the following outputs are extracted and saved in ".csv" format under the path: "global_output_path/level1/data/". \cr
#' \itemize{
#' \item{Process 1.1 Raising Factor level 1 (RF1 and RF2) calculation (\href{https://ob7-ird.github.io/t3/reference/full_trips.html#method-full_trips-rf1}{\code{full_trips$rf1()}}): }
#'  \itemize{
#'  \item{process_1_1_detail a table (.csv) with as many rows as elementary catches and 23 columns:}
#'  \itemize{
#'  \item{full_trip_id: } retained full trip id, type \code{\link[base]{integer}}.
#'  \item{full_trip_name: } full trip id, type \code{\link[base]{integer}}.
#'  \item{trip_id: } trip identification (unique topiaid from database), type \code{\link[base]{character}}.
#'  \item{activity_id: } activity identification (unique topiaid from database), type \code{\link[base]{character}}.
#'  \item{activity_latitude: } activity latitude, type \code{\link[base]{numeric}}.
#'  \item{activity_longitude: } activity longitude, type \code{\link[base]{numeric}}.
#'  \item{trip_end_date: } trip end date (y-m-d format), type \code{\link[base]{character}}.
#'  \item{year_trip_end_date: } year of trip end, type \code{\link[base]{integer}}.
#'  \item{vessel_code: } vessel code, type \code{\link[base]{integer}}.
#'  \item{vessel_type_code: } vessel type code, type \code{\link[base]{integer}}.
#'  \item{rf1: } raising factor to correct the weight visual estimation bias of catches filled in logbooks.
#'  Rf1 is the ratio of landing weight on catch weight, of the species defined by the \code{species_fao_codes_rf1} argument.
#'  , type \code{\link[base]{numeric}}.
#'  \item{statut_rf1: } status rf1, type \code{\link[base]{character}}.
#'  \item{rf2: } raising factor to correct missing logbook(s) not implemented yet (rf2=1), type \code{\link[base]{numeric}}.
#'  \item{statut_rf2: } status rf2, type \code{\link[base]{character}}.
#'  \item{species_fao_code: } species FAO code, type \code{\link[base]{character}}.
#'  \item{elementarycatch_id: } elementary catch identification (unique topiaid from database), type \code{\link[base]{character}}.
#'  \item{species_fate_code: } species fate codes, type \code{\link[base]{integer}}.
#'  For example in Observe database :  \itemize{
#'  \item{4 : } discarded alive.
#'  \item{5 : } discarded dead.
#'  \item{6 : } Retained, presumably destined for the cannery
#'  \item{8 : } used for crew consumption on board.
#'  \item{11 : } discarded status unknown (only for EMS and logbook).
#'  \item{15 : } retained for local market or dried/salted fish on board.
#'  }
#'  \item{landing_weight: } landing weight (without local market), in tonnes, type \code{\link[base]{numeric}}.
#'  \item{catch_weight: } catch weight (visual estimation), in tonnes, type \code{\link[base]{numeric}}.
#'  \item{catch_count: } catch count, type \code{\link[base]{integer}}.
#'  \item{catch_weight_rf2: } catch weight after visual estimation correction, in tonnes: \code{catch_weight_rf2=catch_weight x rf1 (x rf2)}
#'  (\href{https://ob7-ird.github.io/t3/articles/level_1.html#process-1-1-raising-factors-level-1}{Process 1.1: Raising Factors level 1}), type \code{\link[base]{numeric}}.
#'  \item{statut_rf1_label: } status rf1 label, type \code{\link[base]{character}}.
#'  \item{statut_rf2_label: } status rf2 label, type \code{\link[base]{character}}.
#'  }
#'  \item{process_1_1_global a table (.csv) with as many rows as full trips and 17 columns:}
#'  \itemize{
#'  \item{full_trip_id: } retained full trip id, type \code{\link[base]{integer}}.
#'  \item{full_trip_name: } full trip id, type \code{\link[base]{integer}}.
#'  \item{trip_id: } trip identification (unique topiaid from database), type \code{\link[base]{character}}.
#'  \item{trip_end_date: } trip end date (y-m-d format), type \code{\link[base]{character}}.
#'  \item{year_trip_end_date: } year of trip end, type \code{\link[base]{integer}}.
#'  \item{vessel_code: } vessel code, type \code{\link[base]{integer}}.
#'  \item{vessel_type_code: } vessel type code, type \code{\link[base]{integer}}.
#'  \item{rf1: } raising factor to correct the weight visual estimation bias of catches filled in logbooks.
#'  Rf1 is the ratio of landing weight on catch weight, of the species defined by the \code{species_fao_codes_rf1} argument.
#' , type \code{\link[base]{numeric}}.
#'  \item{statut_rf1: } status rf1, type \code{\link[base]{character}}.
#'  \item{rf2: } raising factor to correct missing logbook(s) not implemented yet (rf2=1), type \code{\link[base]{numeric}}.
#'  \item{statut_rf2: } status rf2, type \code{\link[base]{character}}.
#'  \item{landing_weight: } landing weight (without local market), in tonnes, type \code{\link[base]{numeric}}.
#'  \item{catch_weight: } catch weight (visual estimation), in tonnes, type \code{\link[base]{numeric}}.
#'  \item{catch_count: } catch count, type \code{\link[base]{integer}}.
#'  \item{catch_weight_rf2: } catch weight after visual estimation correction (tonnes): \code{catch_weight_rf2=catch_weight x rf1 (x rf2)}
#'  (\href{https://ob7-ird.github.io/t3/reference/full_trips.html#method-full_trips-rf1}{\code{full_trips$rf1()}}), type \code{\link[base]{numeric}}.
#'  \item{statut_rf1_label: } status rf1 label, type \code{\link[base]{character}}.
#'  \item{statut_rf2_label: } status rf2 label, type \code{\link[base]{character}}.
#'  }
#'  }
#'  \item{Process 1.2 Conversion of logbook weight categories (\href{https://ob7-ird.github.io/t3/reference/full_trips.html#method-full_trips-conversion_weight_category}{\code{full_trips$conversion_weight_category()}}): }
#'  process_1_2 a table (.csv) with as many rows as elementary catches, plus the catches resulting from the conversion of weight categories and 23 columns:
#'  \itemize{
#'  \item{full_trip_id: } retained full trip id, type \code{\link[base]{integer}}.
#'  \item{full_trip_name: } full trip id, type \code{\link[base]{integer}}.
#'  \item{trip_id: } trip identification (unique topiaid from database), type \code{\link[base]{character}}.
#'  \item{trip_end_date: } trip end date, type \code{\link[base]{character}}.
#'  \item{year_trip_end_date: } year of trip end, type \code{\link[base]{integer}}.
#'  \item{vessel_code: } vessel code, type \code{\link[base]{integer}}.
#'  \item{vessel_type_code: } vessel type code, type \code{\link[base]{integer}}.
#'  \item{activity_id: } activity identification (unique topiaid from database), type \code{\link[base]{character}}.
#'  \item{activity_latitude: } activity latitude, type \code{\link[base]{numeric}}.
#'  \item{activity_longitude: } activity longitude, type \code{\link[base]{numeric}}.
#'  \item{activity_date: } activity date, type \code{\link[base]{POSIXct}}.
#'  \item{ocean_code: } ocean code, type \code{\link[base]{integer}}.
#'   For example \code{ocean_code=1} for the Atlantic Ocean and \code{ocean_code=2} the Indian Ocean.
#'  \item{school_type_code:} school type code, type \code{\link[base]{integer}}.
#'   In Observe referential template: 1 for floating object school, 2 for free school and 0 for undetermined school.
#'  \item{elementarycatch_id: } elementary catch identification (unique topiaid from database), type \code{\link[base]{character}}.
#'  \item{species_fao_code: } species FAO code, type \code{\link[base]{character}}.
#'  \item{weight_category_code: } weight category code defined in logbooks, type \code{\link[base]{character}}.
#'  \item{weight_category_min: } weight category's lower limit (kg), type \code{\link[base]{numeric}}.
#'  \item{weight_category_max: } weight category's upper limit (kg), type \code{\link[base]{numeric}}.
#'  \item{weight_category_label: } weight category label defined in logbooks, type \code{\link[base]{character}}.
#'  \item{catch_weight_rf2: } catch weight after visual estimation correction  (tonnes): \code{catch_weight_rf2=catch_weight x rf1 (x rf2)}
#'  (\href{https://ob7-ird.github.io/t3/articles/level_1.html#process-1-1-raising-factors-level-1}{Process 1.1: Raising Factors level 1}), type \code{\link[base]{numeric}}.
#'  \item{weight_category_code_corrected: } weight category after conversion, type \code{\link[base]{character}}.
#'  \item{catch_weigh_category_code_corrected: } catch weight after weight category conversion (tonnes), type \code{\link[base]{numeric}}.\cr
#'   In fact, the catch weight corresponding to the logbook weight category can be divided between several corrected weight categories according to the distribution key applied for conversion to standardized weight categories.
#'  \item{catch_count: } catch count, type \code{\link[base]{integer}}.
#'  }
#'  \item{Process 1.3 Positive sets count (\href{https://ob7-ird.github.io/t3/reference/full_trips.html#method-full_trips-set_count}{\code{full_trips$set_count()}}): }
#'  process_1_3 a table (.csv) with as many rows as activities and 15 columns:
#'  \itemize{
#'  \item{full_trip_id: } retained full trip id, type \code{\link[base]{integer}}.
#'  \item{full_trip_name: } full trip id, type \code{\link[base]{integer}}.
#'  \item{trip_id: } trip identification (unique topiaid from database), type \code{\link[base]{character}}.
#'  \item{trip_end_date: } trip end date, type \code{\link[base]{character}}.
#'  \item{year_trip_end_date: } year of trip end, type \code{\link[base]{integer}}.
#'  \item{vessel_code: } vessel code, type \code{\link[base]{integer}}.
#'  \item{vessel_type_code: } vessel type code, type \code{\link[base]{integer}}.
#'  \item{activity_id: } activity identification (unique topiaid from database), type \code{\link[base]{character}}.
#'  \item{activity_latitude: } activity latitude, type \code{\link[base]{numeric}}.
#'  \item{activity_longitude: } activity longitude, type \code{\link[base]{numeric}}.
#'  \item{activity_date: } activity date, type \code{\link[base]{POSIXct}}.
#'  \item{activity_code: } activity code to define the type of activity, type \code{\link[base]{integer}}.
#'  \item{ocean_code: } ocean code, type \code{\link[base]{integer}}.
#'   For example \code{ocean_code=1} for the Atlantic Ocean and \code{ocean_code=2} the Indian Ocean.
#'  \item{school_type_code:} school type code, type \code{\link[base]{integer}}.
#'   In Observe referential template: 1 for floating object school, 2 for free school and 0 for undetermined school.
#'  \item{positive_set_count: } count of positive set (catch weight and/or catch count not zero), type \code{\link[base]{integer}}.
#'  }
#'  \item{ Process 1.4 Fishing effort indicators (\href{https://ob7-ird.github.io/t3/reference/full_trips.html#method-full_trips-fishing_effort}{\code{full_trips$fishing_effort()}}): }
#'   process_1_4 a table (.csv) with as many rows as activities and 20 columns:
#'  \itemize{
#'  \item{full_trip_id: } retained full trip id, type \code{\link[base]{integer}}.
#'  \item{full_trip_name: } full trip id, type \code{\link[base]{integer}}.
#'  \item{trip_id: } trip identification (unique topiaid from database), type \code{\link[base]{character}}.
#'  \item{trip_end_date: } trip end date, type \code{\link[base]{character}}.
#'  \item{year_trip_end_date: } year of trip end, type \code{\link[base]{integer}}.
#'  \item{vessel_code: } vessel code, type \code{\link[base]{integer}}.
#'  \item{vessel_type_code: } vessel type code, type \code{\link[base]{integer}}.
#'  \item{activity_id: } activity identification (unique topiaid from database), type \code{\link[base]{character}}.
#'  \item{activity_latitude: } activity latitude, type \code{\link[base]{numeric}}.
#'  \item{activity_longitude: } activity longitude, type \code{\link[base]{numeric}}.
#'  \item{activity_date: } activity date, type \code{\link[base]{POSIXct}}.
#'  \item{activity_code: } activity code to define the type of activity, type \code{\link[base]{integer}}.
#'  \item{objectoperation_code: } object operation code to define the type of floating object operation (in Observe referential), type \code{\link[base]{character}}.
#'  \item{ocean_code: } ocean code, type \code{\link[base]{integer}}.
#'   For example \code{ocean_code=1} for the Atlantic Ocean and \code{ocean_code=2} the Indian Ocean.
#'  \item{school_type_code:} school type code, type \code{\link[base]{integer}}.
#'   In Observe referential template: 1 for floating object school, 2 for free school and 0 for undetermined school.
#'  \item{positive_set_count: } count of positive set (catch weight and/or catch count not zero), type \code{\link[base]{integer}}.
#'  \item{set_duration: } set duration in hours, according to the \href{https://ob7-ird.github.io/t3/reference/set_duration_ref.html}{referential set duration table}, type \code{\link[base]{numeric}}.
#'  \item{time_at_sea: } time at sea in hours, type \code{\link[base]{numeric}}.
#'  \item{fishing_time: } fishing time in hours, type \code{\link[base]{numeric}}.
#'  \item{searching_time: } searching time in hours, type \code{\link[base]{numeric}}.\cr
#'   Equal to the fishing time value minus the sum of the sets duration values.
#'  }
#'  }
#' @seealso \code{\link[t3]{full_trips}},  \code{\link[t3]{t3_level2}}, \code{\link[t3]{t3_level3}}, \code{\link[t3]{t3_process}}
#' @export
t3_level1 <- function(object_model_data,
                      object_full_trips,
                      rf1_computation = TRUE,
                      apply_rf1_on_bycatch = TRUE,
                      species_fao_codes_rf1 = c("YFT", "SKJ", "BET", "ALB",
                                                "LOT", "TUN", "MIX"),
                      species_fate_codes_rf1 = as.integer(6),
                      vessel_type_codes_rf1 = as.integer(c(4, 5, 6)),
                      rf1_lowest_limit = 0.8,
                      rf1_highest_limit = 1.2,
                      fishing_effort_computation = TRUE,
                      sunrise_schema = "sunrise",
                      sunset_schema = "sunset",
                      log_file = FALSE,
                      log_path = NULL,
                      log_name = "t3_level1",
                      output_path = NULL,
                      new_directory = FALSE,
                      integrated_process = FALSE,
                      referential_template = "observe") {
  if (paste0(class(x = object_model_data),
             collapse = " ") != "object_model_data R6") {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Invalid \"object_model_data\" argument. Classes \"object_model_data\" and \"R6\" expected.")
  } else if (paste0(class(x = object_full_trips),
                    collapse = " ") != "full_trips list_t3 R6") {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Invalid \"object_full_trips\" argument. Classes \"full_trips\", \"list_t3\" and \"R6\" expected.")
  } else {
    # log file initialisation ----
    initiate_log_file(log_file = log_file,
                      log_path = log_path,
                      log_name = log_name)
    cat(format(x = Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            " - Start function t3 process level 1.\n",
            "[species rf1: ",
            paste0(species_fao_codes_rf1,
                   collapse = ", "),
            "]\n", sep="")
    # directories initialization if outputs extraction ----
    if (integrated_process != TRUE
        && ! is.null(x = output_path)) {
      output_path <- initiate_directory(output_path = output_path,
                                        new_directory = new_directory,
                                        level = "level1")
    }
    # level 1.1: rf1 ----
    object_full_trips$rf1(rf1_computation = rf1_computation,
                          apply_rf1_on_bycatch = apply_rf1_on_bycatch,
                          species_fao_codes_rf1 = species_fao_codes_rf1,
                          species_fate_codes_rf1 = species_fate_codes_rf1,
                          vessel_type_codes_rf1 = vessel_type_codes_rf1,
                          rf1_lowest_limit = rf1_lowest_limit,
                          rf1_highest_limit = rf1_highest_limit,
                          global_output_path = output_path)
    # level 1.2: logbook weight categories conversion ----
    object_full_trips$conversion_weight_category(global_output_path = output_path,
                                                 referential_template = referential_template)
    # level 1.3: set count ----
    object_full_trips$set_count(global_output_path = output_path,
                                referential_template = referential_template)
    if(fishing_effort_computation){
    # level 1.4:  ----
    object_full_trips$fishing_effort(set_duration_ref = object_model_data$.__enclos_env__$private$setdurationrefs,
                                     activity_code_ref = object_model_data$.__enclos_env__$private$activitycoderefs,
                                     sunrise_schema =  sunrise_schema,
                                     sunset_schema = sunset_schema,
                                     global_output_path = output_path,
                                     referential_template = referential_template)
    }
    # close, if necessary log file connection ----
    if (log_file == TRUE) {
      closeAllConnections()
    }
    cat(format(x = Sys.time(),
                   format = "%Y-%m-%d %H:%M:%S"),
            " - Successful function t3 process level 1.\n",
        sep="")
    return(object_full_trips)
  }
}
