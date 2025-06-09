# t3 3.0.3 - 2025-05-21

## Added
* Add the argument apply_rf1_on_bycatch=TRUE/FALSE, in the method [`full_trips$rf1()`](https://ob7-ird.github.io/t3/reference/full_trips.html#method-full_trips-rf1) By default TRUE, the raising factor value, calculated for each trip, is applied to all the logbook catches associated to the trip, including by-catch species. If FALSE, only the catch weights of species belonging to the species list defined by the `species_fao_codes_rf1` argument are corrected, rf1 is not applied to by-catch species.

# t3 3.0.2 - 2025-05-21

## Changed
* Improve weight category conversion ([full_trips$conversion_weight_category()](https://ob7-ird.github.io/t3/reference/full_trips.html#method-full_trips-conversion_weight_category)) for Observe database by importing maximum and minimum of weight categories from the database. 

# t3 3.0.1 - 2025-05-13

## Changed
* Important calculation time reduction by replacing [`future.apply::future_sapply()`](https://rdrr.io/cran/future.apply/man/future_lapply.html) by [`sapply()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/lapply.html) in method [extract()](https://ob7-ird.github.io/t3/reference/list_t3.html#method-list_t3-extract).
* Fix typo (weight instead of weigth) in "inst/avdth_samples.sql".
* In method [fishing_effort()](https://ob7-ird.github.io/t3/articles/level_1.html#process-1-4-fishing-effort-indicators-calculation):
  - For set declared as positive but with no elementary catch associated, the set duration is now set to the null set duration value, taken from the “inst/set_duration_ref.csv” table, and not to NA as before and a warning is generated. 
  - If there is no adjusted set duration model for the year corresponding to the activity date in the reference table (“inst/set_duration_ref.csv”), t3 now allows considering the adjusted set duration model for the year closest to the activity date in the reference table, generating a warning. 

## Added
* Add `global_output_path` argument in function [path_to_level3](https://ob7-ird.github.io/t3/reference/full_trips.html#method-full_trips-path_to_level3) to save the file named "inputs_levl3_target-year_ocean_ocean-code_flag-codes.Rdata", in outputs folder, if `global_output_path` is not NULL. 

# t3 3.0.0 - 2025-04-15

## Changed
* Level 1 optimization gather methods 1.1 and 1.2 for [raising factors computation](https://ob7-ird.github.io/t3/articles/level_2.html#process-1-1-https://ob7-ird.github.io/t3/articles/level_1.html#process-1-1-raising-factors-level-1)
* Fix typo (weight instead of weigth) in :
  - R6 reference object class [well](https://ob7-ird.github.io/t3/reference/well.html),
  - R6 reference object class [standardisedsampleset](https://ob7-ird.github.io/t3/reference/standardisedsampleset.html), 
  - process 1.3 [conversion_weight_categories](https://ob7-ird.github.io/t3/articles/level_1.html#process-1-3-logbook-weight-categories-conversion),
  - process 2.4 [well_set_weight_categories()](https://ob7-ird.github.io/t3/articles/level_2.html#process-2-4-well-set-weight-categories).
**Warning: you will need to correct this typo in your scripts**

## Added
* Add `global_output_path` argument in function [path_to_level3](https://ob7-ird.github.io/t3/reference/full_trips.html#method-full_trips-path_to_level3) to save inputs_levl3_target-year_ocean_ocean-code_flag-codes.Rdata, in outputs folder, if `global_output_path` is not NULL. 
* Add method [fishing_effort()](https://ob7-ird.github.io/t3/articles/level_1.html#process-1-4-fishing-effort-indicators-calculation) gathering methods 1.5 to 1.8:
- set_duration : calculated according to a linear function of catch weight with two parameters a and b. These are found through a reference table (set_duration_ref.csv), for each year, ocean, fishing school and country. 
- time_at_sea : the process divides the day's time at sea declared between the activities, allowing the allocation of time at sea, recorded on that date. If no activity to allocate time at sea is recorded on a given date, with a non-zero time at sea, a transit activity is created (whose id_activity contains #666#) to allocate the time ate sea of that date.
- fishing_time : the process module the duration of a working day according to the real sunrise and sunset of each day. It then divides the day's fishing time between the fishing activities recorded on that date. If no fishing activity is recorded on a given date with a non-zero fishing time, a searching activity is created (whose id_activity contains #666#) to allocate the fishing time of that date.
 - searching_time = fishing_time - set_duration.
 
## Removed 
* Methods 1.5 to 1.8 and 1.2 (rf2). 

**Warning: you will need to correct this in your scripts**

# t3 2.1.2 - 2025-04-14

## Changed
* Add year 2025 in the set duration referential (set_duration_ref.csv).
* Fix typo (weight instead of weigth) in :
  - R6 reference object class [well](https://ob7-ird.github.io/t3/reference/well.html),
  - R6 reference object class [standardisedsampleset](https://ob7-ird.github.io/t3/reference/standardisedsampleset.html), 
  - process 1.3 [conversion_weight_categories](https://ob7-ird.github.io/t3/articles/level_1.html#process-1-3-logbook-weight-categories-conversion),
  - process 2.4 [well_set_weight_categories()](https://ob7-ird.github.io/t3/articles/level_2.html#process-2-4-well-set-weight-categories).
**Warning: you will need to correct this typo in your scripts**

## Added
* Possibility of considering AVDTH and Observe data as input for model fitting at level 3. 

# t3 2.1.1 - 2025-03-21

## Changed
* Manage  fishing effort calculation for activities with multiple floating objects or object operation code declared.

# t3 2.1.0 - 2025-03-19

## Added
* Add a `catch_count` column in elementarycatches data, recording the number of individuals caught in a set, which can be recorded in place of `catch_weight` for by-catch species, in the new version of observe database.  
* Add method ([`object_model_data$activitycoderefs_data()`](https://ob7-ird.github.io/t3/reference/object_model_data.html#method-object_model_data-activitycoderefs_data)) to import activity code referential for time allocation (time_allocation_activity_code_ref.csv).
* Add argument `species_fate_codes` in method [`object_model_data$activities_object_creation()`](https://ob7-ird.github.io/t3/reference/object_model_data.html#method-object_model_data-activities_object_creation) to import activities and elementarycatch(es) with the same function. 
* Add argument `activity_code_ref` in [level_1](https://ob7-ird.github.io/t3/articles/level_1.html), process 1.5 [`set_duration()`](https://ob7-ird.github.io/t3/reference/full_trips.html#method-full_trips-set_duration-), 1.6 [`time_at_sea()`](https://ob7-ird.github.io/t3/reference/full_trips.html#method-time-at-sea-) and 1.7 [`fishing_time()`](https://ob7-ird.github.io/t3/reference/full_trips.html#method-fishing-time-).

## Changed
* Optimization : 
  - Change elementary catch(es) type from R6 Object to data.frame included into activity R6 Object.
  - Gather function to create ([`object_model_data()`](https://ob7-ird.github.io/t3/reference/object_model_data.html)) and add ([`full_trips$add_elementarycatches()`](https://ob7-ird.github.io/t3/reference/full_trips.html)) activities and elementary catches.
* Update the activity code referential for time allocation (time_allocation_activity_code_ref.csv)
* Improve fishing effort evaluation. The duration of activities is now calculated according to the activity code referential, in order to allocate time only to significant activities:
 - set_duration : calculated according to a linear function of catch weight with two parameters a and b. These are found through a reference table (set_duration_ref.csv), for each year, ocean, fishing school and country. 
 - time_at_sea : the process divides the day's time at sea declared between the activities, allowing the allocation of time at sea, recorded on that date. If no activity to allocate time at sea is recorded on a given date, with a non-zero time at sea, a transit activity is created (whose id_activity contains #666#) to allocate the time ate sea of that date.
 - fishing_time : the process module the duration of a working day according to the real sunrise and sunset of each day. It then divides the day's fishing time between the fishing activities recorded on that date. If no fishing activity is recorded on a given date with a non-zero fishing time, a searching activity is created (whose id_activity contains #666#) to allocate the fishing time of that date.
 - searching_time = fishing_time - set_duration.
* Manage  fishing effort calculation for activities with multiple floating objects or object operation code declared.
 
## Removed 
* Remove method [`object_model_data$elementarycatches_object_creation()`](https://ob7-ird.github.io/t3/reference/object_model_data.html#method-object_model_data-elementarycatches_object_creation).
* Remove method [`full_trips$add_elementarycatches()`](https://ob7-ird.github.io/t3/reference/full_trips.html#method-full_trips-add_elementarycatches).
* Remove [`elementarycatches`](https://ob7-ird.github.io/t3/reference/elementarycatches.html) and [`elementarycatch`](https://ob7-ird.github.io/t3/reference/elementarycatch.html) R6 Object definition and methods. 

# t3 2.0.1 - 2025-01-21

## Changed
* Change the type of the `flag_codes` parameter of [`object_model_data()`](https://ob7-ird.github.io/t3/reference/object_model_data.html) function from integer to character (three-letter FAO code(s) for the country(ies)). 

## Added
* Add arguments `country_flag` and `input_type = "observe_database"` by default, in [`level_3`](https://ob7-ird.github.io/t3/articles/level_3.html) process 3.4 `data_formatting_for_predictions()` and 3.5 `model_predictions()` and in function [`t3_level3()`](https://ob7-ird.github.io/t3/reference/t3_level3.html).

# t3 2.0.0 - 2024-11-29

## Added
* Add data source observe_database.
* Add weight category + 60kg (code 14) for free school, undetermined school and floating object school in Atlantic ocean and Indian ocean.
* Add functionality for querying multiple databases, for example the main and acquisition observe databases to simultaneously import and process ‘recent’ data from acquisition database, not yet imported into the main database, and older data from the main database.
* Add activity code referential to allocate time at sea and fishing time and set duration.

## Changed
* Update vignettes. 
* Update unit tests.
* Use of [codama](https://ob7-ird.github.io/codama/) for checking arguments.
* Update referential tables : set_duration_ref, length_step and length_weight_relationship.

## Removed 
* Remove data source t3_db. 

# t3 1.0.0 - 2022-04-21

## Added
* Implementation of unit tests
* Implementation of the documentation for the level 3
* Development of output extraction functions for process 1.1
* Adding an output directory initialization function and integrating it into the processes

## Changed
* Fix minor bugs and optimization of the code and regarding the overall process
* Update of the level 3 process

# t3 0.9.1 - 2020-09-21

## Added
* Implementation for the import of annual dataset from outputs of the level 1 and 2 (period length customizable)
* Implementation of bootstrap method as base for all confidence intervals
* Computation of confidence interval (bootstrap interval) for the nominal catches by species and by fishing mode
* Implementation of outputs for the he nominal catches by species and by fishing mode
* Fix minor bugs in all different processes (P1, P2, P3)

## Changed
* Improve modelling configuration (addition of new parameters)
* Rewrite several figures for the model checking
* Improve time computing using faster implementation of random forests for the species composition modeling in the level3 

## Removed
* Disabling task and effort computation while waiting for confident interval implementation
* Remove the documentation from the package. Stay available at https://ob7-ird.github.io/t3/index.html

# t3 0.9.0 - 2020-06-05

## Added
* Implentation of the model layer in R6class
* Implementation of all logbooks standardization sub-processes
* Implementation of all samples standardization sub-processes
* Implementation of sub-processes for assessing species composition in relation to non-sampled assemblages
* Strict selection of samples used in the model training according to several criteria on sets and wells (school type, location, date, sample quality).      
* Classification of unknown school type (no data available) by clustering according to major tuna species composition.      
* Development of a statistical model accounting for:
    - Spatial and temporal structure of the fishery (location and date of the catch)
    - Vessel specificity which impacts the species composition of the catch
    - Crew reporting information which enables more flexibility in the model when a marginal composition is caught
    - Several years of fishing in the training to increase the robustness of the estimates
* Model checking to ensure reliability of the results
* Computation of a confidence interval (bootstrap method) which give the trust level on catch estimations
