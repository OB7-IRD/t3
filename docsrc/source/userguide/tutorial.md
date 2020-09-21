
## Tutorial 

### Minimal example to run **T3**

```R
library(t3)
library(furdeb)

work_path <- getwd()
log_path <- paste0(work_path, "/logs")

t3_data <-

```


### More complex example

#### Create the config file

The first step is to create the config file as followed :

```
section;sub_section;value
parameters_for_r_environnement;location_of_working_directory;/path/to/working/directory
configuration_for_java;memory_allowed_for_java;8g
configuration_for_java;location_of_java_folder;/usr/lib/jvm/default-java/
parameters_of_t3plus_db;t3plus_user_id;DB_USER
parameters_of_t3plus_db;t3plus_password_id;DB_PWD
parameters_of_t3plus_db;t3plus_dbname;DB_NAME
parameters_of_t3plus_db;t3plus_host;DB_HOST
parameters_of_t3plus_db;t3plus_port;DB_PORT
```

#### Connection to the T3 database

```R
library(t3)
library(furdeb)

config_t3 <- furdeb::configuration_file(new_config  = F,
                                        path_config= "full/path/to/configfile_t3.csv")

# database connection ----
t3_con <- furdeb::db_connection(
  db_user = config_t3[["t3plus_user"]],
  db_password = config_t3[["t3plus_password"]],
  db_dbname = config_t3[["t3plus_dbname"]],
  db_host = config_t3[["t3plus_host"]],
  db_port = config_t3[["t3plus_port"]]
)

log_path <- paste0(config_t3[["work_path"]], "/logs")

global_start_time <- Sys.time()
# shortcut functions ----
# data model initialisation
start_time <- Sys.time()
t3::data_model_initialisation(periode_reference = as.integer(2015),
                              countries = c("FRA", "MYT"),
                              oceans = as.integer(c(1,2)),
                              sample_type = as.integer(1),
                              db_con = t3_con,
                              log_file = TRUE,
                              log_path = log_path)
end_time <- Sys.time()
cat("Data model init : time elpased", end_time - start_time, " mins")

# level 1
start_time <- Sys.time()
t3::t3_level1(species_rf1 = as.integer(c(1, 2, 3, 4, 9, 11)),
              set_duration_ref = object_model_data$.__enclos_env__$private$setdurationref,
              log_file = TRUE,
              log_path = log_path)
end_time <- Sys.time()
cat("Level 1 : time elpased", end_time - start_time, " mins")

# level 2
start_time <- Sys.time()
t3::t3_level2(length_step = object_model_data$.__enclos_env__$private$lengthstep,
               sample_set = object_model_data$.__enclos_env__$private$sampleset,
               length_weight_relationship_data = object_model_data$.__enclos_env__$private$lengthweightrelationship,
               log_file = TRUE,
               log_path = log_path)
end_time <- Sys.time()
cat("Level 2 : time elpased", end_time - start_time, " mins")

# path to level 3 ----
start_time <- Sys.time()
object_full_trips$path_to_level3()
save(
  data_level3,
  file = paste0(config_t3[["work_path"]], "inputs_level3_2015.RData")
)
end_time <- Sys.time()
cat("Path to level 3 : time elpased", end_time - start_time, " mins")
global_end_time <- Sys.time()
cat("Global : time elpased", global_end_time - global_start_time, " mins")

# Level 3

load(paste0(config_t3[["work_path"]], "inputs_level3_2017.RData"))
object_full_trips <- t3:::full_trips$new()

# level 3.1: data preparatory ----
object_full_trips$data_preparatory(inputs_level3 = data_level3[[1]],
                                   outputs_directory = paste0(config_t3[["work_path"]],
                                                              "\\05-Outputs"),
                                   periode_reference = as.integer(2015:2017),
                                   targeted_year = as.integer(2017),
                                   vessel_id_ignored = as.integer(427))

# level 3.2: random forest models ----
object_full_trips$random_forest_models(outputs_level3_process1 = data_level3[[3]][[4]][[1]])

# level 3.3: models checking ----
object_full_trips$models_checking(outputs_level3_process2 = data_level3[[4]],
                                  outputs_path = data_level3[[2]])

# level 3.4: data formatting for predictions ----
object_full_trips$data_formatting_for_predictions(inputs_level3 = data_level3[[1]],
                                                  outputs_level3_process1 = data_level3[[3]][[4]][[1]],
                                                  targeted_year = as.integer(2017),
                                                  vessel_id_ignored = as.integer(427))

# level 3.5: predictions ----
object_full_trips$model_predictions(outputs_level3_process2 = data_level3[[4]],
                                    outputs_level3_process4 = data_level3[[6]],
                                    outputs_path = data_level3[[2]])
```
