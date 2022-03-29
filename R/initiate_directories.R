#' @name initiate_directories
#' @title Output directories creation
#' @description Initiation of output directories creation.
#' @param output_path (character) expected. General output directory.
#' @param level (character) expected. Initiation of a full output directory ("all") or specific to a level (choose between "level1", "level2" or "level3").
#' @return A text variable with output directory path.
#' @export
initiate_directories <- function(output_path,
                                 level = "all") {
  # verification ----
  if (missing(x = output_path)
      || class(x = output_path) != "character"
      || length(x = output_path) != 1) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalid \"output_path\" argument, ",
        sep = "")
    stop()
  } else if (class(x = level) != "character"
             || length(x = level) != 1
             || ! level %in% c("all",
                             "level1",
                             "level2",
                             "level3")) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalid \"level\" argument, ",
        sep = "")
    stop()
  } else {
    # function ----
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Start process for output directories creation.\n",
        sep = "")
    outputs_directory_name <- format(Sys.time(),
                                     "%Y%m%d_%H%M%S_t3_outputs")
    dir.create(path = file.path(output_path,
                                outputs_directory_name))
    if (level %in% c("all",
                     "level1")) {
      dir.create(path = file.path(output_path,
                                  outputs_directory_name,
                                  "level1"))
    }
    if (level %in% c("all",
                     "level2")) {
      dir.create(path = file.path(output_path,
                                  outputs_directory_name,
                                  "level2"))
    }
    if (level %in% c("all",
                     "level3")) {
      dir.create(path = file.path(output_path,
                                  outputs_directory_name,
                                  "level3"))
    }
    for (level_directory in list.dirs(path = file.path(output_path,
                                                       outputs_directory_name))[-1]) {
      for (directory in c("data_outputs")) {
        dir.create(path = file.path(level_directory,
                                    directory))
      }
    }
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Successful process for output directories creation.\n",
        "outputs availables here: ",
        file.path(output_path,
                  outputs_directory_name),
        "\n",
        sep = "")
    return(file.path(output_path,
                     outputs_directory_name))
  }
}
