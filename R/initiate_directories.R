#' @name initiate_directories
#' @title Output directories creation
#' @description Initiation of output directories creation.
#' @param outputs_path (character) expected. General output directory.
#' @param level (character) expected. Initiation of a full output directory ("all") or specific to a level (choose between "level1", "level2", "until_level2 or "level3").
#' @return A text variable with output directory path.
#' @export
initiate_directories <- function(outputs_path,
                                 new_directory = TRUE,
                                 level = "all") {
  # verification ----
  if (missing(x = outputs_path)
      || class(x = outputs_path) != "character"
      || length(x = outputs_path) != 1) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalid \"outputs_path\" argument, ",
        sep = "")
    stop()
  } else if (class(x = level) != "character"
             || length(x = level) != 1
             || ! level %in% c("all",
                             "level1",
                             "level2",
                             "until_level2",
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
    if (new_directory == TRUE) {
      outputs_directory_name <- file.path(outputs_path,
                                          format(Sys.time(),
                                                 "%Y%m%d_%H%M%S_t3_outputs"))
      dir.create(path = outputs_directory_name)
    } else {
      outputs_directory_name <- outputs_path
    }
    list_directories <- list.dirs(path = outputs_directory_name,
                                  recursive = FALSE)
    if (level %in% c("all",
                     "level1",
                     "until_level2")
        && all(list_directories != file.path(outputs_directory_name,
                                             "level1"))) {
      dir.create(path = file.path(outputs_directory_name,
                                  "level1"))
    }
    if (level %in% c("all",
                     "level2",
                     "until_level2")
        && all(list_directories != file.path(outputs_directory_name,
                                             "level2"))) {
      dir.create(path = file.path(outputs_directory_name,
                                  "level2"))
    }
    if (level %in% c("all",
                     "level3")
        && all(list_directories != file.path(outputs_directory_name,
                                             "level3"))) {
      dir.create(path = file.path(outputs_directory_name,
                                  "level3"))
    }
    list_directories <- list.dirs(path = outputs_directory_name,
                                  recursive = FALSE)
    for (level_directory in which(x = list_directories %in% c(file.path(outputs_directory_name,
                                                                        "level1"),
                                                              file.path(outputs_directory_name,
                                                                        "level2"),
                                                              file.path(outputs_directory_name,
                                                                        "level3")))) {
      current_directory <- list_directories[level_directory]
      for (directory in c("data_outputs",
                          "figures")) {
        if (all(list.dirs(path = current_directory,
                          recursive = FALSE) != file.path(current_directory,
                                                          directory))) {
          dir.create(path = file.path(current_directory,
                                      directory))
        }
      }
    }
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Successful process for output directories creation.\n",
        "outputs availables here: ",
        outputs_directory_name,
        "\n",
        sep = "")
    return(outputs_directory_name)
  }
}
