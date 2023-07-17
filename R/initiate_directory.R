#' @name initiate_directory
#' @title Output directories creation
#' @description Initiation of output directories creation.
#' @param output_path Object of class \code{\link[base]{character}} expected. Output path directory.
#' @param new_directory Object of class \code{\link[base]{logical}} expected. Initiate a new output directory of use an existing one. By default TRUE.
#' @param level Object of class \code{\link[base]{character}} expected. Initiation of a full output directory ("all") or specific to a level (choose between "level1", "level2", "until_level2 or "level3").
#' @return A list with output directory path, at least, and log path if requested.
#' @importFrom codama r_type_checking
#' @export
initiate_directory <- function(output_path,
                               new_directory = TRUE,
                               level = "all") {
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = output_path,
                              type = "character",
                              length = 1L,
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = output_path,
                                   type = "character",
                                   length = 1L,
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = new_directory,
                              type = "logical",
                              length = 1L,
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = new_directory,
                                   type = "logical",
                                   length = 1L,
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = level,
                              type = "character",
                              length = 1L,
                              allowed_value = c("all",
                                                "level1",
                                                "level2",
                                                "until_level2",
                                                "level3"),
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = level,
                                   type = "character",
                                   length = 1L,
                                   allowed_value = c("all",
                                                     "level1",
                                                     "level2",
                                                     "until_level2",
                                                     "level3"),
                                   output = "message"))
  }
  # 2 - Process ----
  cat(format(x = Sys.time(),
             format = "%Y-%m-%d %H:%M:%S"),
      " - Start process for output directories creation.\n",
      sep = "")
  if (new_directory == TRUE) {
    output_directory_name <- file.path(output_path,
                                       format(Sys.time(),
                                              "%Y%m%d_%H%M%S_t3_outputs"))
    dir.create(path = output_directory_name)
  } else {
    output_directory_name <- output_path
  }
  list_directories <- list.dirs(path = output_directory_name,
                                recursive = FALSE)
  if (level %in% c("all",
                   "level1",
                   "until_level2")
      && all(list_directories != file.path(output_directory_name,
                                           "level1"))) {
    dir.create(path = file.path(output_directory_name,
                                "level1"))
  }
  if (level %in% c("all",
                   "level2",
                   "until_level2")
      && all(list_directories != file.path(output_directory_name,
                                           "level2"))) {
    dir.create(path = file.path(output_directory_name,
                                "level2"))
  }
  if (level %in% c("all",
                   "level3")
      && all(list_directories != file.path(output_directory_name,
                                           "level3"))) {
    dir.create(path = file.path(output_directory_name,
                                "level3"))
  }
  list_directories <- list.dirs(path = output_directory_name,
                                recursive = FALSE)
  for (level_directory in which(x = list_directories %in% c(file.path(output_directory_name,
                                                                      "level1"),
                                                            file.path(output_directory_name,
                                                                      "level2"),
                                                            file.path(output_directory_name,
                                                                      "level3")))) {
    current_directory <- list_directories[level_directory]
    for (directory in c("data",
                        "figure")) {
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
      output_directory_name,
      "\n",
      sep = "")
  return(output_directory_name)
}
