#' @name initiate_log_file
#' @title Log file creation
#' @description Initiation a log file creation.
#' @param log_file Object of class \code{\link[base]{logical}} expected. Initiation or not for log file creation. By default FALSE (no).
#' @param log_path Object of class \code{\link[base]{character}} expected. Path of the log file directory. By default NULL.
#' @param log_name Object of class \code{\link[base]{character}} expected. Name of the log file. By default "t3log".
#' @return A log file in text format at the location define in the argument "log_path".
#' @export
initiate_log_file <- function(log_file = FALSE,
                              log_path = NULL,
                              log_name = "t3log") {
  codama::r_type_checking(r_object = log_file,
                          type = "logical",
                          length = 1L)
  if (log_file %in% c(T,
                      TRUE)) {
    codama::r_type_checking(r_object = log_path,
                            type = "character",
                            length = 1L)
    codama::r_type_checking(r_object = log_name,
                            type = "character",
                            length = 1L)
    # 2 - Process ----
    final_log_path <- file.path(log_path,
                                paste0(format(Sys.time(),
                                              "%Y%m%d_%H%M%S_"),
                                       log_name,
                                       ".txt"))
    log_path_connection <- file(description = final_log_path,
                                open = "wt")
    sink(file = log_path_connection,
         type = "message",
         split = FALSE)
    sink(file = log_path_connection,
         append = FALSE,
         type = "output",
         split = TRUE)
    message(format(Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            " - Filling of the log file start now.\n",
            "[file path: ",
            final_log_path,
            "]\n",
            "For stop the recording run:",
            " closeAllConnections().")
  }
}
