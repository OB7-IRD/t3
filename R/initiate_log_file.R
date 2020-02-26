#' @name initiate_log_file
#' @title Log file creation
#' @description Initiation a log file creation.
#' @param log_path (character) Path of the log file.
#' @return A log file in text format at the location define in the argument "log_path".
#' @export
initiate_log_file = function(
  log_path
) {
  # log_path verification ----
  if (length(class(log_path)) != 1 || class(log_path) != "character") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalid \"log_path\" argument\n",
        "class \"character\" expected",
        sep = "")
    stop()
  } else {
    # function ----
    log_path_connection <- file(description = log_path,
                                open = "wt")
    sink(file = log_path_connection,
         type = "message",
         split = F)
    sink(file = log_path_connection,
         append = F,
         type = "output",
         split = T)
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Filling of the log file start now\n",
        "For stop the recording run:",
        " closeAllConnections()",
        "\n",
        sep = "")
  }
}
