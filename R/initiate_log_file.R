#' @name initiate_log_file
#' @title Log file creation
#' @description Initiation a log file creation.
#' @param log_file (logical) Initiation or not for log file creation. By default FALSE (no).
#' @param log_path (character) Path of the log file directory. By default NULL.
#' @param log_name (character) Name of the log file. By default "t3log".
#' @return A log file in text format at the location define in the argument "log_path".
#' @export
initiate_log_file <- function(log_file = FALSE,
                              log_path = NULL,
                              log_name = "t3log") {
  # verifications ----
  if (class(log_file) != "logical") {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Error: invalid \"log_file\" argument\n",
        "class \"logical\" expected\n",
        sep = "")
  } else {
    if (log_file %in% c(T, TRUE)) {
      if (class(log_path) != "character") {
        cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            " - Error: invalid \"log_path\" argument\n",
            "class \"character\" expected\n",
            sep = "")
      } else if (class(log_name) != "character") {
        cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            " - Error: invalid \"log_name\" argument\n",
            "class \"character\" expected\n",
            sep = "")
      } else {
        # function ----
        final_log_path <- file.path(log_path,
                                    paste0(format(Sys.time(), "%Y%m%d_%H%M%S_"),
                                           log_name,
                                           ".txt"))
        log_path_connection <- file(description = final_log_path,
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
            "[file path: ",
            final_log_path,
            "]\n",
            "For stop the recording run:",
            " closeAllConnections()",
            "\n",
            sep = "")
      }
    }
  }
}
