#' @name list_t3
#' @title R6 class list_t3
#' @description Create R6 reference object class list_t3.
#' @importFrom R6 R6Class
#' @importFrom future.apply future_sapply
list_t3 <- R6Class(
  classname = "list_t3",
  public = list(
    # initialize ----
    #' @description Initialize function for R6 list_t3 class.
    initialize = function() {
      private$data <- list()
    },
    # attribut(s) name(s) ----
    #' @description Function for display item's name(s) attribut.
    #' @param attribut (character) Display all names with "all" attribut or specify one.
    name = function(attribut = "all") {
      if (attribut == "all") {
        return(names(private))
      } else if (!attribut %in% names(private)) {
        cat(
          format(Sys.time(),
                 "%Y-%m-%d %H:%M:%S"),
          " - Error: invalid \"attribut\" argument, attribut doesn't exist.\n",
          sep = ""
        )
        stop()
      } else {
        return(names(private[[attribut]]))
      }
    },
    # add new element ----
    #' @description Function for add new element in specific attribut.
    #' @param new_item (all type) Item to add.
    #' @param attribut (character) Attribut's name. By default "data".
    add = function(new_item, attribut = "data") {
      cat(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Start add new item(s) of class ",
        paste(class(new_item),
              collapse = " - "),
        ".\n",
        sep = ""
      )
      if (!attribut %in% names(private)) {
        cat(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: invalid \"attribut\" argument, attribut doesn't exist.\n",
          sep = ""
        )
        stop()
      }
      private[[attribut]] <-
        append(private[[attribut]], new_item)
      cat(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " - Successful added new item(s) of class ",
        paste(class(new_item),
              collapse = " - "),
        ".\n",
        sep = ""
      )
      invisible(self)
    },
    # remove element ----
    #' @description Function for removed element of specific attribut.
    #' @param item_id (numeric) Identification number of item to remove.
    #' @param attribut (character) Attribut's name. By default "data".
    remove = function(item_id, attribut = "data") {
      if (!attribut %in% names(private)) {
        cat(
          format(Sys.time(),
                 "%Y-%m-%d %H:%M:%S"),
          " - Error: invalid \"attribut\" argument, attribut doesn't exist.\n",
          sep = ""
        )
        stop()
      } else if (length(private[[attribut]]) == 0) {
        cat(
          format(Sys.time(),
                 "%Y-%m-%d %H:%M:%S"),
          " - Error: you can't delete an item from an empty list.\n",
          sep = ""
        )
        stop()
      } else if (length(x = class(x = item_id)) != 1 ||
                 ! class(x = item_id) %in% c("numeric", "integer")) {
        cat(
          format(Sys.time(),
                 "%Y-%m-%d %H:%M:%S"),
          " - Error: invalid \"item_id\" argument, class numeric or integer expected.\n",
          sep = ""
        )
        stop()
      } else if (any(as.integer(item_id) != item_id)) {
        cat(
          format(Sys.time(),
                 "%Y-%m-%d %H:%M:%S"),
          " - Error: invalid \"item_id\" argument, number integer expected.\n",
          sep = ""
        )
        stop()
      } else if (anyDuplicated(item_id) != 0) {
        cat(
          format(Sys.time(),
                 "%Y-%m-%d %H:%M:%S"),
          " - Error: invalid \"item_id\" argument, duplicate(s) element(s).\n",
          sep = ""
        )
        stop()
      } else {
        if (length(item_id) == 1) {
          if (item_id <= 0 | item_id > length(private[[attribut]])) {
            cat(
              format(Sys.time(),
                     "%Y-%m-%d %H:%M:%S"),
              " - Error: invalid \"item_number\" argument, subscript out of bounds.\n",
              sep = ""
            )
            stop()
          }  else {
            private[[attribut]] <- private[[attribut]][-item_id]
          }
        } else {
          item_id <- sort(item_id)
          for (i in seq_len(length.out = length(item_id))) {
            private[[attribut]] <- private[[attribut]][-item_id[i]]
            item_id <- item_id - 1
          }
        }
      }
      invisible(self)
    },
    # view element ----
    #' @description Function for display element(s) of specific attribut.
    #' @param ... (list) Identification number or name of item(s) to display.
    #' @param attribut (character) Attribut's name. By default "data".
    view = function(..., attribut = "data") {
      if (!attribut %in% names(private)) {
        cat(format(Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            " - Error: invalid \"attribut\" argument, attribut doesn't exist.\n",
            sep = ""
        )
        stop()
      }
      item_id <- list(...)
      if (length(item_id) == 0) {
        return(private[[attribut]])
      } else {
        if (length(private[[attribut]]) == 0) {
          cat(format(Sys.time(),
                     "%Y-%m-%d %H:%M:%S"),
              " - Error: empty list.\n",
              sep = "")
          stop()
        } else {
          tmp <- list()
          for (i in seq_len(length.out = length(item_id))) {
            if (length(x = class(x = item_id[[i]])) != 1 ||
                ! class(x = item_id[[i]]) %in% c("numeric",
                                                 "character",
                                                 "integer")) {
              cat(format(Sys.time(),
                         "%Y-%m-%d %H:%M:%S"),
                  " - Error: invalid \"item_id\" argument, more than 1 class or not numeric/character.\n",
                  sep = "")
              stop()
            } else {
              for (j in seq_len(length.out = length(item_id[[i]]))) {
                if (class(x = item_id[[i]]) %in% c("numeric",
                                                   "integer")
                    && (item_id[[i]][j] <= 0 |
                        item_id[[i]][j] > length(x = private[[attribut]]))) {
                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      " - Error: invalid \"item_id\" argument, subscript out of bounds.\n",
                      sep = "")
                  stop()
                } else if (class(item_id[[i]]) %in% c("character")
                           && !item_id[[i]][j] %in% names(x = private[[attribut]])) {
                  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      " - Error: invalid \"item_id\" argument, item_id not exist in the attribut.\n",
                      sep = "")
                  stop()
                } else {
                  if (length(private[[attribut]][[item_id[[i]][j]]]) == 1) {
                    tmp <- append(tmp, private[[attribut]][[item_id[[i]][j]]])
                  } else {
                    tmp <- append(tmp, list(private[[attribut]][[item_id[[i]][j]]]))
                  }
                }
              }
            }
          }
          return(tmp)
        }
      }
      invisible(self)
    },
    # number of element ----
    #' @description Function for display number of elements of a specific attribut.
    #' @param attribut (character) Attribut's name. By default "data".
    count = function(attribut = "data") {
      if (!attribut %in% names(private)) {
        cat(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: invalid \"attribut\" argument, attribut doesn't exist.\n",
          sep = ""
        )
        stop()
      } else {
        return(length(private[[attribut]]))
      }
    },
    # extract elements ----
    #' @description Function for extract element(s) of a specific attribut.
    #' @param attribut_l1 (character) First strate attribut's name. By default "data".
    #' @param attribut_l2 (character or NULL) Second strate attribut's name. By default NULL.
    #' @param id (integer) Number identification of element in the attribut.
    extract = function(attribut_l1 = "data",
                       attribut_l2 = NULL,
                       id = NULL) {
      attributs <- append(attribut_l1,
                          attribut_l2)
      # 1 - Arguments verification
      if (codama::r_type_checking(r_object = attributs,
                                  type = "character",
                                  output = "logical") != TRUE) {
        codama::r_type_checking(r_object = attributs,
                                type = "character",
                                output = "message")
        stop()
      }
      if (! attribut_l1 %in% names(x = private)) {
        stop(format(Sys.time(),
                    "%Y-%m-%d %H:%M:%S"),
             " - Error: invalid \"attribut_l1\" argument, attribut's name not in the object.",
             "\n",
             sep = "")
      }
      # 2 - Process
      if (! is.null(x = id)) {
        if (length(x = class(x = id)) != 1
            || ! any(class(x = id) %in% c("character",
                                          "numeric",
                                          "integer"))) {
          stop(format(Sys.time(),
                      "%Y-%m-%d %H:%M:%S"),
               " - Error: invalid \"id\" argument, class character, numeric or integer expected.",
               "\n",
               sep = "")
        } else if (class(x = id) %in% c("numeric",
                                        "integer")
                   && (id > length(x = private[[attributs[1]]])
                       | id <= 0)) {
          stop(format(Sys.time(),
                      "%Y-%m-%d %H:%M:%S"),
               " - Error: invalid \"id\" argument, subscript out of bounds.\n",
               sep = "")
        } else if (class(x = id) %in% c("character")
                   && ! id %in% names(x = private[[attributs[1]]])) {
          stop(format(Sys.time(),
                      "%Y-%m-%d %H:%M:%S"),
               " - Error: invalid \"id\" argument, id's name not exist in the attribut.\n",
               sep = "")
        } else {
          data <- unlist(x = private[[attributs[1]]][id])
        }
      } else {
        data <- unlist(x = private[[attributs[1]]])
      }
      output <- future.apply::future_sapply(X = seq_len(length.out = length(data)),
                                            FUN = function(current_data) {
                                              if (! is.null(x = attribut_l2)) {
                                                if (! attribut_l2 %in% names(data[[current_data]]$.__enclos_env__$private)) {
                                                  stop(format(Sys.time(),
                                                              "%Y-%m-%d %H:%M:%S"),
                                                       " - Error: invalid \"attribut_l2\" argument, attribut's name not in the object.",
                                                       "\n",
                                                       sep = "")
                                                } else {
                                                  data[[current_data]]$.__enclos_env__$private[[attribut_l2]]
                                                }
                                              } else {
                                                data[[current_data]]
                                              }
                                            })
      return(output)
    },
    # filter element ----
    # filter example arg$trip_id %in% trips_selected
    #' @description Function for select attribut's element(s) by specific filter.
    #' @param attribut_l1 (character) First strate attribut's name. By default "data".
    #' @param filter (character) Filter by a specific filter.
    filter = function(attribut_l1 = "data",
                      filter) {
      if (length(x = class(x = attribut_l1)) != 1
          || ! inherits(x = attribut_l1,
                        what = "character")) {
        cat(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: invalid \"attribut_l1\" argument, only class character expected.\n",
          sep = ""
        )
        stop()
      } else if (! attribut_l1 %in% names(x = private)) {
        cat(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: invalid \"attribut_l1\" argument, attribut's name not in the object.\n",
          sep = ""
        )
        stop()
      } else {
        data <- list()
        for (i in seq_len(length.out = length(private[[attribut_l1]]))) {
          tmp1 <- private[[attribut_l1]][[i]]
          if (is.list(tmp1)) {
            for (j in seq_len(length.out = length(tmp1))) {
              tmp2 <- tmp1[[j]]
              if (!any(class(x = tmp2) == "R6") |
                  is.list(x = tmp2)) {
                cat(
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  " - Error: invalid object's structure, R6 object and maximum list of two levels are expected.\n",
                  sep = ""
                )
                stop()
              } else {
                final_filter <-
                  gsub(
                    pattern = "arg$",
                    replacement = "tmp2$.__enclos_env__$private$",
                    x = filter,
                    fixed = TRUE
                  )
                final_filter <- parse(text = final_filter)
                tryCatch(
                  expr = if (eval(final_filter)) {
                    data <- append(x = data,
                                   values = tmp2)
                  },
                  error = function(err) {
                    cat(
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      " - Error: invalid \"filter\" argument\n",
                      sep = ""
                    )
                    stop()
                  }
                )
              }
            }
          } else {
            if (!any(class(x = tmp1) == "R6")) {
              cat(
                format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                " - Error: invalid object's structure, at least class R6 expected.\n",
                sep = ""
              )
              stop()
            } else {
              final_filter <- gsub(
                pattern = "arg$",
                replacement = "tmp1$.__enclos_env__$private$",
                x = filter,
                fixed = TRUE
              )
              final_filter <- parse(text = final_filter)
              tryCatch(
                expr = if (eval(final_filter)) {
                  data <- append(data, tmp1)
                },
                error = function(err) {
                  cat(
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    " - Error: invalid \"filter\" argument\n",
                    sep = ""
                  )
                  stop()
                }

              )
            }
          }
        }
        return(data)
      }
    },
    # filter element of data level 1 ----
    #' @description Function for select item(s) by specific selection.
    #' @param attribut_l1 Object of type \code{\link[base]{character}} expected. First strate attribut's name. By default "data".
    #' @param filter Object of type \code{\link[base]{character}} expected. Filter by specific selection. Use the pattern $path$ for specify path of element in the R6 object. For example: "$path$elementarycatch_id == "elementarycatch4" & $path$activity_id == "activity168""
    #' @param clone Object of type \code{\link[base]{logical}} expected. TRUE if you want to create a new object (not link to the original object).
    filter_l1 = function(attribut_l1 = "data",
                         filter,
                         clone = FALSE) {
      # 1 - Arguments verification
      if (codama::r_type_checking(r_object = attribut_l1,
                                  type = "character",
                                  output = "logical") != TRUE) {
        codama::r_type_checking(r_object = attribut_l1,
                                type = "character",
                                output = "message")
        stop()
      }
      if (! attribut_l1 %in% names(x = private)) {
        stop(cat(format(Sys.time(),
                        "%Y-%m-%d %H:%M:%S"),
                 " - Error: invalid \"attribut_l1\" argument, attribut's name not in the object.",
                 "\n",
                 sep = ""))
      }
      if (codama::r_type_checking(r_object = filter,
                                  type = "character",
                                  length = 1L,
                                  output = "logical") != TRUE) {
        codama::r_type_checking(r_object = filter,
                                type = "character",
                                length = 1L,
                                output = "message")
        stop()
      }
      if (codama::r_type_checking(r_object = clone,
                                  type = "logical",
                                  length = 1L,
                                  output = "logical") != TRUE) {
        codama::r_type_checking(r_object = clone,
                                type = "logical",
                                length = 1L,
                                output = "message")
        stop()
      }
      # 2 - Global process
      data <- private[[attribut_l1]]
      final_filter <- gsub(pattern = "$path$",
                           replacement = "current_data$.__enclos_env__$private$",
                           x = filter,
                           fixed = TRUE)
      final_filter <- parse(text = final_filter)
      data_final <- Filter(Negate(is.null),
                           lapply(X = seq_len(length.out = length(data)),
                                  FUN = function(data_id) {
                                    current_data <- data[[data_id]]
                                    tryCatch(
                                      expr = if (eval(expr = final_filter)) {
                                        if (clone == TRUE) {
                                          current_data$clone()
                                        } else {
                                          current_data
                                        }
                                      },
                                      error = function(err) {
                                        cat(format(Sys.time(),
                                                   "%Y-%m-%d %H:%M:%S"),
                                            " - Error: invalid \"filter\" argument\n",
                                            sep = "")
                                        stop()
                                      }
                                    )
                                  }))
      return(data_final)
    },
    # extract elements ----
    #' @description Function for extract element(s) of a specific attribut.
    #' @param attribut_l1 Object of type \code{\link[base]{character}} expected. First strate attribut's name. By default "data".
    #' @param element Object of type \code{\link[base]{character}} expected. Name of the element.
    extract_l1_element_value = function(attribut_l1 = "data",
                                        element) {
      if (length(x = class(x = attribut_l1)) != 1
          || ! inherits(x = attribut_l1,
                        what = "character")) {
        cat(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: invalid \"attribut_l1\" argument, only class character expected.\n",
          sep = ""
        )
        stop()
      } else if (!attribut_l1 %in% names(private)) {
        cat(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: invalid \"attribut_l1\" argument, attribut's name not in the object.\n",
          sep = ""
        )
        stop()
      } else {
        tmp1 <- private[[attribut_l1]]
        element_final <- parse(text = paste0("tmp2$.__enclos_env__$private$",
                                             element))
        tmp_final <- lapply(X = seq_len(length.out = length(tmp1)),
                            FUN = function(a) {
                              tmp2 <- tmp1[[a]]
                              tryCatch(
                                expr = eval(element_final),
                                error = function(err) {
                                  cat(
                                    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " - Error: invalid \"element\" argument in the item ",
                                    a,
                                    "\n",
                                    sep = ""
                                  )
                                  stop()
                                }
                              )
                            })
        return(tmp_final)
      }
    },
    # modify element of data level 1 ----
    #' @description Function for modification item(s).
    #' @param attribut_l1 Object of type \code{\link[base]{character}} expected. First strate attribut's name. By default "data".
    #' @param modification Object of type \code{\link[base]{character}} expected. Attribute to modify. Use the pattern $path$ for specify path of attribute in the R6 object. For example: "$path$activity_code = 1"
    #' @param silent Object of type \code{\link[base]{logical}} expected. Display outputs of modificated values.
    modification_l1 = function(attribut_l1 = "data",
                               modification,
                               silent = TRUE) {
      if (length(x = class(x = attribut_l1)) != 1
          || ! inherits(x = attribut_l1,
                        what = "character")) {
        cat(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: invalid \"attribut_l1\" argument, only class character expected.\n",
          sep = ""
        )
        stop()
      } else if (!attribut_l1 %in% names(private)) {
        cat(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Error: invalid \"attribut_l1\" argument, attribut's name not in the object.\n",
          sep = ""
        )
        stop()
      } else {
        tmp1 <- private[[attribut_l1]]
        final_modification <- gsub(pattern = "$path$",
                                   replacement = "tmp2$.__enclos_env__$private$",
                                   x = modification,
                                   fixed = TRUE)
        final_modification <- parse(text = final_modification)
        lapply(X = seq_len(length.out = length(tmp1)),
               FUN = function(a) {
                 tmp2 <- tmp1[[a]]
                 tryCatch(
                   expr = eval(final_modification),
                   error = function(err) {
                     cat(
                       format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                       " - Error: invalid \"modification\" argument\n",
                       sep = ""
                     )
                     stop()
                   }
                 )
               })
        if (silent == TRUE) {
          capture.output(return(),
                         file = "NUL")
        }
      }
    }
  ),
  private = list(data = NULL)
)
