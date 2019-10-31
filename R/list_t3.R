#' @name list_t3
#' @title R6 class list_t3 creation
#' @description Create R6 reference object class list_t3
#' @importFrom R6 R6Class
list_t3 <- R6Class(classname = "list_t3",
                   public = list(
                     initialize = function() {
                       private$data <- list()
                     },
                     # attribut(s) name(s) ----
                     name = function(attribut = "all") {
                       if (attribut == "all") {
                         return(names(private))
                       } else if (! attribut %in% names(private)) {
                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                             " - Error: invalid \"attribut\" argument\nattribut doesn't exist\n",
                             sep = "")
                         stop()
                       } else {
                         return(names(private[[attribut]]))
                       }
                     },
                     # add new element ----
                     add = function(new_item, attribut = "data") {
                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                           " - Start add new item(s) of class ", class(new_item), "\n",
                           sep = "")
                       if (! attribut %in% names(private)) {
                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                             " - Error: invalid \"attribut\" argument\nattribut doesn't exist\n",
                             sep = "")
                         stop()
                       }
                       private[[attribut]] <- append(private[[attribut]], new_item)
                       cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                           " - End added new item(s) of class ", class(new_item), "\n",
                           sep = "")
                       invisible(self)
                     },
                     # remove element ----
                     remove = function(item_id, attribut = "data") {
                       if (! attribut %in% names(private)) {
                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                             " - Error: invalid \"attribut\" argument\nattribut doesn't exist\n",
                             sep = "")
                         stop()
                       } else if (length(private[[attribut]]) == 0) {
                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                             " - Error: you can't delete an item from an empty list\n",
                             sep = "")
                         stop()
                       } else if (length(class(item_id)) != 1 || ! class(item_id) %in% c("numeric", "integer")) {
                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                             " - Error: invalid \"item_id\" argument\nclass numeric or integer expected\n",
                             sep = "")
                         stop()
                       } else if (any(as.integer(item_id) != item_id)) {
                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                             " - Error: invalid \"item_id\" argument\nnumber integer expected\n",
                             sep = "")
                         stop()
                       } else if (anyDuplicated(item_id) != 0) {
                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                             " - Error: invalid \"item_id\" argument\nduplicate(s) element(s)\n",
                             sep = "")
                         stop()
                       } else {
                         if (length(item_id) == 1) {
                           if (item_id <= 0 | item_id > length(private[[attribut]])) {
                             cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                 " - Error: invalid \"item_number\" argument\nsubscript out of bounds\n",
                                 sep = "")
                             stop()
                           }  else {
                             private[[attribut]] <- private[[attribut]][-item_id]
                           }
                         } else {
                           item_id <- sort(item_id)
                           for (i in 1:length(item_id)) {
                             private[[attribut]] <- private[[attribut]][-item_id[i]]
                             item_id <- item_id - 1
                           }
                         }
                       }
                       invisible(self)
                     },
                     # view element ----
                     view = function(..., attribut = "data") {
                       if (! attribut %in% names(private)) {
                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                             " - Error: invalid \"attribut\" argument\nattribut doesn't exist\n",
                             sep = "")
                         stop()
                       }
                       item_id <- list(...)
                       if (length(item_id) == 0) {
                         return(private[[attribut]])
                       } else {
                         if (length(private[[attribut]]) == 0) {
                           cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                               " - Error: empty list\n",
                               sep = "")
                           stop()
                         } else {
                           tmp <- list()
                           for (i in 1:length(item_id)) {
                             if (length(class(item_id[[i]])) != 1 || ! class(item_id[[i]]) %in% c("numeric", "character", "integer")) {
                               cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                   " - Error: invalid \"item_id\" argument\nmore than 1 class or not numeric/character\n",
                                   sep = "")
                               stop()
                             } else {
                               for (j in 1:length(item_id[[i]])) {
                                 if (class(item_id[[i]]) %in% c("numeric", "integer") && (item_id[[i]][j] <= 0 | item_id[[i]][j] > length(private[[attribut]]))) {
                                   cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                       " - Error: invalid \"item_id\" argument\nsubscript out of bounds\n",
                                       sep = "")
                                   stop()
                                 } else if (class(item_id[[i]]) %in% c("character") && ! item_id[[i]][j] %in% names(private[[attribut]])) {
                                   cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                       " - Error: invalid \"item_id\" argument\nitem_id not exist in the attribut\n",
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
                     count = function(attribut = "data") {
                       if (! attribut %in% names(private)) {
                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                             " - Error: invalid \"attribut\" argument\nattribut doesn't exist\n",
                             sep = "")
                         stop()
                       } else {
                         return(length(private[[attribut]]))
                       }
                     },
                     # extract elements ----
                     extract = function(attribut_l1 = "data", attribut_l2 = NULL, id = NULL) {
                       attributs <- append(attribut_l1, attribut_l2)
                       if (length(class(attributs)) != 1 || class(attributs) != "character") {
                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                             " - Error: invalid \"attribut(s)\" argument\nonly class character expected\n",
                             sep = "")
                         stop()
                       } else if (! attribut_l1 %in% names(private)) {
                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                             " - Error: invalid \"attribut_l1\" argument\nattribut's name not in the object\n",
                             sep = "")
                         stop()
                       } else {
                         if (! is.null(id)) {
                           if (length(class(id)) != 1 || ! any(class(id) %in% c("character", "numeric", "integer"))) {
                             cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                 " - Error: invalid \"id\" argument\nClass character or numeric expected\n",
                                 sep = "")
                             stop()
                           } else if (class(id) %in% c("numeric", "integer") && (id > length(private[[attributs[1]]]) | id <= 0)) {
                             cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                 " - Error: invalid \"id\" argument\nsubscript out of bounds\n",
                                 sep = "")
                             stop()
                           } else if (class(id) %in% c("character") && ! id %in% names(private[[attributs[1]]])) {
                             cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                 " - Error: invalid \"id\" argument\nid's name not exist in the attribut\n",
                                 sep = "")
                             stop()
                           } else {
                             data <- unlist(private[[attributs[1]]][id])
                           }
                         } else {
                           data <- unlist(private[[attributs[1]]])
                         }
                         output <- sapply(1:length(data),
                                          function(i) {
                                            if (! is.null(attribut_l2)) {
                                              if (! attribut_l2 %in% names(data[[i]]$.__enclos_env__$private)) {
                                                cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                    " - Error: invalid \"attribut_l2\" argument\nattribut's name not in the object\n",
                                                    sep = "")
                                                stop()
                                              } else {
                                                data[[i]]$.__enclos_env__$private[[attribut_l2]]
                                              }
                                            } else {
                                              data[[i]]
                                            }
                                          })
                         return(output)
                       }
                     },
                     # filter element ----
                     # filter = "arg$trip_id %in% trips_selected"
                     filter = function(attribut_l1 = "data", filter) {
                       if (length(class(attribut_l1)) != 1 || class(attribut_l1) != "character") {
                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                             " - Error: invalid \"attribut_l1\" argument\nonly class character expected\n",
                             sep = "")
                         stop()
                       } else if (! attribut_l1 %in% names(private)) {
                         cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                             " - Error: invalid \"attribut_l1\" argument\nattribut's name not in the object\n",
                             sep = "")
                         stop()
                       } else {
                         data <- list()
                         for (i in 1:length(private[[attribut_l1]])) {
                           tmp1 <- private[[attribut_l1]][[i]]
                           if (is.list(tmp1)) {
                             for (j in 1:length(tmp1)) {
                               tmp2 <- tmp1[[j]]
                               if (! any(class(tmp2) == "R6") | is.list(tmp2)) {
                                 cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                     " - Error: invalid object's structure\nR6 object and maximum list of two levels are expected\n",
                                     sep = "")
                                 stop()
                               } else {
                                 final_filter = gsub(pattern = "arg$",
                                                     replacement = "tmp2$.__enclos_env__$private$",
                                                     x = filter,
                                                     fixed = TRUE)
                                 final_filter = parse(text = final_filter)
                                 tryCatch(expr = if (eval(final_filter)) {
                                   data <- append(data, tmp2)
                                 }, error = function(err) {
                                   cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                       " - Error: invalid \"filter\" argument\n",
                                       sep = "")
                                   stop()
                                   })
                               }
                             }
                           } else {
                             if (! any(class(tmp1) == "R6")) {
                               cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                   " - Error: invalid object's structure\nAt least class R6 expected\n",
                                   sep = "")
                               stop()
                             } else {
                               final_filter = gsub(pattern = "arg$",
                                                   replacement = "tmp1$.__enclos_env__$private$",
                                                   x = filter,
                                                   fixed = TRUE)
                               final_filter = parse(text = final_filter)
                               tryCatch(expr = if (eval(final_filter)) {
                                 data <- append(data, tmp1)
                               }, error = function(err) {
                                 cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                     " - Error: invalid \"filter\" argument\n",
                                     sep = "")
                                 stop()
                                 })
                             }
                           }
                         }
                         return(data)
                       }
                     }),
                   private = list(
                     data = NULL
                   ))
