#' @name tunaboot
#' @title Boostrap for Model Predictions
#' @description Bootstrap function which compute proportion by species fishing mode and ocean for confindence interval
#' @param sample_data (data frame) Data used for the modelling. Output table from process 3.1.
#' @param allset_data (data frame) Data used for prediction.Output table from process 3.4.
# @param schooltype (integer) Fishing mode of the catch.
# @param ocean (integer) Target ocean.
# @param species (character) Target species. 'SKJ' for kipjack and 'YFT' for yellowfin.
#' @param Ntree (integer) Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times. The default value is 1000.
#' @param Nmtry Number of variables randomly sampled as candidates at each split. The default value is 2.
#' @param Nseed Set the initial seed for the modelling. The default value is 7.
#' @param min_node Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time).The default value is 5.
#' @param Nboot The number of bootstrap samples desired. The fefault value is 10
#' @param Nseed_boot Set the initial seed for the modelling. The default value is equal to Nseed parameter
#' @param target_period Time period for the predictions in year. Default is the year of the data to predict
#' @importFrom ranger ranger
#' @importFrom dplyr sample_n rename
#' @return

tunaboot <- function(sample_data,
                     allset_data,
                     # schooltype = 1,
                     # ocean = 1,
                     # species = "SKJ",
                     # # model parameters
                     Ntree = 1000,
                     Nmtry = 2,
                     Nseed = 7, # fix seed number for reproducibility of the predicts
                     min_node = 5,
                     # bootstrap parameters
                     Nboot = 10,
                     Nseed_boot, # fix seed number in the bootstrap model
                     target_period
)
{
  # local binding global variables ----
  prop_t3 <- modrf0 <- NULL
  if(missing(Nseed_boot)){Nseed_boot = Nboot}
  if(missing(target_period)){target_period = allset_data$yr[1]}

  # build structure of the output list for the bootstrap
  boot_output_list <- vector('list', length = Nboot)
  # boot_output_list <- lapply(boot_output_list, function (x){
  #   tmp <- vector('list', length = 3)
  #   names(tmp) <- c("task1", "task2","loop")
  #   return(tmp)
  # })

  # subset of all sampled set
  sub <- sample_data

  # format columns
  sub$resp <- (sub$prop_t3)
  sub$tlb <- (sub$prop_lb)
  sub$yr <- factor(sub$year)
  sub$mon <- factor(sub$mon)
  sub$vessel <- factor(sub$vessel)
  sub <- droplevels(sub)
  sub$ocean <- factor(sub$ocean)
  sub$fmod <- factor(sub$fmod)

  #### bootstrap

  # select sample and data to predict

    ### remove set used to train models from data to predict
    no_sampled_set <- droplevels(allset_data[!(allset_data$id_act %in% unique(sub$id_act)),])
    no_sampled_set <- droplevels(no_sampled_set[no_sampled_set$yr %in% target_period,])

    ## Prepare data

    no_sampled_set$tlb <- (no_sampled_set$prop_lb)  # transform predictor if necessary
    no_sampled_set$yr <- factor(no_sampled_set$yr)
    no_sampled_set$mon <- factor(no_sampled_set$mon)
    no_sampled_set$vessel <- factor(no_sampled_set$vessel)

    ### split dataframe for different treatment if needed
    # no_sampled_set$wtot_lb_t3 <- no_sampled_set$w_tuna #  total tuna catch of each set

    no_sampled_set$data_source <- NA # assign source later
    no_sampled_set$fit_prop <- NA # stock final proportion

    # add potential missing level - bug of the predict function

    # no_sampled_set$yr <- addmissinglevel(no_sampled_set, "yr")
    # no_sampled_set$mon <- addmissinglevel(no_sampled_set, "mon")
    # no_sampled_set$vessel <- addmissinglevel(no_sampled_set, "vessel")

    # missinglevel<- setdiff(levels(sub$vessel), levels(tmp$vessel))
    # levels(tmp$vessel) <- c(levels(tmp$vessel),missinglevel)

    # levels(no_sampled_set$vessel) <- modrf$forest$xlevels$vessel
    # levels(no_sampled_set$yr) <- modrf$forest$xlevels$yr
    # levels(no_sampled_set$mon) <- modrf$forest$xlevels$mon

    # not sampled vessel list
    vessel_not_train <- base::setdiff(levels(no_sampled_set$vessel),
                                      levels(sub$vessel))

    # dataset with all information
    newd <- (no_sampled_set[!no_sampled_set$vessel %in% vessel_not_train, ])
    # newd$vessel <- factor(newd$vessel, levels = setdiff(levels(newd$vessel),vessel_not_train)) # remove levels of vessel not train

    # dataset with vessel not sampled
    new_wtv <- no_sampled_set[no_sampled_set$vessel %in% vessel_not_train & !is.na(no_sampled_set$prop_lb), ]

    # dataset with no logbook
    new_0 <- no_sampled_set[no_sampled_set$vessel %in% vessel_not_train & is.na(no_sampled_set$prop_lb), ]
    new_0$data_source <- as.character(new_0$data_source)


    # compute catch by species by set
    sampled_set <- unique(sub[sub$yr %in% target_period,])
    # add sample not corrected catch by species
    if(nrow(sampled_set) > 0){
      sampled_set$data_source <- "sample"
    } # add flag
    sampled_set <- dplyr::rename(sampled_set,
                                 fit_prop = prop_t3)
    # output columns
    column_keep <- c("id_act",
                     "fmod",
                     "sp",
                     "lat",
                     "lon",
                     "date_act",
                     "vessel",
                     "ocean",
                     "yr",
                     "mon",
                     "fit_prop",
                     "wtot_lb_t3",
                     # "w_lb_t3",
                     "data_source")

    for (i in seq.int(from = 1, to = Nboot)){
      print(i)
      set.seed(i)
      newsample <- dplyr::sample_n(tbl = sub,
                                   size = nrow(sub),
                                   replace = TRUE)

    #-----------------------#
    ## models and predicts ##
    #-----------------------#

    # best case  (all information available)###
    if(nrow(newd)>0) {

      set.seed(Nseed_boot)
      model_rf_full <- ranger::ranger(resp ~ tlb + lon + lat + yr + mon + vessel,
                                      data = newsample,
                                      num.trees = Ntree,
                                      mtry = Nmtry,
                                      importance = "none",
                                      min.node.size = min_node,
                                      splitrule = "variance",
                                      replace = TRUE,
                                      quantreg = FALSE,
                                      keep.inbag= FALSE
      )
      newd$fit_prop <- ranger:::predict.ranger(object = model_rf_full,
                                               data = newd)$predictions
      newd$data_source <- "full_model" # add flag
    }

    ##  without vessel information
    if(nrow(new_wtv)>0) {
      set.seed(Nseed_boot)
      model_rf_wtvessel <- ranger::ranger(resp ~ tlb + lon + lat + yr + mon,
                                          data = newsample,
                                          num.trees = Ntree,
                                          mtry = Nmtry,
                                          importance = "none",
                                          min.node.size = min_node,
                                          splitrule = "variance",
                                          replace = TRUE,
                                          quantreg = FALSE,
                                          keep.inbag= FALSE
      )

      new_wtv$fit_prop<- ranger:::predict.ranger(model_rf_wtvessel,data=new_wtv)$predictions
      new_wtv$data_source <- "model_wtv" # add flag
    }

    # without logbook information on the catch, location and date only
    if(nrow(new_0)>0) {

      set.seed(Nseed_boot)
      model_rf_simple <- ranger::ranger(resp ~ lon + lat + yr + mon,
                                        data = newsample,
                                        num.trees = Ntree,
                                        mtry = Nmtry,
                                        importance = "none",
                                        min.node.size = min_node,
                                        splitrule = "variance",
                                        replace = TRUE,
                                        quantreg = FALSE,
                                        keep.inbag= FALSE
      )

      new_0$fit_prop<- ranger:::predict.ranger(modrf0,newdata=new_0)$predictions
      new_0$data_source <- "simple_model" # add flag
    }

    # remove set with no data and combine all catch
    all_set <- list(newd,new_wtv,new_0, sampled_set)
    all_set <- all_set[which(unlist(lapply(all_set, nrow)) > 0)]
    all_set <- dplyr::bind_rows(all_set)

    boot_output_list[[i]] <- all_set
  }
  return(boot_output_list)
}
