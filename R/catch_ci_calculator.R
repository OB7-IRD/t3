#' @name catch_ci_calculator
#' @title Compute final confidence interval
#' @description Compute final confidence interval
#' @param to The observed value of statistic applied to data
#' @param t A matrix with sum(R) rows each of which is a bootstrap replicate of the result of calling statistic.
#' @return

# source(file = file.path(root_path, "function", "df2boot.R", fsep = "\\")) # bootstrap function


catch_ci_calculator <- function(fit_data,
                                boot_data){
  # set catch
  # if(!is.null(fit_data$id_act)){
    if("id_act" %in% colnames(fit_data)){
    output <- do.call(rbind,lapply(seq.int(1, nrow(fit_data)), function (x){
      print(fit_data$id_act[x])
      # sample ci are zero
      if(fit_data$data_source[x] == "sample"){
        output_ci <- cbind (fit_data[x,],
                            data.frame(ci_inf = fit_data$catch_set_fit[x],
                                       ci_sup = fit_data$catch_set_fit[x]))
      } else {
      tmp <- t3:::df2boot(t0 = fit_data$catch_set_fit[x] ,
                          t = as.matrix(boot_data$catch_set_fit[boot_data$sp == fit_data$sp[x] &
                                                                  boot_data$id_act == fit_data$id_act[x]]), # bootstrap values)
                          R = max(boot_data$loop) # number of loop
      )
      ci <- boot::boot.ci(tmp,
                          conf = 0.95,
                          type = c("basic", "norm","perc"))

      output_ci <- cbind (fit_data[x,],
                          data.frame(ci_inf = ci$percent[,c(4)],
                                     ci_sup = ci$percent[,c(5)]))
      }
      return(output_ci)
    }))
  } else {
    # nominal catch or catch effort
    # if(is.null(fit_data$cwp)){
      if(!("cwp" %in% colnames(fit_data))){
      # by fishing mode or not
      ifelse(!("fmod" %in% colnames(fit_data)), #is.null(fit_data$fmod),
             # nominal catch total
             output <- do.call(rbind,lapply(seq.int(1, nrow(fit_data)), function (x){
               tmp <- t3:::df2boot(t0 = fit_data$catch_set_fit[x] ,
                                   t = as.matrix(boot_data$catch_set_fit[boot_data$sp == fit_data$sp[x]]), # bootstrap values)
                                   R = max(boot_data$loop) # number of loop
               )
               ci <- boot::boot.ci(tmp,
                                    conf = 0.95,
                                    type = c("basic", "norm","perc"))

               output_ci <- cbind (fit_data[x,],
                                   data.frame(ci_inf = ci$percent[,c(4)],
                                              ci_sup = ci$percent[,c(5)]))

               return(output_ci)
             })),
             # nominal catch by fishing mode
             output <- do.call(rbind,lapply(seq.int(1, nrow(fit_data)), function (x){
               tmp <- t3:::df2boot(t0 = fit_data$catch_set_fit[x] ,
                                   t = as.matrix(boot_data$catch_set_fit[boot_data$sp == fit_data$sp[x] &
                                                                           boot_data$fmod == fit_data$fmod[x]]), # bootstrap values)
                                   R = max(boot_data$loop) # number of loop
               )
               ci <- boot::boot.ci(tmp,
                                   conf = 0.95,
                                   type = c("basic", "norm","perc"))

               output_ci <- cbind (fit_data[x,],
                                   data.frame(ci_inf = ci$percent[,c(4)],
                                              ci_sup = ci$percent[,c(5)]))

               return(output_ci)
             }))
             )
    } else {
      ifelse(!("fmod" %in% colnames(fit_data)), #is.null(fit_data$fmod),
             # nominal catch total by CWP
             output <- do.call(rbind,lapply(seq.int(1, nrow(fit_data)), function (x){
               tmp <- t3:::df2boot(t0 = fit_data$catch_set_fit[x] ,
                                   t = as.matrix(boot_data$catch_set_fit[boot_data$sp == fit_data$sp[x] &
                                                                           boot_data$cwp == fit_data$cwp[x] &
                                                                           boot_data$mon == fit_data$mon[x]]), # bootstrap values)
                                   R = max(boot_data$loop) # number of loop
               )
               ci <- boot::boot.ci(tmp,
                                   conf = 0.95,
                                   type = c("basic", "norm","perc"))

               output_ci <- cbind (fit_data[x,],
                                   data.frame(ci_inf = ci$percent[,c(4)],
                                              ci_sup = ci$percent[,c(5)]))

               return(output_ci)
             })),
             # nominal catch total by CWP and fishing mode
             output <- do.call(rbind,lapply(seq.int(1, nrow(fit_data)), function (x){
               tmp <- t3:::df2boot(t0 = fit_data$catch_set_fit[x] ,
                                   t = as.matrix(boot_data$catch_set_fit[boot_data$sp == fit_data$sp[x] &
                                                                           boot_data$fmod == fit_data$fmod[x] &
                                                                           boot_data$cwp == fit_data$cwp[x] &
                                                                           boot_data$mon == fit_data$mon[x]]), # bootstrap values)
                                   R = max(boot_data$loop) # number of loop
               )
               ci <- boot::boot.ci(tmp,
                                   conf = 0.95,
                                   type = c("basic", "norm","perc"))

               output_ci <- cbind (fit_data[x,],
                                   data.frame(ci_inf = ci$percent[,c(4)],
                                              ci_sup = ci$percent[,c(5)]))

               return(output_ci)
             }))
      )
    }
  }
  return(output)
}

