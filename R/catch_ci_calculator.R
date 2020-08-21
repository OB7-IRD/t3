#' @name catch_ci_calculator
#' @title Compute final confidence interval
#' @description Compute final confidence interval
#' @param to The observed value of statistic applied to data
#' @param t A matrix with sum(R) rows each of which is a bootstrap replicate of the result of calling statistic.
#' @return

# source(file = file.path(root_path, "function", "df2boot.R", fsep = "\\")) # bootstrap function


catch_ci_calculator <- function(fit_data,
                                boot_data){
  output <- do.call(rbind,lapply(seq.int(1, nrow(fit_data)), function (x){
    # nomicatch or catch effort
    if(is.null(fit_data$CWP)){
    # by fishing mode or not

    ifelse(is.null(fit_data$fmod),
      # nomial catch total
      tmp <- t3:::df2boot(t0 = fit_data$catch_set_fit[x] ,
                     t = as.matrix(boot_data$catch_set_fit[boot_data$sp == fit_data$sp[x]]), # bootstrap values)
                     R = max(boot_data$loop) # number of loop
                    ),
      # nominal catch by fishing mode
      tmp <- t3:::df2boot(t0 = fit_data$catch_set_fit[x] ,
                   t = as.matrix(boot_data$catch_set_fit[boot_data$sp == fit_data$sp[x] &
                                                    boot_data$fmod == fit_data$fmod[x]]), # bootstrap values)
                   R = max(boot_data$loop) # number of loop
                   )
    )
  } else {
    ifelse(is.null(fit_data$fmod),
      # nominal catch total by CWP
      tmp <- t3:::df2boot(t0 = fit_data$catch_set_fit[x] ,
                     t = as.matrix(boot_data$catch_set_fit[boot_data$sp == fit_data$sp[x] &
                                                        boot_data$CWP == fit_data$CWP[x]]), # bootstrap values)
                     R = max(boot_data$loop) # number of loop
      ),
      # nominal catch total by CWP and fishing mode
      tmp <- t3:::df2boot(t0 = fit_data$catch_set_fit[x] ,
                     t = as.matrix(boot_data$catch_set_fit[boot_data$sp == fit_data$sp[x] &
                                                        boot_data$fmod == fit_data$fmod[x] &
                                                        boot_data$CWP == fit_data$CWP[x]]), # bootstrap values)
                     R = max(boot_data$loop) # number of loop
      )
    )
  }
    ci <- boot::boot.ci(tmp,
                        conf = 0.95,
                        type = c("basic", "norm","perc"))

    output_ci <- cbind (fit_data[x,],
                        data.frame(ci_inf = ci$percent[,c(4)],
                                   ci_sup = ci$percent[,c(5)]))

    return(output_ci)
   }
  )
)
return(output)
  }

