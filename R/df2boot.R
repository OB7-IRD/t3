#' @name df2boot
#' @title Translate Data frame to boot class
#' @description Function to translate data frame to boot class (boot package) for ci computation
#' @param t0 The observed value of statistic applied to data
#' @param t A matrix with sum(R) rows each of which is a bootstrap replicate of the result of calling statistic.
#' @param R The number of bootstrap replicates. Usually this will be a single positive integer.
#' @param sample_data The sample data
#' @param strata An integer vector or factor specifying the strata for multi-sample problems. This may be specified for any simulation, but is ignored when sim = "parametric". When strata is supplied for a nonparametric bootstrap, the simulations are done within the specified strata
#' @importFrom stats rnorm
df2boot <- function(t0,
                    t,
                    R,
                    sample_data,
                    strata) {
  # if (missing(sample_data)) {
  sample_data <- stats::rnorm(100,
                              mean = t0,
                              sd = 1*abs(t0))
  # }
   # if (missing(strata)) {
    strata <- rep(1,
                  times = length(sample_data))
    # }

  boot_obj_tmp <- vector("list", length = 11)
  names(boot_obj_tmp) <- c("t0",
                           "t",
                           "R",
                           "data",
                           "seed",
                           "statistic",
                           "sim", "call",
                           "stype",
                           "strata",
                           "weights")
  class(boot_obj_tmp) <- "boot"
  boot_obj_tmp$t0 <- t0
  boot_obj_tmp$t <- t # Bootstrap values
  boot_obj_tmp$R <- R # number of loop
  boot_obj_tmp$data <- sample_data # sample data
  boot_obj_tmp$seed <- .Random.seed
  boot_obj_tmp$sim <- "ordinary"
  boot_obj_tmp$stype <- "i"
  boot_obj_tmp$strata <- strata # same length as the sample
  boot_obj_tmp$weights <- 1 # not used

  return(boot_obj_tmp)
}
