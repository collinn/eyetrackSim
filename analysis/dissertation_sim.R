library(eyetrackSim)
library(bdots)

logistic2 <- function(dat, y, time, params = NULL, ...) {

  logisticPars <- function(dat, y, time, ...) {
    time <- dat[[time]]
    y <- dat[[y]]

    # idx <- order(time)
    # time <- time[idx]
    # y <- y[idx]

    ## Remove cases with zero variance
    if (var(y) == 0) {
      return(NULL)
    }

    ## Starting estimates based on thing
    mini <- 0
    peak <- 1
    cross <- 750
    slope <- 0.002

    return(c(mini = mini, peak = peak, slope = slope, cross = cross))
  }

  if (is.null(params)) {
    params <- logisticPars(dat, y, time)
  } else {
    if (length(params) != 4) stop("logistic requires 4 parameters be specified for refitting")
    if (!all(names(params) %in% c("mini", "peak", "slope", "cross"))) {
      stop("logistic parameters for refitting must be correctly labeled")
    }
  }
  ## Return NA list if var(y) is 0
  if (is.null(params)) {
    return(NULL)
  }
  y <- str2lang(y)
  time <- str2lang(time)
  ff <- bquote(.(y) ~ mini + (peak - mini) / (1 + exp(4 * slope * (cross - (.(time))) / (peak - mini))))
  attr(ff, "parnames") <- names(params)
  return(list(formula = ff, params = params))
}



sim_fixed <- runSim(nsub = 1000, ntrials = 300,
               fnct = "logistic", fbst = FALSE,
               sacDelay = 0)

cat("1")

sim_random <- runSim(nsub = 1000, ntrials = 300,
               fnct = "logistic", fbst = FALSE,
               sacDelay = NULL)

cat("2")

save.image()

fit_fix_fixed <- bdotsFit(sim_fixed$trialData,
                      subject = "id",
                      time = "times",
                      y = "looks",
                      group = "group",
                      curveType = logistic())

cat("3")

fit_sac_fixed <- bdotsFit(sim_fixed$fixations,
                          subject = "id",
                          time = "starttime",
                          y = "looks",
                          group = "group",
                          curveType = logistic())

cat("3.5")

save.image()

fit_sac_fixed2 <- bdotsFit(sim_fixed$fixations,
                          subject = "id",
                          time = "starttime",
                          y = "looks",
                          group = "group",
                          curveType = logistic2())

save.image()

cat("4")

fit_fix_random <- bdotsFit(sim_random$trialData,
                      subject = "id",
                      time = "times",
                      y = "looks",
                      group = "group",
                      curveType = logistic())
cat("5")

fit_sac_random <- bdotsFit(sim_random$fixations,
                          subject = "id",
                          time = "starttime",
                          y = "looks",
                          group = "group",
                          curveType = logistic())

cat("6")

save.image()
# load(".RData")
#
#
# ## sim to be subset
# # idx to keep (as id)
# # subsetSim <- function(ss, idx) {
# #
# # }
#
# ## Need to check  fitcodes first but ok here is fine
#
# ss <- sim_fixed
# ff <- fit_fix_fixed
# getParBias <- function(ss, ff) {
#   tp <- as.matrix(ss$subPars$pars[, 2:5])
#   fp <- coef(ff)
#   bb <- suppressWarnings(melt(as.data.table(tp-fp))) #obs bias
# }
# library(ggplot2)
# ggplot(bb, aes(x = value)) + geom_histogram(bins=40) +
#   geom_vline(xintercept = 0, color = 'red') +
#   facet_wrap(~variable, scales = "free") +
#   theme_bw(base_size = 16)
#
# mm <- coef(fit_sac_fixed)
# idx <- mm[, 4] != 0 & mm[,2] > mm[,1]
#
# ss <-
# bb2 <- getParBias(sim_fixed, fit_sac_fixed)
# bb3 <- getParBias(sim_fixed, fit_sac_fixed2)
#
# ggplot(bb3, aes(x = value)) + geom_histogram(bins=40) +
#   geom_vline(xintercept = 0, color = 'red') +
#   facet_wrap(~variable, scales = "free") +
#   theme_bw(base_size = 16)
#


############ New logistic
#' Logistic curve function for nlme (test use with saccade, NOT FOR GENERAL USE)
#'
#' Logistic function used in fitting nlme curve for observations
#'
#' @param dat subject data to be used
#' @param y outcome variable
#' @param time time variable
#' @param params \code{NULL} unless user wants to specify starting parameters for gnls
#' @param ... just in case
#'
#' @details \code{y ~ mini + (peak - mini) / (1 + exp(4 * slope * (cross - (time)) / (peak - mini)))}
#' @export
logistic2 <- function(dat, y, time, params = NULL, ...) {

  logisticPars <- function(dat, y, time, ...) {
    time <- dat[[time]]
    y <- dat[[y]]

    # idx <- order(time)
    # time <- time[idx]
    # y <- y[idx]

    ## Remove cases with zero variance
    if (var(y) == 0) {
      return(NULL)
    }

    ## Starting estimates based on thing
    mini <- 0
    peak <- 1
    cross <- 750
    slope <- 0.002

    return(c(mini = mini, peak = peak, slope = slope, cross = cross))
  }

  if (is.null(params)) {
    params <- logisticPars(dat, y, time)
  } else {
    if (length(params) != 4) stop("logistic requires 4 parameters be specified for refitting")
    if (!all(names(params) %in% c("mini", "peak", "slope", "cross"))) {
      stop("logistic parameters for refitting must be correctly labeled")
    }
  }
  ## Return NA list if var(y) is 0
  if (is.null(params)) {
    return(NULL)
  }
  y <- str2lang(y)
  time <- str2lang(time)
  ff <- bquote(.(y) ~ mini + (peak - mini) / (1 + exp(4 * slope * (cross - (.(time))) / (peak - mini))))
  attr(ff, "parnames") <- names(params)
  return(list(formula = ff, params = params))
}
