library(eyetrackSim)
library(bdots)
library(ggplot2)


sim_fixed <- runSim(nsub = 10, ntrials = 300,
               fnct = "logistic", fbst = FALSE,
               sacDelay = 0)

sim_random <- runSim(nsub = 1000, ntrials = 300,
               fnct = "logistic", fbst = FALSE,
               sacDelay = NULL)


fit_fix_fixed <- bdotsFit(sim_fixed$trialData,
                      subject = "id",
                      time = "times",
                      y = "looks",
                      group = "group",
                      curveType = logistic(params =
                                             c(mini = 0, peak = 1,
                                               slope = .002, cross = 750)))


fit_sac_fixed <- bdotsFit(sim_fixed$fixations,
                          subject = "id",
                          time = "starttime",
                          y = "looks",
                          group = "group",
                          curveType = logistic(params =
                                                 c(mini = 0, peak = 1,
                                                   slope = .002, cross = 750)))



fit_fix_random <- bdotsFit(sim_random$trialData,
                      subject = "id",
                      time = "times",
                      y = "looks",
                      group = "group",
                      curveType = logistic(params =
                                             c(mini = 0, peak = 1,
                                               slope = .002, cross = 750)))

# Im a fucking retard and did this with wrong function goddamnit
fit_sac_random <- bdotsFit(sim_random$fixations,
                          subject = "id",
                          time = "starttime",
                          y = "looks",
                          group = "group",
                          curveType = logistic(params =
                                                 c(mini = 0, peak = 1,
                                                   slope = .002, cross = 750)))


save.image()
#load("index.RData")
#
#
# ## sim to be subset
# # idx to keep (as id)
ss <- sim_fixed
rr <- coef(fit_sac_fixed2)
idx <- which(rr[,4] == 0 | rr[,2] < rr[,1] | rr[,3] < 0)
idx <- setdiff(1:1000, idx)
subsetSim <- function(ss, idx) {
  ss$trialData <- ss$trialData[id %in% idx, ]
  ss$fixations <- ss$fixations[id %in% idx, ]
  pp <- ss$subPars
  pp$pars <- pp$pars[id %in% idx, ]
  pp$em <- pp$em[id  %in% idx, ]
  pp$emT <- pp$emT[id  %in% idx, ]
  ss$subPars <- pp
  ss
}
ss2 <- subsetSim(ss, idx)


ss <- subsetSim(sim_fixed, idx)
ff <- fit_fix_fixed[idx, ]
getParBias <- function(ss, ff) {
  tp <- as.matrix(ss$subPars$pars[, 2:5])
  fp <- coef(ff)
  bb <- suppressWarnings(melt(as.data.table(tp-fp))) #obs bias
}

# Bias in added observation
bb <- getParBias(ss, ff)
ggplot(bb, aes(x = value)) + geom_histogram(bins=40) +
  geom_vline(xintercept = 0, color = 'red') +
  facet_wrap(~variable, scales = "free") +
  labs(y = "Number of Runs", x = "Bias") +
  theme_bw(base_size = 16) +
  ggtitle("Parameter Bias, Fixation Fixed Delay")


## Biasa with saccade
bb2 <- getParBias(ss, fit_sac_fixed2[idx, ])
ggplot(bb2, aes(x = value)) + geom_histogram(bins=40) +
  geom_vline(xintercept = 0, color = 'red') +
  facet_wrap(~variable, scales = "free") +
  labs(y = "Number of Runs", x = "Bias") +
  theme_bw(base_size = 16) +
  ggtitle("Parameter Bias, Saccade Fixed Delay")


## Bias with random delay
## But i also fucked up fitting this (goddamnit)
rr <- coef(fit_sac_random)
idx <- which(rr[,4] == 0 | rr[,2] < rr[,1] | rr[,3] < 0)
idx <- setdiff(1:1000, idx)

ss <- subsetSim(sim_random, idx)
ff <- fit_fix_random[idx, ]
bb3 <- getParBias(ss, ff)
ggplot(bb3, aes(x = value)) + geom_histogram(bins=40) +
  geom_vline(xintercept = 0, color = 'red') +
  facet_wrap(~variable, scales = "free") +
  labs(y = "Number of Runs", x = "Bias") +
  theme_bw(base_size = 16) +
  ggtitle("Parameter Bias, Fixation Random Delay")



ss <- subsetSim(sim_random, idx)
ff <- fit_sac_random[idx, ]
bb4 <- getParBias(ss, ff)
ggplot(bb4, aes(x = value)) + geom_histogram(bins=40) +
  geom_vline(xintercept = 0, color = 'red') +
  facet_wrap(~variable, scales = "free") +
  labs(y = "Number of Runs", x = "Bias") +
  theme_bw(base_size = 16) +
  ggtitle("Parameter Bias, Saccade Random Delay")

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

    return(c(mini = 0, peak = 1, slope = .002, cross = 750))
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
