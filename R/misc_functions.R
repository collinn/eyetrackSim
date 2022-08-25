#######################
### Curve Functions ###
#######################

## Logistic function
#' @export
logistic_f <- function(p, t) {
  b0 <- p[1] # base
  b1 <- p[2] # max
  sl <- p[3] # slope
  xo <- p[4] # crossover
  b0 + (b1-b0) / (1 + exp(4*sl*((xo-t)/(b1-b0))))
}

#' @export
doubleGauss_f <- function(p, t) {
  mu <- p[1]
  ht <- p[2]
  s1 <- p[3]
  s2 <- p[4]
  b1 <- p[5]
  b2 <- p[6]

  lhs <- (t < mu) * ((ht-b1) * exp((t - mu)^2/(-2*s1^2)) + b1)
  rhs <- (t >= mu) * ((ht-b2) * exp((t - mu)^2/(-2*s2^2)) + b2)
  lhs+rhs
}

#' @export
linear_f <- function(p, t) {
  b <- p[1]
  m <- p[2]
  m*t + b
}




## Functions below now exist elsewhere

# makeSubject <- function(fnct = "logistic") {
#
#   if (fnct == "logistic") {
#     bb <- baseParams[fn == 1, ]
#     fn <- logistic
#   } else {
#     bb <- baseParams[fn == 2, ]
#     fn <- doubleGauss
#   }
#
#   ## Parameters for the curve, right now, just logistic
#   subPars <- vector("numeric", nrow(bb))
#   subPars[] <- Inf
#   maxFix <- 2
#   while (maxFix > 1) {
#     while (any(bb[, subPars <= min | subPars >= max ])) {
#       ## Missed opportunity here to use correlation maybe mvtnorm?
#       subPars <- bb[, rnorm(nrow(bb))*sd + mean]
#     }
#     maxFix <- max(fn(subPars, times))
#   }
#
#
#   ### This portion below independent of fixation curve
#   ## Eye movement parameters
#   # bw subject mean / bw sub sd / mean trial x trial sd wn sub / sd of trial x sd of sub
#   baseEMparams <- matrix(c(204.73, 32.63, 96.55, 24.57,
#                            360.28, 65.78, 195.11, 30.04),
#                          byrow = TRUE, ncol = 4)
#
#
#   ## For eye movement, i.e., eyemovementSubject (missing FBS+T)
#   emSub <- vector("numeric", 2L)
#   emSub[1] <- rgamma(1, shape = baseEMparams[1,1]^2 / baseEMparams[1,2]^2,
#                      scale = baseEMparams[1,2]^2/baseEMparams[1,1])
#   emSub[2] <- rgamma(1, shape = baseEMparams[1,3]^2 / baseEMparams[1,4]^2,
#                      scale = baseEMparams[1,4]^2/baseEMparams[1,3])
#
#   ## For FBS+T (i.e., these are target looks, which are slightly longer)
#   emSubT <- vector("numeric", 2L)
#   emSubT[1] <- rgamma(1, shape = baseEMparams[2,1]^2 / baseEMparams[2,2]^2,
#                       scale = baseEMparams[2,2]^2/baseEMparams[2,1])
#   emSubT[2] <- rgamma(1, shape = baseEMparams[2,3]^2 / baseEMparams[2,4]^2,
#                       scale = baseEMparams[2,4]^2/baseEMparams[2,3])
#
#   return(list(pars = subPars, em = emSub, emT = emSubT, fn = fnct))
# }
#

#' containing information on subject, as well as trial data
# runSub <- function(fnct = "logistic", ntrials = 300, fbst = FALSE) {
#
#   ## Set up parameter stuff for subject
#   subInfo <- makeSubject(fnct)
#   pars <- subInfo$pars
#   em <- subInfo$em
#   emT <- subInfo$emT # for target
#   rg <- function() rgamma(1, em[1]^2/em[2]^2, scale = em[2]^2/em[1])
#   rgT <- function() rgamma(1, emT[1]^2/emT[2]^2, scale = emT[2]^2/emT[1])
#
#   ## Assign curve fitting function
#   if (fnct == "logistic") {
#     fn <- logistic
#   } else {
#     fn <- doubleGauss
#   }
#
#   trialDataList <- vector("list", length = ntrials)
#
#   ## Go through trials
#   for (i in seq_len(ntrials)) {
#     trialdata <- data.table(trial = i,
#                             times = times,
#                             looks = 0L * times[],
#                             saccade = 0L)
#
#     ## Step 1 of looks
#     curtime <- min(times) - runif(1)*em[1] # current time
#     lasttime <- curtime - rg() - runif(1)*em[1]
#     while (curtime < max(times)) {
#       currProb <- fn(pars, lasttime)
#       targ <- runif(1) <= currProb
#
#       ## Duration depends on looking at target or not
#       currdur <- ifelse(fbst & targ, rg(), rgT())
#
#       ## update trial data
#       idx <- which(times >= curtime & times <= curtime + currdur)
#       trialdata[idx, looks := targ]
#       sac_idx <- which.min(abs(times - curtime)) # time less current closest to zero
#       trialdata[sac_idx, saccade := 1]
#       lasttime <- curtime
#       curtime <- curtime + currdur
#     }
#     ## this first index is actually wrong
#     trialdata[1, saccade := 0]
#     trialDataList[[i]] <- trialdata
#   }
#   ## Aggregate to single DT
#   tt <- rbindlist(trialDataList)
#   return(list(subInfo = subInfo, trialData = tt))
# }









#################
### Constants ###
#################

# ## These are base parameters (from matlab code)
# # 1 (logistic) -- min / max / xo / slove
# # 2 (double gauss) -- mu / ht / s1 / s2 / b1 / b2
# baseParams <- structure(list(fn = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L),
#                              param = c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 5L, 6L),
#                              mean = c(0.115, 0.885, 765, 0.0016, 630, 0.18, 130, 250, 0.05, 0.05),
#                              sd = c(0.12, 0.12, 85, 0.00075, 77, 0.05, 30, 120, 0.015, 0.015),
#                              min = c(0, 0.5, 300, 0.0009, 300, 0.05, 50, 50, 0, 0),
#                              max = c(0.3, 1, 1100, 0.01, 1300, 0.35, 250, 400, 0.15, 0.15)),
#                         row.names = c(NA, -10L),
#                         class = c("data.table", "data.frame"))
# colnames(baseParams) <- c("fn", "param", "mean", "sd", "min", "max")
#
