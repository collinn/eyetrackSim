
# ## Replicating for logistic eyetrack (princess bride)
# times <- seq(0, 2000, by = 4)
# oculomotordelay <- 200
#
# test <- runSub(fbst = TRUE)
# plot(aggregateSub(test))
#
# ## Try to recover start times
# tt <- test$trialData
# tt[, test:= saccadenum - shift(saccadenum)]
# tt[, `:=`(starttime = min(times), endtime = max(times)), by = .(trial, saccadenum)]
#
# rr <- copy(tt)
# rr[, `:=`(times = NULL, looks = NULL, test = NULL)]
# rr <- unique(rr)
#
#
#
#
# dat <- aggregateSub(test)
# dat$subject <- 1
# dat$group <- "a"
#
# fit <- bdotsFit(dat, subject = "subject",
#                 time = "times",
#                 y = "looks",
#                 group = "group",
#                 curveType = logistic())
#
# orig_p <- test$subInfo$pars
# fit_p <- coef(fit)[c(1,2,4,3)]
#
# orig_l <- logistic(orig_p, times)
# fit_l <- logistic(fit_p, times)
# looks <- dat$looks
#
# plot(times, orig_l, col = 'red', lwd = 2, type = 'l', ylim = c(0, 1))
# lines(times, looks, col = 'blue', lwd = 2)
# lines(times, fit_l, col = 'green', lwd = 2)

#
# tt <- runSim(nsub = 1)
#
# fix <- tt$fixations
# td <- tt$trialData
#
# tt <- runSub()
# fix <- buildSaccadeSub(tt)
# td <- aggregateSub(tt)
#
# hist(fix$dur)
#
# histogram(~starttime | as.character(saccadenum), data = fix)
#
# sacs <- fix[, max(saccadenum), by = trial]$V1
#
# tt2 <- runSub(fbst = TRUE)
# fix2 <- buildSaccadeSub(tt2)
# td2 <- aggregateSub(tt2)

## Trying running simulation
library(eyetrackSim)
## fbs, ntrials = 300, logitic
# about 53 minutes
system.time({
  #sim <- runSim(nsub = 1000L)
  #sim_fbst <- runSim(nsub = 1000L, fbst = TRUE)

  sim_co <- runSim(nsub = 1000L, fnct = "doubleGauss")
  sim_co_fbst <- runSim(nsub = 1000L, fbst = TRUE, fnct = "doubleGauss")
})


sim_l <- runSim(nsub = 10L, fnct = "logistic", fbst = TRUE)

save.image(file = "trialSim_dg.RData")
#load("trialSim.RData")
#
# sim <- runSim(nsub = 750L)
ss <- copy(sim_l)

## Basically reproduce bob's table for each
pbcheck <- function(ss) {

  ss <- copy(ss)
  fnx <- ss$subPars$fn
  if (fnx == "logistic") {
    fn <- bdots::logistic
  } else if (fnx == "doubleGauss") {
    fn <- bdots::doubleGauss2
  } else if (fnx == "linear") {
    fn <- bdots::linear
  }

  dat <- copy(ss$trialData)
  dat[, group := "grp"]

  if (fnx == "doubleGauss") {
    test <- split(dat, by = "id")
    badid <- vector("numeric", length = 1L)
    for (i in seq_along(test)) {
      rr <-  bdots:::dgaussPars(test[[i]], "looks", "times", TRUE)
      if (length(rr) != 6) badid <- c(badid, i)
    }
    if (length(badid) > 1) {
      bad <- TRUE
      badid <- badid[2:length(badid)]
      dat <- dat[!(id %in% badid), ]
    }
  }


  fits <- bdotsFit(data = dat,
                   y = "looks",
                   time = "times",
                   subject = "id",
                   group = "group",
                   curveType = fn(),
                   cores = detectCores() - 1L)



  hmm <- sapply(split(fits, by = "id"), function(x) {
    if (x$fitCode == 6) return(FALSE)
    obj <- x[["fit"]][[1]]
    fv <- fitted(obj)
    res <- residuals(obj)
    cor(fv, fv+res) > 0.8
  })

  fits <- fits[hmm, ]


  # fits <- bdotsFit(df,
  #                  y = "looks",
  #                  time = "time",
  #                  subject = "id",
  #                  group = "group",
  #                  curveType = cc(),
  #                  cores = 1L)

  fits <- fits[fitCode %in% c(0,1,3), ]

  fit_coef <- coef(fits)
  sim_coef <- ss$subPars$pars
  sim_coef <- sim_coef[id %in% fits$id, ]



  ## Clean up
  sim_coef[, id := NULL]
  sim_coef <- as.matrix(sim_coef)

  ## This is just to match what is presented in the paper
  # if (fnx == "logistic") {
  #   bob_idx <- c("mini", "peak", "cross", "slope")
  #   sim_coef <- sim_coef[, bob_idx]
  #   fit_coef <- fit_coef[, bob_idx]
  # }


  colMeans(sim_coef)
  colMeans(fit_coef)
  diag(cor(sim_coef, fit_coef))
  cor(fit_coef)

  return(list(simcoef = sim_coef, fitcoef = fit_coef, fits = fits,
              nvalid = sum(hmm)))
}

sims <- list(sim, sim_fbst, sim_co, sim_co_fbst)

sims <- vector("list", 4)
sims[[1]] <- pbcheck(sim)
sims[[2]] <- pbcheck(sim_co)
sims[[3]] <- pbcheck(sim_co)



tt <- runSim_fixed()
