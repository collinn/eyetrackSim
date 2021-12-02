
library(eyetrackSim)
library(bdots)

sim_l <- runSim(nsub = 10L, fnct = "logistic", fbst = TRUE)
sim_l <- runSim_fixed(nsub = 10L, fnct = "logistic", fbst = TRUE)

## Get bdot fits from simulation
# returns bdots object
getFits <- function(ss) {
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

  ## Remove bad fits
  idx <- sapply(split(fits, by = "id"), function(x) {
    if (x$fitCode == 6) return(FALSE)
    obj <- x[["fit"]][[1]]
    fv <- fitted(obj)
    res <- residuals(obj)
    cor(fv, fv+res) > 0.8
  })

  fits <- fits[idx, ]
}

## Coefficient matrix for simulation, based on valid fits
# returns matrix
getSimCoef <- function(ss, fits) {
  ss <- copy(ss)
  sim_coef <- ss$subPars$pars
  sim_coef <- sim_coef[id %in% fits$id, ]
  sim_coef[, id := NULL]
  sim_coef <- as.matrix(sim_coef)
}

## These functions are first and second derivatives of logistic
logistic_d <- function(t, p) {
  b0 <- p[1] # base
  b1 <- p[2] # max
  sl <- p[3] # slope
  xo <- p[4] # crossover
  r <- b1-b0
  th <- 4 * sl/r
  ee <- exp(th*(xo-t))

  nm <- th*r*ee
  dn <- (ee + 1)^2
  nm/dm
}
logistic_dd <- function(t, p) {
  b0 <- p[1] # base
  b1 <- p[2] # max
  sl <- p[3] # slope
  xo <- p[4] # crossover
  r <- b1-b0
  th <- 4 * sl/r
  ee <- exp(th*(xo-t))

  p1 <- th^2 * r * ee
  p2 <- ee - 1
  dd <- (ee + 1)^3

  (p1*p2)/dd
}

fits <- getFits(sim_l)

sim_coef <- getSimCoef(sim_l, fits)
fit_coef <- coef(fits)

## Consider now the *actual* curves
fity <- sapply(split(fits, by = "id"), function(x) {
  fitted(x[['fit']][[1]])
})
simy <- apply(sim_coef, 1, logistic_f, t = times)

## Some funtionals to help us with this analysis
plotY <- function(idx) {
  oldpar <- par(mfrow = c(2,1))
  on.exit(par(oldpar))
  plot(times, fity[, idx], type = 'l', ylim = c(0, 1), lwd = 2,
       main = paste0("Subject ", idx))
  lines(times, simy[, idx], type = 'l', col = 'steelblue', lwd = 2)
  legend(x = 1300, y = 0.4, legend = c("Underlying", "bdots"),
         col = c("steelblue", "black"), lty = 1, lwd = 2)
  fix <- sim_l$fixations[id == idx & starttime != 0, ]
  plot(density(fix$starttime), main = "saccade density")
}

## Look at density of saccades vs horizontal shift
# consider running simulation where I choose fixation points
# for fuller coverage
plotY(3)
plotY(4) # iffy
plotY(5)
plotY(6) # interesting case
plotY(7)
plotY(8)
plotY(9)
plotY(10)

#
plotY(1)
plotY(2)


#### Same with fixed density
sim_l <- runSim_fixed(nsub = 10L, fnct = "logistic", fbst = TRUE)

## Really, need a function that will take the bdots fitted values and
# make oculomotor adjustment, padding with zero or max values where necessary
# just so that we can make a clearer distinction b/w horizontal shift
