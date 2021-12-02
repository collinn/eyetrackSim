
library(eyetrackSim)
library(bdots)

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

  if (fnx == "logistic") {
    fits <- bdotsFit(data = dat,
                     y = "looks",
                     time = "times",
                     subject = "id",
                     group = "group",
                     curveType = logistic(),
                     cores = detectCores() - 1L)
  } else if (fnx == "linear") {
    fits <- bdotsFit(data = dat,
                     y = "looks",
                     time = "times",
                     subject = "id",
                     group = "group",
                     curveType = linear(),
                     cores = detectCores() - 1L)
  }


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

plotY <- function(idx, mm = NULL) {
  oldpar <- par(mfrow = c(2,1))
  on.exit(par(oldpar))
  plot(times, fity[, idx], type = 'l', ylim = c(0, 1), lwd = 2,
       main = paste0("Subject ", idx, "\n", mm))
  lines(times, simy[, idx], type = 'l', col = 'steelblue', lwd = 2)
  legend(x = 1300, y = 0.4, legend = c("Underlying", "bdots"),
         col = c("steelblue", "black"), lty = 1, lwd = 2)
  fix <- sim_l$fixations[id == idx & starttime != 0, ]
  plot(density(fix$starttime), main = "saccade density")
}

sim_l <- runSim(nsub = 2, ntrials = 300, fnct = "logistic", fbst = FALSE)

fits <- getFits(sim_l)

sim_coef <- getSimCoef(sim_l, fits)
fit_coef <- coef(fits)

## Consider now the *actual* curves
fity <- sapply(split(fits, by = "id"), function(x) {
  fitted(x[['fit']][[1]])
})
simy <- apply(sim_coef, 1, logistic_f, t = times)

plotY(2, mm = "noshift")


## time shift
td <- copy(sim_l$trialData)
#td <- td[id == 1, ]

nsub <- length(unique(td$id))
N <- 50
bb <- 4
maxt <- max(td$times)
val <- td[times == maxt, looks]
padt <- seq(from = maxt+bb, to = maxt + bb*N, by = bb)

add_dt <- data.table(
  id = rep(unique(td$id), each = length(padt)),
  times = rep(padt, times = nsub),
  looks = rep(val, each = length(padt)))

new_dt <- rbind(td, add_dt)

new_dt[,  times := shift(times, n = 50, "lead"), by = .(id)]
new_dt <- new_dt[!is.na(times), ]
setorder(new_dt)



sim_ln <- copy(sim_l)
sim_ln$trialData <- new_dt

fitsn <- getFits(sim_ln)

sim_coefn <- getSimCoef(sim_ln, fitsn)
fit_coefn <- coef(fitsn)

## Consider now the *actual* curves
fityn <- sapply(split(fitsn, by = "id"), function(x) {
  fitted(x[['fit']][[1]])
})
simyn <- apply(sim_coefn, 1, logistic_f, t = times)


plotYn <- function(idx, mm = NULL) {
  oldpar <- par(mfrow = c(2,1))
  on.exit(par(oldpar))
  plot(times, fityn[, idx], type = 'l', ylim = c(0, 1), lwd = 2,
       main = paste0("Subject ", idx, "\n", mm))
  lines(times, simyn[, idx], type = 'l', col = 'steelblue', lwd = 2)
  legend(x = 1300, y = 0.4, legend = c("Underlying", "bdots"),
         col = c("steelblue", "black"), lty = 1, lwd = 2)
  fix <- sim_ln$fixations[id == idx & starttime != 0, ]
  plot(density(fix$starttime), main = "saccade density")
}

plotYn(2, mm = "shift")
