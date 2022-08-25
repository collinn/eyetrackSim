

## Questions here:
# 1. Asymptotically, does fbst matter?
# 2. What does my samp density look like with fbst multiplier

## Check the asymptotic question
sim_1 <- runSim(nsub = 1L, ntrials = 300, fnct = "logistic", fbst = FALSE)
sim_2 <- runSim(nsub = 1L, ntrials = 300, fnct = "logistic", fbst = TRUE)

sim_1a <- runSim(nsub = 1L, ntrials = 1e4, fnct = "logistic", fbst = FALSE)
sim_2a <- runSim(nsub = 1L, ntrials = 1e4, fnct = "logistic", fbst = TRUE)

sims <- list(sim_1, sim_2, sim_1a, sim_2a)
fits <- lapply(sims, getFits)
sim_coef <- Map(getSimCoef, sims, fits)

fit_coef <- lapply(fits, coef)
fity <- lapply(fits, function(y) {
  sapply(split(y, by = "id"), function(x) {
    fitted(x[['fit']][[1]])
  })
})

simy <- lapply(sim_coef, function(x) {
  apply(x, 1, logistic_f, t = times)
})
beepr::beep(1)

plotYa <- function(idx, mm = NULL) {
  oldpar <- par(mfrow = c(2,1))
  on.exit(par(oldpar))
  plot(times, fity[[idx]], type = 'l', ylim = c(0, 1), lwd = 2,
       main = paste0("Subject ", idx, "\n", mm))
  lines(times, simy[[idx]], type = 'l', col = 'steelblue', lwd = 2)
  legend(x = 1300, y = 0.4, legend = c("Underlying", "bdots"),
         col = c("steelblue", "black"), lty = 1, lwd = 2)
  fix <- sims[[idx]]$fixations[starttime != 0, ]
  plot(density(fix$starttime), main = "saccade density")
}


plotYa(1,"300,fbst false")
plotYa(2,"300,fbst true")
plotYa(3,"10000,fbst false")
plotYa(4,"10000,fbst true")

## Check fbst multiplier
sim_l <- runSim_fixed(nsub = 2L, fnct = "logistic", sampDensity = 10L, fbst = TRUE, targMult = 5L)
sim_l2 <- runSim_fixed(nsub = 2L, fnct = "logistic", sampDensity = 10L, fbst = TRUE, targMult = 1L)

fits <- getFits(sim_l)
fits2 <- getFits(sim_l2)

sim_coef <- getSimCoef(sim_l, fits)
fit_coef <- coef(fits)
sim_coef2 <- getSimCoef(sim_l2, fits2)
fit_coef2 <- coef(fits2)

## Consider now the *actual* curves
fity <- sapply(split(fits, by = "id"), function(x) {
  fitted(x[['fit']][[1]])
})
simy <- apply(sim_coef, 1, logistic_f, t = times)
fity2 <- sapply(split(fits2, by = "id"), function(x) {
  fitted(x[['fit']][[1]])
})
simy2 <- apply(sim_coef2, 1, logistic_f, t = times)


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
plotY2 <- function(idx, mm = NULL) {
  oldpar <- par(mfrow = c(2,1))
  on.exit(par(oldpar))
  plot(times, fity2[, idx], type = 'l', ylim = c(0, 1), lwd = 2,
       main = paste0("Subject ", idx, "\n", mm))
  lines(times, simy2[, idx], type = 'l', col = 'steelblue', lwd = 2)
  legend(x = 1300, y = 0.4, legend = c("Underlying", "bdots"),
         col = c("steelblue", "black"), lty = 1, lwd = 2)
  fix <- sim_l2$fixations[id == idx & starttime != 0, ]
  plot(density(fix$starttime), main = "saccade density")
}

plotY(1, "fbst")
plotY(2, "fbst")
plotY2(1, "no fbst")
plotY2(2, "no fbst")
