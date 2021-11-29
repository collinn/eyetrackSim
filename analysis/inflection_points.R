
# basically whats being done here is looking at difference between
# the fit and underlying curve, looking at the first and second derivatives
# and determining where our estimate and the true values differ the most

bfits <- sapply(split(fits, by = "id"), function(x) {
  fitted(x[['fit']][[1]])
})


sim_coef <- sim_coef[, c(1,2,4,3)]
ofits <- apply(sim_coef, 1, logistic_f, t = times)


par(mfrow = c(2, 1))

idx <- 9
plot(times, ofits[, idx], type = 'l', ylim = c(0, 1), main = 'red is bdots, black is true')
lines(times, bfits[, idx], type = 'l', col = 'red')
abline(v=824, col = 'blue')
abline(v = 731.4, col = 'green')
abline(v = 917.5, col = 'green')

fix <- ss$fixations[id == idx, ]
hist(fix$starttime)

# 2nd derivative of logistic
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

simp <- sim_coef[idx, ]
fitp <- fit_coef[idx, ]
uniroot(logistic_dd, interval = c(0, 1000), p = simp) # inflection point
optimize(logistic_dd,interval = c(0, 750), p =simp, maximum = TRUE)
optimize(logistic_dd,interval = c(800, 2000), p =simp, maximum = FALSE)


nfits <- ofits[, idx] -bfits[, idx]
plot(times, nfits, type = 'l')

times[which.min((nfits))]
times[which.max((nfits))]

plot(times, cumsum(nfits), main = 'cumsum')
plot(times, nfits, main = 'true-bdots')
