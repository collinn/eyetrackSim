# goal is to compare results of sampling in different time windows
# it looks like higher density *early* sampling provides better results!
# that is, high density early with time adjustment is best
# hypothesis: exact equality on any continuous window == identical functions
# so high density sampling at less variability (early times) leads to more
# accurate representation  of true function at the relevent points.
# mid window has higher variability, so less precise with true function
# hence the estimate is further off

library(eyetrackSim)
library(bdots)


## Start with 1 to get parameters and curve

sim1 <- runSub_fixed(sampDensity = 200)

true_coef <- sim1$subInfo$pars

raw_ag <- eyetrackSim:::aggregateSub(sim1)
raw_ag[, `:=`(id = 1, gp = "A")]

## Now two more with different windows
sim2 <- runSub_fixed(sampDensity = 100, window = c(100, 400), windowDensity = 25,
                     pars = true_coef)
sim3 <- runSub_fixed(sampDensity = 100, window = c(500, 900), windowDensity = 25,
                     pars = true_coef)

ag2 <- eyetrackSim:::aggregateSub(sim2)
ag3 <- eyetrackSim:::aggregateSub(sim3)

ag2[, `:=`(id = 2, gp = "A")]
ag3[, `:=`(id = 3, gp = "A")]
ag4 <- copy(ag3)
ag4[, `:=`(id=4)]
dat <- rbind(raw_ag, ag2, ag3, ag4)

## Try adjustment to looks
dat[, adj_times := times - 200L]

fit <- bdotsFit(data = dat,
                   subject = "id",
                   group = "gp",
                   curveType = logistic(),
                   time = "times",
                   y = "looks",
                   cores = 7)
times <- unique(dat$times)
c1 <- eyetrackSim:::logistic_f(coef(fit)[1, ], times)
c2 <- eyetrackSim:::logistic_f(coef(fit)[2, ], times)
c3 <- eyetrackSim:::logistic_f(coef(fit)[3, ], times)
true_f <- eyetrackSim:::logistic_f(true_coef, times)

with(raw_ag[id == 1, ], plot(times, looks, lty = 2, col = 'orange', ylim = c(0, 1),
                             main = "without 200ms adjust"))
lines(times, c1, type = 'l', col = "steelblue", lwd = 2)
lines(times, c2, type = 'l', col = "red", lwd = 2)
lines(times, c3, type = 'l', col = "green", lwd = 2)
lines(times, true_f, type = 'l', col = "purple", lwd = 2)
legend(1250, 0.3, legend = c("obs data (aggregated)", "standard sample", "early samp", "mid samp", "true curve"),
       col = c("orange", "steelblue", "red", "green", "purple"),
       lty = 1)

### try with adjusted time

fit <- bdotsFit(data = dat,
                subject = "id",
                group = "gp",
                curveType = logistic(),
                time = "adj_times",
                y = "looks",
                cores = 7)
times <- unique(dat$times)
c1 <- eyetrackSim:::logistic_f(coef(fit)[1, ], times)
c2 <- eyetrackSim:::logistic_f(coef(fit)[2, ], times)
c3 <- eyetrackSim:::logistic_f(coef(fit)[3, ], times)
true_f <- eyetrackSim:::logistic_f(true_coef, times)

with(raw_ag[id == 1, ], plot(times, looks, lty = 2, col = 'orange', ylim = c(0, 1),
                             main = "with 200ms adjust"))
lines(times, c1, type = 'l', col = "steelblue", lwd = 2)
lines(times, c2, type = 'l', col = "red", lwd = 2)
lines(times, c3, type = 'l', col = "green", lwd = 2)
lines(times, true_f, type = 'l', col = "purple", lwd = 2)
legend(1250, 0.3, legend = c("obs data (aggregated)", "standard sample", "early samp", "mid samp", "true curve"),
       col = c("orange", "steelblue", "red", "green", "purple"),
       lty = 1)


##### Lets repeat above but check with saccades

sim1 <- runSub_fixed(sampDensity = 200)

true_coef <- sim1$subInfo$pars
actual_ag <- eyetrackSim:::aggregateSub(sim1)
raw_ag <- eyetrackSim:::buildSaccadeSub(sim1)
raw_ag[, `:=`(id = 1, gp = "A")]

## Now two more with different windows
sim2 <- runSub_fixed(sampDensity = 100, window = c(100, 400), windowDensity = 25,
                     pars = true_coef)
sim3 <- runSub_fixed(sampDensity = 100, window = c(500, 900), windowDensity = 25,
                     pars = true_coef)

ag2 <- eyetrackSim:::buildSaccadeSub(sim2)
ag3 <- eyetrackSim:::buildSaccadeSub(sim3)

ag2[, `:=`(id = 2, gp = "A")]
ag3[, `:=`(id = 3, gp = "A")]
ag4 <- copy(ag3)
ag4[, `:=`(id=4)]
dat <- rbind(raw_ag, ag2, ag3, ag4)

## Try adjustment to looks
dat[, adj_times := times - 200L]

fit <- bdotsFit(data = dat,
                subject = "id",
                group = "gp",
                curveType = logistic2(),
                time = "times",
                y = "looks",
                cores = 7)
times <- unique(sim1$trialData$times)
c1 <- eyetrackSim:::logistic_f(coef(fit)[1, ], times)
c2 <- eyetrackSim:::logistic_f(coef(fit)[2, ], times)
c3 <- eyetrackSim:::logistic_f(coef(fit)[3, ], times)
true_f <- eyetrackSim:::logistic_f(true_coef, times)

with(actual_ag, plot(times, looks, lty = 2, col = 'orange', ylim = c(0, 1),
                             main = "saccade without 200ms adjust"))
lines(times, c1, type = 'l', col = "steelblue", lwd = 2)
lines(times, c2, type = 'l', col = "red", lwd = 2)
lines(times, c3, type = 'l', col = "green", lwd = 2)
lines(times, true_f, type = 'l', col = "purple", lwd = 2)
legend(1250, 0.3, legend = c("obs data (aggregated)", "standard sample", "early samp", "mid samp", "true curve"),
       col = c("orange", "steelblue", "red", "green", "purple"),
       lty = 1)
