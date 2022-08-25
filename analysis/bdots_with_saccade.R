# Goal of this file is to compare bdots results when using
# observed saccades rather than collapsed estimated curve

library(eyetrackSim)
library(bdots)

# sim_l <- runSim_fixed(nsub = 10L, fnct = "logistic", fbst = TRUE, sampDensity = 10L)
#
# raw <- sim_l$fixations[id == 1, ]
# clean <- sim_l$trialData[id == 1, ]


sim1 <- runSub(fbst = TRUE, ntrials = 1000)

true_coef <- sim1$subInfo$pars

raw_ag <- eyetrackSim:::aggregateSub(sim1)
raw_ag[, `:=`(id = 1, gp = "A")]

raw <- copy(sim1$trialData)
raw[, `:=`(id = 1, gp = "A", saccadenum = NULL)]

raw_sac <- eyetrackSim:::buildSaccadeSub(sim1)
raw_sac[, `:=`(id = 1, gp = "A", saccadenum = NULL)]

fit_ag <- bdotsFit(data = raw_ag,
                     subject = "id",
                     group = "gp",
                     curveType = logistic(),
                     time = "times",
                     y = "looks",
                     cores = 7)


fit_sac <- bdotsFit(data = raw_sac,
                subject = "id",
                group = "gp",
                curveType = logistic2(),
                time = "starttime",
                y = "looks",
                cores = 7)
raw_sac2 <- copy(raw_sac)
raw_sac2[, starttime := starttime - 200]
fit_sac2 <- bdotsFit(data = raw_sac2,
                    subject = "id",
                    group = "gp",
                    curveType = logistic2(),
                    time = "starttime",
                    y = "looks",
                    cores = 7)
times <- unique(raw_ag$times)
# tt <- fitted.values(fit_sac$fit[[1]])
#with(raw_ag, plot(times, looks, lty = 2, col = 'orange', ylim = c(0, 1)))

ag_f <- eyetrackSim:::logistic_f(coef(fit_ag), times)
sac_f <- eyetrackSim:::logistic_f(coef(fit_sac), times)
sac_f2 <- eyetrackSim:::logistic_f(coef(fit_sac2), times)
true_f <- eyetrackSim:::logistic_f(true_coef, times)

with(raw_ag, plot(times, looks, lty = 2, col = 'orange', ylim = c(0, 1),
                  main = "no window"))
lines(times, ag_f, type = 'l', col = "steelblue", lwd = 2)
lines(times, sac_f, type = 'l', col = "red", lwd = 2)
lines(times, sac_f2, type = 'l', col = "green", lwd = 2)
lines(times, true_f, type = 'l', col = "purple", lwd = 2)
legend(1250, 0.3, legend = c("obs data (aggregated)", "aggregated fit", "saccade fit", "sac shift", "true curve"),
       col = c("orange", "steelblue", "red", "green", "purple"),
       lty = 1)

# lines(raw_sac$starttime[order(tt)], sort(tt), col = 'steelblue', type = 'l',
#       lwd = 2)
#
# plot(fit_ag)

##### Try again with (early) window sampling #########

sim1 <- runSub(fbst = TRUE, window = c(100, 300), windowRate = 25,
               pars = true_coef)

true_coef <- sim1$subInfo$pars

raw_ag <- eyetrackSim:::aggregateSub(sim1)
raw_ag[, `:=`(id = 1, gp = "A")]

raw <- copy(sim1$trialData)
raw[, `:=`(id = 1, gp = "A", saccadenum = NULL)]

raw_sac <- eyetrackSim:::buildSaccadeSub(sim1)
raw_sac[, `:=`(id = 1, gp = "A", saccadenum = NULL)]

fit_ag <- bdotsFit(data = raw_ag,
                   subject = "id",
                   group = "gp",
                   curveType = logistic(),
                   time = "times",
                   y = "looks",
                   cores = 7)


fit_sac <- bdotsFit(data = raw_sac,
                    subject = "id",
                    group = "gp",
                    curveType = logistic2(),
                    time = "starttime",
                    y = "looks",
                    cores = 7)
raw_sac2 <- copy(raw_sac)
raw_sac2[, starttime := starttime - 200]
fit_sac2 <- bdotsFit(data = raw_sac2,
                     subject = "id",
                     group = "gp",
                     curveType = logistic2(),
                     time = "starttime",
                     y = "looks",
                     cores = 7)
times <- unique(raw_ag$times)
# tt <- fitted.values(fit_sac$fit[[1]])
#with(raw_ag, plot(times, looks, lty = 2, col = 'orange', ylim = c(0, 1)))

ag_f <- eyetrackSim:::logistic_f(coef(fit_ag), times)
sac_f <- eyetrackSim:::logistic_f(coef(fit_sac), times)
sac_f2 <- eyetrackSim:::logistic_f(coef(fit_sac2), times)
true_f <- eyetrackSim:::logistic_f(true_coef, times)

with(raw_ag, plot(times, looks, lty = 2, col = 'orange', ylim = c(0, 1),
                  main = "early window"))
lines(times, ag_f, type = 'l', col = "steelblue", lwd = 2)
lines(times, sac_f, type = 'l', col = "red", lwd = 2)
lines(times, sac_f2, type = 'l', col = "green", lwd = 2)
lines(times, true_f, type = 'l', col = "purple", lwd = 2)
legend(1250, 0.3, legend = c("obs data (aggregated)", "aggregated fit", "saccade fit", "sac shift", "true curve"),
       col = c("orange", "steelblue", "red", "green", "purple"),
       lty = 1)



##### Try again with (mid) window sampling #########

sim1 <- runSub(fbst = TRUE, window = c(500, 700), windowRate = 25,
               pars = true_coef)

true_coef <- sim1$subInfo$pars

raw_ag <- eyetrackSim:::aggregateSub(sim1)
raw_ag[, `:=`(id = 1, gp = "A")]

raw <- copy(sim1$trialData)
raw[, `:=`(id = 1, gp = "A", saccadenum = NULL)]

raw_sac <- eyetrackSim:::buildSaccadeSub(sim1)
raw_sac[, `:=`(id = 1, gp = "A", saccadenum = NULL)]

fit_ag <- bdotsFit(data = raw_ag,
                   subject = "id",
                   group = "gp",
                   curveType = logistic(),
                   time = "times",
                   y = "looks",
                   cores = 7)


fit_sac <- bdotsFit(data = raw_sac,
                    subject = "id",
                    group = "gp",
                    curveType = logistic2(),
                    time = "starttime",
                    y = "looks",
                    cores = 7)
raw_sac2 <- copy(raw_sac)
raw_sac2[, starttime := starttime - 200]
fit_sac2 <- bdotsFit(data = raw_sac2,
                     subject = "id",
                     group = "gp",
                     curveType = logistic2(),
                     time = "starttime",
                     y = "looks",
                     cores = 7)
times <- unique(raw_ag$times)
# tt <- fitted.values(fit_sac$fit[[1]])
with(raw_ag, plot(times, looks, lty = 2, col = 'orange', ylim = c(0, 1)))

ag_f <- eyetrackSim:::logistic_f(coef(fit_ag), times)
sac_f <- eyetrackSim:::logistic_f(coef(fit_sac), times)
sac_f2 <- eyetrackSim:::logistic_f(coef(fit_sac2), times)
true_f <- eyetrackSim:::logistic_f(true_coef, times)

with(raw_ag, plot(times, looks, lty = 2, col = 'orange', ylim = c(0, 1),
                  main = "mid window"))
lines(times, ag_f, type = 'l', col = "steelblue", lwd = 2)
lines(times, sac_f, type = 'l', col = "red", lwd = 2)
lines(times, sac_f2, type = 'l', col = "green", lwd = 2)
lines(times, true_f, type = 'l', col = "purple", lwd = 2)
legend(1250, 0.3, legend = c("obs data (aggregated)", "aggregated fit", "saccade fit", "sac shift", "true curve"),
       col = c("orange", "steelblue", "red", "green", "purple"),
       lty = 1)
