library(eyetrackSim)
library(bdots)
library(ggplot2)

## Tryin some new shit

rrbeta <- function(n=1, s1=2, s2=0.55, p=20, q=250) {
  b <- rbeta(n, s1, s2)
  y <- b*(q-p) + p
  y
}


N <- 1000
nt <- 300
FBST <- FALSE # change fixation length
sim_no_delay <- runSim_pb(nsub = N, ntrials = nt,
                          fnct = "logistic", fbst = FBST,
                          omDelay = 0)

unf <- function() runif(1, min = 100, max = 300) #abs(rnorm(1, 200, sd = 30))
makeActiveBinding("unf_rv", unf, .GlobalEnv)
sim_uniform <- runSim_pb(nsub = N, ntrials = nt,
                         fnct = "logistic", fbst = FBST,
                         omDelay = unf_rv)

wb <- function() max(100, rweibull(1, shape = 1.8, scale = 224.9))
makeActiveBinding("wb_rv", wb, .GlobalEnv)
sim_weibull <- runSim_pb(nsub = N, ntrials = nt,
                         fnct = "logistic", fbst = FBST,
                         omDelay = wb_rv)

nn <- function() max(100, rnorm(1, 200, 15)) #abs(rnorm(1, 200, sd = 30))
makeActiveBinding("nn_rv", nn, .GlobalEnv)
sim_normal <- runSim_pb(nsub = N, ntrials = nt,
                        fnct = "logistic", fbst = FBST,
                        omDelay = nn_rv)

bbe <- function() rrbeta(1, 2, 1, 100, 250) #rweibull(1, shape = 1.8, scale = 224.9)
makeActiveBinding("bb_rv", bbe, .GlobalEnv)
sim_beta <- runSim_pb(nsub = N, ntrials = nt,
                      fnct = "logistic", fbst = FBST,
                      omDelay = bb_rv)


## Now using same starting parameters for all (as to not bias things greatly)
# or to maintain fairness rather
# fit_fix_no_delay <- bdotsFit(sim_no_delay$trialData,
#                           subject = "id",
#                           time = "times",
#                           y = "looks",
#                           group = "group",
#                           curveType = logistic(params =
#                                                  c(mini = 0, peak = 1,
#                                                    slope = .002, cross = 750)))
fit_fix_no_delay <- bdotsFit(sim_no_delay$trialData,
                             subject = "id",
                             time = "times",
                             y = "looks",
                             group = "group",
                             curveType = logistic_sac())
cat("1\n")

fit_sac_no_delay <- bdotsFit(sim_no_delay$fixations,
                          subject = "id",
                          time = "starttime",
                          y = "looks",
                          group = "group",
                          curveType = logistic_sac())
cat("2\n")
sim_uniform$trialData[, times := times - 200L]
sim_uniform$fixations[, starttime := starttime - 200L]
fit_fix_uniform <- bdotsFit(sim_uniform$trialData,
                           subject = "id",
                           time = "times",
                           y = "looks",
                           group = "group",
                           curveType = logistic_sac())
cat("3\n")
fit_sac_uniform <- bdotsFit(sim_uniform$fixations,
                           subject = "id",
                           time = "starttime",
                           y = "looks",
                           group = "group",
                           curveType = logistic_sac())
cat("4\n")
sim_weibull$trialData[, times := times - 200L]
sim_weibull$fixations[, starttime := starttime - 200L]
fit_fix_weibull <- bdotsFit(sim_weibull$trialData,
                            subject = "id",
                            time = "times",
                            y = "looks",
                            group = "group",
                            curveType = logistic_sac())
cat("5\n")
fit_sac_weibull <- bdotsFit(sim_weibull$fixations,
                            subject = "id",
                            time = "starttime",
                            y = "looks",
                            group = "group",
                            curveType = logistic_sac())

cat("6\n")
sim_normal$trialData[, times := times - 200L]
sim_normal$fixations[, starttime := starttime - 200L]
fit_fix_normal <- bdotsFit(sim_normal$trialData,
                            subject = "id",
                            time = "times",
                            y = "looks",
                            group = "group",
                            curveType = logistic_sac())
cat("7\n")
fit_sac_normal <- bdotsFit(sim_normal$fixations,
                            subject = "id",
                            time = "starttime",
                            y = "looks",
                            group = "group",
                            curveType = logistic_sac())

cat("8\n")
sim_beta$trialData[, times := times - 200L]
sim_beta$fixations[, starttime := starttime - 200L]
fit_fix_beta <- bdotsFit(sim_beta$trialData,
                           subject = "id",
                           time = "times",
                           y = "looks",
                           group = "group",
                           curveType = logistic_sac())
cat("9\n")
fit_sac_beta <- bdotsFit(sim_beta$fixations,
                           subject = "id",
                           time = "starttime",
                           y = "looks",
                           group = "group",
                           curveType = logistic_sac())

if (FBST) {
  save.image(file = "pb_data_sim_fbst_normal.RData")
} else {
  save.image(file = "pb_data_sim_no_fbst_normal.RData")
}
