library(eyetrackSim)
library(bdots)
library(ggplot2)

## Doing this with double gauss just to show bobby
# but will fit with correct parameters

## Tryin some new shit
N <- 100
FBST <- TRUE # change fixation length
dg_sim_no_delay <- runSim_pb(nsub = N, ntrials = 300,
                             fnct = "doubleGauss", fbst = FBST,
                             sacDelay = 0)

unf <- function() runif(1, min = 100, max = 300)
makeActiveBinding("unf_rv", unf, .GlobalEnv)
dg_sim_uniform <- runSim_pb(nsub = N, ntrials = 300,
                            fnct = "doubleGauss", fbst = FBST,
                            sacDelay = unf_rv)

wb <- function() rweibull(1, shape = 1.8, scale = 224.9)
makeActiveBinding("wb_rv", wb, .GlobalEnv)
dg_sim_weibull <- runSim_pb(nsub = N, ntrials = 300,
                            fnct = "doubleGauss", fbst = FBST,
                            sacDelay = wb_rv)



## Now using same starting parameters for all (as to not bias things greatly)
# or to maintain fairness rather
dg_fit_fix_no_delay <- bdotsFit(dg_sim_no_delay$trialData,
                                subject = "id",
                                time = "times",
                                y = "looks",
                                group = "group",
                                curveType = doubleGauss2(
                                  params = c(mu = 630,
                                             ht = 0.18,
                                             sig1 = 130,
                                             sig2 = 250,
                                             base1 = 0.05,
                                             base2 = 0.05)))
cat("1\n")

dg_fit_sac_no_delay <- bdotsFit(dg_sim_no_delay$fixations,
                                subject = "id",
                                time = "starttime",
                                y = "looks",
                                group = "group",
                                curveType = doubleGauss2(
                                  params = c(mu = 630,
                                             ht = 0.18,
                                             sig1 = 130,
                                             sig2 = 250,
                                             base1 = 0.05,
                                             base2 = 0.05)))
cat("2\n")
dg_sim_uniform$trialData[, times := times - 200L]
dg_sim_uniform$fixations[, starttime := starttime - 200L]
dg_fit_fix_uniform <- bdotsFit(dg_sim_uniform$trialData,
                               subject = "id",
                               time = "times",
                               y = "looks",
                               group = "group",
                               curveType = doubleGauss2(
                                 params = c(mu = 630,
                                            ht = 0.18,
                                            sig1 = 130,
                                            sig2 = 250,
                                            base1 = 0.05,
                                            base2 = 0.05)))
cat("3\n")
dg_fit_sac_uniform <- bdotsFit(dg_sim_uniform$fixations,
                               subject = "id",
                               time = "starttime",
                               y = "looks",
                               group = "group",
                               curveType = doubleGauss2(
                                 params = c(mu = 630,
                                            ht = 0.18,
                                            sig1 = 130,
                                            sig2 = 250,
                                            base1 = 0.05,
                                            base2 = 0.05)))
cat("4\n")
dg_sim_weibull$trialData[, times := times - 200L]
dg_sim_weibull$fixations[, starttime := starttime - 200L]
dg_fit_fix_weibull <- bdotsFit(dg_sim_weibull$trialData,
                               subject = "id",
                               time = "times",
                               y = "looks",
                               group = "group",
                               curveType = doubleGauss2(
                                 params = c(mu = 630,
                                            ht = 0.18,
                                            sig1 = 130,
                                            sig2 = 250,
                                            base1 = 0.05,
                                            base2 = 0.05)))
cat("5\n")
dg_fit_sac_weibull <- bdotsFit(dg_sim_weibull$fixations,
                               subject = "id",
                               time = "starttime",
                               y = "looks",
                               group = "group",
                               curveType = doubleGauss2(
                                 params = c(mu = 630,
                                            ht = 0.18,
                                            sig1 = 130,
                                            sig2 = 250,
                                            base1 = 0.05,
                                            base2 = 0.05)))

if (FBST) {
  save.image(file = "dg_pb_data_sim_fbst.RData")
} else {
  save.image(file = "dg_pb_data_sim_no_fbst.RData")
}
