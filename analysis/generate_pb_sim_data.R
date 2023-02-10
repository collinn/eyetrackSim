library(eyetrackSim)
library(bdots)
library(ggplot2)


# sim_fixed <- runSim(nsub = 1000, ntrials = 300,
#                     fnct = "logistic", fbst = FALSE,
#                     sacDelay = 0)
# cat("-1\n")
#
# sim_random <- runSim(nsub = 1000, ntrials = 300,
#                      fnct = "logistic", fbst = FALSE,
#                      sacDelay = NULL)
# cat("0\n")

## Tryin some new shit
N <- 1000
FBST <- TRUE # change fixation length
sim_no_delay <- runSim_pb(nsub = N, ntrials = 300,
                    fnct = "logistic", fbst = FBST,
                    omDelay = 0)

unf <- function() runif(1, min = 100, max = 300)
makeActiveBinding("unf_rv", unf, .GlobalEnv)
sim_uniform <- runSim_pb(nsub = N, ntrials = 300,
                    fnct = "logistic", fbst = FBST,
                    omDelay = unf_rv)

wb <- function() rweibull(1, shape = 1.8, scale = 224.9)
makeActiveBinding("wb_rv", wb, .GlobalEnv)
sim_weibull <- runSim_pb(nsub = N, ntrials = 300,
                      fnct = "logistic", fbst = FBST,
                      omDelay = wb_rv)



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
                             curveType = logistic())
cat("1\n")

fit_sac_no_delay <- bdotsFit(sim_no_delay$fixations,
                          subject = "id",
                          time = "starttime",
                          y = "looks",
                          group = "group",
                          curveType = logistic())
cat("2\n")
sim_uniform$trialData[, times := times - 200L]
sim_uniform$fixations[, starttime := starttime - 200L]
fit_fix_uniform <- bdotsFit(sim_uniform$trialData,
                           subject = "id",
                           time = "times",
                           y = "looks",
                           group = "group",
                           curveType = logistic())
cat("3\n")
fit_sac_uniform <- bdotsFit(sim_uniform$fixations,
                           subject = "id",
                           time = "starttime",
                           y = "looks",
                           group = "group",
                           curveType = logistic())
cat("4\n")
sim_weibull$trialData[, times := times - 200L]
sim_weibull$fixations[, starttime := starttime - 200L]
fit_fix_weibull <- bdotsFit(sim_weibull$trialData,
                            subject = "id",
                            time = "times",
                            y = "looks",
                            group = "group",
                            curveType = logistic())
cat("5\n")
fit_sac_weibull <- bdotsFit(sim_weibull$fixations,
                            subject = "id",
                            time = "starttime",
                            y = "looks",
                            group = "group",
                            curveType = logistic())

if (FBST) {
  save.image(file = "pb_data_sim_fbst_no_start_par.RData")
} else {
  save.image(file = "pb_data_sim_no_fbst_no_start_par.RData")
}
