library(eyetrackSim)
library(bdots)
library(ggplot2)


sim_fixed <- runSim(nsub = 1000, ntrials = 300,
                    fnct = "logistic", fbst = FALSE,
                    sacDelay = 0)
cat("-1\n")

sim_random <- runSim(nsub = 1000, ntrials = 300,
                     fnct = "logistic", fbst = FALSE,
                     sacDelay = NULL)
cat("0\n")
## Now using same starting parameters for all (as to not bias things greatly)
# or to maintain fairness rather
fit_fix_fixed <- bdotsFit(sim_fixed$trialData,
                          subject = "id",
                          time = "times",
                          y = "looks",
                          group = "group",
                          curveType = logistic(params =
                                                 c(mini = 0, peak = 1,
                                                   slope = .002, cross = 750)))
cat("1\n")

fit_sac_fixed <- bdotsFit(sim_fixed$fixations,
                          subject = "id",
                          time = "starttime",
                          y = "looks",
                          group = "group",
                          curveType = logistic(params =
                                                 c(mini = 0, peak = 1,
                                                   slope = .002, cross = 750)))
cat("2\n")
fit_fix_random <- bdotsFit(sim_random$trialData,
                           subject = "id",
                           time = "times",
                           y = "looks",
                           group = "group",
                           curveType = logistic(params =
                                                  c(mini = 0, peak = 1,
                                                    slope = .002, cross = 750)))
cat("3\n")
fit_sac_random <- bdotsFit(sim_random$fixations,
                           subject = "id",
                           time = "starttime",
                           y = "looks",
                           group = "group",
                           curveType = logistic(params =
                                                  c(mini = 0, peak = 1,
                                                    slope = .002, cross = 750)))
cat("4\n")

save.image(file = "pb_data_sim.RData")
