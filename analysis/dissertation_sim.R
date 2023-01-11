library(eyetrackSim)
library(bdots)


sim_fixed <- runSim(nsub = 1000, ntrials = 300, 
               fnct = "logistic", fbst = FALSE, 
               sacDelay = 0)


sim_random <- runSim(nsub = 1000, ntrials = 300, 
               fnct = "logistic", fbst = FALSE, 
               sacDelay = NULL)


fit_fix_fixed <- bdotsFit(sim_fixed$trialData, 
                      subject = "id",
                      time = "times", 
                      y = "looks", 
                      group = "group", 
                      curveType = logistic())

fit_sac_fixed <- bdotsFit(sim_fixed$fixations, 
                          subject = "id",
                          time = "starttime", 
                          y = "looks", 
                          group = "group", 
                          curveType = logistic())


fit_fix_random <- bdotsFit(sim_random$trialData, 
                      subject = "id",
                      time = "times", 
                      y = "looks", 
                      group = "group", 
                      curveType = logistic())

fit_sac_random <- bdotsFit(sim_random$fixations, 
                          subject = "id",
                          time = "starttime", 
                          y = "looks", 
                          group = "group", 
                          curveType = logistic())


save.image()