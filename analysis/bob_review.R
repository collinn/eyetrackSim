## Goals here include:
# 1. MISE between curves and true
# 2. Compare standard fit with saccade (fbs+t)
# 3. Look for convergence of saccade to true (with shift)
# 4. High density sampling at beginning vs middle
# 5. Come up with  names for each of these things


#### 1. MISE between curves and true (generate function) ####
library(eyetrackSim)
sim <- runSub(ntrials = 10, fbst = TRUE)

makeJointFits <- function(ss) {
  ss <- copy(ss)

  true_coef <- ss$subInfo$pars

  ## Get aggregated and saccade
  raw_ag <- eyetrackSim:::aggregateSub(ss)
  raw_ag[, `:=`(id = 1, gp = "A")]

  raw_sac <- eyetrackSim:::buildSaccadeSub(ss)
  raw_sac[, `:=`(id = 1, gp = "A", saccadenum = NULL)]

  ## Shift curves
  raw_ag2 <- copy(raw_ag)
  raw_sac2 <- copy(raw_sac)
  raw_ag2[, times := times - 200L]
  raw_sac2[, starttime := starttime - 200L]

  ## Fit the bdots
  fit_ag <- bdotsFit(data = raw_ag,
                     subject = "id",
                     group = "gp",
                     curveType = logistic(),
                     time = "times",
                     y = "looks",
                     cores = 7)
  fit_ag2 <- bdotsFit(data = raw_ag2,
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
  fit_sac2 <- bdotsFit(data = raw_sac2,
                       subject = "id",
                       group = "gp",
                       curveType = logistic2(),
                       time = "starttime",
                       y = "looks",
                       cores = 7)
  ## Stuff for plots
  times <- unique(raw_ag$times)
  ag_f <- eyetrackSim:::logistic_f(coef(fit_ag), times)
  ag_f2 <- eyetrackSim:::logistic_f(coef(fit_ag2), times)
  sac_f <- eyetrackSim:::logistic_f(coef(fit_sac), times)
  sac_f2 <- eyetrackSim:::logistic_f(coef(fit_sac2), times)
  true_f <- eyetrackSim:::logistic_f(true_coef, times)

  obs_ag <- copy(raw_ag)
  obs_ag[, `:=`(id = NULL, gp = NULL)]

  ## Return obs
  bfits <- list("ag" = fit_ag, "ag2" = fit_ag2, "sac" = fit_sac, "sac2" = fit_sac2)
  curves <- list("ag_f" = ag_f, "ag_f2" = ag_f2, "sac_f" = sac_f, "sac_f2" = sac_f2, "true_f" = true_f)
  return(list(bfits = bfits, curves = curves, times = times, true_coef = true_coef,
              obs_ag = obs_ag, raw_sac = raw_sac))
}

ff <- makeJointFits(sim)

## Curve MISE of fits (ff)
mise <- function(ff) {
  curves <- ff$curves
  times <- ff$times

  cc <- lapply(ff$bfits, coef)
  cc[["true_f"]] <- ff$true_coef

  misev <- vapply(cc, function(x) {
    ## tt = time
    g <- function(tt) {
      (eyetrackSim:::logistic_f(x, tt) - eyetrackSim:::logistic_f(ff$true_coef, tt))^2
    }
    integrate(g, lower = min(times), upper = max(times))$value
  }, 1)

  if (length(misev) == 3) {
    names(misev) <- c("Aggregate -- Shifted", "Saccade -- Shifted", "Underlying")
  } else {
    names(misev) <- c("Aggregate", "Aggregate -- Shifted","Saccade", "Saccade -- Shifted", "Underlying")
  }
  misev
}

plotBfits <- function(ff, mm = NULL) {
  obs <- ff$obs_ag
  times <- obs$times
  curves <- ff$curves
  sac <- ff$raw_sac
  par(mfrow = c(2, 1))

  if (length(curves) == 5) {
    plot(obs, lty = 2, col = 'gray', ylim = c(0, 1),
         main = mm, xlab = "time", ylab = "activation")
    c_lty <- c("longdash", "solid", "longdash", "solid", "solid")
    c_lwd <- c(2, 3, 2, 3, 4)
    c_col <- c("firebrick1", "firebrick1", "chartreuse", "chartreuse", " darkorchid1")
    for (i in rev(seq_along(curves))) {
      lines(times, curves[[i]], lty = c_lty[i], col = c_col[i], lwd = c_lwd[i])
    }
    legend(1250, 0.4, col = c("gray", c_col), lwd = c(1, c_lwd), lty = c("solid", c_lty),
           legend = c("Obs aggregate", "Aggregated fit", "Aggregated fit -- shifted",
                      "Saccade fit", "Saccade fit -- shifted", "Underlying Curve"))
  } else {
    plot(obs, lty = 2, col = 'gray', ylim = c(0, 1),
         main = mm)
    c_lty <- c("solid", "solid", "solid")
    c_lwd <- c(3, 3, 4)
    c_col <- c("firebrick1", "chartreuse", " darkorchid1")
    for (i in rev(seq_along(curves))) {
      lines(times, curves[[i]], lty = c_lty[i], col = c_col[i], lwd = c_lwd[i])
    }
    legend(1250, 0.4, col = c("gray", c_col), lwd = c(1, c_lwd), lty = c("solid", c_lty),
           legend = c("Obs aggregate",  "Aggregated fit -- shifted",
                      "Saccade fit -- shifted", "Underlying Curve"))
  }
  hist(sac$starttime, main = "Saccades", xlab = "time")
}

simNFits <- function(ss) {
  ss <- copy(ss)

  true_coef <- ss$subInfo$pars

  ## Get aggregated and saccade
  raw_ag <- eyetrackSim:::aggregateSub(ss)
  raw_ag[, `:=`(id = 1, gp = "A")]

  raw_sac <- eyetrackSim:::buildSaccadeSub(ss)
  raw_sac[, `:=`(id = 1, gp = "A", saccadenum = NULL)]

  ## Shift curves
  raw_ag2 <- copy(raw_ag)
  raw_sac2 <- copy(raw_sac)
  raw_ag2[, times := times - 200L]
  raw_sac2[, starttime := starttime - 200L]

  ## Fit the bdots
  fit_ag2 <- bdotsFit(data = raw_ag2,
                      subject = "id",
                      group = "gp",
                      curveType = logistic(),
                      time = "times",
                      y = "looks",
                      cores = 7)
  fit_sac2 <- bdotsFit(data = raw_sac2,
                       subject = "id",
                       group = "gp",
                       curveType = logistic2(),
                       time = "starttime",
                       y = "looks",
                       cores = 7)
  ## Stuff for plots
  times <- unique(raw_ag$times)
  ag_f2 <- eyetrackSim:::logistic_f(coef(fit_ag2), times)
  sac_f2 <- eyetrackSim:::logistic_f(coef(fit_sac2), times)
  true_f <- eyetrackSim:::logistic_f(true_coef, times)

  obs_ag <- copy(raw_ag)
  obs_ag[, `:=`(id = NULL, gp = NULL)]

  ## Return obs
  bfits <- list("ag2" = fit_ag2, "sac2" = fit_sac2)
  curves <- list("ag_f2" = ag_f2, "sac_f2" = sac_f2, "true_f" = true_f)
  return(list(bfits = bfits, curves = curves, times = times, true_coef = true_coef,
              obs_ag = obs_ag, raw_sac = raw_sac))
}


#### 2. Compare Standard (aggregate) with Saccade ####
# set.seed(69)
# sim <- runSub(ntrials = 300, fbst = TRUE)
# true_coef <- sim$subInfo$pars
# bfit <- makeJointFits(sim)
#
#
# #### 3. Compare Standard (aggregate) with Saccade -- Early Window  ####
# sim_early <- runSub(ntrials = 300, fbst = TRUE, pars = true_coef,
#               window = c(100, 400), windowRate = 25)
# bfit_early <- makeJointFits(sim_early)
#
# #### 3. Compare Standard (aggregate) with Saccade -- Mid Window  ####
# sim_mid <- runSub(ntrials = 300, fbst = TRUE, pars = true_coef,
#               window = c(700, 1000), windowRate = 25)
# bfit_mid <- makeJointFits(sim_mid)
#
#
# #### 3. Compare Standard (aggregate) with Saccade -- Late Window  ####
# sim_late <- runSub(ntrials = 300, fbst = TRUE, pars = true_coef,
#               window = c(1400, 1700), windowRate = 25)
# bfit_late <- makeJointFits(sim_late)
#
#
# #### 3. Convergence of saccade to true with large N ####
# sim_N <- runSub(ntrials = 1e5, fbst = TRUE, pars = true_coef)
#
# bfitN <- simNFits(sim_N)



#### Let's do this all at once
runSet <- function(seed = 69, FBST = TRUE) {

  set.seed(seed)

  sim <- runSub(ntrials = 300, fbst = FBST)
  true_coef <- sim$subInfo$pars
  bfit <- makeJointFits(sim)

  reg <- list("sim" = sim, "fit" = bfit)

  #### 3. Compare Standard (aggregate) with Saccade -- Early Window  ####
  sim_early <- runSub(ntrials = 300, fbst = FBST, pars = true_coef,
                      window = c(100, 400), windowRate = 25)
  bfit_early <- makeJointFits(sim_early)

  early = list("sim" = sim_early, "fit" = bfit_early)

  #### 3. Compare Standard (aggregate) with Saccade -- Mid Window  ####
  sim_mid <- runSub(ntrials = 300, fbst = FBST, pars = true_coef,
                    window = c(700, 1000), windowRate = 25)
  bfit_mid <- makeJointFits(sim_mid)

  mid <- list("sim" = sim_mid, "fit" = bfit_mid)

  #### 3. Compare Standard (aggregate) with Saccade -- Late Window  ####
  sim_late <- runSub(ntrials = 300, fbst = FBST, pars = true_coef,
                     window = c(1400, 1700), windowRate = 25)
  bfit_late <- makeJointFits(sim_late)

  late <- list("sim" = sim_late, "fit" = bfit_late)

  #### 3. Convergence of saccade to true with large N ####
  sim_N <- runSub(ntrials = 1e5, fbst = FBST, pars = true_coef)

  bfitN <- simNFits(sim_N)

  nlist <- list("sim" = sim_N, "fit" = bfitN)

  return(list("reg" = reg, "early" = early,
              "mid" = mid, "late" = late, "nlist" = nlist))
}

# ## Seed = 69, fbst = true/false
# res_69_fbst <- runSet(69, TRUE)
# res_69_fbs <- runSet(69, FALSE)
#
# ## Seed = 6969, fbst = true/false
# res_6969_fbst <- runSet(6969, TRUE)
# res_6969_fbs <- runSet(6969, FALSE)
#
# save.image(file = "allSims.RData")

## Seed = 69, fbst = true/false
res_69_fbst <- runSet(79, TRUE)
res_69_fbs <- runSet(79, FALSE)

## Seed = 6969, fbst = true/false
res_6969_fbst <- runSet(7969, TRUE)
res_6969_fbs <- runSet(7969, FALSE)

save.image(file = "allSims2.RData")
