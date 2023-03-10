

#' Create artificial fixation data
#'
#' Data to create "empirical" data centered around four parameter logistic
#'
#' @param n number of subjects (it actually creates two groups of 25 each for bootstrap)
#' @param trials number of trials for binomial method
#' @param pars starting parameters for groups, gotten empirically. Probably will never change this value
#' @param paired is this paired data?
#' @param pairMag how much variability between paired subjects
#' @description Create data for validating different situations in competing bdots implementations
#' ar = 0.8 and sd in ar noise is 0.025
#' @import mvtnorm
#' @export
createDataShiftLogistic <- function(n = 25, trials = 100, pars = EMPIRICAL_START_PARS,
                            paired = TRUE, pairMag = 1) {


  time <- seq(0, 1600, by = 4)

  pars$sigma <- pars$sigma * pairMag


  ## If AR1 we can implement trials by impacting noise as such var = p(1-p)) / n
  # when n = 10 that gives us sig = 0.025 as used in trials

  newpars <- do.call(rmvnorm, as.list(c(n, pars)))
  newpars[,1] <- abs(newpars[,1]) # need base > 0
  newpars[,2] <- pmin(newpars[,2], 1) # need peak < 1
  spars <- split(newpars, row(newpars))
  dts1 <- lapply(seq_len(n), function(x) {
    pp <- spars[[x]]
    dt <- data.table(id = x,
                     time = time,
                     group = "A",
                     true = eyetrackSim:::logistic_f(pp, time))
      dt[, fixations := mean(rbinom(trials, 1, true)), by = time]
  })

  dts1 <- rbindlist(dts1)

  ## Then we make our parameters for group 2
  newpars2 <- newpars
  newpars2[, 4] <- newpars2[, 4] + 200

  spars2 <- split(newpars2, row(newpars2))
  ipn <- ifelse(paired, 0, n)

  dts2 <- lapply(seq_len(n), function(x) {
    pp <- spars2[[x]]
    dt <- data.table(id = x + ipn, #ipn is 0 if paired
                     time = time,
                     group = "B",
                     true = eyetrackSim:::logistic_f(pp, time))
      dt[, fixations := mean(rbinom(trials, 1, true)), by = time]
  })
  dts2 <- data.table::rbindlist(dts2)
  dts <- data.table::rbindlist(list(dts1, dts2))

  return(list(dts = dts, parsA = newpars, parsB = newpars2))
}
