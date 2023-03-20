
### This represents the createData functions on February 18 2023

#' Create artificial fixation data (February)
#'
#' Data to create "empirical" data centered around four parameter logistic
#'
#' @param n number of subjects (it actually creates two groups of 25 each for bootstrap)
#' @param trials number of trials for binomial method
#' @param pars starting parameters for groups, gotten empirically. Probably will never change this value
#' @param paired is this paired data?
#' @param pairMag how much variability between paired subjects
#' @param ar1 do i create ar1 data instead of binomial
#' @param manymeans do i do this the right way or the wrong way
#'
#' @description Create data for validating different situations in competing bdots implementations
#' ar = 0.8 and sd in ar noise is 0.025
#' @import mvtnorm
#' @export
createData_feb <- function(n = 25, trials = 100, pars = EMPIRICAL_START_PARS,
                       paired = FALSE, pairMag = 0.05, ar1 = FALSE,
                       manymeans = TRUE) {

  time <- seq(0, 1600, by = 4)

  if (!manymeans) {
    res <- singleMeans_feb(n, trials, pars, paired, ar1, time)
    return(res)
  }


  ## If AR1 we can implement trials by impacting noise as such var = p(1-p)) / n
  # when n = 10 that gives us sig = 0.025 as used in trials
  if (ar1) {
    sigv <- (0.5)*(1-0.5) / sqrt(trials)
    rhop <- ifelse(paired, 0, 0.8)
  }

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
    if (ar1) {
      dt[, fixations := addARerror(val = true, sig = sigv)]
    } else {
      dt[, fixations := mean(rbinom(trials, 1, true)), by = time]
    }
  })

  dts1 <- rbindlist(dts1)

  ## Then we make our parameters for group 2
  if (!paired) {
    ## Basically just repeat above, exact same distribution
    newpars2 <- do.call(rmvnorm, as.list(c(n, pars)))
    newpars2[,1] <- abs(newpars2[,1]) # need base > 0
    newpars2[,2] <- pmin(newpars2[,2], 1) # need peak < 1
  } else {
    ## Keep the original pars from newpars
    newpars2 <- newpars
  }
  spars2 <- split(newpars2, row(newpars2))
  ipn <- ifelse(paired, 0, n)

  dts2 <- lapply(seq_len(n), function(x) {
    pp <- spars2[[x]]
    dt <- data.table(id = x + ipn, #ipn is 0 if paired
                     time = time,
                     group = "B",
                     true = eyetrackSim:::logistic_f(pp, time))
    if (ar1) {
      # if paired don't add any more ar1 error
      dt[, fixations := addARerror(val = true, sig = sigv, rho = rhop)]
    } else {
      dt[, fixations := mean(rbinom(trials, 1, true)), by = time]
    }
  })
  dts2 <- data.table::rbindlist(dts2)
  dts <- data.table::rbindlist(list(dts1, dts2))

  return(list(dts = dts, parsA = newpars, parsB = newpars2))
}


singleMeans_feb <- function(n, trials, pars, paired, ar1, time) {
  pars <- pars[[1]]
  sigv <- 0.25 / sqrt(trials)

  group1 <- createSingleMeanSubs_feb(n, ar1, pars = pars, sig = sigv, rho = 0.8,
                                 trials = trials, time = time)

  if (paired) {
    # This just adds gaussian noise to first group
    group2 <- createSingleMeanSubs_feb(n, ar1, pars, rho = 0, sig = sigv, gg = "B",
                                   trials = trials, time=time)
  } else {
    group2 <- createSingleMeanSubs_feb(n, ar1, pars, rho = 0.8, sig = sigv, gg = "B",
                                   trials = trials, time = time)
  }
  dts <- rbindlist(list(group1, group2))
  parsA <- matrix(pars, ncol = 4, nrow = n, byrow = TRUE)
  return(list(dts = dts, parsA = parsA, parsB = parsA))
}

## Pars are from jakes paper
# but using more realistic values actually because xo of 200 is retarded
createSingleMeanSubs_feb <- function(n, ar1 = FALSE, pars = c(0, 0.9, 0.0025, 750),
                                 rho = 0.8, sig = 0.025, gg = "A", trials, time) {
  if (length(pars) != 4) stop("4 pars for single mean subs")

  ## Indicates if paired
  ipn <- ifelse(rho == 0, 0, n)

  dts1 <- lapply(seq_len(n), function(x) {
    dt <- data.table(id = x + ipn, # ipn is 0 if paired
                     time = time,
                     group = gg,
                     true = eyetrackSim:::logistic_f(pars, time))
    if (ar1) {
      dt[, fixations := addARerror(val = true, rho, sig = sig)]
    } else {
      dt[, fixations := mean(rbinom(trials, 1, true)), by = time]
    }
  })
  rbindlist(dts1)
}

#' Create ar1 error
#'
#' @export
addARerror <- function(val, rho = 0.8, sig = 0.025) {

  ## Let's make a maybe possibility for paired (kinda)
  if (rho == 0) {
    w <- rnorm(length(val), mean = 0, sd = sig)
    val <- val + w
    return(val)
  }

  n <- length(val)
  w <- rnorm(n, mean = 0, sd = sig)
  e <- numeric(n)
  e[1] <- w[1]
  for (i in 2:n) {
    e[i] <- rho*e[i-1] + w[i]
  }
  #e[1] <- 0
  #e <- e + w
  val <- val + e
}



