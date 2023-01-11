
## File includes
# runSim - run full simulation for nsub subjects, aggregate results
# makeSubject - creates subject with curve pars
# runSub - run simulation for single subject



## Run set of simulation on nsub subjects
#' Run simulation for multiple subjects
#'
#' @param nsub numeric, number of subjects in simulation
#' @param fnct character vector indicatin curve type
#' @param ntrials number of vwp trials per subject
#' @param fbst use FBS+T assumption or not
#' @param sacDelay alternative mechanism for eye delay
#'
#' @returns This runs simluation for single subject, returns a list
#' containing information on subject, as well as trial data
#' @export
runSim <- function(nsub = 10, ntrials = 300,
                   fnct = "logistic", fbst = FALSE, 
                   sacDelay = NULL) {

  ## Probably ought to do in parallel
  #subs <- replicate(nsub, runSub(fnct, ntrials, fbst), simplify = FALSE)
  subs <- mclapply(seq_len(nsub), function(i) {
    j <- i # dumb that this is necessary
    tt <- runSub(fnct, ntrials, fbst, saccadeDelay = sacDelay)
    tt$trialData[, id := i]
    nn <- ncol(tt$trialData)
    nam <- colnames(tt$trialData)[c(nn, 1:(nn-1))]
    tt$trialData <- tt$trialData[, ..nam]
    tt
  }, mc.cores = detectCores() - 2L)

  trialData <- lapply(subs, aggregateSub)
  trialData <- rbindlist(trialData)
  trialData$group <- "A" # for bdots

  fixations <- lapply(subs, buildSaccadeSub)
  fixations <- rbindlist(fixations)
  fixations$group <-  "A"

  subject <- lapply(subs, function(x) {
    x[['subInfo']]
  })

  subInfo <- names(subject[[1]])

  rr <- lapply(subInfo,  function(x) {
    tt <- lapply(subject, function(y) (as.data.table(t(y[[x]]))))
    tt <- rbindlist(tt)
    tt[, id := seq_len(nrow(tt))]

    # stupid way to handle this
    nn <- ncol(tt)
    nam <- colnames(tt)[c(nn, 1:(nn-1))]
    tt <- tt[, ..nam]
    tt
  })
  names(rr) <- subInfo
  rr[['fn']] <- as.character(rr[['fn']][1, 2])

  list(trialData = trialData,
       fixations = fixations,
       subPars = rr)
}



#' Create single subject for VWP trial
#'
#' @param fnct character vector indicatin curve type
#'
#' @returns This creates parameters for an individual subject,
#' returning the fixation curve parameters, as well as eye  movement
#' parameters for both standard and FBS+T
#' @export
makeSubject <- function(fnct = "logistic") {

  if (fnct == "logistic") {
    bb <- copy(baseParams[fn == 1, ])
    fn <- logistic_f
  } else if (fnct == "doubleGauss") {
    bb <- copy(baseParams[fn == 2, ])
    fn <- doubleGauss_f
  } else if (fnct == "linear") {
    bb <- copy(baseParams[fn == 3, ])
    fn <- linear_f
  }


  ## Parameters for the curve, right now, just logistic
  subPars <- vector("numeric", nrow(bb))
  subPars[] <- Inf
  maxFix <- 2
  while (maxFix > 1 | maxFix < 0.6) { # added minimum independent of mbob
    subPars[] <- Inf
    while (any(bb[, subPars <= min | subPars >= max ])) {
      subPars <- bb[, rnorm(nrow(bb))*sd + mean]
    }
    maxFix <- max(fn(subPars, times))
  }
  (maxFix)

  names(subPars) <- bb$param

  ### This portion below independent of fixation curve
  ## Eye movement parameters
  # bw subject mean / bw sub sd / mean trial x trial sd wn sub / sd of trial x sd of sub
  baseEMparams <- matrix(c(204.73, 32.63, 96.55, 24.57,
                           360.28, 65.78, 195.11, 30.04),
                         byrow = TRUE, ncol = 4)


  ## For eye movement, i.e., eyemovementSubject (missing FBS+T)
  emSub <- vector("numeric", 2L)
  emSub[1] <- rgamma(1, shape = baseEMparams[1,1]^2 / baseEMparams[1,2]^2,
                     scale = baseEMparams[1,2]^2/baseEMparams[1,1])
  emSub[2] <- rgamma(1, shape = baseEMparams[1,3]^2 / baseEMparams[1,4]^2,
                     scale = baseEMparams[1,4]^2/baseEMparams[1,3])

  ## For FBS+T (i.e., these are target looks, which are slightly longer)
  emSubT <- vector("numeric", 2L)
  emSubT[1] <- rgamma(1, shape = baseEMparams[2,1]^2 / baseEMparams[2,2]^2,
                      scale = baseEMparams[2,2]^2/baseEMparams[2,1])
  emSubT[2] <- rgamma(1, shape = baseEMparams[2,3]^2 / baseEMparams[2,4]^2,
                      scale = baseEMparams[2,4]^2/baseEMparams[2,3])

  names(emSub) <- c("meanEM", "sdEM")
  names(emSubT) <- c("meanEM_T", "sdEM_T")

  return(list(pars = subPars, em = emSub, emT = emSubT, fn = fnct))
}


## Run set of simulation on single subject
#' Run simulation for single subject
#'
#' @param fnct character vector indicatin curve type
#' @param ntrials number of vwp trials per subject
#' @param fbst use FBS+T assumption or not
#' @param window A time window to sample at different density
#' @param windowRate sampling rate within window
#' @param pars curve parameters
#' @param saccadeDelay Alternative to using `lasttime` for saccade time -- instead
#' can take on a fixed value
#'
#' @returns This runs simluation for single subject, returns a list
#' containing information on subject, as well as trial data
#' @export
runSub <- function(fnct = "logistic", ntrials = 300, fbst = TRUE,
                   window = NULL, windowRate = NULL, pars = NULL,
                   saccadeDelay = NULL) {

  ## Set up parameter stuff for subject
  subInfo <- makeSubject(fnct)
  if (is.null(pars)) {
    pars <- subInfo$pars
  } else {
    subInfo$pars <- pars
  }
  em <- subInfo$em
  emT <- subInfo$emT # for target
  rg <- function() rgamma(1, em[1]^2/em[2]^2, scale = em[2]^2/em[1])
  rgT <- function() rgamma(1, emT[1]^2/emT[2]^2, scale = emT[2]^2/emT[1])

  ## Assign curve fitting function
  if (fnct == "logistic") {
    fn <- logistic_f
  } else if (fnct == "doubleGauss") {
    fn <- doubleGauss_f
  }  else if (fnct == "linear") {
    fn <- linear_f
  }

  #trialDataList <- vector("list", length = ntrials)

  ## Go through trials
  trialDataList <- mclapply(seq_len(ntrials), function(i) {
    trialdata <- data.table(trial = i,
                            times = times,
                            looks = 0L * times[],
                            #saccade = 0L,
                            saccadenum = 0L)

    ## Step 1 of looks
    curtime <- min(times) - runif(1)*em[1] # current time
    lasttime <- curtime - rg() - runif(1)*em[1]

    # ## What are we looking at *first*
    # currProb <- fn(pars, lasttime)
    # targ <- runif(1) <= currProb

    while (curtime < max(times)) {

      # ? integral value instead?
      if (is.null(saccadeDelay)) {
        currProb <- fn(pars, lasttime)
      } else {
        currProb <- fn(pars, curtime - saccadeDelay)
      }

      targ <- runif(1) <= currProb

      # ## Only investigate if not already looking at target
      # if (!targ) {
      #   currProb <- fn(pars, lasttime)
      #   targ <- runif(1) <= currProb
      # } else {
      #   targ <- FALSE # were looking at target on last saccade so now were not
      # }
      #
      ## Duration depends on looking at target or not
      (currdur <- ifelse(fbst & targ, rgT(), rg()))

      if (!is.null(window) & !is.null(windowRate)) {
        wmin <- min(window)
        wmax <- max(window)
        inWindow <- curtime >= wmin & curtime <= wmax
        currdur <- ifelse(inWindow, windowRate, currdur)
      }

      # while (currdur + curtime < 0) {
      #   currdur <- ifelse(fbst & targ, rg(), rgT())
      # }

      ## update trial data
      idx <- which(times >= curtime & times <= curtime + currdur)
      if (length(idx) == 0) next
      trialdata[idx, looks := targ]
      #sac_idx <- which.min(abs(times - curtime)) # time less current closest to zero
      #trialdata[sac_idx, saccade := 1]
      trialdata[idx[1]:nrow(trialdata), saccadenum := saccadenum + 1L]
      lasttime <- curtime
      curtime <- curtime + currdur
    }
    trialdata

  }, mc.cores = detectCores()-1L)

  ## Aggregate to single DT
  tt <- rbindlist(trialDataList)
  return(list(subInfo = subInfo, trialData = tt))
}



#' Aggregates raw trial data into vwp style whatever
#'
#' @param x A subject run from simulation
#'
#' @returns data.table with looks and proportions
#' @export
aggregateSub <- function(x) {
  dat <- copy(x$trialData)
  dat[, looks := mean(looks), by = times]
  dat[, `:=`(trial = NULL, saccadenum = NULL)]
  dat <- unique(dat)
}

#' Get start/end times from trial data for saccades
#'
#' @param x A subject run from simulation
#'
#' @returns data.table with entries being saccades and times
#' @export
buildSaccadeSub <- function(x) {
  dat <- copy(x$trialData)

  ## Find start and end times
  dat[, `:=`(starttime = min(times), endtime = max(times)),
      by = .(trial, saccadenum)]
  dat[, `:=`(times = NULL, dur = endtime - starttime)]
  dat <- unique(dat)
}

