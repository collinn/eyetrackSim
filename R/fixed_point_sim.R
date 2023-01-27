## Run set of simulation on single subject with fixed points
#' Run simulation for single subject
#'
#' @param fnct character vector indicatin curve type
#' @param ntrials number of vwp trials per subject
#' @param fbst use FBS+T assumption or not
#' @param sampDensity misnomer. Density for saccades
#' @param targMult Multiplier for target, i.e., multiple of sampDensity when fbst == TRUE
#' @param window A time window to sample at different density
#' @param windowRate sampling density within window
#' @param pars curve parameters
#'
#' @returns This runs simluation for single subject, returns a list
#' containing information on subject, as well as trial data
#' @export
runSub_fixed_pb <- function(fnct = "logistic", ntrials = 300, fbst = FALSE,
                         sampDensity = 50, targMult = 1, window = NULL,
                         windowRate = sampDensity, pars = NULL) {

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

    while (curtime < -sampDensity) {
      curtime <- min(times) - runif(1)*em[1] # current time
      lasttime <- curtime - rg() - runif(1)*em[1]
    }
    #curtime <- sampDensity
    #lasttime <- 0

    while (curtime < max(times)) {

      # ? integral value instead?
      currProb <- fn(pars, lasttime)
      targ <- runif(1) <= currProb


      ## Duration depends on looking at target or not
      #(currdur <- ifelse(fbst & targ, rgT(), rg()))
      #currdur <- sampDensity
      currdur <- ifelse(fbst & targ, targMult*sampDensity, sampDensity)

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

## Run set of simulation on nsub subjects with fixed sampling points
#' Run simulation for multiple subjects
#'
#' @param nsub numeric, number of subjects in simulation
#' @param fnct character vector indicatin curve type
#' @param ntrials number of vwp trials per subject
#' @param fbst use FBS+T assumption or not
#' @param sampDensity misnomer. Density for saccades
#' @param targMult Multiplier for target, i.e., multiple of sampDensity when fbst == TRUE
#'
#' @returns This runs simluation for single subject, returns a list
#' containing information on subject, as well as trial data
#' @export
runSim_fixed <- function(nsub = 10, ntrials = 300, fnct = "logistic",
                         fbst = FALSE, sampDensity = 50, targMult = 1) {

  ## Probably ought to do in parallel
  #subs <- replicate(nsub, runSub(fnct, ntrials, fbst), simplify = FALSE)
  subs <- mclapply(seq_len(nsub), function(i) {
    j <- i # dumb that this is necessary
    tt <- runSub_fixed(fnct, ntrials, fbst, sampDensity, targMult)
    tt$trialData[, id := i]
    nn <- ncol(tt$trialData)
    nam <- colnames(tt$trialData)[c(nn, 1:(nn-1))]
    tt$trialData <- tt$trialData[, ..nam]
    tt
  }, mc.cores = detectCores() - 1L)

  trialData <- lapply(subs, aggregateSub)
  trialData <- rbindlist(trialData)

  fixations <- lapply(subs, buildSaccadeSub)
  fixations <- rbindlist(fixations)

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
