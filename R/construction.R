

## Run set of simulation on nsub subjects
#' Run simulation for multiple subjects
#'
#' @param nsub numeric, number of subjects in simulation
#' @param fnct character vector indicatin curve type
#' @param ntrials number of vwp trials per subject
#' @param fbst use FBS+T assumption or not
#'
#' @returns This runs simluation for single subject, returns a list
#' containing information on subject, as well as trial data
#' @export
runSim <- function(nsub = 10, ntrials = 300,
                   fnct = "logistic", fbst = FALSE) {

  ## Probably ought to do in parallel
  #subs <- replicate(nsub, runSub(fnct, ntrials, fbst), simplify = FALSE)
  subs <- mclapply(seq_len(nsub), function(i) {
    j <- i # dumb that this is necessary
    tt <- runSub(fnct, ntrials, fbst)
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


  # dots <- function(...) {eval(substitute(alist(...)))}
  # test <- Map(function(...) {
  #   tt <- dots(...)
  #   tt[[1]]
  # }, subject)

  list(trialData = trialData,
       fixations = fixations,
       subPars = rr)
 }
