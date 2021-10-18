


runSim <- function(nsub = 10, ntrials = 300,
                   fnct = "logistic", fbst = FALSE) {

  ## Probably ought to do in parallel
  subs <- replicate(nsub, runSub(fnct, ntrials, fbst), simplify = FALSE)
  subs <- lapply(seq_along(subs), function(i) {
    subs[[i]]$trialData[, id := i]
    subs[[i]]
  })

  trialData <- lapply(subs, aggregateSub)
  trialData <- rbindlist(trialData)

  fixations <- lapply(subs, buildSaccadeSub)
  fixations <- rbindlist(fixations)

  subject <- lapply(subs, function(x) {
    x[['subInfo']]
  })

  stuff <- names(subject[[1]])

  stuff2 <- lapply(stuff, function(x) {
    dt <- lapply(subject, function(y) {
      as.data.table(t(y[x]))
    }) |> rbindlist()
  })

  list(trialData, fixations)
}
