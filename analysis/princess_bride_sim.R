
## Replicating for logistic eyetrack (princess bride)
times <- seq(0, 2000, by = 4)
oculomotordelay <- 200

test <- runSub(fbst = TRUE)
plot(aggregateSub(test))

## Try to recover start times
tt <- test$trialData
tt[, test:= saccadenum - shift(saccadenum)]
tt[, `:=`(starttime = min(times), endtime = max(times)), by = .(trial, saccadenum)]

rr <- copy(tt)
rr[, `:=`(times = NULL, looks = NULL, test = NULL)]
rr <- unique(rr)




dat <- aggregateSub(test)
dat$subject <- 1
dat$group <- "a"

fit <- bdotsFit(dat, subject = "subject",
                time = "times",
                y = "looks",
                group = "group",
                curveType = logistic())

orig_p <- test$subInfo$pars
fit_p <- coef(fit)[c(1,2,4,3)]

orig_l <- logistic(orig_p, times)
fit_l <- logistic(fit_p, times)
looks <- dat$looks

plot(times, orig_l, col = 'red', lwd = 2, type = 'l', ylim = c(0, 1))
lines(times, looks, col = 'blue', lwd = 2)
lines(times, fit_l, col = 'green', lwd = 2)



