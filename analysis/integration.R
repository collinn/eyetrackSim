
sub <- runSub()
subfix <- buildSaccadeSub(sub)

ll <- function(t) logistic(pars, t)
ll(3)

int <- integrate(ll, 200, 500)
int$value/300

ll(500)
mean(ll(200:500))


times <- seq(0, 2000, 4)
fix <- vector("numeric",  length = length(times))
fix <- matrix(NA, nrow = length(times), ncol = 300)

for (j in seq_len(300)) {
  for (i in seq_along(times)) {
    tt <- times[i]
    # dur <- 200
    dur <- sample(subfix$dur, 1)
    vec <- (tt-dur):tt
    fix[i, j] <- mean(ll(vec))
  }
}
rr <- rowMeans(fix)

## add oculomotor delay (2ooms)
rr_adj <- c(rr[-c(1:50)], rep(rr[length(rr)], 50))

real <- ll(times)

plot(times, real, type = 'l', col = 'green', lwd = 2, ylim = c(-0.1, 0.9))
lines(times, rr, lwd = 2, col = 'blue')
lines(times, rr_adj, lwd = 2, col = 'purple')
lines(times, real - rr, lwd = 2, col = 'red')
lines(times, rr_adj - rr, lwd = 2, lty = 2, col = 'red')


## bdots stuff below

looks <- list(real, rr, rr_adj)

tt <- lapply(1:3, function(i) {
  dt <- data.table(times = times,
                   id = i,
                   looks = looks[[i]],
                   group = "a")
})
dt <- rbindlist(tt)

library(bdots)
fit <- bdotsFit(y = "looks",
                subject = "id",
                group = "group",
                curveType = logistic(),
                time = "times",
                dat = dt,
                cores = 1L)

mat <- coef(fit)
rownames(mat) <- c("real", "rr", "rr_adj")
