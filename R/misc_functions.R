#######################
### Curve Functions ###
#######################

## Logistic function
#' @export
logistic_f <- function(p, t) {
  b0 <- p[1] # base
  b1 <- p[2] # max
  sl <- p[3] # slope
  xo <- p[4] # crossover
  b0 + (b1-b0) / (1 + exp(4*sl*((xo-t)/(b1-b0))))
}

#' @export
doubleGauss_f <- function(p, t) {
  mu <- p[1]
  ht <- p[2]
  s1 <- p[3]
  s2 <- p[4]
  b1 <- p[5]
  b2 <- p[6]

  lhs <- (t < mu) * ((ht-b1) * exp((t - mu)^2/(-2*s1^2)) + b1)
  rhs <- (t >= mu) * ((ht-b2) * exp((t - mu)^2/(-2*s2^2)) + b2)
  lhs+rhs
}

#' @export
linear_f <- function(p, t) {
  b <- p[1]
  m <- p[2]
  m*t + b
}


### Here is how I got the starting par/var for testing
# ## Start by generating an empirical distribution
# ci <- as.data.table(ci)
# ci <- ci[LookType == "Target", ]
# #ci <- ci[LookType == "Target" & protocol != "NH", ]
# fit <- bdotsFit(data = ci,
#                 y = "Fixations",
#                 subject = "Subject",
#                 time = "Time",
#                 group = "protocol", curveType = logistic())
#
# ## Except this time I only need one empirical dist
# ## Here I am using BOTH TD/ND to create more variability
# cc <- coef(fit[])
# vv <- var(cc)
# cc <- colMeans(cc)
# cc[1] <- abs(cc[1])
# cc[2] <- pmin(cc[2], 1)
# pars <- list(mean = cc, sigma = vv)
## using deput to get these
EMPIRICAL_START_PARS <- list(mean = c(mini = 0.0214223180284706, peak = 0.904754582273567,
                                      slope = 0.00165802717190677, cross = 725.031533586957), sigma = structure(c(0.000175969499369012,
                                                                                                                  0.0000487830783643044, 0.00000313632100316084, 0.00364081890233776,
                                                                                                                  0.0000487830783643044, 0.00696428035548042, 0.0000240045450371927,
                                                                                                                  -1.75318934674301, 0.00000313632100316084, 0.0000240045450371927,
                                                                                                                  0.000000180059372175171, -0.0135790777489765, 0.00364081890233776,
                                                                                                                  -1.75318934674301, -0.0135790777489765, 3513.62904635378), dim = c(4L,
                                                                                                                                                                                     4L), dimnames = list(c("mini", "peak", "slope", "cross"), c("mini",
                                                                                                                                                                                                                                                 "peak", "slope", "cross"))))
