## This file contains w/e constants or parameter matrices
# that may have been pulled from bobs matlab code

## Initial time variable too i guess
times <- seq(0, 2000, by = 4)


#################
### Constants ###
#################

## These are base parameters (from matlab code)
# 1 (logistic) -- min / max / xo / slove
# 2 (double gauss) -- mu / ht / s1 / s2 / b1 / b2
# baseParams <- structure(list(fn = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L),
#                              param = c("mini", "peak", "cross", "slope", "mu", "ht", "sig1", "sig2", "base1", "base2"),
#                              mean = c(0.115, 0.885, 765, 0.0016, 630, 0.18, 130, 250, 0.05, 0.05),
#                              sd = c(0.12, 0.12, 85, 0.00075, 77, 0.05, 30, 120, 0.015, 0.015),
#                              min = c(0, 0.5, 300, 0.0009, 300, 0.05, 50, 50, 0, 0),
#                              max = c(0.3, 1, 1100, 0.01, 1300, 0.35, 250, 400, 0.15, 0.15)),
#                         row.names = c(NA, -10L),
#                         class = c("data.table", "data.frame"))
# colnames(baseParams) <- c("fn", "param", "mean", "sd", "min", "max")


baseParams <- structure(list(fn = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L),
                             param = c("mini", "peak", "slope", "cross", "mu", "ht", "sig1", "sig2", "base1", "base2"),
                             mean = c(0.115, 0.885, 0.0016, 765,630, 0.18, 130, 250, 0.05, 0.05),
                             sd = c(0.12, 0.12, 0.00075, 85, 77, 0.05, 30, 120, 0.015, 0.015),
                             min = c(0, 0.5, 0.0009, 300, 300, 0.05, 50, 50, 0, 0),
                             max = c(0.3, 1, 0.01, 1100, 1300, 0.35, 250, 400, 0.15, 0.15)),
                        row.names = c(NA, -10L),
                        class = c("data.table", "data.frame"))
colnames(baseParams) <- c("fn", "param", "mean", "sd", "min", "max")

## Add linear terms

## Estimate values for slope and intercept
# bb <- rnorm(5000, 0.05, 0.05)
# bb <- bb[bb > 0 & bb < 0.1]
#
# ep <- rnorm(length(bb), 1-bb, sd(bb))
# idx <- which(ep <= 1)
# ep <- ep[idx]
# bb <- bb[idx]
#
# mm <- (ep-bb)/2000


bp2 <- data.table(fn = c(3L, 3L),
                  param = c("intercept", "slope"),
                  mean = c(0.05, 0.0004444),
                  sd = c(0.05, 0.0000268),
                  min = c(0, 0.00035934),
                  max = c(0.1, 0.0005))

baseParams <- rbind(baseParams, bp2)
