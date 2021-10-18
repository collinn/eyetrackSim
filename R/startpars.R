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
baseParams <- structure(list(fn = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L),
                             param = c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 5L, 6L),
                             mean = c(0.115, 0.885, 765, 0.0016, 630, 0.18, 130, 250, 0.05, 0.05),
                             sd = c(0.12, 0.12, 85, 0.00075, 77, 0.05, 30, 120, 0.015, 0.015),
                             min = c(0, 0.5, 300, 0.0009, 300, 0.05, 50, 50, 0, 0),
                             max = c(0.3, 1, 1100, 0.01, 1300, 0.35, 250, 400, 0.15, 0.15)),
                        row.names = c(NA, -10L),
                        class = c("data.table", "data.frame"))
colnames(baseParams) <- c("fn", "param", "mean", "sd", "min", "max")
