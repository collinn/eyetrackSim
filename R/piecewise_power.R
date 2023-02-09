
#' Function to create piecewise line
#' @export
pline <- function(p, time, gp) {
  stopifnot(length(p) == 2)

  ll <- length(time)
  mm <- round(ll/2)
  r1 <- `:`(1, mm+1) # range first half
  r2 <- `:`(mm+1, ll) # range second half

  y <- vector("numeric", length = length(time))
  if (gp == "A") {
    y[r1] <- p[1]
    y[r2] <- p[1] + p[2] * time[r2]
  } else {
    y[] <- p[1]
  }
  y
}

#' Create piecewise linear data
#'
#' @param n stuff
#' @param trials stuff
#' @param ar1 stuff
#' @param pars stuff
#' @param manymeans stuff
#' @param TIME stuff
#'
#' @export
createPlineData <- function(n = 25, trials = 100, ar1 = FALSE, pars = c(0,0.5),
                            manymeans = TRUE,
                            TIME = seq(-2, 2, length.out = 501),
                            distSig = 0.025) {

  ## Make first group
  p <- rmvnorm(n, mean = pars, sigma = diag(length(pars))*distSig)

  if (!manymeans) {
    p[,1] <- min(abs(p[,1]))
    p[,2] <- max(abs(p[,2]))
  }
  p1 <- p

  p <- abs(p)
  spars <- split(p, row(p))
  dts <- lapply(seq_len(n), function(x) {
    pp <- spars[[x]]
    dt <- data.table(id = x,
                     time = TIME,
                     group = "A",
                     true = pline(pp, TIME, "A"))
    if (ar1) {
      dt[, fixations := addARerror(val = true, rho = 0.8, sig = 0.25/sqrt(trials))]
    } else {
      dt[, fixations := rnorm(1, true, sd = 0.25/sqrt(trials)), by = time]
    }
  })
  dtsA <- rbindlist(dts)

  # make second group
  pars <- c(0,0)
  p <- rmvnorm(n, mean = pars, sigma = diag(length(pars))*distSig)
  p[,1] <- p1[,1]
  p[,2] <- p[,1] # make there be no slope

  if (!manymeans) {
    p[] <- p[1,1]
  }

  p <- abs(p)
  spars <- split(p, row(p))
  dts <- lapply(seq_len(n), function(x) {
    pp <- spars[[x]]
    dt <- data.table(id = x + n,
                     time = TIME,
                     group = "B",
                     true = pline(pp, TIME, "B"))
    if (ar1) {
      dt[, fixations := addARerror(val = true, rho = 0.8, sig = 0.25/sqrt(trials))]
    } else {
      dt[, fixations := rnorm(1, true, sd = 0.25/sqrt(trials)), by = time]
    }
  })
  dtsB <- rbindlist(dts)
  rbindlist(list(dtsA, dtsB))
}

#' piecewise linear for bdots
#' @export
plinePars <- function(dat, y, time, params = NULL, ...) {
  y2 <- dat[[y]]

  ll <- length(y2)
  mm <- round(ll/2)
  r1 <- `:`(1, mm+1) # range first half
  r2 <- `:`(mm+1, ll) # range second half
  yy <- y2[r2]
  tt <- dat[[time]][r2]

  ## baseline
  b <- mean(y2[r1]) / 10 # let's move this b* closer to zero
  ## slope
  m <- max(0, (yy %*% tt) / (tt %*% tt))
  params <- c(b, m)
  names(params) <- c("b", "m")

  y <- str2lang(y)
  time <- str2lang(time)
  ff <- bquote(.(y) ~ (.(time) < 0)*b +
                 (.(time) >= 0)*(m*.(time) + b))
  attr(ff, "parnames") <- names(params)
  return(list(formula = ff, params = params))
}




#' Create piecewise linear data
#'
#' Again, because maybe I'm retarded?
#'
#' @param n stuff
#' @param trials stuff
#' @param ar1 stuff
#' @param pars stuff
#' @param manymeans stuff
#' @param TIME stuff
#'
#' @export
createPlineData2 <- function(n = 25, trials = 100, ar1 = FALSE, pars = c(0,0.5),
                            manymeans = TRUE,
                            TIME = seq(-0.5, 2, length.out = 501),
                            distSig = 0.025) {

  ## Make first group
  p <- rmvnorm(n, mean = pars, sigma = diag(length(pars))*distSig)

  if (!manymeans) {
    p[,1] <- min(abs(p[,1]))
    p[,2] <- max(abs(p[,2]))
  }

  p <- abs(p)
  p1 <- p # will use this for other group

  spars <- split(p, row(p))
  dts <- lapply(seq_len(n), function(x) {
    pp <- spars[[x]]
    dt <- data.table(id = x,
                     time = TIME,
                     group = "A",
                     true = pline(pp, TIME, "A"))
    if (ar1) {
      dt[, fixations := addARerror(val = true, rho = 0.8, sig = 0.25/sqrt(trials))]
    } else {
      dt[, fixations := rnorm(1, true, sd = 0.25/sqrt(trials)), by = time]
    }
  })
  dtsA <- rbindlist(dts)

  # make second group
  pars <- c(0,0)
  p <- rmvnorm(n, mean = pars, sigma = diag(length(pars))*distSig)
  p[,1] <- p1[,1]
  #p[,2] <- p[,1] # make there be no slope

  if (!manymeans) {
    p[,1] <- min(abs(p[,1]))
    p[,2] <- min(abs(p[,2]))
  }

  p <- abs(p)
  spars <- split(p, row(p))
  dts <- lapply(seq_len(n), function(x) {
    pp <- spars[[x]]
    dt <- data.table(id = x + n,
                     time = TIME,
                     group = "B",
                     true = pline(pp, TIME, "B"))
    if (ar1) {
      dt[, fixations := addARerror(val = true, rho = 0.8, sig = 0.25/sqrt(trials))]
    } else {
      dt[, fixations := rnorm(1, true, sd = 0.25/sqrt(trials)), by = time]
    }
  })
  dtsB <- rbindlist(dts)
  rbindlist(list(dtsA, dtsB))
}
