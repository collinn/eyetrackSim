

## This compute overall TIE across all time points


getFWER <- function(y) {
  tt <- readRDS(y)
  sigs <- lapply(tt,  function(y) {
    y <- lapply(y, `[[`, 1)
  })

  sm <- sapply(sigs, function(x) !is.null(x[[1]])) |> mean()
  mm <- sapply(sigs, function(x) !is.null(x[[2]])) |> mean()
  pm <- sapply(sigs, function(x) !is.null(x[[3]])) |> mean()

  smt <- sapply(sigs, function(x) timetie(x[[1]])) |> rowMeans()
  mmt <- sapply(sigs, function(x) timetie(x[[2]])) |> rowMeans()
  pmt <- sapply(sigs, function(x) timetie(x[[3]])) |> rowMeans()

  fwer <- data.table(sm = sm, mm = mm, pm = pm)
  tsmat <- matrix(c(smt, mmt, pmt), byrow = TRUE, nrow = 3,
                  dimnames = list(c("sm", "mm", "pm"), NULL))
  return(list(fwer = fwer, timeSliceMat = tsmat))
}

#' Time Slice FWER rate
#'
#' Given list returned from getFWER (of all simulations) return function of
#' number of errors. For example, what is the mean/median number of of per comparison
#' error rate per simulation
#'
#' @param mm signifiance matrix
#' @export
timeSliceFwer <- function(y, f = mean) {
  y <- lapply(y, `[[`, 2)
  y <- lapply(y, function(z) {
    apply(z, 1, f)
  })
  y <- Reduce(rbind, y)
  #y <- sapply(y, rowMeans)
  rownames(y) <- paste0("sim", 1:8)
  y
}


## Given signifiance mat, need to return length 401 bool vector

#' Signifiance matrix to boolean vector
#'
#' Create length 401 boolean vector of where sig difference detected
#'
#' @param mm signifiance matrix
#' @export
timetie <- function(mm) {
  time <- seq(0, 1600, 4)
  vec <- vector("numeric", length = length(time))

  if (is.null(dim(mm))) {
    vec <- as.logical(vec)
    return(vec)
  }

  # each of these into a list
  sm <- split(mm, row(mm))
  bv <- lapply(sm, function(m) {
    sigt <- do.call(seq, as.list(c(m, 4)))
  })
  bv <- Reduce(union, bv)
  rr <- time %in% bv
  return(rr)
}

