% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/onset_fix_comparison_sim_functions.R
\name{runSim_compare}
\alias{runSim_compare}
\title{Run simulation for multiple subjects}
\usage{
runSim_compare(
  nsub = 10,
  ntrials = 300,
  fnct = "logistic",
  fbst = FALSE,
  omDelay = 0,
  group = "A",
  alt = FALSE
)
}
\arguments{
\item{nsub}{numeric, number of subjects in simulation}

\item{ntrials}{number of vwp trials per subject}

\item{fnct}{character vector indicatin curve type}

\item{fbst}{use FBS+T assumption or not}

\item{omDelay}{alternative mechanism for eye delay, can be constant or active bound var}

\item{group}{name of group}

\item{alt}{TRUE/FALSE this determines if we use baseParams or baseParams2 for creating data}
}
\value{
This runs simluation for single subject, returns a list
containing information on subject, as well as trial data
}
\description{
Run simulation for multiple subjects
}
