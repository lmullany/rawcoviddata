% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_functions.R
\name{smooth_outcome}
\alias{smooth_outcome}
\title{Get a smoothed outcome}
\usage{
smooth_outcome(
  dt,
  xvar = "Date",
  yvar = "Confirmed",
  knot_interval = 21,
  family = c("poisson", "gaussian")
)
}
\arguments{
\item{dt}{data frame containing the data to be smoothed}

\item{xvar}{string name of column in \code{dt} containing xvar, default is "Date"}

\item{yvar}{string name of column in \code{dt} containng yvar, default is "Confirmed"}

\item{knot_interval}{how frequently should the nots be placed? default is 21,
which in the case of plotting daily covid data, represents one knot every 3 weeks}

\item{family}{string indicating family, must be either "poisson" (default)
or "gaussian"}
}
\value{
vector of smoothed predictions
}
\description{
Function is a wrapped around \code{smooth_pre_process} and \code{smooth_dt}
}
\examples{
smooth_outcome(brazil)
}
\keyword{internal}
