% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repo_pull_functions.R
\name{fix_cumul_counts}
\alias{fix_cumul_counts}
\title{correct cumulative, using low, med, or high strategy}
\usage{
fix_cumul_counts(x, type = c("mid", "low", "high"))
}
\arguments{
\item{x}{vector of cumulative outcomes}

\item{type}{string default is "mid"}
}
\description{
correct cumulative, using low, med, or high strategy
}
\examples{
fix_cumul_counts(x)
}
\keyword{internal}
