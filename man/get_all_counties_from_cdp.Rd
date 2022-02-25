% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repo_pull_functions.R
\name{get_all_counties_from_cdp}
\alias{get_all_counties_from_cdp}
\title{Get all counties from cdp}
\usage{
get_all_counties_from_cdp(cdp)
}
\arguments{
\item{cdp}{this is a cdp list as returned by \code{cssedata(return_compact=T)}}
}
\description{
This function extends the approach of \code{get_county_from_cdp()}, returning
all counties in long format, rather than a single county
}
\examples{
get_all_counties_from_cdp(cdp)
}