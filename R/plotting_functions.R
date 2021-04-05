#' Plot single location
#'
#' Function receives data frame/tibble/dt, and names of smoothed
#' the empirical data and the smoothed outcome, xvar, and yvar
#' and returns a plot
#' @param dt data frame containing the data to be plotted
#' @param sm string name of column in `dt` containing smoothed observations
#' @param xvar string name of column in `dt` containing xvar, default is "Date"
#' @param yvar string name of column in `dt` containng yvar, default is "Confirmed"
#' @return ggplot2 object
#' @keywords internal
#' @examples
#' plot_single_location(brazil)
#' plot_single_location(us,yvar="Deaths")
plot_single_location <- function(dt,sm="phat",xvar="Date",yvar="Confirmed",...) {

  plt <- ggplot2::ggplot(dt, ggplot2::aes(x=!!rlang::sym(xvar))) +
    ggplot2::geom_point(ggplot2::aes(y=!!rlang::sym(yvar)), color="black") +
    ggplot2::geom_line(ggplot2::aes(y=!!rlang::sym(sm)), color="blue", size=1.5)

  return(plt)
}

#' Return smooth predictions, using generalized additive model
#'
#' Function returns the predictions from a simple generalized
#' additive model that uses natural cubic splines to regress
#' y on splines of x, with k knots, given the family
#' @param x vector of x values
#' @param y vector of y values
#' @param k integer number of knots
#' @param family string indicating family, must be either "poisson" (default)
#' or "gaussian"
#' @return vector of smoothed predictions
#' @keywords internal
#' @examples
#' smooth_dt(x,y,10,"poisson")
smooth_dt <- function(x,y,k,family=c("poisson","gaussian")) {
  family = match.arg(family)
  pmod = mgcv::gam(y~s(x,bs="cs",k=k), family=family)
  phat = predict(pmod,newdata=data.frame(x=x))
  if(family=="poisson") return(exp(phat))
  else return(phat)
}

#' Pre process smoothing
#'
#' Function pre-processes data for sending to smoothing function
#' @param dt data frame containing the data to be smoothed
#' @param xvar string name of column in `dt` containing xvar, default is "Date"
#' @param yvar string name of column in `dt` containng yvar, default is "Confirmed"
#' @param knot_interval how frequently should the nots be placed? default is 21,
#' which in the case of plotting daily covid data, represents one knot every 3 weeks
#' @param family string indicating family, must be either "poisson" (default)
#' or "gaussian"
#' @return named list containing x,y,and k, which can be fed to `smooth_dt`
#' @keywords internal
#' @examples
#' smooth_pre_process(brazil)
smooth_pre_process <- function(dt,xvar="Date",yvar="Confirmed",knot_interval=21,
                               family=c("poisson","gaussian")) {
  family = match.arg(family)

  x=as.numeric(dt[[xvar]])
  y=dt[[yvar]]
  knots = floor(sum(!is.na(x))/knot_interval)

  if(family=="poisson") {
    y[y<0] <- NA
  }
  return(list("x"=x,"y"=y,"k"=knots))
}

#' Get a smoothed outcome
#'
#' Function is a wrapped around `smooth_pre_process` and `smooth_dt`
#' @param dt data frame containing the data to be smoothed
#' @param xvar string name of column in `dt` containing xvar, default is "Date"
#' @param yvar string name of column in `dt` containng yvar, default is "Confirmed"
#' @param knot_interval how frequently should the nots be placed? default is 21,
#' which in the case of plotting daily covid data, represents one knot every 3 weeks
#' @param family string indicating family, must be either "poisson" (default)
#' or "gaussian"
#' @return vector of smoothed predictions
#' @keywords internal
#' @examples
#' smooth_outcome(brazil)
smooth_outcome <- function(dt,xvar="Date",yvar="Confirmed",knot_interval = 21,
                           family=c("poisson", "gaussian")) {

  family = match.arg(family)

  sm_i <- smooth_pre_process(dt,xvar=xvar,yvar=yvar,knot_interval=knot_interval,family=family)

  phat <- smooth_dt(sm_i$x,sm_i$y,sm_i$k,family)

  return(phat)
}

#' Function to plot a location
#'
#' Function returns plot of a given location. The only required input is
#' a data frame containing covid data to be plotted. Without any other
#' arguments, the function will look for an xvar named "Date", an outcome
#' variable named "Confirmed", and will smooth these using a generalized additive
#' model with natural cubic spline regression (following `mgcv::gam`, with `bs="cs"`)
#' and knots placed every 21 days, and poisson distribution assumed for the outcome
#' These parameters can be adjusted by the user, including passing along user-defined
#' values for `xvar` (for example "date"), `yvar` (e.g. "Deaths"), `knot_interval` (e.g.
#' 7), and `family` (e.g. "gaussian")
#' @param dt data frame containing the data
#' @param ... parameters (as above) passed on
#' @return ggplot2 plot
#' @export
#' @examples
#' plot_covid(brazil)
#' plot_covid(australia, yvar="Deaths", knot_interval=7, family="gaussian")
plot_covid <- function(dt,...) {
  # set DT and make copy
  gginput <- data.table::copy(data.table::setDT(dt))
  # smooth
  gginput[,phat:=smooth_outcome(gginput,...)]
  # plot
  plot_single_location(gginput,"phat",...)

}


