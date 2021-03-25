#' Pull the CSSE data from the repo
#'
#' This function pull csse data directly from JHU CSSE repo, or from a local clone, if provided
#' @param gitpath path to local repo (default is NULL, in which case, a live version is directly pulled using url)
#' @param updategit option to check for an update to the git repo
#' @import data.table
#' @export
#' @examples
#' cssedata("jhudata/")
cssedata <- function(gitpath=NULL, updategit=F) {

  read_jhu_wide_us_dt <- function(fname, outcomename) {
    dt <- data.table::fread(fname,showProgress = FALSE)
    idvars = c("FIPS","Admin2","Province_State")
      dt <- data.table::melt(dt,
                             id.vars=idvars,
                             measure.vars = grep("\\d{1,2}/\\d{1,2}/\\d{1,2}",names(dt)),
                             variable.name="Date", value.name=outcomename)
    return(dt)
  }


  #git pull to fetch any updates from the csse git repo, completely ignores errors and warnings
  if(!is.null(gitpath)) {
    if(updategit) {
      tryCatch({git2r::pull(gitpath)},
               warning=function(w) {},
               error = function(w) {})
    }
    basedir <- paste0(gitpath, "csse_covid_19_data/csse_covid_19_time_series/")
  } else {
    basedir <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/"
  }

  cases_fname <- paste0(basedir, "time_series_covid19_confirmed_US.csv")
  deaths_fname <-paste0(basedir, "time_series_covid19_deaths_US.csv")

  #Create long versions of these, by feeding to the read_jhu_wide function, above
  confirmed <- read_jhu_wide_us_dt(cases_fname, "cumConfirmed")
  deaths <- read_jhu_wide_us_dt(deaths_fname,"cumDeaths")

  #Inner join the confirmed and deaths data frames
  csse <- data.table::merge.data.table(confirmed, deaths, by = c("FIPS", "Admin2", "Province_State", "Date"))
  csse[,Date:= as.Date(Date, "%m/%d/%y")]
  csse[,FIPS:= stringr::str_pad(FIPS,width=5,side="left",pad="0")]

  csse[,`:=`(Confirmed=c(cumConfirmed[1],diff(cumConfirmed)),
             Deaths = c(cumDeaths[1], diff(cumDeaths))), by=.(FIPS, Admin2, Province_State)]
  setcolorder(csse,c("FIPS","Admin2","Province_State","Date"))

  #return the resulting combined dataframe
  #return(dplyr::as_tibble(csse))
  return(csse[,])

}

#' Pull the USAFacts data from the repo
#'
#' This function pulls the USA Facts Data
#' @export
#' @examples
#' usafactsdata()
usafactsdata <- function() {

  #function to read in a usafacts url (url) along with an outcome (outcomename)
  #and produce a long-formatted file
  read_usafacts_wide <- function(url, outcomename) {
    dt <- data.table::fread(url, showProgress = FALSE)
    dt <- data.table::melt(dt, id.vars = c(1,2,3,4),
                           measure.vars = grep("\\d{1,2}/\\d{1,2}/\\d{1,2}",names(dt)),
                           variable.name = "Date",
                           value.name = outcomename)
    return(dt)
  }

  #base url for usa facts covid repo
  baseurl <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/"

  #specific urls for the cases and deaths
  confirmed_url <- paste0(baseurl, "covid_confirmed_usafacts.csv")
  deaths_url <- paste0(baseurl, "covid_deaths_usafacts.csv")

  #get the confirmed and deaths from the website, using the function above
  #to get long-formatted data frames
  confirmed <- read_usafacts_wide(confirmed_url, "cumConfirmed")
  deaths <- read_usafacts_wide(deaths_url,"cumDeaths")

  #merge the data frames
  usafacts <- data.table::merge.data.table(confirmed, deaths,
                         by = c("countyFIPS", "County Name", "State", "stateFIPS", "Date"))
  #currently (5/5/2020) am renaming to match the csse data convention
  data.table::setnames(usafacts, old=c("countyFIPS", "County Name", "State"), new=c("FIPS", "Admin2", "Province_State"))
  usafacts[,Date:=as.Date(Date,"%m/%d/%y")]

  #change FIPS to five character code, using state FIPS if unallocated
  #and using the county_fips if the data are allocated to specific county
  usafacts[, FIPS:= dplyr::if_else(stringr::str_starts(Admin2,"Statewide Unallocated"),
                            stringr::str_pad(stateFIPS,width=5,side="left",pad="0"),
                            stringr::str_pad(FIPS,width=5,side="left",pad="0"))]
  #drop the stateFIPS columns
  usafacts$stateFIPS = NULL

  usafacts[,`:=`(Confirmed=c(cumConfirmed[1],diff(cumConfirmed)),
             Deaths = c(cumDeaths[1], diff(cumDeaths))), by=FIPS]

  #return the usafacts data
  #return(dplyr::as_tibble(usafacts))
  return(usafacts[])

}

#' Pull the Global CSSE data from the repo
#'
#' This function pulls county level data from JHU CSSE repo (directly by defualt, from local repo if provided). Pulls Country Level Data
#' @param gitpath optional path to local repo
#' @param updategit option to update the local git repo
#' @export
#' @examples
#' cssedataglobal("jhudata/")
#' cssedataglobal()
cssedataglobal <- function(gitpath=NULL, updategit=F) {

  read_jhu_wide_dt <- function(fname, outcomename) {
    dt <- data.table::fread(fname, showProgress = FALSE)
    vars_to_keep <- c(2,5:ncol(dt))
    dt <- dt[,..vars_to_keep]
    dt <- dt[,lapply(.SD, sum), by="Country/Region"]
    dt <- data.table::melt(dt, id.vars="Country/Region", variable.name="Date", value.name=outcomename)
    return(dt)
  }

  #git pull to fetch any updates from the csse git repo, completely ignores errors and warnings
  if(!is.null(gitpath)) {
    if(updategit) {
      tryCatch({git2r::pull(gitpath)},
               warning=function(w) {},
              error = function(w) {})
    }
    #Create pointers to the wide-formatted filenames in the repo that will be read in
    basedir <- paste0(gitpath, "csse_covid_19_data/csse_covid_19_time_series/")
  } else {
    basedir <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/"
  }

  cases_fname <- paste0(basedir, "time_series_covid19_confirmed_global.csv")
  deaths_fname <-paste0(basedir, "time_series_covid19_deaths_global.csv")

  #Create long versions of these, by feeding to the read_jhu_wide function, above
  confirmed <- read_jhu_wide_dt(cases_fname, "cumConfirmed")
  deaths <- read_jhu_wide_dt(deaths_fname,"cumDeaths")

  #Inner join the confirmed and deaths data frames
  csse <- data.table::merge.data.table(confirmed,deaths)
  csse[,`:=`(Confirmed=c(cumConfirmed[1],diff(cumConfirmed)),
             Deaths = c(cumDeaths[1], diff(cumDeaths))), by=`Country/Region`]
  #change date from character to Date format
  csse[,Date:=as.Date(Date,"%m/%d/%y")]

  #return the resulting combined dataframe
  return(csse[])

}

#' Pull the STATE level dx testing data from CDC
#'
#' This function pulls state level testing data from private and public labs, aggregated at state level
#' @export
#' @examples
#' dxtestingdata()
dxtestingdata <- function() {

  # TODO changing this to use the legacy sub-domain to keep the code working for now, but that URL will be retired in 2? weeks
  # need to migrate to use the new API released on the main site over the weekend.
  dxtest <- data.table::fread(
      jsonlite::fromJSON("https://legacy.healthdata.gov/api/3/action/package_show?id=c13c00e3-f3d0-4d49-8c43-bf600a6c0a0d")$result$resources[[1]]$url
  )
  return(dxtest[])
}



