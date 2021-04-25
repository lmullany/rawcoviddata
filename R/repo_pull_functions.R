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
                           measure.vars = grep("\\d{1,4}-\\d{2,2}-\\d{2,2}",names(dt)),
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
                         by = c("countyFIPS", "County Name", "State", "StateFIPS", "Date"))

  #currently (5/5/2020) am renaming to match the csse data convention
  data.table::setnames(usafacts, old=c("countyFIPS", "County Name", "State"), new=c("FIPS", "Admin2", "Province_State"))

  #usafacts[,Date:=as.Date(Date,"%m/%d/%y")]

  #change FIPS to five character code, using state FIPS if unallocated
  #and using the county_fips if the data are allocated to specific county
  usafacts[, FIPS:= data.table::fifelse(stringr::str_starts(Admin2,"Statewide Unallocated"),
                            stringr::str_pad(StateFIPS,width=5,side="left",pad="0"),
                            stringr::str_pad(FIPS,width=5,side="left",pad="0"))]
  #drop the stateFIPS columns
  usafacts$StateFIPS = NULL

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

  dxtest <- data.table::fread(
      "https://healthdata.gov/api/views/j8mb-icvb/rows.csv"
      #jsonlite::fromJSON("https://legacy.healthdata.gov/api/3/action/package_show?id=c13c00e3-f3d0-4d49-8c43-bf600a6c0a0d")$result$resources[[1]]$url
  )
  return(dxtest[])
}

#' pull data with improved speed
#'
#' This function pulls state level testing data from private and public labs, aggregated at state level
#' @export
#' @examples
#' dxtestingdata()

fast_pull <- function() {

  base_url = "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/"
  c_url = paste0(base_url, "time_series_covid19_confirmed_US.csv")
  d_url = paste0(base_url, "time_series_covid19_deaths_US.csv")

  #get cases
  c = fread(c_url, showProgress = F, drop=c(1:4, 6:11))[!is.na(FIPS)]
  c[, FIPS:=stringr::str_pad(FIPS,width=5,pad="0",side="left")]
  #get deaths
  d = fread(d_url, showProgress = F, drop=c(1:4, 8:11))[!is.na(FIPS)]
  d[, FIPS:=stringr::str_pad(FIPS,width=5,pad="0",side="left")]
  #remove pop from deaths
  p = d[,c(1:4)]
  d = d[,!c(2:4)]

  #create a dates lookup table.. for later merging on date.
  dates = data.table("sDate" = colnames(d)[-1], "Date" = as.Date(colnames(d)[-1], "%m/%d/%y"))

  #melt cases and deaths
  c = melt(c,id.vars = "FIPS",value.name="cumConfirmed",variable.name = "sDate", variable.factor=FALSE)
  d = melt(d,id.vars = "FIPS",value.name="cumDeaths",variable.name = "sDate",variable.factor=FALSE)

  #add daily case and deaths
  c[, Confirmed:=cumConfirmed-shift(cumConfirmed), by=FIPS]
  d[, Deaths:=cumDeaths-shift(cumDeaths), by=FIPS]

  #cbind cases, and deaths, and merge with pop
  res = cbind(c,d[,c(3,4)])[p,on="FIPS"]

  #if cumConfirmed is 0 , then Confirmed must be (same with deaths)
  res[cumConfirmed==0,Confirmed:=0]
  res[cumDeaths==0,Deaths:=0]

  res <- res[dates,on="sDate"][,!"sDate"]

  return(res)


}
#' Function to pull global data quickly
#'
#' Function is faster approach to returning global data
#' @export
fast_pull_global <- function() {

  base_url = "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/"
  c_url = paste0(base_url, "time_series_covid19_confirmed_global.csv")
  d_url = paste0(base_url, "time_series_covid19_deaths_global.csv")

  #get cases
  c = fread(c_url, showProgress = F, drop=c(1,3,4))
  #get deaths
  d = fread(d_url, showProgress = F, drop=c(1,3,4))

  #create a dates lookup table.. for later merging on date.
  dates = data.table("sDate" = colnames(d)[-1], "Date" = as.Date(colnames(d)[-1], "%m/%d/%y"))

  #melt cases and deaths
  c = melt(c,id.vars = "Country/Region",value.name="cumConfirmed",variable.name = "sDate", variable.factor=FALSE)
  d = melt(d,id.vars = "Country/Region",value.name="cumDeaths",variable.name = "sDate",variable.factor=FALSE)

  #add over admin unit within country
  c <- c[, .("cumConfirmed" = sum(cumConfirmed)), by=.(`Country/Region`,sDate)]
  d <- d[, .("cumDeaths" = sum(cumDeaths)), by=.(`Country/Region`,sDate)]

  #add daily case and deaths
  c[, Confirmed:=cumConfirmed-shift(cumConfirmed), by=`Country/Region`]
  d[, Deaths:=cumDeaths-shift(cumDeaths), by=`Country/Region`]

  #cbind cases, and deaths, and merge with pop
  res = cbind(c,d[,c(3,4)])

  #if cumConfirmed is 0 , then Confirmed must be (same with deaths)
  res[cumConfirmed==0,Confirmed:=0]
  res[cumDeaths==0,Deaths:=0]

  res <- res[dates,on="sDate"][,!"sDate"]

  return(res)


}

