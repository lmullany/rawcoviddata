#' Pull the CSSE data from the repo
#'
#' This function pull csse data directly from JHU CSSE repo, or from a local clone, if provided
#' @param gitpath path to local repo (default is NULL, in which case, a live version is directly pulled using url)
#' @param updategit option to check for an update to the git repo
#' @import data.table
#' @keywords internal
#' @examples
#' cssedata("jhudata/")
cssedata_old <- function(gitpath=NULL, updategit=F) {

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
#' @keywords internal
#' @examples
#' cssedataglobal("jhudata/")
#' cssedataglobal()
cssedataglobal_old <- function(gitpath=NULL, updategit=F) {

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
#' @param gitpath NULL if local gitpath is provided then a filename from this locaiton will be extracted
#' @param updategit FALSE; if set to TRUE the local git will be updated
#' @param return_compact FALSE; if set to TRUE a compact version of c,d,p will be returned. This
#' is slightly faster; data will be returned in wide format as initial downloaded from the JHU CSSE site
#' @export
#' @examples
#' cssedata()
cssedata <- function(gitpath = NULL, updategit=F, return_compact = F) {

  urls = get_urls(gitpath = gitpath, updategit = updategit, scope="US")
  #get cases
  c = fread(urls$case_url, showProgress = F, drop=c(2:4, 6:11))#[!is.na(FIPS)]
  c[is.na(FIPS), FIPS:=UID %% 84000000]
  c$UID <- NULL
  c[, FIPS:=stringr::str_pad(FIPS,width=5,pad="0",side="left")]

  #get deaths
  d = fread(urls$death_url, showProgress = F, drop=c(2:4, 8:11))#[!is.na(FIPS)]
  d[is.na(FIPS), FIPS:=UID %% 84000000]
  d[, FIPS:=stringr::str_pad(FIPS,width=5,pad="0",side="left")]
  d$UID <- NULL
  #remove pop from deaths
  p = d[,c(1:4)]
  d = d[,!c(2:4)]

  if(return_compact) return(list("c" = c,"d" = d,"p" =p))

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

  #if cumConfirmed is 0 and (confirmed is na or positive) , then Confirmed must be 0 (same with deaths)
  res[cumConfirmed==0 & (is.na(Confirmed) | Confirmed>0),Confirmed:=0]
  res[cumDeaths==0 & (is.na(Deaths) | Deaths>0),Deaths:=0]

  res <- res[dates,on="sDate"][,!"sDate"]

  return(res)


}
#' Function to pull global data quickly
#'
#' Function is faster approach to returning global data
#' @export
cssedataglobal <- function(gitpath=NULL, updategit=F) {

  urls = get_urls(gitpath = gitpath, updategit = updategit, scope="global")

  #get cases
  c = fread(urls$case_url, showProgress = F, drop=c(1,3,4))
  #get deaths
  d = fread(urls$death_url, showProgress = F, drop=c(1,3,4))

  #create a dates lookup table.. for later merging on date.
  dates = data.table("sDate" = colnames(d)[-1], "Date" = as.Date(colnames(d)[-1], "%m/%d/%y"))

  #melt cases and deaths
  c = melt(c,id.vars = "Country/Region",value.name="cumConfirmed",variable.name = "sDate", variable.factor=FALSE)
  d = melt(d,id.vars = "Country/Region",value.name="cumDeaths",variable.name = "sDate",variable.factor=FALSE)

  #add over admin unit within country
  c <- c[, .("cumConfirmed" = sum(cumConfirmed,na.rm = T)), by=.(`Country/Region`,sDate)]
  d <- d[, .("cumDeaths" = sum(cumDeaths,na.rm=T)), by=.(`Country/Region`,sDate)]

  #add daily case and deaths
  c[, Confirmed:=cumConfirmed-shift(cumConfirmed), by=`Country/Region`]
  d[, Deaths:=cumDeaths-shift(cumDeaths), by=`Country/Region`]

  #cbind cases, and deaths, and merge with pop
  res = cbind(c,d[,c(3,4)])


  #if cumConfirmed is 0 and (confirmed is na or positive) , then Confirmed must be 0 (same with deaths)
  res[cumConfirmed==0 & (is.na(Confirmed) | Confirmed>0),Confirmed:=0]
  res[cumDeaths==0 & (is.na(Deaths) | Deaths>0),Deaths:=0]

  res <- res[dates,on="sDate"][,!"sDate"]

  return(res)


}

#' Function to update git and finalize urls
#'
#' Function takes gitpath or NULL, a flag to update the git
#' and a scope
#' @param gitpath string path to local git repo, default NULL
#' @param updategit boolean default FALSE, set to TRUE to update the repo
#' @param scope = string, one of "US" or "global"
#' @export
#' @examples
#' get_urls(scope="US")
#' get_urls(scope="global")
#' get_urls(gitpath="mypath/", updategit=T, scope="US")
get_urls <- function(gitpath=NULL, updategit=F, scope=c("US","global")) {

    scope = match.arg(scope)

    if(!is.null(gitpath)) {
      if(updategit) {
        tryCatch({git2r::pull(gitpath)}, warning=function(w) {}, error = function(w) {})
      }
      #Create pointers to the wide-formatted filenames in the repo that will be read in
      basedir <- paste0(gitpath, "csse_covid_19_data/csse_covid_19_time_series/")
    } else {
      basedir <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/"
    }

    c_url = paste0(basedir, "time_series_covid19_confirmed_",scope,".csv")
    d_url = paste0(basedir, "time_series_covid19_deaths_", scope, ".csv")

    return(list("case_url" = c_url,
                "death_url" = d_url)
           )

}

#' Function to just get a single state given cdp
#'
#' Function use compact csse data (c,d,p) to return
#' a datatable for a specific state only
#' @param cdp compact version of cases, deaths, population as return from
#' cssedata(return_compact=T)
#' @param state string (default is NULL, will return all states), otherwise specifiy a
#' specifc state abbreviation (i.e. "TX")
#' @param fix_cumul (logical, default=F); set to TRUE to fix cumulative data
#' @param type (string, default="mid"); strategy to fix cumulative data
#' @export
#' @examples
#' get_state_from_cdp(cdp, "MD")
get_state_from_cdp <- function(cdp, state=NULL, fix_cumul=FALSE,type=c("mid", "low", "high")) {
  type=match.arg(type)
  if(is.null(state)) {
    return(get_all_states_from_cdp(cdp, fix_cumul=fix_cumul, type=type))
  } else {
    fips = cdp$p[Province_State == statenames()[state_abbreviation==state,state_name],FIPS]
    return(quick_melt(cdp$c[FIPS %chin% fips], cdp$d[FIPS %chin% fips], fix_cumul = fix_cumul, type=type)[])
  }

}

#' Function to just get a US from c,d,p
#'
#' Function use compact csse data (c,d,p) to return
#' a datatable for entire US
#' @param cdp compact version of cases, deaths, population as return from
#' cssedata(return_compact=T)
#' @param fix_cumul (logical, default=F); set to TRUE to fix cumulative data
#' @param type (string, default="mid"); strategy to fix cumulative data
#' @export
#' @examples
#' get_us_from_cdp(c,d,p)
get_us_from_cdp <- function(cdp, fix_cumul=FALSE, type=c("mid", "low", "high")) {
  return(quick_melt(cdp$c,cdp$d, fix_cumul = fix_cumul, type=type)[])
}

#' Function to just get a US county from c,d,p
#'
#' Function use compact csse data (c,d,p) to return
#' a datatable for a single county
#' @param fips five-digit fips
#' @param cdp compact version of cases, deaths, population as return from
#' cssedata(return_compact=T)
#' @param fix_cumul (logical, default=F); set to TRUE to fix cumulative data
#' @param type (string, default="mid"); strategy to fix cumulative data
#' @export
#' @examples
#' get_county_from_cdp(21027,cdp)
get_county_from_cdp <- function(fips,cdp, fix_cumul=FALSE, type=c("mid", "low", "high")) {
  type=match.arg(type)
  k = cbind(t(cdp$c[FIPS==fips,-1]), t(cdp$d[FIPS==fips,-1]))
  k <- data.table(Date=data.table::as.IDate(rownames(k),"%m/%d/%y"), cumConfirmed = k[,1], cumDeaths = k[,2])
  if(fix_cumul) {
    k[, `:=`(cumConfirmed = fix_cumul_counts(cumConfirmed, type=type), cumDeaths=fix_cumul_counts(cumDeaths, type=type))]
  }
  k[, `:=`(Confirmed=cumConfirmed-shift(cumConfirmed),Deaths=cumDeaths-shift(cumDeaths))]
  return(k[])
}


#' Function to quickly melt c an d into a data table
#'
#' Given a compact version of cases and deaths, function
#' will convert the information into a datatable
#' first summing over all the rows of c and d
#' @param c compact version of confirmed cases
#' @param d compact version of deaths
#' @param fix_cumul (logical, default=F); set to TRUE to fix cumulative data
#' @param type (string, default="mid"); strategy to fix cumulative data
#' @export
#' @examples
#' quick_melt(c,d)
quick_melt <- function(c,d, fix_cumul=FALSE, type=c("mid","low","high")) {

  type=match.arg(type)

  k <- cbind(t(c[,lapply(.SD,sum), .SDcols=-1]),t(d[,lapply(.SD,sum), .SDcols=-1]))
  k <- data.table(Date=data.table::as.IDate(rownames(k),"%m/%d/%y"), cumConfirmed = k[,1], cumDeaths = k[,2])
  if(fix_cumul) {
    k[, `:=`(cumConfirmed = fix_cumul_counts(cumConfirmed, type=type), cumDeaths=fix_cumul_counts(cumDeaths, type=type))]
  }
  k[, `:=`(Confirmed=cumConfirmed-shift(cumConfirmed),Deaths=cumDeaths-shift(cumDeaths))]
  return(k)
}

#'Convert long csse data from daily to weekly format
#'
#'This function will take a long format csse file (for example
#'something like that returned from `get_us_from_cdp(cdp)`, or
#'the long format returned from `cssedata()`, or the geo-level
#'elements in the list returney `us_empirical_by_level()`) and
#'convert it from daily data to weekly format.  The function will
#'use epiweek and epiyear from the lubridate package, and will
#'define the end of the week as day of week 7. The general approach
#'of the function is to sum up incident outcomes by epiweek/epiyear
#'combinations, and then generate weekly cumulative data by using
#'baseR `cumsum` function.
#'
#' @param df this is the source long formatted data described above
#' @param byvars defaults to NULL, character vector of columns names to
#' group by
#' @export
#' @examples
#' convert_weekly(us)
#' convert_weekly(get_state_from_cdp("Texas",cdp), byvar="USPS")
#' convert_weekly(county, byvars="FIPS")
convert_weekly <- function(df, byvars=NULL) {
  # get day-of-week, wk,yr
  df[,`:=`(wk=lubridate::epiweek(Date), yr=lubridate::epiyear(Date))]
  # get column Sums, by byvars,
  df = df[,as.list(colSums(.SD,na.rm=T)), by=c(byvars,"wk","yr"),.SDcols=c("Confirmed", "Deaths")]
  # add cumulative columns
  df[,`:=`(cumConfirmed=cumsum(Confirmed),cumDeaths=cumsum(Deaths)),
     by=byvars][]
}



#'Get all states from cdp
#'
#'This function extends the approach of `get_state_from_cdp()`, returning
#'all states in long format, rather than a single state
#'
#' @param cdp this is a cdp list as returned by `cssedata(return_compact=T)`
#' @param fix_cumul (logical, default=F); set to TRUE to fix cumulative data
#' @param type (string, default="mid"); strategy to fix cumulative data
#' @export
#' @examples
#' get_all_states_from_cdp(cdp)

get_all_states_from_cdp <- function(cdp, fix_cumul=FALSE, type=c("mid","low","high")) {
  type=match.arg(type)

  p = statenames()[cdp$p, on=.(state_name=Province_State), j=.(FIPS,USPS=state_abbreviation)]
  ps = length(unique(p$USPS))
  k = cbind(
    data.table::melt(cdp$c[p, on="FIPS"][
      , lapply(.SD, sum,na.rm=T),by=.(USPS), .SDcols=-1], id="USPS", value.name = "cumConfirmed"),
    data.table::melt(cdp$d[p, on="FIPS"][
      , lapply(.SD, sum,na.rm=T),by=.(USPS), .SDcols=-1], id="USPS", value.name="cumDeaths")[,.(cumDeaths)]
  )

  if(fix_cumul) {
    k[, `:=`(cumConfirmed = fix_cumul_counts(cumConfirmed, type=type), cumDeaths=fix_cumul_counts(cumDeaths, type=type)), by=.(USPS)]
  }
  k[,`:=`(
    Confirmed=cumConfirmed-shift(cumConfirmed),
    Deaths=cumDeaths-shift(cumDeaths)), by=.(USPS)][
      ,`:=`(Date=rep(as.IDate(colnames(cdp$c)[-1], "%m/%d/%y"),ps),
            variable=NULL)][,.(USPS, Date,cumConfirmed, cumDeaths, Confirmed, Deaths)][]

}

#'Get all counties from cdp
#'
#'This function extends the approach of `get_county_from_cdp()`, returning
#'all counties in long format, rather than a single county
#'
#' @param cdp this is a cdp list as returned by `cssedata(return_compact=T)`
#' @param fix_cumul (logical, default=F); set to TRUE to fix cumulative data
#' @param type (string, default="mid"); strategy to fix cumulative data
#' @export
#' @examples
#' get_all_counties_from_cdp(cdp)
get_all_counties_from_cdp <- function(cdp, fix_cumul=F, type=c("mid", "low", "high")) {
  type=match.arg(type)
  dates = as.IDate(colnames(cdp$c)[-1], "%m/%d/%y")
  k <- data.table(
    FIPS = rep(cdp$c$FIPS, each=length(dates)),
    Date = rep(dates,times=nrow(cdp$c)),
    cumConfirmed = as.vector(t(cdp$c[,-1])),
    cumDeaths = as.vector(t(cdp$d[,-1]))
  )
  if(fix_cumul) {
    k[, `:=`(cumConfirmed = fix_cumul_counts(cumConfirmed, type=type), cumDeaths=fix_cumul_counts(cumDeaths, type=type)), by=.(FIPS)]
  }
  k[, `:=`(Confirmed=cumConfirmed-shift(cumConfirmed),
           Deaths=cumDeaths-shift(cumDeaths)),
    by=.(FIPS)][]
}


#'correct cumulative, using low strategy
#' @param x vector of cumulative outcomes
#' @keywords internal
#' @examples
#' c_low(x)

c_low <- function(x) {
  x[is.na(x)] <- Inf
  x <- rev(cummin(rev(x)))
  x[!is.finite(x)] <- 0
  x <- cummax(x)
}

#'correct cumulative, using high strategy
#' @param x vector of cumulative outcomes
#' @keywords internal
#' @examples
#' c_high(x)

c_high <- function(x) {
  x[is.na(x)] <- 0
  cummax(x)
}

#'correct cumulative, using mid strategy
#' @param x vector of cumulative outcomes
#' @keywords internal
#' @examples
#' c_mid(x)

c_mid <- function(x) {
  unlagged=x
  unlagged[is.na(x)] <- Inf
  lagged = shift(x)
  lagged[is.na(x)] <- Inf

  max_indices <- which(unlagged < lagged)
  x[max_indices] <- lagged[max_indices]
  c_low(x)

}

#' correct cumulative, using low, med, or high strategy
#' @param x vector of cumulative outcomes
#' @param type string default is "mid"
#' @keywords internal
#' @examples
#' fix_cumul_counts(x)


fix_cumul_counts <- function(x,type=c("mid","low","high")) {
  type=match.arg(type)
  list("high"=c_high, "low"=c_low, "mid"=c_mid)[[type]](x)
}
