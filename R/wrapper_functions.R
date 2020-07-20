#' Prepare us level empirical data into three levels
#'
#' This function prepares the empirical data. Depending on source it will call functions to prepare the csse or usafacts data
#' @param empirical_source source of empirical data.. must be 'csse','usafacts'
#' @param csse_repo_path defaults to NULL, otherwise a path to local clone of JHU CSSE repo
#' @param filterdates defaults to all NULL, otherwise a pair of dates (e.g. c("2020-03-01", "2020-06-01")
#' @param updategit defaults to FALSE; should the local csse git repo be queried for possible update?
#' @export
#' @examples
#' prepare_empirical_data("usafacts", "jhudata/", c("2020-03-01", "2020-06-01"))
#' prepare_empirical_data("csse")

us_empirical_by_level <- function(empirical_source,
                                   csse_repo_path=NULL,
                                   filterdates=NULL,
                                   updategit=F,
                                   supplement_territories=T) {


  emp_type = empirical_source

  if(emp_type=="usafacts") {
    empirical_base <- usafactsdata()[,list(Province_State, Admin2, FIPS, Date, cumConfirmed, cumDeaths, Confirmed, Deaths)]
    data.table::setnames(empirical_base,old="Province_State", new="USPS")
    empirical_base <- data.table::merge.data.table(empirical_base, statepops[,list(USPS,Province_State)], by="USPS")

    #As of 2020-05-05, usafacts does not have data for the territories
    #we are going to supplement by pulling the csse data, and adding in the
    #rows from the territories

    if(supplement_territories) {
      csse_territory_data <- cssedata(csse_repo_path,updategit = updategit)
      csse_territory_data <- data.table::merge.data.table(csse_territory_data,
                                                          statepops[USPS %in% c("AS","GU","MP","PR","VI"),list(USPS,Province_State)])

      empirical_base <- data.table::setorder(rbind(empirical_base,csse_territory_data),USPS, Admin2, Date)
    }
  }

  if(emp_type=="csse") {
    empirical_base <- data.table::merge.data.table(cssedata(csse_repo_path, updategit=updategit),
                                                   statepops[,.(Province_State, USPS)],
                                                   by="Province_State")
  }

  if(emp_type %in% c("usafacts","csse")) {
    #lets drop any rows without FIPS, because we are going to plot across these
    empirical_base <- empirical_base[!is.na(FIPS)]

    #add county population
    empirical_base[countypops,Population:=i.Population, on="FIPS"]

    county <- data.table::copy(empirical_base)
    if(!is.null(filterdates)) {
      county <- county[Date>=filterdates[1] & Date<=filterdates[2],]
    }
    county$Province_State=NULL
    setcolorder(county,c("USPS","FIPS", "Admin2", "Population","Date"))

    state <- data.table::copy(empirical_base)
    state <- state[,lapply(.SD, sum), by=c("USPS","Date"), .SDcols=c("cumConfirmed","cumDeaths","Confirmed", "Deaths")]
    state[statepops, Population:=i.Population, on="USPS"]
    setcolorder(state,c("USPS","Population","Date"))
    if(!is.null(filterdates)) {
      state <- state[Date>=filterdates[1] & Date<=filterdates[2],]
    }
    #for last, one, US, no need to make copy
    empirical_base <- empirical_base[,lapply(.SD, sum), by="Date", .SDcols=c("cumConfirmed","cumDeaths", "Confirmed","Deaths")]
    empirical_base[,Population:=statepops[,sum(Population,na.rm=T)]]
    if(!is.null(filterdates)) {
      empirical_base <- empirical_base[Date>=filterdates[1] & Date<=filterdates[2],]
    }
    setcolorder(empirical_base, c("Population","Date"))
  }


  return(list(us = empirical_base[], state=state[], county=county[]))

}

#' Prepare a utility dataframe for state names and abbreviations
#'
#' This function prepares a utility dataframe for state names and abbreviations
#' @noRd
#' @examples
#' #' statenames()
statenames <- function() {
  states_names <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut",
                    "Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois",
                    "Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts",
                    "Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada",
                    "New Hampshire","New Jersey","New Mexico","New York","North Carolina",
                    "North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
                    "Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont",
                    "Virginia","Washington","West Virginia","Wisconsin","Wyoming","Puerto Rico",
                    "American Samoa","Virgin Islands","Guam","Northern Mariana Islands")
  state_abbreviations <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA",
                           "MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX",
                           "UT","VT","VA","WA","WV","WI","WY","PR","AS","VI","GU", "MP")

  return(data.table::data.table(state_name = states_names, state_abbreviation=state_abbreviations))

}

