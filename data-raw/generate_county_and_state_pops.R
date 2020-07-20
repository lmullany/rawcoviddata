#code to produce the statepops and countypops internal datasets

# First, get the county level pops from census.gov
countypops <- fread("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv",select = c(1:7,19))

# Second, statepops are the rows where COUNTY==0
statepops <- countypops[COUNTY==0,.(STNAME,POPESTIMATE2019)]
setnames(statepops,old=c("Province_State", "Population"))
# ADD USPS, and merge
snames = statenames()
setnames(snames,old=c("Province_State","USPS"))
statepops = snames[statepops, Population:=i.Population, on="Province_State"]

# Third, clean up countypops
countypops = setnames(countypops[COUNTY!=0,
                        ][,FIPS:=paste0(
                          stringr::str_pad(STATE,2,"left","0"),
                          stringr::str_pad(COUNTY,3,"left","0"))
                          ][,.(FIPS,POPESTIMATE2019)], old=c("FIPS","Population")
)

#save as internal data objects
usethis::use_data(statepops, countypops, internal=TRUE, overwrite=T)




