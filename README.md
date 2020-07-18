# rawcoviddata
simple R package to read data from publicly available covid data repositories

# installation, using devtools()
devtools::install_github("lmullany/rawcoviddata")

# example usage in R
```r
#load the package
library(rawcoviddata)

#pull the csse data by country and date
global <- cssedataglobal() # pulls the csse data by country and date

#pull the usafacts data
usafactsdata()

#get us/state/county level data, as a list, indicating either usafacts or csse as the source data
us_empirical_by_level("csse")
us_empirical_by_level("usafacts")
```
