# mnsentinellakes

The Minnesota Sentinel Lakes Program is a Long-Term Ecological Monitoring Program on 25 Minnesota lakes, lead by the Minnesota Department of Natural Resources Fisheries Section along with the Minnesota Pollution Control Agency. This package includes a variety of tools developed to aid in the management of this program. It contains functions used to access, analyze, and visualize Sentinel Lakes data. If available, these functions can be used with data from Minnesota lakes that are not a part of the Sentinel Lakes Program.

### Installing
Install the devtools package and load it.
```
install.packages("devtools")
library(devtools)
```
Currently the mnsentinellakes is only available on github and must be installed from it.
```
devtools::install_github("mnsentinellakes/mnsentinellakes")
```

### Examples
Currently, there are several different datasets that can be accessed directly through this package including fisheries survey, water level, and ice data from the MNDNR, water quality data from the MNPCA, and weather data from the Iowa State Mesonet. Below are some examples for downloading, processing, and visualizing some of these datasets.

##### Fisheries
```
#Download lakefinder survey data
x <- lakefinderdownload("21005700")

#Select the fish survey data
y <- fishsurveydata(x)

#Select fish species according to the optimal survey gear
z <- fishtable(
       fishsurvey = y,
       fishspecies = c("Bluegill","Black Crappie","Walleye","Largemouth Bass","Northern Pike","Yellow Perch")
     )
      
#Create Trend Plots
fishtrendplots(z)
```
##### Water Quality
```
#Download the Water Quality Data
x <- wqdatadownload(c("15-0010-00-100","15-0010-00-101","15-0010-00-102"))

#Process multiple parameters for multiple months within a given year range
y <- wqmonthtable(
       wqdata = x,
       parameters = c("Depth, Secchi disk depth","Temperature, water","pH","Chloride"),
       months = c(7,8,9),
       startyear = 2008,
       endyear = 2018
      )
#Create Trend Plots
wqmonthtrendplots(y)
```
##### Sentinel Lakes Format
Because the data for the Sentinel Lakes Program come from a variety of agencies, this package provides several functions that formats the data into similar schemas, making it easier to understand and compare the data.
```
#Convert Fish Survey Data into the Sentinel Lakes Format
fish2sentinel(y)

#Convert Water Quality Data into the Sentinel Lakes Format
wq2sentinel(x)

#The ice, water level, and weather data are already processed into the Sentinel Lakes format when they are downloaded.
```
##### Lake Ids and Names
This package includes a number of functions to handle issues with Minnesota Lake Ids as well as convert Lake Ids to Names and vice versa.
```
#Remove punctuation and replace spaces with underscores
addunderscore("St. Olaf")

#Convert LakeIds to characters and add "0" at the beginning of LakeIds with only 7 characters. Remove dashes.
fixlakeid(6000200)
fixlakeid("21-0057-00")

#Look up a lake name based upon the LakeId
lakeidtoname("11041300")

#Look up LakeIds from a lake name and county
lakenametoid(
  lakename = "Greenwood",
  county = "Cook"
)
```
