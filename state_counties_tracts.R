library(tigris)

library(sp)
library(rgeos)
library(rgdal)
library(tidyverse)

library(censusapi)
library(maps)
library(GISTools)
source("key.R")

# Get list of FIPS codes by state
data(state.fips)

state_fips <- state.fips %>%
  select(fips, abb) %>%
  unique()

state_fips <- rbind(state_fips, data.frame(fips=2,abb="AK"))
state_fips$fips <- as.character(state_fips$fips)
state_fips$state <- ifelse(nchar(state_fips$fips)==1, paste0("0", state_fips$fips), state_fips$fips)

# Get list of FIPS codes by county

counties <- getCensus(name="acs5", 
                      vintage=2014,
                      key=key, 
                      vars=c("NAME", "B02001_001E"), 
                      region="county:*")
counties$state_name <- gsub(".*,", "", counties$NAME)
counties$county_name <- gsub(",.*", "", counties$NAME)


relationship <- left_join(counties, state_fips, by="state")
relationship$B02001_001E <- NULL


for (i in 1:nrow(state_fips)) {
  
  region_in <- paste0("state:", state_fips$state[i])
  
  
  census_tracts <- getCensus(name="acs5", 
                             vintage=2014,
                             key=key, 
                             vars=c("NAME", "B23025_005E", "B23025_002E"), 
                             region="tract:*", regionin=region_in)
  
  #census_tracts$B02001_001E <- NULL
  census_tracts$tract_name <- gsub(",.*", "", census_tracts$NAME)
  
  census_tracts$NAME <- NULL
  
  if (i ==1) {
    census_tracts_all <- census_tracts
  } else {
    census_tracts_all <- rbind(census_tracts_all, census_tracts)
  }
  
}

relationship <- left_join(relationship, census_tracts_all, by=c("state", "county"))

relationship <- filter(relationship, !is.na(tract_name))
relationship$tract_full <- paste0(relationship$state, relationship$county, relationship$tract)
relationship$state_county <- paste0(relationship$state, relationship$county) 
write.csv(relationship, "states_counties_tracts.csv")

# getting shapefiles now
states_counties_tracts$county_name <- gsub(" County", "", states_counties_tracts$county_name)

#unique_counties <- unique(states_counties_tracts$NAME)

unique_counties <- states_counties_tracts[!duplicated(states_counties_tracts$NAME),]

for (i in 1:nrow(unique_counties)) {
  #county_name <- unique_counties$NAME[i]
  
  #filtered <- subset(states_counties_tracts, NAME==county_name)
  
  file_name <- paste0(state=unique_counties$state[i], county=unique_counties$county[i])

  if(!file.exists(paste0("sm_shapes/", file_name, ".dbf"))) {
  tract_sm <- tracts(state=unique_counties$state[i], county=unique_counties$county[i], cb=T)
  writeOGR(obj=tract_sm, dsn="sm_shapes", layer=file_name, driver="ESRI Shapefile")
  }
  
  if(!file.exists(paste0("lg_shapes/", file_name, ".dbf"))) {
  tract_lg <- tracts(state=unique_counties$state[i], county=unique_counties$county[i])
  writeOGR(obj=tract_lg, dsn="lg_shapes", layer=file_name, driver="ESRI Shapefile")
  }
  print(unique_counties$NAME[i])
}

# Get list of FIPS codes by state
state_fips <- state.fips %>%
  select(fips, abb) %>%
  unique()

state_fips <- rbind(state_fips, data.frame(fips=2,abb="AK"))
state_fips$fips <- as.character(state_fips$fips)
state_fips$state <- ifelse(nchar(state_fips$fips)==1, paste0("0", state_fips$fips), state_fips$fips)

