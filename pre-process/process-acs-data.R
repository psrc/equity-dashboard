# Packages for Data Cleaning/Processing
library(tidyverse)
library(data.table)

# Packages for Census Data Downloads
library(tidycensus)

setwd("C:/projects/equity-dashboard")
Sys.getenv("CENSUS_API_KEY")

acs <- "acs5"
yr <- 2019
psrc.county <- c("King County","Kitsap County","Pierce County","Snohomish County")
psrc.msa <- c("14740","42660")

ownership <- "S2502"
education <- "S1501"
income <- "S1903"

# Census Data for Home Ownership (Table S2502) ----------------------------
home.ownership <- NULL

# Load labels for all variables in the dataset
variable.labels <- load_variables(yr, paste0(acs,"/subject"), cache = TRUE) %>% rename(variable = name)

# Download the data for all counties
county.tbl <- get_acs(geography = "county", state="53", year=yr, survey = acs, table = ownership) %>%
  mutate(NAME = gsub(", Washington", "", NAME)) %>%
  filter(NAME %in% psrc.county) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Geography="County")
county.tbl <- inner_join(county.tbl, variable.labels,by=c("variable"))

# Download the data for all msa's
msa.tbl <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", year=yr, survey = acs, table = ownership) %>%
  filter(GEOID %in% psrc.msa) %>%
  mutate(NAME = gsub(", WA Metro Area", "", NAME)) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Geography="MSA")  
msa.tbl <- inner_join(msa.tbl, variable.labels,by=c("variable"))
  
# Download Tract data for ACS 5 yr
tract.tbl <- get_acs(geography = "tract", state="53", year=yr, survey = "acs5", table = ownership) %>%
  filter(str_detect(NAME, 'King County|Kitsap County|Pierce County|Snohomish County')) %>%
  mutate(NAME = gsub(", Washington", "", NAME)) %>%
  mutate(ACS_Year=yr, ACS_Type="acs5", ACS_Geography="Tract")
tract.tbl <- inner_join(tract.tbl, variable.labels,by=c("variable"))

# Join Tables
home.ownership <- bind_rows(list(county.tbl, msa.tbl, tract.tbl))  

# Trim down to only include racial categories  
home.ownership <- home.ownership %>%
  filter(str_detect(label, 'RACE AND HISPANIC OR LATINO ORIGIN')) %>%
  filter(!str_detect(label, 'Percent')) %>%
  separate(variable, c("ACS_Table", "ACS_Code", "ACS_Variable"), "_") %>%
  mutate(label = gsub("Estimate!!","",label)) %>%
  mutate(label = gsub("Occupied housing units!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!","",label)) %>%
  mutate(label = gsub("One race --!!","",label)) %>%
  separate(label, c("ACS_category","ACS_race"), "!!")

# Get totals by racial category
totals <- home.ownership %>%
  filter(ACS_Code=="C01") %>%
  select(NAME,ACS_Variable,estimate) %>%
  rename(total=estimate)

# Join total by racial category and calculate the share
home.ownership <- inner_join(home.ownership, totals, by=c("NAME","ACS_Variable")) %>%
  mutate(share=estimate/total)

# Remove extra tables from memory
rm(county.tbl, msa.tbl, tract.tbl, totals, variable.labels)

# Census Data for Educational Attainment (Table S1501) ----------------------------
educational.attainment <- NULL

# Load labels for all variables in the dataset
variable.labels <- load_variables(yr, paste0(acs,"/subject"), cache = TRUE) %>% rename(variable = name)

# Download the data for all counties
county.tbl <- get_acs(geography = "county", state="53", year=yr, survey = acs, table = education) %>%
  mutate(NAME = gsub(", Washington", "", NAME)) %>%
  filter(NAME %in% psrc.county) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Geography="County")
county.tbl <- inner_join(county.tbl, variable.labels,by=c("variable"))

# Download the data for all msa's
msa.tbl <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", year=yr, survey = acs, table = education) %>%
  filter(GEOID %in% psrc.msa) %>%
  mutate(NAME = gsub(", WA Metro Area", "", NAME)) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Geography="MSA")  
msa.tbl <- inner_join(msa.tbl, variable.labels,by=c("variable"))

# Download Tract data for ACS 5 yr
tract.tbl <- get_acs(geography = "tract", state="53", year=yr, survey = "acs5", table = education) %>%
  filter(str_detect(NAME, 'King County|Kitsap County|Pierce County|Snohomish County')) %>%
  mutate(NAME = gsub(", Washington", "", NAME)) %>%
  mutate(ACS_Year=yr, ACS_Type="acs5", ACS_Geography="Tract")
tract.tbl <- inner_join(tract.tbl, variable.labels,by=c("variable"))

# Join Tables
educational.attainment <- bind_rows(list(county.tbl, msa.tbl, tract.tbl))  

# Trim down to only include racial categories  
educational.attainment <- educational.attainment %>%
  filter(str_detect(label, 'RACE AND HISPANIC OR LATINO ORIGIN')) %>%
  filter(!str_detect(label, 'Male'),!str_detect(label, 'Female'),!str_detect(label, 'Percent')) %>%
  separate(variable, c("ACS_Table", "ACS_Code", "ACS_Variable"), "_") %>%
  mutate(label = gsub("Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!","",label)) %>%
  separate(label, c("ACS_race", "ACS_category"), "!!") %>%
  mutate(ACS_category = replace(ACS_category,is.na(ACS_category),"Total"))

# Get totals by racial category
totals <- educational.attainment %>%
  filter(ACS_category=="Total") %>%
  select(NAME,ACS_race,estimate) %>%
  rename(total=estimate)

# Join total by racial category and calculate the share
educational.attainment <- inner_join(educational.attainment, totals, by=c("NAME","ACS_race")) %>%
  mutate(share=estimate/total)

# Remove extra tables from memory
rm(county.tbl, msa.tbl, tract.tbl, totals, variable.labels)

# Census Data for Median Income (Table S1903) ----------------------------
median.income <- NULL

# Load labels for all variables in the dataset
variable.labels <- load_variables(yr, paste0(acs,"/subject"), cache = TRUE) %>% rename(variable = name)

# Download the data for all counties
county.tbl <- get_acs(geography = "county", state="53", year=yr, survey = acs, table = income) %>%
  mutate(NAME = gsub(", Washington", "", NAME)) %>%
  filter(NAME %in% psrc.county) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Geography="County")
county.tbl <- inner_join(county.tbl, variable.labels,by=c("variable"))

# Download the data for all msa's
msa.tbl <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", year=yr, survey = acs, table = income) %>%
  filter(GEOID %in% psrc.msa) %>%
  mutate(NAME = gsub(", WA Metro Area", "", NAME)) %>%
  mutate(ACS_Year=yr, ACS_Type=acs, ACS_Geography="MSA")  
msa.tbl <- inner_join(msa.tbl, variable.labels,by=c("variable"))

# Download Tract data for ACS 5 yr
tract.tbl <- get_acs(geography = "tract", state="53", year=yr, survey = "acs5", table = income) %>%
  filter(str_detect(NAME, 'King County|Kitsap County|Pierce County|Snohomish County')) %>%
  mutate(NAME = gsub(", Washington", "", NAME)) %>%
  mutate(ACS_Year=yr, ACS_Type="acs5", ACS_Geography="Tract")
tract.tbl <- inner_join(tract.tbl, variable.labels,by=c("variable"))

# Join Tables
median.income <- bind_rows(list(county.tbl, msa.tbl, tract.tbl))  

# Trim down to only include racial categories  
median.income <- median.income %>%
  filter(str_detect(label, 'RACE AND HISPANIC OR LATINO ORIGIN'), !str_detect(label, 'Percent'), str_detect(label,"Median income")) %>%
  separate(variable, c("ACS_Table", "ACS_Code", "ACS_Variable"), "_") %>%
  mutate(label = gsub("Estimate!!","",label)) %>%
  mutate(label = gsub("!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households", "", label)) %>%
  mutate(label = gsub("One race--!!", "", label)) %>%
  mutate(label = gsub(" \\(dollars\\)", "", label)) %>%
  separate(label, c("ACS_category", "ACS_race"), "!!") %>%
  mutate(ACS_race = replace(ACS_race,is.na(ACS_race),"All"))

# Get totals by racial category
totals <- median.income %>%
  filter(ACS_race=="All") %>%
  select(NAME,estimate) %>%
  rename(total=estimate)

# Join total by racial category and calculate the share
median.income <- inner_join(median.income, totals, by=c("NAME")) %>%
  mutate(share=estimate/total)

# Remove extra tables from memory
rm(county.tbl, msa.tbl, tract.tbl, totals, variable.labels)

# Output Data -------------------------------------------------------------
census.tbls <- bind_rows(list(home.ownership,educational.attainment,median.income))
fwrite(census.tbls, "shiny/data/equity-dashboard-census-data.csv")
