# Packages for Data Cleaning/Processing
library(tidyverse)
library(data.table)

# Database Connection
library(censusapi)

Sys.setenv(CENSUS_KEY='<enter-api-key>')
setwd("C:/projects/equity-dashboard")
results <- NULL

# Inputs ------------------------------------------------------------------
yr <- list(2014,2019)

psrc.counties <- c("033","035","053","061")
psrc.msas <- c("14740","42660")

census.errors <- c(-222222222, -333333333, -666666666)

median.income.all.tables <- list("B19013" = list("Median Household Income in the Past 12 Months", "All households"))

median.income.detail.tables <- list("B19013B" = list("Median Household Income in the Past 12 Months (Black or African American Alone Householder)", "Black or African American"),
                                    "B19013C" = list("Median Household Income in the Past 12 Months (American Indian and Alaskan Native Alone Householder)", "American Indian/Alaska Native"),
                                    "B19013D" = list("Median Household Income in the Past 12 Months (Asian Alone Householder)", "Asian"),
                                    "B19013E" = list("Median Household Income in the Past 12 Months (Native Hawaiian and Other Pacific Islander Alone Householder)", "Native Hawaiian/Other Pacific Islander"),
                                    "B19013F" = list("Median Household Income in the Past 12 Months (Some Other Race Householder)", "Some other race"),
                                    "B19013G" = list("Median Household Income in the Past 12 Months (Two or More Races Householder)", "Two or more races"),
                                    "B19013A" = list("Median Household Income in the Past 12 Months (White Alone Householder)", "White"),
                                    "B19013I" = list("Median Household Income in the Past 12 Months (Hispanic or Latino Householder)", "Hispanic or Latino"),
                                    "B19013H" = list("Median Household Income in the Past 12 Months (White Alone, Not Hispanic or Latino Householder)", "White, not Hispanic or Latino"))

# Functions ---------------------------------------------------------------

get.census.data <- function(c.type, c.yr, c.table, c.geo="tract:*", c.state="state:53", c.list) {
  
  # Download Table from API
  tbl.values <- suppressWarnings(getCensus(name = c.type, vintage = c.yr, vars = c("NAME",paste0("group(",c.table,")")),region = c.geo, regionin = c.state)) %>%
    filter(county %in% psrc.counties) %>%
    select(county,NAME,GEO_ID,ends_with(c("E","M"))) %>%
    select(-state) %>%
    rename(Geography=NAME, ID=county) %>%
    setNames(c("ID","Geography","GEOID",paste0("Estimate-",c.list[[c.table]][[2]]),paste0("MoE-",c.list[[c.table]][[2]])))
  
  return(tbl.values)
  
}

get.census.data.msa <- function(c.type, c.yr, c.table, c.geo="metropolitan statistical area/micropolitan statistical area:*", c.list) {
  
  # Download Table from API
  tbl.values <- suppressWarnings(getCensus(name = c.type, vintage = c.yr, vars = c("NAME",paste0("group(",c.table,")")),region = c.geo)) %>%
    filter(`metropolitan_statistical_area_micropolitan_statistical_area` %in% psrc.msas) %>%
    select(`metropolitan_statistical_area_micropolitan_statistical_area`,NAME,GEO_ID,ends_with(c("E","M"))) %>%
    rename(Geography=NAME, ID=`metropolitan_statistical_area_micropolitan_statistical_area`) %>%
    setNames(c("ID","Geography","GEOID",paste0("Estimate-",c.list[[c.table]][[2]]),paste0("MoE-",c.list[[c.table]][[2]])))
  
  return(tbl.values)
  
}

i <- 0
# Load Census Tables for Median Household Income ------------------------------------------------------
for (analysis.yr in yr) {
  c.yr <- as.character(analysis.yr)
  
  ############################################################################################################################################################
  ############################################################################################################################################################
  ### Counties
  ############################################################################################################################################################
  ############################################################################################################################################################
  
  # First get values for all households
  total.income <- get.census.data(c.geo="county:033,035,053,061",c.type="acs/acs5", c.yr = analysis.yr, c.table = "B19013", c.list = median.income.all.tables)
  median.hh.income.county <- total.income
  
  # Loop over households by race and join results to the all households table
  for (c in names(median.income.detail.tables)) {
    
    temp <- get.census.data(c.geo="county:033,035,053,061",c.type="acs/acs5", c.yr = analysis.yr, c.table = c, c.list = median.income.detail.tables)
    
    temp <- inner_join(total.income, temp, by = c("ID", "Geography", "GEOID")) %>%
      mutate(`Estimate-Share` = .data[[paste0("Estimate-",median.income.detail.tables[[c]][[2]])]] / `Estimate-All households`) %>%
      mutate(`MoE-Share` = .data[[paste0("MoE-",median.income.detail.tables[[c]][[2]])]] / `Estimate-All households`) %>%
      select(-`Estimate-All households`, -`MoE-All households`) %>%
      setNames(c("ID","Geography","GEOID",paste0("Estimate-",median.income.detail.tables[[c]][[2]]),paste0("MoE-",median.income.detail.tables[[c]][[2]]),paste0("Share-Estimate-",median.income.detail.tables[[c]][[2]]),paste0("Share-MoE-",median.income.detail.tables[[c]][[2]])))
    
    median.hh.income.county <- inner_join(median.hh.income.county, temp, by = c("ID", "Geography", "GEOID")) %>%
      mutate(Type = "County")
    
  } # end of loop for census tables
  
  rm(temp, total.income) 
  
  ############################################################################################################################################################
  ############################################################################################################################################################
  ### MSA's
  ############################################################################################################################################################
  ############################################################################################################################################################
  
  # First get values for all households
  total.income <- get.census.data.msa(c.type="acs/acs5", c.yr = analysis.yr, c.table = "B19013", c.list = median.income.all.tables)
  median.hh.income.msa <- total.income
  
  # Loop over households by race and join results to the all households table
  for (c in names(median.income.detail.tables)) {
    
    temp <- get.census.data.msa(c.type="acs/acs5", c.yr = analysis.yr, c.table = c, c.list = median.income.detail.tables)
    
    temp <- inner_join(total.income, temp, by = c("ID", "Geography", "GEOID")) %>%
      mutate(`Estimate-Share` = .data[[paste0("Estimate-",median.income.detail.tables[[c]][[2]])]] / `Estimate-All households`) %>%
      mutate(`MoE-Share` = .data[[paste0("MoE-",median.income.detail.tables[[c]][[2]])]] / `Estimate-All households`) %>%
      select(-`Estimate-All households`, -`MoE-All households`) %>%
      setNames(c("ID","Geography","GEOID",paste0("Estimate-",median.income.detail.tables[[c]][[2]]),paste0("MoE-",median.income.detail.tables[[c]][[2]]),paste0("Share-Estimate-",median.income.detail.tables[[c]][[2]]),paste0("Share-MoE-",median.income.detail.tables[[c]][[2]])))
    
    median.hh.income.msa <- inner_join(median.hh.income.msa, temp, by = c("ID", "Geography", "GEOID")) %>%
      mutate(Type = "MSA")
    
  } # end of loop for census tables
  
  rm(temp, total.income)   
  
  ############################################################################################################################################################
  ############################################################################################################################################################
  ### Census Tracts
  ############################################################################################################################################################
  ############################################################################################################################################################
  
  # First get values for all households
  total.income <- get.census.data(c.geo="tract:*",c.type="acs/acs5", c.yr = analysis.yr, c.table = "B19013", c.list = median.income.all.tables)
  total.income[total.income == -222222222 | total.income == -333333333 | total.income == -666666666] <- 0
  median.hh.income.tracts <- total.income
  
  # Loop over households by race and join results to the all households table
  for (c in names(median.income.detail.tables)) {
    
    temp <- get.census.data(c.geo="tract:*",c.type="acs/acs5", c.yr = analysis.yr, c.table = c, c.list = median.income.detail.tables)
    temp[temp == -222222222 | temp == -333333333 | temp == -666666666] <- 0
    
    temp <- inner_join(total.income, temp, by = c("ID", "Geography", "GEOID")) %>%
      mutate(`Estimate-Share` = .data[[paste0("Estimate-",median.income.detail.tables[[c]][[2]])]] / `Estimate-All households`) %>%
      mutate(`MoE-Share` = .data[[paste0("MoE-",median.income.detail.tables[[c]][[2]])]] / `Estimate-All households`) %>%
      select(-`Estimate-All households`, -`MoE-All households`) %>%
      setNames(c("ID","Geography","GEOID",paste0("Estimate-",median.income.detail.tables[[c]][[2]]),paste0("MoE-",median.income.detail.tables[[c]][[2]]),paste0("Share-Estimate-",median.income.detail.tables[[c]][[2]]),paste0("Share-MoE-",median.income.detail.tables[[c]][[2]])))
    
    median.hh.income.tracts <- inner_join(median.hh.income.tracts, temp, by = c("ID", "Geography", "GEOID")) %>%
      mutate(Type = "Tract")
    
  } # end of loop for census tables
  
  rm(temp, total.income) 
  
  ############################################################################################################################################################
  ############################################################################################################################################################
  ### Combine County, MSA and Tract data
  ############################################################################################################################################################
  ############################################################################################################################################################  
  
  temp <- bind_rows(list(median.hh.income.county, median.hh.income.msa, median.hh.income.tracts))
  
  temp <- temp %>%
    pivot_longer(cols=contains("-"), names_to="Variable", values_to="Estimate") %>%
    mutate(Year=analysis.yr) %>%
    mutate(Estimate_Type = ifelse(grepl('Estimate', Variable, ignore.case=T), "Estimate","MoE")) %>%
    mutate(Share_Type = ifelse(grepl('Share', Variable, ignore.case=T), "Share","Total")) %>%
    mutate(Household_Race = gsub("^.*-", "", Variable)) %>%
    mutate(Geography = gsub(", Washington", "", Geography)) %>%
    mutate(Geography = gsub(", WA Metro Area", " MSA", Geography))
  
  if(i==0) {median.hh.income <- temp} else {median.hh.income <- bind_rows(median.hh.income, temp)}
  
  rm(median.hh.income.county, median.hh.income.msa, median.hh.income.tracts, temp)
  i <- i + 1
  
} # end of loop for analysis years

# Output data to csv
 
fwrite(median.hh.income, "shiny/data/median_hh_income.csv")
