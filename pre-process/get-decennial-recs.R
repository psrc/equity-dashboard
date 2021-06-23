# The primary function is get_decennial_recs() to retrieve sf1 2010 data for
# single table or a vector of tables by a single county or a vector of counties 
# single table or a vector of tables by tracts in a county or a vector of counties
# single table or a vector of tables by msa/place or a vector of msa/place
# returns all tables/geographies as one data frame 
# geography arguments = 'tract', 'county', 'place', 'msa'
# See test examples below function

library(tidycensus)
library(tidyverse)

Sys.getenv("CENSUS_API_KEY")

get_decennial_tract_county <- function(geography, counties = c('King', 'Kitsap', 'Pierce', 'Snohomish'), 
                                       table_codes, year, state = 'WA') {
  get_decennial_geogs <- partial(get_decennial,
                                 geography = geography,
                                 state = state,
                                 table = table)
  dfs <- NULL
  for(table in table_codes) {
    
    all_geogs <- map(counties, ~get_decennial_geogs(county = .x))
    
    # append via recursion
    df <- reduce(all_geogs, bind_rows)
    ifelse(is.null(dfs), dfs <- df, dfs <- bind_rows(dfs, df))
  }
  return(dfs)
}

get_decennial_msa <- function(table_codes, year, fips = NULL) {
  msa_geog <- 'metropolitan statistical area/micropolitan statistical area'
  
  dfs <- NULL
  for(table_code in table_codes) {
    df <- get_decennial(geography = msa_geog,
                        state = NULL,
                        table = table_code)
    ifelse(is.null(dfs), dfs <- df, dfs <- bind_rows(dfs, df))
  }
  
  if(!is.null(fips)) dfs <- dfs %>% filter(GEOID %in% fips)
  
  return(dfs)
}

get_decennial_place <- function(table_codes, year, fips = NULL, state = 'WA') {
  dfs <- NULL
  for(table_code in table_codes) {
    df <- get_decennial(geography = 'place',
                        state = state,
                        table = table_code)
    ifelse(is.null(dfs), dfs <- df, dfs <- bind_rows(dfs, df))
  }
  
  if(!is.null(fips)) dfs <- dfs %>% filter(GEOID %in% fips)
  
  return(dfs)
}

get_decennial_recs <- function(geography, counties = c('King', 'Kitsap', 'Pierce', 'Snohomish'), table_codes, year, 
                               fips = NULL) {

  if(geography %in% c('tract', 'county')) {
    dfs <- get_decennial_tract_county(geography = geography, table_codes = table_codes, year = year)
  } else if (geography == 'msa'){ 
    dfs <- get_decennial_msa(table_codes, year, fips = fips)
  } else if(geography == 'place') {
    dfs <- get_decennial_place(table_codes, year, fips = fips)
  }
  # add labels
  vars <- load_variables(year, "sf1")
  df_join <- left_join(dfs, vars, by = c("variable" = "name"))
  return(df_join)
}



# Test --------------------------------------------------------------------


# geographies
# counties <- c('King', 'Kitsap', 'Pierce', 'Snohomish')

# tables (Group Quarters Population by Group Quarters Type, Races A-F)
# tbl_names <- paste0('PCT020', LETTERS[1:6])

# tt2 <- get_decennial_recs(geography = 'county', "King", "PCT021", 2010)
# tt5 <- get_decennial_recs(geography = 'tract', counties, tbl_names, 2010)
# tt6 <- get_decennial_recs(geography = 'county', counties, tbl_names, 2010)
# tt6a <- get_decennial_recs(geography = 'county', table_codes = tbl_names, year = 2010)

# tt7 <- get_decennial_recs(geography = 'place', table_codes = 'PCT013', year = 2010)
# tt7a <- get_decennial_recs(geography = 'place', table_codes = 'PCT013', year = 2010, fips = c("5363000", "5308850"))
# tt8 <- get_decennial_recs(geography = 'place', table_codes = c('PCT013', 'PCT022'), year = 2010)

# tt9 <-  get_decennial_recs(geography = 'msa', table_codes = "H001", year = 2010)
# tt9a <- get_decennial_recs(geography = 'msa', table_codes = c("H001", "P001"), year = 2010)
# tt9b <- get_decennial_recs(geography = 'msa', table_codes = c("H001", "P001"), year = 2010, fips = c('42660', "28420"))


