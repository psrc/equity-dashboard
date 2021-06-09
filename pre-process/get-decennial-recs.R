library(tidycensus)
library(tidyverse)

Sys.getenv("CENSUS_API_KEY")


get_decennial_recs <- function(geography, counties = c('King', 'Kitsap', 'Pierce', 'Snohomish'), table_code, year, 
                               place_fips = NULL, msa_fips = NULL) {
  ## get_decennial_recs ----
  
  # retrieve sf1 20XX data for
  # single table or a vector of tables by a single county or a vector of counties 
  # single table or a vector of tables by tracts in a county or a vector of counties
  # single msa or place table, multiple msa/place table, or multiple msa/place by multiple tables.
  # geography arguments = 'tract', 'county', 'place', 'msa'
  
  dfs <- NULL
  
  if(geography %in% c('tract', 'county')) {
    
    get_decennial_geogs <- partial(get_decennial,
                                   geography = geography,
                                   state = 'WA',
                                   table = table)
    
    if(length(table_code) == 1) {
      
      table <- table_code
      all_geogs <- map(counties, ~get_decennial_geogs(county = .x))
      
      # append via recursion, add labels
      dfs <- reduce(all_geogs, bind_rows)
      
    } else if(length(table_code) > 1) {
      
      # loop through tables, map county/counties for every table
      for(table in table_code) {
        
        all_geogs <- map(counties, ~get_decennial_geogs(county = .x))
        
        # append via recursion
        df <- reduce(all_geogs, bind_rows)
        ifelse(is.null(dfs), dfs <- df, dfs <- bind_rows(dfs, df))
      }
      
    }
    
  } else { # e.g. place, msa
    
    msa_geog <- 'metropolitan statistical area/micropolitan statistical area'
    
    if (!is.null(msa_fips) & is.null(place_fips)) {
      fips <- msa_fips
    } else if (!is.null(place_fips) & is.null(msa_fips)) {
      fips <- place_fips
    } else if (is.null(msa_fips) & is.null(place_fips)) {
      fips <- NULL
    }
     
    if(length(table_code) == 1) {
      
      if(geography == 'place') {
        dfs <- get_decennial(geography = geography,
                             state = 'WA',
                             table = table_code)
      } else if (geography == 'msa') {
        dfs <- get_decennial(geography = msa_geog,
                             table = table_code)
      }
      
      if(!is.null(fips)) {
        dfs <- dfs %>% filter(GEOID %in% fips)
      }
      
    } else if (length(table_code) > 1) {
      
      for(table in table_code) {
        
        if(geography == 'place') {
          df <- get_decennial(geography = geography,
                              state = 'WA',
                              table = table)
        } else if (geography == 'msa') {
          df <- get_decennial(geography = msa_geog,
                              table = table)
          
        }
        
        ifelse(is.null(dfs), dfs <- df, dfs <- bind_rows(dfs, df))
        
      }
      
      if(!is.null(fips)) {
        dfs <- dfs %>% filter(GEOID %in% fips)
      }
      
    }
    
  }
  
  # add labels
  vars <- load_variables(year, "sf1")
  df_join <- left_join(dfs, vars, by = c("variable" = "name"))
  
  return(df_join)
  
}



# Test --------------------------------------------------------------------


# geographies
counties <- c('King', 'Kitsap', 'Pierce', 'Snohomish')

# tables (Group Quarters Population by Group Quarters Type, Races A-F)
tbl_names <- paste0('PCT020', LETTERS[1:6])

# tt2 <- get_decennial_recs(geography = 'county', "King", "PCT021", 2010)
# tt5 <- get_decennial_recs(geography = 'tract', counties, tbl_names, 2010)
# tt6 <- get_decennial_recs(geography = 'county', counties, tbl_names, 2010)
# tt6a <- get_decennial_recs(geography = 'county', table_code = tbl_names, year = 2010)
# tt7 <- get_decennial_recs(geography = 'place', table_code = 'PCT013', year = 2010)
# tt7a <- get_decennial_recs(geography = 'place', table_code = 'PCT013', year = 2010, place_fips = c("5363000", "5308850"))
# tt8 <- get_decennial_recs(geography = 'place', table_code = c('PCT013', 'PCT022'), year = 2010)

# tt9 <-  get_decennial_recs(geography = 'msa', table_code = "H001", year = 2010)
# tt9a <- get_decennial_recs(geography = 'msa', table_code = c("H001", "P001"), year = 2010)
# tt9b <- get_decennial_recs(geography = 'msa', table_code = c("H001", "P001"), year = 2010, msa_fips = c('42660', "28420"))


