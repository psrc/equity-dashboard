library(tidycensus)
library(tidyverse)

Sys.getenv("CENSUS_API_KEY")


get_decennial_recs <- function(geography = c('tract', 'county'), counties, table_code, year) {
  ## get_decennial_recs ----
  
  # retrieve sf1 20XX data for
  # single table or a vector of tables by a single county or a vector of counties 
  # single table or a vector of tables by tracts in a county or a vector of counties
  
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
    dfs <- NULL
    
    for(table in table_code) {
      
      all_geogs <- map(counties, ~get_decennial_geogs(county = .x))
      
      # append via recursion
      df <- reduce(all_geogs, bind_rows)
      ifelse(is.null(dfs), dfs <- df, dfs <- bind_rows(dfs, df))
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
# tt4 <- get_decennial_tracts(counties, tbl_names, 2010)
# tt5 <- get_decennial_recs(geography = 'tract', counties, tbl_names, 2010)
# tt6 <- get_decennial_recs(geography = 'county', counties, tbl_names, 2010)

# get_decennial( geography = 'county',
#                state = 'WA',
#                # table = "PCT021",
#                variables = "PCT021001",
#                county = 'King')
