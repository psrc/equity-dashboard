library(tidycensus)
library(tidyverse)

Sys.getenv("CENSUS_API_KEY")


get_decennial_county <- function(counties, table_code, year) {
  # retrieve sf1 20XX data for
  # single table or a vector of tables by a single county or a vector of counties 
  
  get_decennial_counties <- partial(get_decennial,
                                    geography = 'county',
                                    state = 'WA',
                                    table = table)
  
  if(length(table_code) == 1) {
    
    table <- table_code
    
    all_counties <- map(counties, ~get_decennial_counties(county = .x))
    
    # append via recursion, add labels
    dfs <- reduce(all_counties, bind_rows)
  
  } else if(length(table_code) > 1) {
    
    # loop through tables, map county/counties for every table
    dfs <- NULL
    
    for(table in table_code) {

      all_counties <- map(counties, ~get_decennial_counties(county = .x))
      
      # append via recursion
      df <- reduce(all_counties, bind_rows)
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

# tables
tbl_names <- paste0('PCT020', LETTERS[1:6])

# tt2 <- get_decennial_county("King", "PCT021", 2010)
# tt3 <- get_decennial_county(counties, "PCT020A", 2010)
# tt <- get_decennial_county(counties, tbl_names, 2010)