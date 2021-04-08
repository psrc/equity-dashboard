# Packages ----------------------------------------------------------------

# Packages for Interactive Web application
library(shiny)

# Packages for Data Cleaning/Processing
library(tidyverse)
library(data.table)

# Packages for Chart Creation
library(ggplot2)
library(scales)
library(plotly)

# Packages for Table Creation
library(DT)

# Inputs ---------------------------------------------------------------

setwd("C:/projects/equity-dashboard/shiny")

census.data <- as_tibble(fread("data/equity-dashboard-census-data.csv"))

data.years <- census.data %>% select(ACS_Year) %>% pull() %>% unique()
latest.yr <- max(data.years)

psrc.colors <- list("Black or African American" = "#AD5CAB",
                    "American Indian and Alaska Native" = "#C388C2",
                    "Asian" = "#E3C9E3",
                    "Native Hawaiian and Other Pacific Islander" = "#F4835E",
                    "Some other race" = "#F7A489",
                    "Two or more races" = "#FBD6C9",
                    "White" = "#A9D46E",
                    "Hispanic or Latino Origin" = "#C0E095",
                    "White, not Hispanic or Latino" = "#E2F1CF",
                    "All households" = "#BCBEC0")

# Functions --------------------------------------------------------

create.bar.chart.facet <- function(data=census.data, yr, g.type, e.type="estimate", y.limit, w.label, w.dec, w.pre="", w.suff="", w.fact=1.0, c.name, c.facet=2) {

  # Filter Data
  tbl <- data %>%
    filter(ACS_Geography %in% g.type & ACS_Year == yr & ACS_category == c.name) %>%
    select(NAME, .data[[e.type]], ACS_category, ACS_race) %>%
    filter(ACS_race!="All")

  tbl$ACS_race <- factor(tbl$ACS_race, levels=c("Black or African American", 
                                              "American Indian and Alaska Native", 
                                              "Asian", 
                                              "Native Hawaiian and Other Pacific Islander",
                                              "Some other race", "Two or more races",
                                              "Hispanic or Latino Origin", "White", "White, not Hispanic or Latino"))

  g <-  ggplotly(ggplot(data = tbl,
                      aes(x = ACS_race, 
                          y = get(eval(e.type)), 
                          fill = ACS_race,
                          text = paste0("<b>", ACS_race, ": ",w.pre,"</b>", prettyNum(round(get(eval(e.type))*w.fact, w.dec), big.mark = ","), w.suff,"<br>"))) +
                 geom_col(
                   color = "black",
                   alpha = 1.0,
                   position = "dodge") +
                 labs(x = NULL, y = NULL) +
                 scale_y_continuous(labels = w.label, limits=c(0,y.limit)) +
                 scale_fill_manual(values= psrc.colors) +
                 theme(plot.title = element_text(size = 10, face = 'bold'),
                       axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.line = element_blank(),
                       panel.background = element_blank(),
                       panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                       panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                       panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank(),
                       text = element_text(family = "Segoe UI"),
                       legend.position = "bottom",
                       legend.title = element_blank())+
                 facet_wrap(vars(NAME), scales = "free", ncol=c.facet) +
                 theme(panel.spacing.y = unit(4, "lines")),
               tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25))
  
  return(g)
  
}

create.msa.income.tbl <- function(data=median.income, g.type="MSA", yr) {

  ######################################################################################################
  ######################################################################################################
  ### Totals
  ######################################################################################################
  ######################################################################################################
  est.tbl <- data %>%
    filter(Type == g.type & Year == yr & Share_Type == "Total" & Estimate_Type == "Estimate") %>%
    select(Geography, Household_Race, Estimate)

  moe.tbl <- data %>%
    filter(Type == g.type & Year == yr & Share_Type == "Total" & Estimate_Type == "MoE") %>%
    select(Geography, Household_Race, Estimate) %>%
    rename(MoE=Estimate)

  tot.tbl <- inner_join(est.tbl,moe.tbl, by = c("Geography", "Household_Race"))

  # Transform to Wide Format for table creation
  tot.tbl <- tot.tbl %>%
    pivot_wider(id_cols=c(Household_Race), names_from=Geography, values_from = c(Estimate,MoE))

  f.cols <- c("Household_Race", "Estimate_Seattle-Tacoma-Bellevue MSA", "MoE_Seattle-Tacoma-Bellevue MSA", "Estimate_Bremerton-Silverdale-Port Orchard MSA", "MoE_Bremerton-Silverdale-Port Orchard MSA")
  tot.tbl <- tot.tbl[,f.cols]

  ######################################################################################################
  ######################################################################################################
  ### Shares
  ######################################################################################################
  ######################################################################################################
  est.tbl <- data %>%
    filter(Type == g.type & Year == yr & Share_Type == "Share" & Estimate_Type == "Estimate") %>%
    select(Geography, Household_Race, Estimate)

  moe.tbl <- data %>%
    filter(Type == g.type & Year == yr & Share_Type == "Share" & Estimate_Type == "MoE") %>%
    select(Geography, Household_Race, Estimate) %>%
    rename(MoE=Estimate)

  shr.tbl <- inner_join(est.tbl,moe.tbl, by = c("Geography", "Household_Race"))

  # Transform to Wide Format for table creation
  shr.tbl <- shr.tbl %>%
    pivot_wider(id_cols=c(Household_Race), names_from=Geography, values_from = c(Estimate,MoE))

  f.cols <- c("Household_Race", "Estimate_Seattle-Tacoma-Bellevue MSA", "MoE_Seattle-Tacoma-Bellevue MSA", "Estimate_Bremerton-Silverdale-Port Orchard MSA", "MoE_Bremerton-Silverdale-Port Orchard MSA")
  shr.tbl <- shr.tbl[,f.cols]

  c.tbl <- inner_join(tot.tbl, shr.tbl, by=c("Household_Race"))

  ######################################################################################################
  ######################################################################################################
  ### Custom Container
  ######################################################################################################
  ######################################################################################################
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(class = 'dt-center', rowspan = 3, 'Race'),
        th(class = 'dt-center', colspan = 4, 'Median Household Income'),
        th(class = 'dt-center', colspan = 4, 'Percentage of Regional Median Household Income')
      ),
      tr(
        th(class = 'dt-center', colspan = 2, 'Seattle-Tacoma-Bellevue'),
        th(class = 'dt-center', colspan = 2, 'Bremerton-Silverdale-Port Orchard'),
        th(class = 'dt-center', colspan = 2, 'Seattle-Tacoma-Bellevue'),
        th(class = 'dt-center', colspan = 2, 'Bremerton-Silverdale-Port Orchard')
      ),
      tr(
        lapply(rep(c('Estimate', 'MoE'), 4), th, class = 'dt-center')
      )
    )
  ))

  # Create Table
  t <- datatable(c.tbl, container = sketch, rownames = FALSE, options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center', targets =1:8))))

  currency_columns <- c("Estimate_Seattle-Tacoma-Bellevue MSA.x", "MoE_Seattle-Tacoma-Bellevue MSA.x", "Estimate_Bremerton-Silverdale-Port Orchard MSA.x", "MoE_Bremerton-Silverdale-Port Orchard MSA.x")
  percentage_columns <- c("Estimate_Seattle-Tacoma-Bellevue MSA.y", "MoE_Seattle-Tacoma-Bellevue MSA.y", "Estimate_Bremerton-Silverdale-Port Orchard MSA.y", "MoE_Bremerton-Silverdale-Port Orchard MSA.y")

  for (working_column in currency_columns) {
    t <- t %>% formatCurrency(working_column, "$", digits = 0) %>% formatStyle(working_column,`text-align` = 'center')
  }

  for (working_column in percentage_columns) {
    t <- t %>% formatPercentage(working_column,0) %>% formatStyle(working_column,`text-align` = 'center')
  }
  
  return(t)
}


