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
#setwd("/home/shiny/apps/equity-dashboard/shiny")

census.data <- as_tibble(fread("data/equity-dashboard-census-data.csv"))

data.years <- census.data %>% select(ACS_Year) %>% pull() %>% unique()
latest.yr <- max(data.years)

chart_value_types <- c("Total", "Share")

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

# Column definitions Geographies from ACS data
msa.tot.cols <- c("estimate_Seattle-Tacoma-Bellevue", "moe_Seattle-Tacoma-Bellevue", "estimate_Bremerton-Silverdale-Port Orchard", "moe_Bremerton-Silverdale-Port Orchard")
msa.shr.cols <- c("share_Seattle-Tacoma-Bellevue", "share_moe_Seattle-Tacoma-Bellevue", "share_Bremerton-Silverdale-Port Orchard", "share_moe_Bremerton-Silverdale-Port Orchard")

county.tot.cols <- c("estimate_King County", "moe_King County", "estimate_Kitsap County", "moe_Kitsap County", "estimate_Pierce County", "moe_Pierce County", "estimate_Snohomish County", "moe_Snohomish County")
county.shr.cols <- c("share_King County", "share_moe_King County", "share_Kitsap County", "share_moe_Kitsap County", "share_Pierce County", "share_moe_Pierce County", "share_Snohomish County", "share_moe_Snohomish County")

region.tot.cols <- c("estimate_Region", "moe_Region")
region.shr.cols <- c("share_Region", "share_moe_Region")


# Functions --------------------------------------------------------
create.custom.container <- function(g.type, c.name) {
  
  if (g.type=="MSA") {
    c = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(class = 'dt-center', rowspan = 3, 'Race'),
          th(class = 'dt-center', colspan = 4, c.name)
        ),
        tr(
          th(class = 'dt-center', colspan = 2, 'Seattle-Tacoma-Bellevue'),
          th(class = 'dt-center', colspan = 2, 'Bremerton-Silverdale-Port Orchard')
        ),
        tr(
          lapply(rep(c('Estimate', 'MoE'), 2), th, class = 'dt-center')
        )
      )
    ))
  } # end of msa container if

  if (g.type=="Region") {
    c = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(class = 'dt-center', rowspan = 3, 'Race'),
          th(class = 'dt-center', colspan = 2, c.name)
        ),
        tr(
          th(class = 'dt-center', colspan = 2, 'Region')
        ),
        tr(
          lapply(rep(c('Estimate', 'MoE'), 1), th, class = 'dt-center')
        )
      )
    ))
    
  } # end of region container if 
  
  if (g.type=="County") {
  
    c = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(class = 'dt-center', rowspan = 3, 'Race'),
          th(class = 'dt-center', colspan = 8, c.name)
        ),
        tr(
          th(class = 'dt-center', colspan = 2, 'King County'),
          th(class = 'dt-center', colspan = 2, 'Kitsap County'),
          th(class = 'dt-center', colspan = 2, 'Pierce County'),
          th(class = 'dt-center', colspan = 2, 'Snohomish County')
        ),
        tr(
          lapply(rep(c('Estimate', 'MoE'), 4), th, class = 'dt-center')
        )
      )
    ))
    
  } # end of county container if
  
  return(c)
  
}

create.bar.chart.facet <- function(data=census.data, yr, g.type, e.type, c.name, c.facet=2, w.label=scales::comma, w.pre="") {

  if (e.type == "Total") {
    
    est.value <- "estimate"
    w.suff <- ""
    w.fact <- 1.0
    w.dec <- -2

  } else {
    
    est.value <- "share"
    w.suff <- "%"
    w.fact <- 100
    w.label <- scales::percent
    w.dec <- 0
    w.pre <- ""
  }
  
  # Filter Data
  tbl <- data %>%
    filter(ACS_Geography %in% g.type & ACS_Year == yr & ACS_category == c.name) %>%
    select(NAME, .data[[est.value]], ACS_category, ACS_race) %>%
    filter(ACS_race!="All")

  tbl$ACS_race <- factor(tbl$ACS_race, levels=c("Black or African American", 
                                              "American Indian and Alaska Native", 
                                              "Asian", 
                                              "Native Hawaiian and Other Pacific Islander",
                                              "Some other race", "Two or more races",
                                              "Hispanic or Latino Origin", "White", "White, not Hispanic or Latino"))

  g <-  ggplotly(ggplot(data = tbl,
                      aes(x = ACS_race, 
                          y = get(eval(est.value)), 
                          fill = ACS_race,
                          text = paste0("<b>", ACS_race, ": ",w.pre,"</b>", prettyNum(round(get(eval(est.value))*w.fact, w.dec), big.mark = ","), w.suff,"<br>"))) +
                 geom_col(
                   color = "black",
                   alpha = 1.0,
                   position = "dodge") +
                 labs(x = NULL, y = NULL) +
                 scale_y_continuous(labels = w.label) +
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

create.clean.tbl <- function(data=census.data, g.type, e.type, yr=latest.yr, c.name, t.container, t.cols, s.cols, w.pre="") {

  # Do this if user selects Total for type of output
  if (e.type=="Total") {
    c.tbl <- data %>%
      filter(ACS_Geography %in% g.type & ACS_Year == yr & ACS_category == c.name) %>%
      filter(ACS_race!="All") %>%
      select(NAME, ACS_race, estimate, moe)
    
    c.tbl <- c.tbl %>%
      pivot_wider(id_cols=c(ACS_race), names_from=NAME, values_from = c(estimate,moe))
    f.cols <- c("ACS_race", t.cols)
    c.tbl <- c.tbl[,f.cols]
    num.cols <- length(t.cols)
    t <- datatable(c.tbl, container = t.container, rownames = FALSE, options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center', targets =1:num.cols))))
    for (working_column in t.cols) {
      t <- t %>% formatCurrency(working_column, w.pre, digits = 0) %>% formatStyle(working_column,`text-align` = 'center')
    } # end of table format loop for totals

  } else {
    c.tbl <- data %>%
      filter(ACS_Geography %in% g.type & ACS_Year == yr & ACS_category == c.name) %>%
      filter(ACS_race!="All") %>%
      mutate(share_moe = moe / total) %>%
      select(NAME, ACS_race, share, share_moe)
  
    c.tbl <- c.tbl %>%
      pivot_wider(id_cols=c(ACS_race), names_from=NAME, values_from = c(share, share_moe))
    f.cols <- c("ACS_race", s.cols)
    c.tbl <- c.tbl[,f.cols]
    num.cols <- length(s.cols)
    t <- datatable(c.tbl, container = t.container, rownames = FALSE, options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center', targets =1:num.cols))))
    for (working_column in s.cols) {
      t <- t %>% formatPercentage(working_column,0) %>% formatStyle(working_column,`text-align` = 'center')
    } # end of table format loop for shares
  
  } # end of else condition

  return(t)
}
