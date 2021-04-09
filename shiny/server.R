# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # Home Ownership Charts and Tables
    output$chart_region_homeownership <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("Region"), e.type="share", y.limit=1, w.label=scales::percent, w.dec=0, w.suff="%", w.fact=100, c.name="Owner-occupied housing units")})
    output$chart_county_homeownership <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("County"), e.type="share", y.limit=1, w.label=scales::percent, w.dec=0, w.suff="%", w.fact=100, c.name="Owner-occupied housing units")})

    # Educational Attainment Charts and Tables
    output$chart_region_education_hs <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("Region"), e.type="share", y.limit=1, w.label=scales::percent, w.dec=0, w.suff="%", w.fact=100, c.name="High school graduate or higher")})
    output$chart_region_education_bs <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("Region"), e.type="share", y.limit=1, w.label=scales::percent, w.dec=0, w.suff="%", w.fact=100, c.name="Bachelor's degree or higher")})
    output$chart_county_education_hs <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("County"), e.type="share", y.limit=1, w.label=scales::percent, w.dec=0, w.suff="%", w.fact=100, c.name="High school graduate or higher")})
    output$chart_county_education_bs <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("County"), e.type="share", y.limit=1, w.label=scales::percent, w.dec=0, w.suff="%", w.fact=100, c.name="Bachelor's degree or higher")})

    # Median Income Charts and Tables
    output$chart_region_income <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("MSA"), e.type="estimate", y.limit=120000, w.label=scales::dollar_format(), w.dec=-2, w.pre="$", c.name="Median income", c.facet=2)})
    output$chart_county_income <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("County"), e.type="estimate", y.limit=120000, w.label=scales::dollar_format(), w.dec=-2, w.pre="$", c.name="Median income")})
    
    output$table_msa_income <- renderDataTable({create.clean.tbl(yr=latest.yr, g.type=c("MSA"), c.name="Median income", t.container=msa.income.container, t.cols=med.inc.msa.tot.cols, s.cols=med.inc.msa.shr.cols)})
    output$table_county_income <- renderDataTable({create.clean.tbl(yr=latest.yr, g.type=c("County"), c.name="Median income", t.container=county.income.container, t.cols=med.inc.county.tot.cols, s.cols=med.inc.county.shr.cols)})
})    

