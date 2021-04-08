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
    output$chart_region_income <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("MSA"), e.type="estimate", y.limit=120000, w.label=scales::dollar_format(), w.dec=-2, w.pre="$", c.name="Median income")})
    output$chart_county_income <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("County"), e.type="estimate", y.limit=120000, w.label=scales::dollar_format(), w.dec=-2, w.pre="$", c.name="Median income")})
    
    
})    
