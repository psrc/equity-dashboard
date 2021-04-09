# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # Home Ownership Charts and Tables
    output$chart_region_homeownership <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("Region"), e.type=input$OwnershipType, c.name="Owner-occupied housing units")})
    output$chart_county_homeownership <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("County"), e.type=input$OwnershipType, c.name="Owner-occupied housing units")})

    # Educational Attainment Charts and Tables
    output$chart_region_education_hs <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("Region"), e.type=input$EducationType, c.name="High school graduate or higher")})
    output$chart_region_education_bs <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("Region"), e.type=input$EducationType, c.name="Bachelor's degree or higher")})
    output$chart_county_education_hs <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("County"), e.type=input$EducationType, c.name="High school graduate or higher")})
    output$chart_county_education_bs <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("County"), e.type=input$EducationType, c.name="Bachelor's degree or higher")})

    # Median Income Charts and Tables
    output$chart_region_income <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("MSA"), e.type=input$IncomeType, c.name="Median income", c.facet=2, w.label=scales::dollar_format(),w.pre="$")})
    output$chart_county_income <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("County"), e.type=input$IncomeType, c.name="Median income",w.label=scales::dollar_format(),w.pre="$")})
    
    output$table_msa_income <- renderDataTable({create.clean.tbl(yr=latest.yr, g.type=c("MSA"), c.name="Median income", t.container=msa.income.container, t.cols=med.inc.msa.tot.cols, s.cols=med.inc.msa.shr.cols, e.type=input$IncomeType)})
    output$table_county_income <- renderDataTable({create.clean.tbl(yr=latest.yr, g.type=c("County"), c.name="Median income", t.container=county.income.container, t.cols=med.inc.county.tot.cols, s.cols=med.inc.county.shr.cols, e.type=input$IncomeType)})
})    

#data=census.data, yr, g.type, e.type, w.label, w.dec, w.pre="", c.name, c.facet=2