# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # Home Ownership Charts and Tables
    output$chart_region_homeownership <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("Region"), e.type=input$OwnershipType, c.name="Owner-occupied housing units")})
    output$table_region_homeownership <- renderDataTable({create.clean.tbl(yr=latest.yr, g.type=c("Region"), c.name="Owner-occupied housing units", t.container=create.custom.container("Region","Home Ownership"), t.cols=region.tot.cols, s.cols=region.shr.cols, e.type=input$OwnershipType)})
    
    output$chart_county_homeownership <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("County"), e.type=input$OwnershipType, c.name="Owner-occupied housing units")})
    output$table_county_homeownership <- renderDataTable({create.clean.tbl(yr=latest.yr, g.type=c("County"), c.name="Owner-occupied housing units", t.container=create.custom.container("County","Home Ownership"), t.cols=county.tot.cols, s.cols=county.shr.cols, e.type=input$OwnershipType)})
    
    # Educational Attainment Charts and Tables
    output$chart_region_education_hs <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("Region"), e.type=input$EducationType, c.name="High school graduate or higher")})
    output$chart_region_education_bs <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("Region"), e.type=input$EducationType, c.name="Bachelor's degree or higher")})
    output$table_region_education_hs <- renderDataTable({create.clean.tbl(yr=latest.yr, g.type=c("Region"), c.name="High school graduate or higher", t.container=create.custom.container("Region","High school graduate or higher"), t.cols=region.tot.cols, s.cols=region.shr.cols, e.type=input$EducationType)})
    output$table_region_education_bs <- renderDataTable({create.clean.tbl(yr=latest.yr, g.type=c("Region"), c.name="Bachelor's degree or higher", t.container=create.custom.container("Region","Bachelor's degree or higher"), t.cols=region.tot.cols, s.cols=region.shr.cols, e.type=input$EducationType)})
    
    output$chart_county_education_hs <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("County"), e.type=input$EducationType, c.name="High school graduate or higher")})
    output$chart_county_education_bs <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("County"), e.type=input$EducationType, c.name="Bachelor's degree or higher")})
    output$table_county_education_hs <- renderDataTable({create.clean.tbl(yr=latest.yr, g.type=c("County"), c.name="High school graduate or higher", t.container=create.custom.container("County","High school graduate or higher"), t.cols=county.tot.cols, s.cols=county.shr.cols, e.type=input$EducationType)})
    output$table_county_education_bs <- renderDataTable({create.clean.tbl(yr=latest.yr, g.type=c("County"), c.name="Bachelor's degree or higher", t.container=create.custom.container("County","Bachelor's degree or higher"), t.cols=county.tot.cols, s.cols=county.shr.cols, e.type=input$EducationType)})
    
    # Median Income Charts and Tables
    output$chart_region_income <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("MSA"), e.type=input$IncomeType, c.name="Median income", c.facet=1, w.label=scales::dollar_format(),w.pre="$")})
    output$table_msa_income <- renderDataTable({create.clean.tbl(yr=latest.yr, g.type=c("MSA"), c.name="Median income", t.container=create.custom.container("MSA","Median Income"), t.cols=msa.tot.cols, s.cols=msa.shr.cols, e.type=input$IncomeType, w.pre="$")})
    
    output$chart_county_income <- renderPlotly({create.bar.chart.facet(yr=latest.yr, g.type=c("County"), e.type=input$IncomeType, c.name="Median income",w.label=scales::dollar_format(),w.pre="$")})
    output$table_county_income <- renderDataTable({create.clean.tbl(yr=latest.yr, g.type=c("County"), c.name="Median income", t.container=create.custom.container("County","Median Income"), t.cols=county.tot.cols, s.cols=county.shr.cols, e.type=input$IncomeType, w.pre="$")})

})    

