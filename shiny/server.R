# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # MSA Data
    output$chart_msa_median_income_total <- renderPlotly({create.bar.chart.facet(yr=input$InputYears,g.type="MSA",s.type="Total", y.limit=150000, w.label=scales::dollar_format(), w.dec=-2, w.pre="$")})
    output$chart_msa_median_income_share <- renderPlotly({create.bar.chart.facet(yr=input$InputYears,g.type="MSA",s.type="Share", y.limit=1.5, w.label=scales::percent, w.dec=0, w.suff="%", w.fact=100)})
    output$table_msa_median_income <- renderDataTable({create.msa.income.tbl(yr=input$InputYears)})

    # County Data
    output$chart_county_median_income_total <- renderPlotly({create.bar.chart.facet(yr=input$InputYears,g.type="County",s.type="Total", y.limit=150000, w.label=scales::dollar_format(), w.dec=-2, w.pre="$")})
    output$chart_county_median_income_share <- renderPlotly({create.bar.chart.facet(yr=input$InputYears,g.type="County",s.type="Share", y.limit=1.5, w.label=scales::percent, w.dec=0, w.suff="%", w.fact=100)})
    

})    
