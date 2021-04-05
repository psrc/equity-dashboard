# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # MSA Data
    output$chart_msa_median_income_total <- renderPlotly({create.bar.chart.facet(yr=input$InputYears,g.type="MSA",s.type="Total", y.limit=150000)})
    output$chart_msa_median_income_share <- renderPlotly({create.bar.chart.facet(yr=input$InputYears,g.type="MSA",s.type="Share", y.limit=1.5)})
    output$table_msa_median_income <- renderDataTable({create.msa.income.tbl(yr=input$InputYears)})

})    
