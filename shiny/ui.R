shinyUI(
navbarPage(title=div(img(src="psrc-logo.png", width = "20%", height = "20%", style = "float: right;")),
          windowTitle = "PSRC Equity Dashboard", 
          theme = "styles.css",

           tabPanel("Overview",
                    sidebarLayout(
                      sidebarPanel(id="sidebar",
                        h2("Equity Resources:"),
                        hr(),
                        tags$a(class = "source_url", href="https://www.psrc.org/sites/default/files/demographicprofile.pdf", "Central Puget Sound Demographic Profile", target="_blank"),
                        br(),
                        tags$a(class = "source_url", href="https://www.psrc.org/displacement-risk-mapping", "Displacement Risk Mapping", target="_blank"),
                        br(),
                        tags$a(class = "source_url", href="https://www.psrc.org/opportunity-mapping", "Opportunity Mapping", target="_blank"),
                        br(),
                        tags$a(class = "source_url", href="https://www.psrc.org/sites/default/files/rtp-appendixb-equityanalysis.pdf", "Regional Transportation Plan Equity Analysis", target="_blank"),
                        br(),
                        tags$a(class = "source_url", href="https://www.psrc.org/sites/default/files/vision2050equitypaper.pdf", "VISION 2050: Equity Briefing Paper", target="_blank"),
                        br(),
                        tags$a(class = "source_url", href="https://www.psrc.org/sites/default/files/v2050finalseis-appendixh-equity-march2020.pdf", "VISION 2050 Final SEIS Appendix H: Equity Analysis", target="_blank"),
                        hr(),
                        h3("Would you like more information?"),
                        strong(tags$div(class="sidebar_notes","Charles Patton, Ph.D.")),
                        tags$a(class = "source_url", href="https://www.psrc.org/contact-center/have-question?contact=9626&destination=node/9362&width=75%25&height=75%25&subject=Planning%20for%20Equity", "Email", target="_blank"),
                        br(),
                        tags$div(class="sidebar_notes","206-971-3285"),
                        hr(),
                        width=2),
                      mainPanel(id="mainpanel",
                        h1("Planning for Equity"),
                        fluidRow(column(12,div(img(src="covid-mural.jpg", width = "50%", height = "50%", style = "padding-top: 25px")))
                        ),
                        br(),
                        h2("PSRC incorporates equity and social justice in its regional growth, transportation, and economic development planning."),
                        "The central Puget Sound region has a long history of racism that continues to cause enormous harm. Generations of discrimination, disinvestment, and inequitable opportunities have helped lay the groundwork for a region where people of color and white residents too often have completely different experiences.",
                        br(),br(),
                        "As a planning agency, PSRC makes decisions that shape transportation, land use, and the built environment. The agency has a responsibility to dismantle systems of inequity and reimagine a region where race can no longer predict life outcomes.",
                        br(),
                        h2("Regional Equity Strategy"),
                        "PSRC will develop a Regional Equity Strategy to improve outcomes for marginalized communities and how the agency operates internally. The agency will lead with race, which has proven to be an effective method for not only increasing equitable outcomes for people of color but developing a framework, tools, and resources that can remove barriers for other marginalized groups.",
                        br(),br(),
                        "The Regional Equity Strategy will provide guidance to help members work in a coordinated manner towards the region's goal of providing an exceptional quality of life and opportunity for all. It will also provide guidance and training for staff to ensure the agency effectively uses its roles to advance racial equity.",
                        br(),br(),
                        "The work plan is anticipated to be finalized in spring 2021, for the next budget and work program. Initial phases of the strategy will be finalized in 2022. This work will be ongoing.",
                        hr(),
                        div(img(src="equity-strategy-timeline.jpg", width = "50%", height = "50%", style = "padding-top: 5px")),
                      ) # End of Overview Panel
                    ) # End of Sidebar of Unemployment
           ),# End of Tab Panel of Overview Panel
           
           tabPanel(icon("briefcase"),
                    sidebarLayout(
                      sidebarPanel(id="sidebar",
                        h2("Economic Data:"),
                        hr(),
                        selectInput("InputYears","Select the Year you would like to see data for:",data.years, selected = 2019),
                        hr(),
                        strong("Summary Data:"),
                        hr(),
                        strong("Notes on Data:"),
                        br(),
                        tags$div(class="sidebar_notes","Testing"),
                        hr(),
                        div(img(src="esd.jpg", width = "30%", height = "30%", style = "padding-top: 5px")),
                        width=2),
                      mainPanel(id="mainpanel",
                        h2("Regional"),
                        "Household income varies across the region and there are .........", 
                        br(),
                        fluidRow(column(6,h3("Median Household Income by Race")),
                                 column(6,h3("Percentage of Regional Median Household Income by Race"))),
                        fluidRow(column(6,plotlyOutput("chart_msa_median_income_total")),
                                 column(6,plotlyOutput("chart_msa_median_income_share"))),
                        fluidRow(br(),column(12, dataTableOutput("table_msa_median_income"))),
                        br(),
                        
                      ) # End of Main Panel of Unemployment
                    ) # End of Sidebar of Unemployment
           )# End of Tab Panel of Unemployment


  ) # End of NavBar Page
) # End of Shiny App
