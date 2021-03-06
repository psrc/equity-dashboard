shinyUI(
navbarPage(title="",
          windowTitle = "Alpha Testing of PSRC Equity Data", 
          theme = "styles.css",
          position = "fixed-top",

           tabPanel("Overview",
                    sidebarLayout(
                      sidebarPanel(id="sidebar",
                        div(img(src="psrc-logo.png", width = "100%", height = "100%", style = "padding-top: 5px")),
                        hr(),
                        h3("Equity Resources:"),
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
                        h4("Would you like more information?"),
                        hr(),
                        strong(tags$div(class="sidebar_notes","Charles Patton, Ph.D.")),
                        tags$a(class = "source_url", href="https://www.psrc.org/contact-center/have-question?contact=9626&destination=node/9362&width=75%25&height=75%25&subject=Planning%20for%20Equity", "Email", target="_blank"),
                        br(),
                        tags$div(class="sidebar_notes","206-971-3285"),
                        hr(),
                        width=3),
                      mainPanel(id="mainpanel",
                                width = 9,
                        h1("Planning for Equity"),
                        fluidRow(column(12,div(img(src="covid-mural.jpg", width = "75%", height = "75%", style = "padding-top: 25px")))
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
                        div(img(src="equity-strategy-timeline.jpg", width = "100%", height = "100%", style = "padding-top: 5px")),
                        hr(),
                        div(actionLink("returntop", "Return to Top", onclick ="window.scrollTo(0, 0)"), style = "position: fixed; bottom: 5px; right: 10px; width: 50px; text-align:center"),
                      ) # End of Overview Panel
                    ) # End of Sidebar of Unemployment
           ),# End of Tab Panel of Overview Panel
           
          navbarMenu("Economic Data", 
            tabPanel("Median Income",
                     sidebarLayout(
                       sidebarPanel(id="sidebar",
                                    div(img(src="psrc-logo.png", width = "100%", height = "100%", style = "padding-top: 5px")),
                                    hr(),
                                    selectInput("IncomeType","Type of Data to display:",chart_value_types, selected = "Total"),
                                    h3("Equity Resources:"),
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
                                    h4("Would you like more information?"),
                                    hr(),
                                    strong(tags$div(class="sidebar_notes","Charles Patton, Ph.D.")),
                                    tags$a(class = "source_url", href="https://www.psrc.org/contact-center/have-question?contact=9626&destination=node/9362&width=75%25&height=75%25&subject=Planning%20for%20Equity", "Email", target="_blank"),
                                    br(),
                                    tags$div(class="sidebar_notes","206-971-3285"),
                                    hr(),
                                    width=3),
                      mainPanel(id="mainpanel",
                                width = 9,
                        h1("Median Household Income"),
                        "There. You see Lord Vader, she can be reasonable. Continue with the operation. You may fire when ready. What? You're far too trusting. Dantooine is too remote to make an effective demonstration. But don't worry. We will deal with your Rebel friends soon enough. No! Commence primary ignition.",
                        hr(),
                        h2("Regional"),
                        "Luke? Luke? Luke? Have you seen Luke this morning? He said he had some things to do before he started today, so he left early. Uh? Did he take those two new droids with him? I think so. Well, he'd better have those units in the south range repaired be midday or there'll be hell to pay! Wait, there's something dead ahead on the scanner. It looks like our droid.hit the accelerator.",
                        br(),
                        fluidRow(column(12,h3("Median Household Income by Race"))),
                        fluidRow(column(4,"The approach will not be easy. You are required to maneuver straight down this trench and skim the surface to this point. The target area is only two meters wide. It's a small thermal exhaust port, right below the main port. The shaft leads directly to the reactor system. A precise hit will start a chain reaction which should destroy the station. Only a precise hit will set up a chain reaction. The shaft is ray-shielded, so you'll have to use proton torpedoes. That's impossible, even for a computer. It's not impossible. I used to bull's-eye womp rats in my T-sixteen back home. They're not much bigger than two meters. Man your ships! And may the Force be with you!"),
                                 column(8,plotlyOutput("chart_region_income"))),
                        fluidRow(br(),column(12, dataTableOutput("table_msa_income"))),
                        hr(),
                        h2("County"),
                        "The shaft leads directly to the reactor system. A precise hit will start a chain reaction which should destroy the station. Only a precise hit will set up a chain reaction. The shaft is ray-shielded, so you'll have to use proton torpedoes. That's impossible, even for a computer. It's not impossible. I used to bull's-eye womp rats in my T-sixteen back home. They're not much bigger than two meters. Man your ships! And may the Force be with you!",
                        br(),
                        fluidRow(column(12,h3("Median Household Income by Race"))),
                        fluidRow(column(12,plotlyOutput("chart_county_income"))),
                        fluidRow(br(),column(12, dataTableOutput("table_county_income"))),
                        hr(),
                        div(actionLink("returntop", "Return to Top", onclick ="window.scrollTo(0, 0)"), style = "position: fixed; bottom: 5px; right: 10px; width: 50px; text-align:center"),
                        
                      ) # End of Main Panel of Median Income
                    ) # End of Sidebar of Median Income
           ),# End of Tab Panel of Median Income
          
           tabPanel("Educational Attainment",
                    sidebarLayout(
                      sidebarPanel(id="sidebar",
                                   div(img(src="psrc-logo.png", width = "100%", height = "100%", style = "padding-top: 5px")),
                                   hr(),
                                   selectInput("EducationType","Type of Data to display:",chart_value_types, selected = "Share"),
                                   h3("Equity Resources:"),
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
                                   h4("Would you like more information?"),
                                   hr(),
                                   strong(tags$div(class="sidebar_notes","Charles Patton, Ph.D.")),
                                   tags$a(class = "source_url", href="https://www.psrc.org/contact-center/have-question?contact=9626&destination=node/9362&width=75%25&height=75%25&subject=Planning%20for%20Equity", "Email", target="_blank"),
                                   br(),
                                   tags$div(class="sidebar_notes","206-971-3285"),
                                   hr(),
                                   width=3),
                      mainPanel(id="mainpanel",
                                width = 9,
                                h1("Educational Attainment"),
                                "When could weathermen predict the weather, let alone the future. Yeah, alright, bye-bye. What? Perfect, just perfect. Can I go now, Mr. Strickland? Over there, on my hope chest. I've never seen purple underwear before, Calvin.",
                                hr(),
                                h2("Regional"),
                                "I over slept, look I need your help. I have to ask Lorraine out but I don't know how to do it. I have to ask Lorraine out but I don't know how to do it. George. George. Weight has nothing to do with it. I know, and all I could say is I'm sorry. I can't believe you loaned me a car, without telling me it had a blindspot. I could've been killed.",
                                br(),
                                fluidRow(column(12,h3("High School Graduation Rate by Race"))),
                                "Pretty Mediocre photographic fakery, they cut off your brother's hair. Save the clock tower, save the clock tower. Mayor Wilson is sponsoring an initiative to replace that clock. Thirty years ago, lightning struck that clock tower and the clock hasn't run since. We at the Hill Valley Preservation Society think it should be preserved exactly the way it is as part of our history and heritage. Biff. I will. Oh.",
                                br(),
                                fluidRow(column(5,plotlyOutput("chart_region_education_hs")),
                                         column(7, dataTableOutput("table_region_education_hs"))),
                                br(),
                                fluidRow(column(12,h3("College Graduation Rate by Race"))),
                                "Not a word, not a word, not a word now. Quiet, uh, donations, you want me to make a donation to the coast guard youth auxiliary? Ohh, no. There's that word again, heavy. Why are things so heavy in the future. Is there a problem with the Earth's gravitational pull? What a nightmare. Oh, great scott. You get the cable, I'll throw the rope down to you.",
                                br(),br(),
                                fluidRow(column(7, dataTableOutput("table_region_education_bs")),
                                         column(5,plotlyOutput("chart_region_education_bs"))),
                                hr(),
                                h2("County"),
                                "Science Fiction Theater. Uh, stories, science fiction stories, about visitors coming down to Earth from another planet. Thank god I still got my hair. What on Earth is that thing I'm wearing? Aw yeah, everything is great. Who are you calling spook, pecker-wood.",
                                br(),
                                fluidRow(column(12,h3("High School Graduation Rate by Race"))),
                                "Hi, it's really a pleasure to meet you. We never would have fallen in love. Flux capacitor. Uh, Doc. Excuse me.",
                                fluidRow(column(12,plotlyOutput("chart_county_education_hs"))),
                                fluidRow(br(),column(12, dataTableOutput("table_county_education_hs"))),
                                br(),
                                fluidRow(column(12,h3("College Graduation Rate by Race"))),
                                "This is for all you lovers out there. Stop it. Yeah Mom, we know, you've told us this story a million times. You felt sorry for him so you decided to go with him to The Fish Under The Sea Dance. Oh, uh, hey you, get your damn hands off her. Do you really think I oughta swear? He's your brother, Mom.",
                                fluidRow(column(12,plotlyOutput("chart_county_education_bs"))),
                                fluidRow(br(),column(12, dataTableOutput("table_county_education_bs"))),
                                hr(),
                                div(actionLink("returntop", "Return to Top", onclick ="window.scrollTo(0, 0)"), style = "position: fixed; bottom: 5px; right: 10px; width: 50px; text-align:center"),
                                
                      ) # End of Main Panel of Educational Attainment
                    ) # End of Sidebar of Educational Attainment
           ),# End of Tab Panel of Educational Attainment
           
           tabPanel("Home Ownership",
                    sidebarLayout(
                      sidebarPanel(id="sidebar",
                                   div(img(src="psrc-logo.png", width = "100%", height = "100%", style = "padding-top: 5px")),
                                   hr(),
                                   selectInput("OwnershipType","Type of Data to display:",chart_value_types, selected = "Share"),
                                   h3("Equity Resources:"),
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
                                   h4("Would you like more information?"),
                                   hr(),
                                   strong(tags$div(class="sidebar_notes","Charles Patton, Ph.D.")),
                                   tags$a(class = "source_url", href="https://www.psrc.org/contact-center/have-question?contact=9626&destination=node/9362&width=75%25&height=75%25&subject=Planning%20for%20Equity", "Email", target="_blank"),
                                   br(),
                                   tags$div(class="sidebar_notes","206-971-3285"),
                                   hr(),
                                   width=3),
                      mainPanel(id="mainpanel",
                                width = 9,
                                h1("Home Ownership"),
                                "Baseball ipsum dolor sit amet passed ball outs, sweep stretch bleeder triple play. Left fielder count fair swing cork balk ball. Full count southpaw reliever lineup crooked number fastball second base. Perfect game outfielder rally force dodgers right fielder dead ball era right field. Pickoff world series peanuts batting average cup of coffee foul inside robbed. Bleeder club appeal first base sidearm mustard steal line drive inning.",
                                hr(),
                                h2("Regional"),
                                "Sidearm bullpen base on balls national pastime losses reliever umpire pull dribbler. Manager 4-bagger tag national pastime pennant good eye relief pitcher. Save on-base percentage fan ejection baseball card skipper reliever strikeout bench. On-base percentage fenway contact win warning track, ball rhubarb center field. Starting pitcher hey batter out cellar cardinals basehit double switch. Play hey batter tigers sidearm in the hole shortstop no decision.",
                                br(),
                                fluidRow(column(12,h3("Home Ownership Rate by Race"))),
                                fluidRow(column(8,plotlyOutput("chart_region_homeownership")),
                                         column(4,"Gap sacrifice bunt force pennant cup of coffee on-base percentage cubs designated hitter. Rhubarb grand slam dead ball era yankees all-star double switch relief pitcher red sox cardinals. Mitt left field bush league glove skipper silver slugger slide alley. Interleague strikeout save ejection ground rule double, baseline sport. Rubber losses grand slam good eye foul pole assist fastball fenway bat. Starter appeal knuckleball airmail loss dead red triple-A.")),
                                fluidRow(br(),column(12, dataTableOutput("table_region_homeownership"))),
                                hr(),
                                h2("County"),
                                "Strikeout strikeout rubber game pickoff foul line, cellar baltimore chop starting pitcher grand slam. Series baseline grass knuckle loss off-speed rip baseline. Moneyball rubber breaking ball tapper pinch hitter cheese flyout. Game foul pole butcher boy scorecard check swing right field plate. Mustard swing fielder's choice bullpen rope strike zone diamond loogy disabled list. Curve bunt first baseman runs cycle loogy pennant perfect game.",
                                br(),
                                fluidRow(column(12,h3("Home Ownership Rate by Race"))),
                                fluidRow(column(12,plotlyOutput("chart_county_homeownership"))),
                                fluidRow(br(),column(12, dataTableOutput("table_county_homeownership"))),
                                hr(),
                                div(actionLink("returntop", "Return to Top", onclick ="window.scrollTo(0, 0)"), style = "position: fixed; bottom: 5px; right: 10px; width: 50px; text-align:center"),
                      ) # End of Main Panel of Home Ownership
                    ) # End of Sidebar of Home Ownership
           )# End of Tab Panel of Home Ownership          
          )# End of Nav Bar Menu for Economic Data


  ) # End of NavBar Page
) # End of Shiny App
