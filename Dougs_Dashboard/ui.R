


navbarPage(windowTitle = "Dougs Data",
           tags$style(type = "text/css", "#myhillmap {height: calc(100vh - 80px) !important;}"),
           tabPanel("Travel",
                    sidebarLayout(
                      sidebarPanel(
                        width=2,
                        checkboxGroupInput("lfilter", "Traveltype Filter",
                                           choices = c(
                                             "Walk" = "Walk",
                                             "Electric Rideable" = "Electric_Skateboard",
                                             "Cycling" = "Cycling",
                                             "Electric Car" = "Electric_Car",
                                             "Car" = "Car",
                                             "Bus - Local" = "Bus - Local",
                                             "Bus - Coach" = "Bus - Coach",
                                             "Train - Light" = "Train - Light",
                                             "Train - National" = "Train - National",
                                             "Canoe" = "Canoe",
                                             "Boat" = "Boat",
                                             "Car Ferry" = "Car_Ferry",
                                             "Airtrack" = "Airtrack",
                                             "Helicopter" = "Helicopter",
                                             "Plane - Domestic" = "Plane - Domestic",
                                             "Plane - Short Haul" = "Plane - Short Haul",
                                             "Plane - Long Haul" = "Plane - Long Haul",
                                             "Plane - International" = "Plane - International",
                                             "None" = "None",
                                             "misc" = "misc"),
                                           selected = c(
                                             #"Canoe",
                                             #"Boat",
                                             #"Car_Ferry",
                                             "Walk",
                                             "Electric_Skateboard"#,
                                             #"Cycling",
                                             #"Bus - Local",
                                             #"Bus - Coach",
                                             #"Electric_Car",
                                             #"Car",
                                             #"Train - Light",
                                             #"Train - National",
                                             #"Airtrack",
                                             #"Helicopter",
                                             #"Plane - Domestic",
                                             #"Plane - Short Haul",
                                             #"Plane - Long Haul",
                                             #"Plane - International"
                                             )
                        ),
                        sliderTextInput(inputId = "dateslide",
                                        label = "Time Filter",
                                        choices = choices_month,
                                        selected = c(choices_month[1],choices_month[length(choices_month)])
                        ),
                        actionButton("rerun","Apply Filter/Redraw Map"),
                        radioButtons("graphtype",label="Graph Data:",choices=c("Time" = "time_taken",
                                                                               "Distance" = "length",
                                                                               "Emissions" = "kg_all_co2e",
                                                                               "Count of Journeys" = "count_journeys"),
                                     selected="time_taken")
                        #,actionButton("updata","Refresh Data Source")
                      ),
                      mainPanel(
                        width=10,
                        leafletOutput("mymap",height=400),
                        tabsetPanel(
                          tabPanel(title="Weekly",plotlyOutput("weekmap",height=400)),
                          tabPanel(title="Monthly",plotlyOutput("monthmap",height=400)),
                          tabPanel(title="Annual",plotlyOutput("annualmap",height=400))
                        )
                      )
                    )
           ),
           tabPanel("Utility Usage",
                    sidebarLayout(
                      sidebarPanel(
                        width=2,
                        sliderTextInput(inputId = "dateslide_uti",
                                        label = "Time Filter",
                                        choices = choices_month_uti,
                                        selected = c(choices_month_uti[1],choices_month_uti[length(choices_month_uti)])
                        ),
                        checkboxGroupInput("utilityfilt","Utility:",
                                           choices=c("Gas" = "gas",
                                                     "Electricity" = "elec"
                                           ),selected = c("gas","elec")
                        ),
                        radioButtons("usageoremissionsrdo",label="Data:",choices=c("Usage" = "usage",
                                                                                   "Emissions" = "emissions"
                        ),
                        selected="usage")
                        #,actionButton("updata_uti","Refresh Data Source")
                      ),mainPanel(width=10,
                                  tabsetPanel(
                                    tabPanel(title="Weekly",plotlyOutput("weekmap_uti",height=400)),
                                    tabPanel(title="Monthly",plotlyOutput("monthmap_uti",height=400)),
                                    tabPanel(title="Annual",plotlyOutput("annualmap_uti",height=400))
                                  ),h4("Electricity usage over the last 6 days:"),
                                  plotlyOutput("daily_week_uti",height=400)
                      ))),
           tabPanel("Emissions",
                    sidebarLayout(
                      sidebarPanel(
                        width=2,
                        sliderTextInput(inputId = "dateslide_emi",
                                        label = "Time Filter",
                                        choices = choices_month_emi,
                                        selected = c(choices_month_emi[1],choices_month_emi[length(choices_month_emi)])
                        ),
                        checkboxGroupInput("emifilt","Emissions Source:",
                                           choices=c("Aircraft" = "Aircraft",
                                                     "Boat" = "Boat",
                                                     "Bus" = "Bus",
                                                     "Car" = "Car",
                                                     "Train" = "Train",
                                                     "Gas" = "gas_emissions",
                                                     "Electricity" = "elec_emissions"
                                           ),selected = c("Aircraft",
                                                          "Boat",
                                                          "Bus",
                                                          "Car",
                                                          "Train",
                                                          "gas_emissions",
                                                          "elec_emissions")
                        )
                        #,actionButton("updata_emi","Refresh Data Source")
                      ),mainPanel(width=10,
                                  tabsetPanel(
                                    tabPanel(title="Weekly",plotlyOutput("weekmap_emi",height="100%")),
                                    tabPanel(title="Monthly",plotlyOutput("monthmap_emi",height="100%")),
                                    tabPanel(title="Annual",plotlyOutput("annualmap_emi",height="100%"))
                                  ))
                    )),
           tabPanel("Hill Walking",
                    sidebarLayout(
             sidebarPanel(
               width=2,
               sliderTextInput(inputId = "dateslide_hil",
                                       label = "Time Filter",
                                       choices = choices_month_hil,
                                       selected = c(choices_month_hil[1],choices_month_hil[length(choices_month_hil)])
               ),
               selectInput("hills","Hill Lists:",
                                  choices=c("Marilyn" = "Ma",
"Marilyn twin-top" = "Ma=",
"Hump" = "Hu",
"Hump twin-top" = "Hu=",
"Tump (all)" = "Tu",
"Simm" = "Sim",
"Dodd" = "5",
"Munro" = "M",
"Munro Top" = "MT",
"Furth" = "F",
"Corbett" = "C",
"Graham" = "G",
"Donald" = "D",
"Donald Top" = "DT",
"Hewitt" = "Hew",
"Nuttall" = "N",
"Dewey" = "Dew",
"Donald Dewey" = "DDew",
"Highland Five" = "HF",
"400-499m hill" = "4",
"300-399m hill" = "3",
"200-299m hill" = "2",
"100-199m hill" = "1",
"0-99m hill" = "0",
"Wainwright" = "W",
"Wainwright Outlying Fell" = "WO",
"Birkett" = "B",
"Synge" = "Sy",
"Fellranger" = "Fel",
"Historic County Top (pre-1974)" = "CoH",
"Administrative County Top (1974 to mid-1990s)" = "CoA",
"Current County/UA Top" = "CoU",
"London Borough Top" = "CoL",
"SIB" = "SIB",
"Submarilyn" = "sMa",
"Subhump" = "sHu",
"Subsimm" = "sSim",
"Subdodd" = "s5",
"Sub490-499m hill" = "s4",
"Murdo" = "Mur",
"Corbett Top" = "CT",
"Graham Top" = "GT",
"Buxton & Lewis" = "BL",
"Bridge" = "Bg",
"Yeaman" = "Y",
"Clem" = "Cm",
"Trail 100" = "T100",
"Deleted Munro Top" = "xMT",
"Deleted Corbett" = "xC",
"Deleted Graham" = "xG",
"Deleted Nuttall" = "xN",
"Deleted Donald Top" = "xDT",
"Dillon" = "Dil",
"Vandeleur-Lynam" = "VL",
"Arderin" = "A",
"Carn" = "Ca",
"Binnion" = "Bin",
"Other lists" = "O",
"Unclassified" = "Un")
                                            
                                  ,selected = c("M"),
                           multiple=TRUE
               ),
               sliderInput("haltitude","Hill Altitude:",
                           min=0,
                           max=1500,
                           step=100,
                           value=c(0,1500)),
               sliderInput("hdrop","Hill Drop:",
                           min=0,
                           max=1500,
                           step=100,
                           value=c(0,1500)),
               checkboxGroupInput("climbed","Ascent Complete:",
                                  choices=c("Yes" = "yes",
                                            "No" = "no"
                                  ),
                                  selected = c("yes")
               ),
              actionButton("rerunhill","Apply Filter/Redraw Map")
),
             mainPanel(width=10,
                       leafletOutput("myhillmap"))
           
           )
)
)