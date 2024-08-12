


navbarPage(windowTitle = "Dougs Data",
           tags$style(type = "text/css", "#myhillmap {height: calc(100vh - 80px) !important;} #daymap_emi {height: calc(100vh - 120px) !important;} #weekmap_emi {height: calc(100vh - 120px) !important;} #monthmap_emi {height: calc(100vh - 120px) !important;} #annualmap_emi {height: calc(100vh - 120px) !important;} #daily_week_elec {height: calc(25vh - 30px) !important;} #daily_week_gas {height: calc(25vh - 30px) !important;} #daymap_uti {height: calc(45vh - 60px) !important;} #weekmap_uti {height: calc(45vh - 60px) !important;} #monthmap_uti {height: calc(45vh - 60px) !important;} #annualmap_uti {height: calc(50vh - 60px) !important;} "),
           
           tags$style(
             ".btn-walk {background-color: white; color: black;}",
             ".btn-electric_skateboard {background-color: white; color: black;}",
             ".btn-cycling {background-color: white; color: black;}",
             ".btn-car {background-color: white; color: black;}",
             ".btn-bus_coach {background-color: white; color: black;}",
             ".btn-bus_local {background-color: white; color: black;}",
             ".btn-train {background-color: white; color: black;}",
             ".btn-canoe {background-color: white; color: black;}",
             ".btn-boat {background-color: white; color: black;}",
             ".btn-car_ferry {background-color: white; color: black;}",
             ".btn-airtrack {background-color: white; color: black;}",
             ".btn-helicopter {background-color: white; color: black;}",
             ".btn-plane_dom {background-color: white; color: black;}",
             ".btn-plane_sh {background-color: white; color: black;}",
             ".btn-plane_lh {background-color: white; color: black;}",
             ".btn-plane_int {background-color: white; color: black;}",
             ".btn-drone {background-color: white; color: black;}",
             ".btn-none {background-color: white; color: black;}",
             ".btn-misc {background-color: white; color: black;}",
             ".btn-walk.active {background-color: #008000; color: white;}",
             ".btn-electric_skateboard.active {background-color: #00fa9a; color: black;}",
             ".btn-cycling.active {background-color: #ffa500; color: white;}",
             ".btn-car.active {background-color: #ff0000; color: white;}",
             ".btn-bus_coach.active {background-color: #cd5c5c; color: white;}",
             ".btn-bus_local.active {background-color: #f08080; color: white;}",
             ".btn-train.active {background-color: #ffd700; color: black;}",
             ".btn-canoe.active {background-color: #00bfff; color: white;}",
             ".btn-boat.active {background-color: #00ced1; color: white;}",
             ".btn-car_ferry.active {background-color: #0000ff; color: white;}",
             ".btn-airtrack.active {background-color: #000000; color: white;}",
             ".btn-helicopter.active {background-color: #800080; color: white;}",
             ".btn-plane_dom.active {background-color: #ba55d3; color: white;}",
             ".btn-plane_sh.active {background-color: #9370db; color: white;}",
             ".btn-plane_lh.active {background-color: #9932cc; color: white;}",
             ".btn-plane_int.active {background-color: #800080; color: white;}",
             ".btn-drone.active {background-color: #000000; color: white;}",
             ".btn-none.active {background-color: #ffffff; color: black;}",
             ".btn-misc.active {background-color: #000000; color: white;}",
             
             ".btn-emi-air.active {background-color: #800080; color: white;}",
             ".btn-emi-boat.active {background-color: #0000ff; color: white;}",
             ".btn-emi-bus.active {background-color: #cd5c5c; color: white;}",
             ".btn-emi-car.active {background-color: #ff0000; color: white;}",
             ".btn-emi-train.active {background-color: #ffd700; color: black;}",
             ".btn-emi-gas.active {background-color: #00999d; color: white;}",
             ".btn-emi-elec.active {background-color: #880f07; color: white;}",
             ".btn-emi-logs.active {background-color: #5C4033; color: white;}",
             
             ".btn-emi-air {background-color: white; color: black;}",
             ".btn-emi-boat {background-color: white; color: black;}",
             ".btn-emi-bus {background-color: white; color: black;}",
             ".btn-emi-car {background-color: white; color: black;}",
             ".btn-emi-train {background-color: white; color: black;}",
             ".btn-emi-gas {background-color: white; color: black;}",
             ".btn-emi-elec {background-color: white; color: black;}",
             ".btn-emi-logs {background-color: white; color: black;}",
             
             ".btn-uti-gas.active {background-color: #880f07; color: white;}",
             ".btn-uti-elec.active {background-color:  #00999d; color: white;}",
             ".btn-uti-logs.active {background-color: #5C4033; color: white;}",
             
             ".btn-uti-gas {background-color: white; color: black;}",
             ".btn-uti-elec {background-color: white; color: black;}",
             ".btn-uti-logs {background-color: white; color: black;}",
             
             ".btn-hwa-doug.active {background-color: blue; color: white;}",
             ".btn-hwa-rho.active {background-color: green; color: white;}",
             ".btn-hwa-no.active {background-color: red; color: white;}",
             
             ".btn-hwa-doug {background-color: white; color: black;}",
             ".btn-hwa-rho {background-color: white; color: black;}",
             ".btn-hwa-no {background-color: white; color: black;}"
             

             
             
             
             

           ),
           
           
           tabPanel("Utility Usage",
                    sidebarLayout(
                      sidebarPanel(
                        width=2,
                        sliderTextInput(inputId = "dateslide_uti",
                                        label = "Time Filter",
                                        choices = choices_month_uti,
                                        #selected = c(choices_month_uti[1],choices_month_uti[length(choices_month_uti)])
                                        selected = c(str_c("Jan-",max_yr_uti),choices_month_uti[length(choices_month_uti)])
                        ),
                        checkboxGroupButtons("utilityfilt","Utility:",
                                           choices=c("Gas" = "gas",
                                                     "Electricity" = "elec",
                                                     "Logs" = "log"
                                           ),selected = c("gas","elec","logs"),
                                           status = c("uti-gas",
                                                      "uti-elec",
                                                      "uti-logs")
                        ),
                        radioGroupButtons("usageoremissionsrdo",label="Data:",choices=emissions_graph_types
                        ,selected="usage")
                        ,actionButton("update_uti","Refresh Data Source")
                      ),mainPanel(width=10,
                                  tabsetPanel(
                                    tabPanel(title="Daily",plotlyOutput("daymap_uti",height="50%")),
                                    tabPanel(title="Weekly",plotlyOutput("weekmap_uti",height="50%")),
                                    tabPanel(title="Monthly",plotlyOutput("monthmap_uti",height="50%")),
                                    tabPanel(title="Annual",plotlyOutput("annualmap_uti",height="50%"))
                                  )
                                  ,h4("Utility usage over the last 3 days:"),
                                  plotlyOutput("daily_week_elec",height="50%"),
                                  plotlyOutput("daily_week_gas",height="50%")
                      ))),
           tabPanel("Emissions",
                    sidebarLayout(
                      sidebarPanel(
                        width=2,
                        sliderTextInput(inputId = "dateslide_emi",
                                        label = "Time Filter",
                                        choices = choices_month_emi,
                                        selected = c(str_c("Jan-",max_yr_emi),choices_month_emi[length(choices_month_emi)])
                        ),
                        checkboxGroupButtons("emifilt","Emissions Source:",
                                           choices=emissions_filters_types,
                                           selected = c("Aircraft",
                                                          "Boat",
                                                          "Bus",
                                                          "Car",
                                                          "Train",
                                                          "gas_emissions",
                                                          "elec_emissions",
                                                        "log_emissions"),
                                           status = c(
                                             "emi-air",
                                             "emi-boat",
                                             "emi-bus",
                                             "emi-car",
                                             "emi-train",
                                             "emi-gas",
                                             "emi-elec",
                                             "emi-logs"
                                           )
                        )
                        ,actionButton("update_emi","Refresh Data Source")
                      ),mainPanel(width=10,id = "emissions_grp",
                                  tabsetPanel(
                                    tabPanel(title="Daily",plotlyOutput("daymap_emi",height="100%")),
                                    tabPanel(title="Weekly",plotlyOutput("weekmap_emi",height="100%")),
                                    tabPanel(title="Monthly",plotlyOutput("monthmap_emi",height="100%")),
                                    tabPanel(title="Annual",plotlyOutput("annualmap_emi",height="100%"))
                                  ))
                    )),
           tabPanel("Travel",
                    sidebarLayout(
                      sidebarPanel(
                        width=2,
                        checkboxGroupButtons("lfilter", "Traveltype Filter",
                                           choices = c(
                                             "Walk" = "Walk",
                                             "Electric Rideable" = "Electric_Skateboard",
                                             "Cycling" = "Cycling",
                                             "Car - EV" = "Car - EV",
                                             "Car - Hydrogen" = "Car - Hydrogen",
                                             "Car" = "Car",
                                             "Car - Small" = "Car - Small",
                                             "Car - Hybrid Small" = "Car - Hybrid Small",
                                             "Car - Hybrid Large" = "Car - Hybrid Large",
                                             "Car - 4x4" = "Car - 4x4",
                                             "Bus - Local" = "Bus - Local",
                                             "Bus - Coach" = "Bus - Coach",
                                             "Train - Light" = "Train - Light",
                                             "Train - National" = "Train - National",
                                             "Canoe" = "Canoe",
                                             "Boat" = "Boat",
                                             "Car Ferry" = "Car_Ferry",
                                             "Aerial Lift" = "Airtrack",
                                             "Helicopter" = "Helicopter",
                                             "Plane - Domestic" = "Plane - Domestic",
                                             "Plane - Short Haul" = "Plane - Short Haul",
                                             "Plane - Long Haul" = "Plane - Long Haul",
                                             "Plane - International" = "Plane - International",
                                             "Drone" = "Drone",
                                             "None" = "None",
                                             "misc" = "misc"),
                                           selected = c(
                                             "Canoe",
                                             "Boat",
                                             "Car_Ferry",
                                             "Walk",
                                             "Electric_Skateboard",
                                             "Cycling",
                                             "Bus - Local",
                                             "Bus - Coach",
                                             "Car - EV",
                                             "Car - Hydrogen",
                                             "Car",
                                             "Car - Small",
                                             "Car - Hybrid Small",
                                             "Car - Hybrid Large",
                                             "Car - 4x4",
                                             "Train - Light",
                                             "Train - National",
                                             "Airtrack",
                                             "Helicopter",
                                             "Plane - Domestic",
                                             "Plane - Short Haul",
                                             "Plane - Long Haul",
                                             "Plane - International"
                                           ),
                                           status = c(
                                             "walk",
                                             "electric_skateboard",
                                             "cycling",
                                             "car",
                                             "car",
                                             "car",
                                             "car",
                                             "car",
                                             "car",
                                             "car",
                                             "bus_coach",
                                             "bus_local",
                                             "train",
                                             "train",
                                             "canoe",
                                             "boat",
                                             "car_ferry",
                                             "airtrack",
                                             "helicopter",
                                             "plane_dom",
                                             "plane_sh",
                                             "plane_lh",
                                             "plane_int",
                                             "drone",
                                             "none",
                                             "misc"
                                           ),
                        ),
                        sliderTextInput(inputId = "dateslide",
                                        label = "Time Filter",
                                        choices = choices_month,
                                        selected = c(str_c("Jan-",max_yr),choices_month[length(choices_month)])
                        ),
                        actionButton("rerun","Apply Filter/Redraw Map"),
                        radioGroupButtons("graphtype",label="Graph Data:",choices=travel_map_types,
                                     selected="time_taken")
                        #,actionButton("updata","Refresh Data Source")
                      ),
                      mainPanel(
                        width=10,
                        #leafglOutput("mymap",height="50vh"),
                        leafletOutput("mymap",height="50vh"),
                        tabsetPanel(
                          tabPanel(title="Daily",plotlyOutput("daymap",height="35vh")),
                          tabPanel(title="Weekly",plotlyOutput("weekmap",height="35vh")),
                          tabPanel(title="Monthly",plotlyOutput("monthmap",height="35vh")),
                          tabPanel(title="Annual",plotlyOutput("annualmap",height="35vh"))
                        )
                      )
                    )
           ),
           tabPanel("Hill Walking",
                    sidebarLayout(
             sidebarPanel(
               width=2,
               sliderTextInput(inputId = "dateslide_hil",
                                       label = "Time Filter",
                                       choices = choices_month_hil,
                                       selected = c(choices_month_hil[1],choices_month_hil[length(choices_month_hil)])
               ),
               multiInput("hills","Hill Lists:",
                                  choices=c(
                                    "Munro" = "M",
                                    "Corbett" = "C",
                                    "Graham" = "G",
                                    "Highland Five" = "HF",
                                    "Donald Dewey" = "DDew",
                                    "400-499m hill" = "4",
                                    "300-399m hill" = "3",
                                    "200-299m hill" = "2",
                                    "100-199m hill" = "1",
                                    "0-99m hill" = "0",
                                    "Munro Top" = "MT",
                                    "Corbett Top" = "CT",
                                    "Graham Top" = "GT",
                                    "Hump" = "Hu",
                                    "Hump twin-top" = "Hu=",
                                    "Tump (all)" = "Tu",
                                    "SIB" = "SIB",
                                    
"Marilyn" = "Ma",
"Marilyn twin-top" = "Ma=",
"Simm" = "Sim",
"Dodd" = "5",
"Furth" = "F",
"Donald" = "D",
"Donald Top" = "DT",
"Hewitt" = "Hew",
"Nuttall" = "N",
"Dewey" = "Dew",
"Wainwright" = "W",
"Wainwright Outlying Fell" = "WO",
"Birkett" = "B",
"Synge" = "Sy",
"Fellranger" = "Fel",
"Historic County Top (pre-1974)" = "CoH",
"Administrative County Top (1974 to mid-1990s)" = "CoA",
"Current County/UA Top" = "CoU",
"London Borough Top" = "CoL",
"Submarilyn" = "sMa",
"Subhump" = "sHu",
"Subsimm" = "sSim",
"Subdodd" = "s5",
"Sub490-499m hill" = "s4",
"Murdo" = "Mur",
"Buxton & Lewis" = "BL",
"Bridge" = "Bg",
"Yeaman" = "Y",
"Clem" = "Cm",
"Trail 100" = "T100",
"Dillon" = "Dil",
"Vandeleur-Lynam" = "VL",
"Arderin" = "A",
"Carn" = "Ca",
"Binnion" = "Bin",
"Deleted Munro Top" = "xMT",
"Deleted Corbett" = "xC",
"Deleted Graham" = "xG",
"Deleted Nuttall" = "xN",
"Deleted Donald Top" = "xDT",
"Other lists" = "O",
"Unclassified" = "Un")
                                            
                                  ,selected = c("M"),
#options = (
# actionsBox = TRUE),
#                           multiple=TRUE
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
checkboxGroupButtons("climbed","Ascent Complete:",
                                  choices=c("Doug" = "Doug",
                                            "Rhona" = "Rhona",
                                            "No" = "no"
                                  ),
                                  selected = c("Doug"),
                     status = c(
                       "hwa-doug",
                       "hwa-rho",
                       "hwa-no"
                       
                     )
               ),
              actionButton("rerunhill","Apply Filter/Redraw Map"),
              tableOutput('tbl')
),
             mainPanel(width=10,
                       #leafglOutput("myhillmap"))
                       leafletOutput("myhillmap"))
           
           )
)
)