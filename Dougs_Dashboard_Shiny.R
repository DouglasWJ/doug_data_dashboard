#imports:
library(shiny)                  #Shiny
library(RPostgres)              #Postgres Access
library(tidyverse)              #dplyr,ggplot2,%>%,stringr
library(sf)                     #Access and Use Spatial Data
library(lubridate)              #time and date functions
library(units)                  #for manipulating physical units
library(ggforce)                #for manipulating ggplot with units values
library(leaflet)                #for adding interactive mapping
library(plotly)                 #for adding interactive graphing
library(shinyWidgets)           #for sliderTextInput
library(timeDate)               #for timeLastDayInMonth
library(scales)
library(htmltools)
library(leaflet.providers)

units_options(group = c("(", ")") )

options(shiny.host = "0.0.0.0")
options(shiny.port = 7322)
options(scipen = 999)
options(bitmapType='cairo')

#connect to postgres:
pgconn <-
  dbConnect(
    RPostgres::Postgres(),
    dbname = "spatial_db",
    host = "10.10.1.200",
    port = 5433,
    user = "doug"
  )
dbExecute(pgconn,paste0("SET search_path = dougtracks,emissions,utilityusage, public"))


#pre-processing:
#travel:
# reload_map_data <- function() {
#   travel_lines <- read_sf(dsn=pgconn,layer="dougtracks_lines_emi_mv_simple2") %>%
#     mutate(duration = (dhours(time_taken_h) + 
#                          dminutes(time_taken_m) + 
#                          dseconds(time_taken_s)))
#   return(travel_lines)
# }
# reload_map_data_old <- function() {
# print("reloading data")
# travel_lines <- read_sf(dsn=pgconn,layer="dougtracks_lines_emi_mv_simple") %>%
#   st_zm() %>%
#   filter(traveltype != 'Imports') %>%
#   mutate(duration = (dhours(as.numeric(str_sub(.$time_taken,1,2))) + 
#                        dminutes(as.numeric(str_sub(.$time_taken,4,5))) + 
#                        dseconds(as.numeric(str_sub(.$time_taken,7,8))))) %>%
#   mutate(traveltype = replace_na(traveltype,"None")) %>%
#   mutate(traveltype_superclass = recode(traveltype,
#                                         "Car" = "Car",
#                                         "Walk" = "Walk",
#                                         "Boat" = "Boat",
#                                         "Canoe" = "Other",
#                                         "Cycling" = "Other",
#                                         "Airtrack" = "Other",
#                                         "Plane - Domestic" = "Aircraft",
#                                         "Plane - Short Haul" = "Aircraft",
#                                         "Plane - Long Haul" = "Aircraft",
#                                         "Plane - International" = "Aircraft",
#                                         "Helicopter" = "Aircraft",
#                                         "Car_Ferry" = "Boat",
#                                         "Bus - Coach" = "Bus",
#                                         "Bus - Local" = "Bus",
#                                         "Coach" = "Bus",
#                                         "None" = "None",
#                                         "misc" = "Other",
#                                         "Train - Light" = "Train",
#                                         "Train - National" = "Train",
#                                         "Electric_Car" = "Car",
#                                         "Electric_Skateboard" = "Rideable")) %>%
#   mutate(superclass_colourv = recode(traveltype_superclass,
#                              "Car" = "#ff0000",
#                              "Boat" = "#0000ff",
#                              "Train" = "#ffd700",
#                              "Aircraft" = "#800080",
#                              "Rideable" = "#00fa9a",
#                              "Walk" = "#008000",
#                              "Other" = "#000000",
#                              "Bus" = "#cd5c5c",
#                              "None" = "#ffffff")) %>%
#   mutate(colourv = recode(traveltype,
#                           "Car" = "#ff0000",
#                           "Walk" = "#008000",
#                           "Boat" = "#00ced1",
#                           "Canoe" = "#00bfff",
#                           "Cycling" = "#ffa500",
#                           "Airtrack" = "#000000",
#                              "Plane - Domestic" = "#ba55d3",
#                              "Plane - Short Haul" = "#9370db",
#                              "Plane - Long Haul" = "#9932cc",
#                              "Plane - International" = "#800080",
#                              "Helicopter" = "#800080",
#                              "Car_Ferry" = "#0000ff",
#                              "Bus - Coach" = "#cd5c5c",
#                              "Bus - Local" = "#f08080",
#                              "Coach" = "#cd5c5c",
#                              "None" = "#ffffff",
#                              "misc" = "#000000",
#                              "Train - Light" = "#ffff00",
#                              "Train - National" = "#ffd700",
#                              "Electric_Car" = "#dc143c",
#                              "Electric_Skateboard" = "#00fa9a")) %>%
#   arrange(match(traveltype,c("None",
#                              "misc",
#                              "Canoe",
#                              "Boat",
#                              "Car_Ferry",
#                              "Walk",
#                              "Electric_Skateboard",
#                              "Cycling",
#                              "Bus - Local",
#                              "Bus - Coach",
#                              "Coach",
#                              "Electric_Car",
#                              "Car",
#                              "Train - Light",
#                              "Train - National",
#                              "Airtrack",
#                              "Helicopter",
#                              "Plane - Domestic",
#                              "Plane - Short Haul",
#                              "Plane - Long Haul",
#                              "Plane - International"))) %>%
#   mutate(year = format(lubridate::ceiling_date(start_time_utc,"day"),"%Y"))
# 
# travel_lines = travel_lines[!st_is_empty(travel_lines),,drop=FALSE]
# print("reload complete")
# return(travel_lines)
# 
# }
# 
# reload_map_data_nogeom <- function() {
#   travel_lines <- dbGetQuery(pgconn,"select * from dougtracks_lines_emi_mv_nogeom") %>%
#     as_tibble() %>%
#     mutate(duration = (dhours(time_taken_h) + 
#                          dminutes(time_taken_m) + 
#                          dseconds(time_taken_s)))
#   return(travel_lines)
# }
# reload_map_data_nogeom_old <- function() {
#   print("reloading data")
#   travel_lines <- dbGetQuery(pgconn,"select * from dougtracks_lines_emi_mv_nogeom") %>%
#     as_tibble() %>%
#     #st_zm() %>%
#     filter(traveltype != 'Imports') %>%
#     mutate(duration = (dhours(as.numeric(str_sub(.$time_taken,1,2))) + 
#                          dminutes(as.numeric(str_sub(.$time_taken,4,5))) + 
#                          dseconds(as.numeric(str_sub(.$time_taken,7,8))))) %>%
#     mutate(traveltype = replace_na(traveltype,"None")) %>%
#     mutate(traveltype_superclass = recode(traveltype,
#                                           "Car" = "Car",
#                                           "Walk" = "Walk",
#                                           "Boat" = "Boat",
#                                           "Canoe" = "Other",
#                                           "Cycling" = "Other",
#                                           "Airtrack" = "Other",
#                                           "Plane - Domestic" = "Aircraft",
#                                           "Plane - Short Haul" = "Aircraft",
#                                           "Plane - Long Haul" = "Aircraft",
#                                           "Plane - International" = "Aircraft",
#                                           "Helicopter" = "Aircraft",
#                                           "Car_Ferry" = "Boat",
#                                           "Bus - Coach" = "Bus",
#                                           "Bus - Local" = "Bus",
#                                           "Coach" = "Bus",
#                                           "None" = "None",
#                                           "misc" = "Other",
#                                           "Train - Light" = "Train",
#                                           "Train - National" = "Train",
#                                           "Electric_Car" = "Car",
#                                           "Electric_Skateboard" = "Rideable")) %>%
#     mutate(superclass_colourv = recode(traveltype_superclass,
#                                        "Car" = "#ff0000",
#                                        "Boat" = "#0000ff",
#                                        "Train" = "#ffd700",
#                                        "Aircraft" = "#800080",
#                                        "Rideable" = "#00fa9a",
#                                        "Walk" = "#008000",
#                                        "Other" = "#000000",
#                                        "Bus" = "#cd5c5c",
#                                        "None" = "#ffffff")) %>%
#     mutate(colourv = recode(traveltype,
#                             "Car" = "#ff0000",
#                             "Walk" = "#008000",
#                             "Boat" = "#00ced1",
#                             "Canoe" = "#00bfff",
#                             "Cycling" = "#ffa500",
#                             "Airtrack" = "#000000",
#                             "Plane - Domestic" = "#ba55d3",
#                             "Plane - Short Haul" = "#9370db",
#                             "Plane - Long Haul" = "#9932cc",
#                             "Plane - International" = "#800080",
#                             "Helicopter" = "#800080",
#                             "Car_Ferry" = "#0000ff",
#                             "Bus - Coach" = "#cd5c5c",
#                             "Bus - Local" = "#f08080",
#                             "Coach" = "#cd5c5c",
#                             "None" = "#ffffff",
#                             "misc" = "#000000",
#                             "Train - Light" = "#ffff00",
#                             "Train - National" = "#ffd700",
#                             "Electric_Car" = "#dc143c",
#                             "Electric_Skateboard" = "#00fa9a")) %>%
#     arrange(match(traveltype,c("None",
#                                "misc",
#                                "Canoe",
#                                "Boat",
#                                "Car_Ferry",
#                                "Walk",
#                                "Electric_Skateboard",
#                                "Cycling",
#                                "Bus - Local",
#                                "Bus - Coach",
#                                "Coach",
#                                "Electric_Car",
#                                "Car",
#                                "Train - Light",
#                                "Train - National",
#                                "Airtrack",
#                                "Helicopter",
#                                "Plane - Domestic",
#                                "Plane - Short Haul",
#                                "Plane - Long Haul",
#                                "Plane - International"))) %>%
#     mutate(year = format(lubridate::ceiling_date(start_time_utc,"day"),"%Y"))
#   
#   #travel_lines = travel_lines[!st_is_empty(travel_lines),,drop=FALSE]
#   print("reload complete")
#   return(travel_lines)
#   
# }

# reload_elec_data <- function() {
# 
#   # elec_emissions_all <- dbGetQuery(pgconn,"select * from electricty_emissions") %>%
#   #   as_tibble() %>%
#   #   select(start_of_bill_period,end_of_bill_period,all_co2e,kwh_usage) %>%
#   #   #convert to daily data to allow calculation of monthly values:
#   #   mutate(days = as.numeric((end_of_bill_period-start_of_bill_period))) %>%
#   #   #mutate(days = days-1) %>%
#   #   mutate(end_of_bill_period = end_of_bill_period-1) %>%
#   #   mutate(emissions = all_co2e/days,
#   #          usage = kwh_usage/days)
#   #
#   # #calculate the dates!
#   # adate_vec = as.Date(vector())
#   # for (i in 1:nrow(elec_emissions_all)) {
#   #   adate_vec = append(adate_vec,seq(elec_emissions_all$start_of_bill_period[i],elec_emissions_all$end_of_bill_period[i],by="days"))
#   # }
#   #
#   # #daily elec usage data:
#   # elec_emissions_daily <- tibble(elec_emissions = rep(elec_emissions_all$emissions,elec_emissions_all$days),
#   #                                elec_usage = rep(elec_emissions_all$usage,elec_emissions_all$days),
#   #                                sdate = rep(elec_emissions_all$start_of_bill_period,elec_emissions_all$days),
#   #                                edate = rep(elec_emissions_all$end_of_bill_period,elec_emissions_all$days),
#   #                                adate = adate_vec) %>%
#   #   mutate(month = (format(lubridate::floor_date(adate,"day"),"%Y-%m")),
#   #          year = (format(lubridate::floor_date(adate,"day"),"%Y")),
#   #          week = format(lubridate::floor_date(adate,"day"),"%Y-%W"))
# 
#   query <- "select
# emissions as elec_emissions,
# usage as elec_usage,
# start_of_bill_period as sdate,
# end_of_bill_period as edate,
# day as adate,
# to_char(day,'YYYY-MM') as month,
# to_char(day,'YYYY') as year,
# to_char(day,'YYYY-WW') as week
# from
# (select emissions,usage,start_of_bill_period,end_of_bill_period,
# generate_series
#         ( start_of_bill_period::timestamp
#         , end_of_bill_period::timestamp
#         , '1 day'::interval)::date as day
# from
# (select start_of_bill_period,end_of_bill_period - 1 as end_of_bill_period,all_co2e,kwh_usage,days,
# all_co2e/days as emissions,
# kwh_usage/days as usage
# from
# (select start_of_bill_period,end_of_bill_period,all_co2e,kwh_usage,
# end_of_bill_period-start_of_bill_period as days
# from utilityusage.electricty_emissions) subq) subq2) subq3"
# 
#   elec_emissions_daily <- dbGetQuery(pgconn,query)
# 
# 
#   return(elec_emissions_daily)
# 
# }
# reload_gas_data <- function() {
# 
#   gas_emissions_all <- dbGetQuery(pgconn,"select * from gas_emissions") %>%
#     as_tibble() %>%
#     select(start_of_bill_period,end_of_bill_period,all_co2e,kwh_usage) %>%
#     #convert to daily data to allow calculation of monthly values:
#     mutate(days = as.numeric((end_of_bill_period-start_of_bill_period))) %>%
#     mutate(end_of_bill_period = end_of_bill_period-1) %>%
#     mutate(emissions = all_co2e/days,
#            usage = kwh_usage/days)
# 
#   #calculate the dates!
#   adate_vec = as.Date(vector())
#   for (i in 1:nrow(gas_emissions_all)) {
#     adate_vec = append(adate_vec,seq(gas_emissions_all$start_of_bill_period[i],gas_emissions_all$end_of_bill_period[i],by="days"))
#   }
# 
#   #daily gas usage data:
#   gas_emissions_daily <- tibble(gas_emissions = rep(gas_emissions_all$emissions,gas_emissions_all$days),
#                                 gas_usage = rep(gas_emissions_all$usage,gas_emissions_all$days),
#                                 sdate = rep(gas_emissions_all$start_of_bill_period,gas_emissions_all$days),
#                                 edate = rep(gas_emissions_all$end_of_bill_period,gas_emissions_all$days),
#                                 adate = adate_vec) %>%
#     mutate(month = (format(lubridate::floor_date(adate,"day"),"%Y-%m")),
#            year = (format(lubridate::floor_date(adate,"day"),"%Y")),
#            week = format(lubridate::floor_date(adate,"day"),"%Y-%W"))
# 
# 
#   return(gas_emissions_daily)
# 
# }
# reload_daily_data <- function() {
# 
#   # elec_emissions_daily <- elec_emissions_daily %>%
#   #   rename(elec_usage = daily_usage,
#   #          elec_emissions = daily_emissions)
#   #
#   # gas_emissions_daily <- gas_emissions_daily %>%
#   #   rename(gas_usage = daily_usage,
#   #          gas_emissions = daily_emissions)
# 
#   gas_emissions_daily <- reload_gas_data()
#   elec_emissions_daily <- reload_elec_data()
# 
# 
#   utilities_daily <- elec_emissions_daily %>%
#     full_join(gas_emissions_daily,by = c("adate", "month", "year", "week")) %>%
#     mutate(gas_usage = replace_na(gas_usage,0),
#            gas_emissions = replace_na(gas_emissions,0)) %>%
#     rename(day = adate) %>%
#     select(day,gas_emissions,gas_usage,elec_emissions,elec_usage) %>%
#     pivot_longer(cols = c("elec_usage","gas_usage","elec_emissions","gas_emissions")) %>%
#     mutate(month = (format(lubridate::floor_date(day,"day"),"%Y-%m")),
#            year = (format(lubridate::floor_date(day,"day"),"%Y")),
#            week = format(lubridate::floor_date(day,"day"),"%Y-%W"))
# 
#   return(utilities_daily)
# 
# }
reload_elec_data <- function() {
  
  minquery <- str_c("select time,utility,value from utilityusage.utilityrecordings_smartthings where 
                    utility = 'energy'
                    AND time >= (CURRENT_DATE - 5)")
  #startd <- today() - 5
  
  minute_elec_day <- dbGetQuery(pgconn,minquery) %>%
    as_tibble() %>%
    mutate(timecut = cut(.$time, breaks = "5 min",right=FALSE)) %>%
    #filter(time < today() & time > ymd(20200715)) %>%
    #filter(time > startd) %>%
    group_by(timecut) %>%
    summarise(value=max(value)) %>%
    #rename(minute = `lubridate::minute(time)`) %>%
    mutate(unaccum = across(value, ~ .-c(0,lag(.)[-1]))) %>%
    mutate(unaccum = .$unaccum$value) %>%
    mutate(time = as.POSIXlt(timecut,format='%F %X',tz="Europe/London") - 2.5*60) %>%
    mutate(day = as.Date(substr(as.character(.$time),1,10),format="%F")) %>%
    #filter(time >= dayfilt & time <= dayfilt+1) %>%
    slice(-1) #remove the first value
  
  return(minute_elec_day)
  
}
# reload_lines_daily <- function() {
#   
#   query <- "select 
# traveltype_superclass as name,superclass_colourv as colourv,
# to_char(start_time_utc,'YYYY-MM-DD') as day,
# sum(kg_all_co2e) as value,
# to_char(start_time_utc,'YYYY-WW') as week,
# to_char(start_time_utc,'YYYY-MM') as month,
# extract('year' from start_time_utc) as year
# from dougtracks.dougtracks_lines_emi_mv_nogeom
# group by traveltype_superclass,superclass_colourv,extract('year' from start_time_utc),day,week,month
# having sum(kg_all_co2e) > 0"
#   
#   travel_lines_daily <- dbGetQuery(pgconn,query)
#   
#   # travel_lines_daily <- travel_lines %>%
#   #   st_drop_geometry() %>%
#   #   mutate(day = lubridate::ceiling_date(start_time_utc,"day")) %>%
#   #   #filter(traveltype != 'Aircraft' & traveltype != 'None') %>%
#   #   group_by(traveltype_superclass,superclass_colourv,day) %>%
#   #   summarise(value = set_units(sum(kg_all_co2e,na.rm=TRUE),"kg")) %>%
#   #   ungroup() %>%
#   #   mutate(week = format(day,"%Y-%W"),
#   #          month = format(day,"%Y-%m"),
#   #          year = format(day,"%Y")) %>%
#   #   rename(name = traveltype_superclass,
#   #          colourv = superclass_colourv) %>%
#   #   filter(value > set_units(0,"kg"))
#   
#   return(travel_lines_daily)
#   
# }
# reload_daily_utilities <- function() {
#   
#   #travel_lines_daily <- reload_lines_daily()
#   
#   utilities_daily_4merge <- utilities_daily %>%
#     filter(name %in% c("gas_emissions","elec_emissions")) %>%
#     mutate(colourv = recode(name,"elec_emissions" = "#00999d",
#                             "gas_emissions" = "#880f07")) %>%
#     select(name,colourv,day,value,week,month,year)
#   
#   return(utilities_daily_4merge)
#   
# }
# reload_daily_emissions <- function() {
#   
#   #travel_lines_daily <- reload_lines_daily()
#   #utilities_daily_4merge <- reload_daily_utilities()
#   
#   #daily_emissions_data <- rbind(travel_lines_daily,utilities_daily_4merge)
#   
#   #travel_lines_daily <- reload_lines_daily()
#   
#   
#   return(daily_emissions_data)
#   
# }

#travel_lines <- reload_map_data()

query <- str_c("select 
               min(start_time_utc)::date as s, 
               (max(end_time_utc)::date + '30 days'::interval)::date as e,
               min(extract('year' from start_time_utc)::smallint) as s_yr,
               max(extract('year' from end_time_utc)::smallint) as e_yr
               from dougtracks.dougtracks_lines_emi_mv_nogeom")

choices_monthq <- dbGetQuery(pgconn,query)

choices_month <- format(seq.Date(from = choices_monthq[1,1],to=choices_monthq[1,2], by = "month"), "%b-%Y")

min_yr <- choices_monthq[1,3]
max_yr <- choices_monthq[1,4]


#gas_emissions_daily <- reload_gas_data()
#elec_emissions_daily <- reload_elec_data()
#utilities_daily <- reload_daily_data()

query <- str_c("select 
               min(start_of_bill_period)::date as s, 
               (max(end_of_bill_period)::date + '30 days'::interval)::date as e,
               min(extract('year' from start_of_bill_period)::smallint) as s_yr,
               max(extract('year' from end_of_bill_period)::smallint) as e_yr
               from utilityusage.electricityusage")


choices_monthq_uti <- dbGetQuery(pgconn,query)

choices_month_uti <- format(seq.Date(from = choices_monthq_uti[1,1],to=choices_monthq_uti[1,2], by = "month"), "%b-%Y")

min_yr_uti <- choices_monthq_uti[1,3]
max_yr_uti <- choices_monthq_uti[1,4]

#choices_month_uti <- format(seq.Date(from = as.Date(min(utilities_daily$day)),to=(as.Date(max(utilities_daily$day))+30), by = "month"), "%b-%Y")

# min_yr_uti <- as.numeric(min(format(utilities_daily$day,"%Y")))
# max_yr_uti <- as.numeric(max(format(utilities_daily$day,"%Y")))

#minute_elec_5min_day <- reload_5min_data()

#travel_lines_daily <- reload_lines_daily()
#daily_emissions_data <- reload_daily_emissions()
#utilities_daily_4merge <- reload_daily_utilities()


query <- str_c("select
               min(day)::date as s,
               (max(day)::date + '30 days'::interval)::date as e,
               min(year)::smallint as s_yr,
               max(year)::smallint as e_yr
               from emissions.emissions_daily")

choices_month_emiq <- dbGetQuery(pgconn,query)

choices_month_emi <- format(seq.Date(from = choices_month_emiq[1,1],to=choices_month_emiq[1,2], by = "month"), "%b-%Y")

min_yr_emi <- choices_month_emiq[1,3]
max_yr_emi <- choices_month_emiq[1,4]




# choices_month_emi <- format(seq.Date(from = min(as.Date(daily_emissions_data$day),na.rm=TRUE),to=(max(as.Date(daily_emissions_data$day),na.rm=TRUE)+30), by = "month"), "%b-%Y")
# 
# min_yr_emi <- as.numeric(min(format(as.Date(daily_emissions_data$day),"%Y"),na.rm=TRUE))
# max_yr_emi <- as.numeric(max(format(as.Date(daily_emissions_data$day),"%Y"),na.rm=TRUE))


ui <- navbarPage("Dougs Data",
                 tabPanel("Travel",
                          sidebarLayout(
                            sidebarPanel(
                              width=2,
                              checkboxGroupInput("lfilter", "Traveltype Filter",
                                           choices = c(
                                             "Walk" = "Walk",
                                             "Electric Skateboard" = "Electric_Skateboard",
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
                                             "Canoe",
                                             "Boat",
                                             "Car_Ferry",
                                             "Walk",
                                             "Electric_Skateboard",
                                             "Cycling",
                                             "Bus - Local",
                                             "Bus - Coach",
                                             "Electric_Car",
                                             "Car",
                                             "Train - Light",
                                             "Train - National",
                                             "Airtrack",
                                             "Helicopter",
                                             "Plane - Domestic",
                                             "Plane - Short Haul",
                                             "Plane - Long Haul",
                                             "Plane - International")
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
                 tabPanel("Hill Climbing",sidebarLayout(
                   sidebarPanel(
                     width=2),
                   mainPanel(width=10,
                             leafletOutput("myhillmap",height=400),)
                   )
                 )
                          )
#)


server <- function(input, output, session) {

  get_traveltypefilter <- function() {
    return(input$lfilter)
  }
  
  get_graphtype <- function() {
    return(input$graphtype)
  }
  
  get_daterange <- function() {
    startd <- as.POSIXct(str_c(input$dateslide[1],"-01"),format="%b-%Y-%d")
    endd <- as.POSIXct(timeLastDayInMonth(as.POSIXct(str_c(input$dateslide[2],"-01"),format="%b-%Y-%d")))
    
    val_ret <- c(startd,endd)
    
    return(val_ret)
  }
  
  get_daterange_uti <- function() {
    startd <- as.POSIXct(str_c(input$dateslide_uti[1],"-01"),format="%b-%Y-%d")
    endd <- as.POSIXct(timeLastDayInMonth(as.POSIXct(str_c(input$dateslide_uti[2],"-01"),format="%b-%Y-%d")))
    
    val_ret <- c(startd,endd)
    
    return(val_ret)
  }
  
  get_daterange_emi <- function() {
    startd <- as.POSIXct(str_c(input$dateslide_emi[1],"-01"),format="%b-%Y-%d")
    endd <- as.POSIXct(timeLastDayInMonth(as.POSIXct(str_c(input$dateslide_emi[2],"-01"),format="%b-%Y-%d")))
    
    val_ret <- c(startd,endd)
    
    return(val_ret)
  }
  
  get_emifilt <- function() {
    return(input$emifilt)
  }
  
  get_utilityfilt <- function() {
    return(input$utilityfilt)
  }
  
  get_usageoremi <- function() {
    return(input$usageoremissionsrdo)
  }
  
  # reload_mapdata <- eventReactive(input$updata,
  #                                {
  #   travel_lines <- reload_map_data()
  #   
  #   choices_month <- format(seq.Date(from = as.Date(min(travel_lines$start_time_utc)),to=(as.Date(max(travel_lines$end_time_utc))+30), by = "month"), "%b-%Y")
  #   
  #   min_yr <- as.numeric(min(format(travel_lines$start_time_utc,"%Y")))
  #   max_yr <- as.numeric(max(format(travel_lines$start_time_utc,"%Y")))
  #   
  # },ignoreNULL=FALSE)
  # 
  # reload_utidata <- eventReactive(input$updata_uti,
  #                                {
  #                                  print("reload emi")
  #                                  gas_emissions_daily <- reload_gas_data()
  #                                  elec_emissions_daily <- reload_elec_data()
  #                                  utilities_daily <- reload_daily_data()
  #                                  
  #                                  minute_elec_5min_day <- reload_5min_data()
  #                                  
  #                                  choices_month_uti <- format(seq.Date(from = as.Date(min(utilities_daily$day)),to=(as.Date(max(utilities_daily$day))+30), by = "month"), "%b-%Y")
  #                                  
  #                                  min_yr_uti <- as.numeric(min(format(utilities_daily$day,"%Y")))
  #                                  max_yr_uti <- as.numeric(max(format(utilities_daily$day,"%Y")))
  #                                  
  #                                  print("reload complete")
  #                                  
  #                                },ignoreNULL=FALSE)
  # 
  # reload_emidata <- eventReactive(input$updata_emi,
  #                                {
  #                                  print("reload emi")
  #                                  travel_lines_daily <- reload_lines_daily()
  #                                  
  #                                  utilities_daily_4merge <- reload_daily_utilities()
  #                                  print("reload complete")
  #                                },ignoreNULL=FALSE)
  
  rerun_filt <- eventReactive(input$rerun,
                              {
    get_traveltypefilter()
  },ignoreNULL=FALSE)
  
  rerun_graphtype <- eventReactive(input$rerun,
                                   {
    get_graphtype()
  },ignoreNULL=FALSE)
  
  rerun_daterange <- eventReactive(input$rerun,
                                   {
    get_daterange()
  },ignoreNULL=FALSE)
  
  rendermap <- observeEvent(input$rerun, 
                            {
    #print("render map")
    filtervs <- get_traveltypefilter()
  
    daterange <- get_daterange()
    #map_dta <- filter(travel_lines,traveltype %in% filtervs & start_time_utc >= daterange[1] & end_time_utc <= daterange[2])
    
    query <- str_c("select colourv,geom,traveltype,start_time_utc,time_taken,length2d_km,kg_all_co2e from dougtracks.dougtracks_lines_emi_mv_simple2
                   where start_time_utc >= to_date('",daterange[1],"','YYYY-MM-DD') and end_time_utc <= to_date('",daterange[2],"','YYYY-MM-DD')
and traveltype in (",str_c("'",filtervs,"'",collapse = ","),")")
    
    map_dta <- st_read(dsn=pgconn,query=query)
    
    popupsv <- paste0( "<b>Traveltype: </b>"
                       , map_dta$traveltype
                       ,"<br>"
                       ,"<b>Start Time (UTC): </b>"
                       ,map_dta$start_time_utc
                       ,"<br>"
                       ,"<b>Time Taken: </b>"
                       ,map_dta$time_taken
                       ,"<br>"
                       ,"<b>Length (km): </b>"
                       ,round(map_dta$length2d_km,2)
                       ,"<br>"
                       ,"<b>Emissions (kg co2e): </b>"
                       ,round(map_dta$kg_all_co2e,2)
    )
    
    #print(map_dta)
  leafletProxy("mymap") %>%
    clearShapes() %>%
    #removeShape("travel_lines_id") %>%
    addPolylines(
      data = map_dta,
      color =  ~colourv,
      popup = popupsv,
      group = "Tracks"
      #layerId = "travel_lines_id"
    )
  })
  
  output$weekmap <- renderPlotly(
    {
    
      #travel_lines <- reload_map_data_nogeom()
      
    filtervs <- rerun_filt()
    daterange <- rerun_daterange()
    #map_dta <- filter(travel_lines,traveltype %in% filtervs & start_time_utc >= daterange[1] & end_time_utc <= daterange[2])
    
    graph_type <- get_graphtype()
    
    query <- str_c("select traveltype_superclass as traveltype,superclass_colourv,week,isoyear,
sum(EXTRACT(epoch FROM duration)/3600) as time_taken,
sum(length2d_km) as length,
sum(scope_1) as scope_1,
sum(scope_2) as scope_2,
sum(scope_3) as scope_3,
sum(kg_all_co2e) as kg_all_co2e,
count(*)::int as count_journeys
from 
(select 
*,
end_time_utc - start_time_utc as duration,
extract('week' from start_time_utc) as week,
extract('isoyear' from start_time_utc) as isoyear
from dougtracks.dougtracks_lines_emi_mv_nogeom
where start_time_utc >= to_date('",daterange[1],"','YYYY-MM-DD') and end_time_utc <= to_date('",daterange[2],"','YYYY-MM-DD')
and traveltype in (",str_c("'",filtervs,"'",collapse = ","),")
) subq
group by traveltype_superclass,superclass_colourv,week,isoyear")
    
    travel_stats_week <- dbGetQuery(pgconn,query) %>%
      rename(`Travel Type` = traveltype)
    
  #By Week:
  # travel_stats_week <- map_dta %>%
  #   #st_drop_geometry() %>%
  #   mutate(week = format(lubridate::ceiling_date(start_time_utc,"day"),"%Y-%W")) %>%
  #   #filter(traveltype != 'Aircraft' & traveltype != 'None') %>%
  #   group_by(traveltype_superclass,superclass_colourv,week,year) %>%
  #   summarise(time_taken = set_units(set_units(sum(duration,na.rm=TRUE),"seconds"),"hours"),
  #             length = set_units(set_units(sum(length2d_km,na.rm=TRUE),"km"),"km"),
  #             scope_1 = set_units(sum(scope_1,na.rm=TRUE),"kg"),
  #             scope_2 = set_units(sum(scope_2,na.rm=TRUE),"kg"),
  #             scope_3 = set_units(sum(scope_3,na.rm=TRUE),"kg"),
  #             kg_all_co2e = set_units(sum(kg_all_co2e,na.rm=TRUE),"kg")
  #             ,count_journeys = n()) %>%
  #   ungroup() %>%
  #   mutate(week = as.numeric(str_sub(week,6,7))) %>%
  #   rename(`Travel Type` = traveltype_superclass)
  
  plot_week <- ggplot() + 
    geom_col(data=travel_stats_week,aes_string(x="week",y=graph_type,fill="`Travel Type`")) +
    facet_wrap(~isoyear,scales="fixed") +
    scale_fill_manual(
      values=travel_stats_week$superclass_colourv,
      breaks=travel_stats_week$`Travel Type`,
      labels=travel_stats_week$`Travel Type`
    ) +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      panel.grid.major.x=element_blank(),
      panel.grid.minor.x=element_blank(),
      #panel.grid.minor.y = element_line(colour = "grey"),
      panel.ontop = FALSE)
  
  pweek <- ggplotly(plot_week)
  })
  
  output$monthmap <- renderPlotly(
    {
    
      #travel_lines <- reload_map_data_nogeom()
      
    filtervs <- rerun_filt()
    daterange <- rerun_daterange()
    #map_dta <- filter(travel_lines,traveltype %in% filtervs & start_time_utc >= daterange[1] & end_time_utc <= daterange[2])
    
    query <- str_c("select traveltype_superclass as traveltype,superclass_colourv,month,year,
sum(EXTRACT(epoch FROM duration)/3600) as time_taken,
sum(length2d_km) as length,
sum(scope_1) as scope_1,
sum(scope_2) as scope_2,
sum(scope_3) as scope_3,
sum(kg_all_co2e) as kg_all_co2e,
count(*)::int as count_journeys
from 
(select 
*,
end_time_utc - start_time_utc as duration,
extract('month' from start_time_utc) as month
from dougtracks.dougtracks_lines_emi_mv_nogeom
where start_time_utc >= to_date('",daterange[1],"','YYYY-MM-DD') and end_time_utc <= to_date('",daterange[2],"','YYYY-MM-DD')
and traveltype in (",str_c("'",filtervs,"'",collapse = ","),")
) subq
group by traveltype_superclass,superclass_colourv,month,year")
    
    travel_stats_month <- dbGetQuery(pgconn,query) %>%
      rename(`Travel Type` = traveltype)
    
    graph_type <- get_graphtype()
    
    #By Week:
    # travel_stats_month <- map_dta %>%
    #   #st_drop_geometry() %>%
    #   mutate(month = format(lubridate::ceiling_date(start_time_utc,"day"),"%Y-%m")) %>%
    #   #filter(traveltype != 'Aircraft' & traveltype != 'None') %>%
    #   group_by(traveltype_superclass,superclass_colourv,month,year) %>%
    #   summarise(time_taken = set_units(set_units(sum(duration,na.rm=TRUE),"seconds"),"hours"),
    #             length = set_units(set_units(sum(length2d_km,na.rm=TRUE),"km"),"km"),
    #             scope_1 = set_units(sum(scope_1,na.rm=TRUE),"kg"),
    #             scope_2 = set_units(sum(scope_2,na.rm=TRUE),"kg"),
    #             scope_3 = set_units(sum(scope_3,na.rm=TRUE),"kg"),
    #             kg_all_co2e = set_units(sum(kg_all_co2e,na.rm=TRUE),"kg")
    #             ,count_journeys = n()) %>%
    #   ungroup() %>%
    #   mutate(month = as.numeric(str_sub(month,6,7))) %>%
    #   rename(`Travel Type` = traveltype_superclass)
    
    plot_month <- ggplot() + 
      geom_col(data=travel_stats_month,aes_string(x="month",y=graph_type,fill="`Travel Type`")) +
      theme(axis.text.x=element_text(angle = 90,vjust=0.5,hjust=0)) +
      facet_wrap(~year,scales="fixed") +
      scale_fill_manual(
        values=travel_stats_month$superclass_colourv,
        breaks=travel_stats_month$`Travel Type`,
        labels=travel_stats_month$`Travel Type`
      ) +
      scale_x_continuous(
        breaks = seq(1,12),
        labels= levels(factor(seq(1,12)))
        #limits=seq(1,12)) 
        )+ 
      theme(
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        #panel.grid.minor.y = element_line(colour = "grey"),
        panel.ontop = FALSE)
    
    
    pmonth <- ggplotly(plot_month)
  })
  
  output$annualmap <- renderPlotly(
    {
    
      #travel_lines <- reload_map_data_nogeom()
      
    filtervs <- rerun_filt()
    daterange <- rerun_daterange()
    #map_dta <- filter(travel_lines,traveltype %in% filtervs & start_time_utc >= daterange[1] & end_time_utc <= daterange[2])
    
    
    graph_type <- get_graphtype()
    
    query <- str_c("select traveltype_superclass as traveltype,superclass_colourv,year,
sum(EXTRACT(epoch FROM duration)/3600) as time_taken,
sum(length2d_km) as length,
sum(scope_1) as scope_1,
sum(scope_2) as scope_2,
sum(scope_3) as scope_3,
sum(kg_all_co2e) as kg_all_co2e,
count(*)::int as count_journeys
from 
(select 
*,
end_time_utc - start_time_utc as duration
from dougtracks.dougtracks_lines_emi_mv_nogeom
where start_time_utc >= to_date('",daterange[1],"','YYYY-MM-DD') and end_time_utc <= to_date('",daterange[2],"','YYYY-MM-DD')
and traveltype in (",str_c("'",filtervs,"'",collapse = ","),")
) subq
group by traveltype_superclass,superclass_colourv,year")
    
    travel_stats_year <- dbGetQuery(pgconn,query) %>%
      rename(`Travel Type` = traveltype)
    
    #By Year:
    # travel_stats_year <- map_dta %>%
    #   #st_drop_geometry() %>%
    #   mutate(year = format(lubridate::ceiling_date(start_time_utc,"day"),"%Y")) %>%
    #   #filter(traveltype != 'Aircraft' & traveltype != 'None') %>%
    #   group_by(traveltype_superclass,superclass_colourv,year) %>%
    #   summarise(time_taken = set_units(set_units(sum(duration,na.rm=TRUE),"seconds"),"hours"),
    #             length = set_units(set_units(sum(length2d_km,na.rm=TRUE),"km"),"km"),
    #             scope_1 = set_units(sum(scope_1,na.rm=TRUE),"kg"),
    #             scope_2 = set_units(sum(scope_2,na.rm=TRUE),"kg"),
    #             scope_3 = set_units(sum(scope_3,na.rm=TRUE),"kg"),
    #             kg_all_co2e = set_units(sum(kg_all_co2e,na.rm=TRUE),"kg")
    #             ,count_journeys = n()) %>%
    #   ungroup() %>%
    #   mutate(year = as.numeric(str_sub(year,1,4))) %>%
    #   rename(`Travel Type` = traveltype_superclass)
    
    plot_year <- ggplot() + 
      geom_col(data=travel_stats_year,aes_string(x="year",y=graph_type,fill="`Travel Type`")) +
      #theme(axis.text.x=element_text(angle = 90,vjust=0.5,hjust=0)) +
      #facet_wrap(~year,scales="fixed") +
      scale_fill_manual(
        values=travel_stats_year$superclass_colourv,
        breaks=travel_stats_year$`Travel Type`,
        labels=travel_stats_year$`Travel Type`
      ) + 
      scale_x_continuous(
        breaks = seq(min_yr,max_yr),
        labels= levels(factor(seq(min_yr,max_yr)))
        #limits=seq(1,12)) 
      ) + 
      theme(
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        #panel.grid.minor.y = element_line(colour = "grey"),
        panel.ontop = FALSE)
    
    
    pyear <- ggplotly(plot_year)
  })
  
  output$mymap <- renderLeaflet(
    {
      
      #filtervs <- get_traveltypefilter()
      
      #daterange <- get_daterange()
      #map_dta <- filter(travel_lines,traveltype %in% filtervs & start_time_utc >= daterange[1] & end_time_utc <= daterange[2])
      
      query <- str_c("select colourv,geom,traveltype,start_time_utc,time_taken,length2d_km,kg_all_co2e from dougtracks.dougtracks_lines_emi_mv_simple2")
      
      map_dta <- st_read(dsn=pgconn,query=query)
      
      popupsv <- paste0( "<b>Traveltype: </b>"
                         , map_dta$traveltype
                         ,"<br>"
                         ,"<b>Start Time (UTC): </b>"
                         ,map_dta$start_time_utc
                         ,"<br>"
                         ,"<b>Time Taken: </b>"
                         ,map_dta$time_taken
                         ,"<br>"
                         ,"<b>Length (km): </b>"
                         ,round(map_dta$length2d_km,2)
                         ,"<br>"
                         ,"<b>Emissions (kg co2e): </b>"
                         ,round(map_dta$kg_all_co2e,2)
      )
      
      #travel_lines <- reload_map_data()
    
    #print(map_dta)
    map <- leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery,group="Imagery (ESRI)") %>%
      addPolylines(
        data = map_dta,
        color =  ~ colourv,
        popup = popupsv,
        group = "Tracks"
        #layerId = "travel_lines_id"
      ) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Imagery (ESRI)"),
        overlayGroups = c("Tracks"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    })
  
  output$annualmap_uti <- renderPlotly(
    {
      
      #elec_emissions_daily <- reload_elec_data()
      #gas_emissions_daily <- reload_gas_data()
      
    daterange_uti <- get_daterange_uti()
    emiuseuti <- get_usageoremi()
    filtuti <- get_utilityfilt()
    
    filt <- str_c(filtuti, "_", emiuseuti)
    
    if (emiuseuti == "usage") grapht <- "dodge" else grapht <- "stack"
    
    # elec_emissions_annual <- elec_emissions_daily %>%
    #   filter(adate >= daterange_uti[1] &
    #            adate <= daterange_uti[2]) %>%
    #   group_by(year) %>%
    #   summarise(elec_emissions = sum(elec_emissions),
    #             elec_usage = sum(elec_usage))
    # 
    # gas_emissions_annual <- gas_emissions_daily %>%
    #   filter(adate >= daterange_uti[1] &
    #            adate <= daterange_uti[2]) %>%
    #   group_by(year) %>%
    #   summarise(gas_emissions = sum(gas_emissions),
    #             gas_usage = sum(gas_usage))
    
    query <- str_c("select year,name,sum(value) as value from utilityusage.dailyusage_emissions
    
                   where day >= to_date('",daterange_uti[1],"','YYYY-MM-DD') and day <= to_date('",daterange_uti[2],"','YYYY-MM-DD') and 
                   name in ('",str_c(filt,collapse="','"),"')
                   group by year,name
                   order by year ASC")
    
    dat4plot <- dbGetQuery(pgconn,query) %>%
      mutate(name = factor(name))
    
    # utilities_annual <- elec_emissions_annual %>%
    #   full_join(gas_emissions_annual) %>%
    #   mutate(gas_usage = replace_na(gas_usage, 0),
    #          gas_emissions = replace_na(gas_emissions, 0)) %>%
    #   pivot_longer(cols = c(
    #     "elec_usage",
    #     "gas_usage",
    #     "elec_emissions",
    #     "gas_emissions"
    #   )) %>%
    #   mutate(name = factor(name))
    
    #dat4plot <- filter(utilities_annual, name %in% c(filt))
    
    plot_year_uti <- ggplot() +
      geom_col(position = grapht,
               data = dat4plot,
               aes(x = year, y = value, fill = name)) +
      #theme(axis.ticks.x=element_blank()) +
      # theme(axis.text.x = element_text(
      #   angle = 90,
      #   vjust = 0.5,
      #   hjust = 1
      # ))+
      ylab(emiuseuti)  +
      #scale_fill_manual(values = c("#b53737","#7ef9ff"))
      scale_x_continuous(
        breaks = seq(min_yr_uti,max_yr_uti),
        labels= levels(factor(seq(min_yr_uti,max_yr_uti))))
    
    pyear_uti <- ggplotly(plot_year_uti)
  })
  
  output$monthmap_uti <- renderPlotly(
    {
    
      #elec_emissions_daily <- reload_elec_data()
      #gas_emissions_daily <- reload_gas_data()
      
    daterange_uti <- get_daterange_uti()
    emiuseuti <- get_usageoremi()
    filtuti <- get_utilityfilt()
    
    filt <- str_c(filtuti, "_", emiuseuti)
    
    if (emiuseuti == "usage") grapht <- "dodge" else grapht <- "stack"
    
    # elec_emissions_monthly <- elec_emissions_daily %>%
    #   filter(adate >= daterange_uti[1] &
    #            adate <= daterange_uti[2]) %>%
    #   group_by(month) %>%
    #   summarise(elec_emissions = sum(elec_emissions),
    #             elec_usage = sum(elec_usage))
    # 
    # gas_emissions_monthly <- gas_emissions_daily %>%
    #   filter(adate >= daterange_uti[1] &
    #            adate <= daterange_uti[2]) %>%
    #   group_by(month) %>%
    #   summarise(gas_emissions = sum(gas_emissions),
    #             gas_usage = sum(gas_usage))
    # 
    # utilities_monthly <- elec_emissions_monthly %>%
    #   full_join(gas_emissions_monthly) %>%
    #   mutate(gas_usage = replace_na(gas_usage,0),
    #          gas_emissions = replace_na(gas_emissions,0)) %>%
    #   pivot_longer(cols = c("elec_usage","gas_usage","elec_emissions","gas_emissions")) %>%
    #   mutate(year = substr(month,1,4),
    #          month = as.numeric(substr(month,6,7)),
    #          name = factor(name))
    # 
    # dat4plot <- filter(utilities_monthly, name %in% c(filt))
    
    query <- str_c("select year,month,name,sum(value) as value from utilityusage.dailyusage_emissions
    
                   where day >= to_date('",daterange_uti[1],"','YYYY-MM-DD') and day <= to_date('",daterange_uti[2],"','YYYY-MM-DD') and 
                   name in ('",str_c(filt,collapse="','"),"')
                   group by year,month,name
                   order by year,month ASC")
    
    dat4plot <- dbGetQuery(pgconn,query) %>%
      mutate(name = factor(name))
    
    
    plot_month_uti <- ggplot() +
      geom_col(
        position = grapht,
        data = dat4plot,
        aes(x = month, y = value, fill = name)
      ) +
      theme(axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )) +
      facet_wrap( ~ year) +
      ylab(emiuseuti) +
      scale_x_continuous(
        breaks = seq(1,12),
        labels= levels(factor(seq(1,12)))
        #limits=seq(1,12)) 
      )
    
    pmonth_uti <- ggplotly(plot_month_uti)
  })
  
  output$weekmap_uti <- renderPlotly(
    {
    
      #elec_emissions_daily <- reload_elec_data()
      #gas_emissions_daily <- reload_gas_data()
      
    daterange_uti <- get_daterange_uti()
    emiuseuti <- get_usageoremi()
    filtuti <- get_utilityfilt()
    
    filt <- str_c(filtuti, "_", emiuseuti)
    
    if (emiuseuti == "usage") grapht <- "dodge" else grapht <- "stack"
    
    
    # elec_emissions_weekly <- elec_emissions_daily %>%
    #   filter(adate >= daterange_uti[1] &
    #            adate <= daterange_uti[2]) %>%
    #   group_by(week) %>%
    #   summarise(elec_emissions = sum(elec_emissions),
    #             elec_usage = sum(elec_usage))
    # 
    # gas_emissions_weekly <- gas_emissions_daily %>%
    #   filter(adate >= daterange_uti[1] &
    #            adate <= daterange_uti[2]) %>%
    #   group_by(week) %>%
    #   summarise(gas_emissions = sum(gas_emissions),
    #             gas_usage = sum(gas_usage))
    # 
    # utilities_weekly <-  elec_emissions_weekly %>%
    #   full_join(gas_emissions_weekly) %>%
    #   mutate(gas_usage = replace_na(gas_usage,0),
    #          gas_emissions = replace_na(gas_emissions,0)) %>%
    #   pivot_longer(cols = c("elec_usage","gas_usage","elec_emissions","gas_emissions")) %>%
    #   mutate(year = substr(week,1,4),
    #          week = (substr(week,6,7)),
    #          name = factor(name))
    # 
    # dat4plot <- filter(utilities_weekly, name %in% c(filt))
    
    query <- str_c("select 
    extract('week' from day) as isoweek,
    extract('isoyear' from day) as isoyear,
    name,sum(value) as value from utilityusage.dailyusage_emissions
    
                   where day >= to_date('",daterange_uti[1],"','YYYY-MM-DD') and day <= to_date('",daterange_uti[2],"','YYYY-MM-DD') and 
                   name in ('",str_c(filt,collapse="','"),"')
                   group by isoyear,isoweek,name
                   order by isoyear,isoweek ASC")
    
    dat4plot <- dbGetQuery(pgconn,query) %>%
      mutate(name = factor(name))
    
    plot_week_uti <- ggplot() +
      geom_col(
        position = grapht,
        data = dat4plot,
        aes(x = isoweek, y = value, fill = name)
      ) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      facet_wrap( ~ isoyear) +
      ylab(emiuseuti)
    
    pweek_uti <- ggplotly(plot_week_uti)
  })
  
  output$daily_week_uti <- renderPlotly(
    {
    
    minute_elec_day <- reload_elec_data()
      
    plot5min <- ggplot() + 
      geom_col(data=minute_elec_day,aes(time,unaccum)) +
      facet_wrap(~day,scales="free_x") +
      theme(axis.text.x=element_text(angle = 30,vjust=1,hjust=1)) +
      ylab("kWh over 5 min intervals") + 
      xlab("time") +
      scale_x_datetime(labels = time_format(format="%H:%M",tz="Europe/London"))
    
    plot5minplotly <- ggplotly(plot5min)
  })
  
  output$weekmap_emi <- renderPlotly(
    {
    
      #travel_lines_daily <- reload_lines_daily()
      #utilities_daily_4merge <- reload_daily_utilities()
      
    daterange_emi <- get_daterange_emi()
    emifilt <- get_emifilt()
    
    # daily_emissions_data <- rbind(travel_lines_daily,utilities_daily_4merge) %>%
    #   filter(as.Date(day) >= daterange_emi[1] &
    #            as.Date(day) <= daterange_emi[2] &
    #            name %in% emifilt)
    # 
    # weekly_emissions_data <- daily_emissions_data %>%
    #   group_by(name,colourv,week,year) %>%
    #   summarise(value = sum(value)) %>%
    #   mutate(week = substr(week,6,7))
    
    query <- str_c("select name,colourv,
    extract('week' from day) as isoweek,
    extract('isoyear' from day) as isoyear,
    sum(value) as value from emissions.emissions_daily 
    where day >= to_date('",daterange_emi[1],"','YYYY-MM-DD') and day <= to_date('",daterange_emi[2],"','YYYY-MM-DD') and 
                   name in ('",str_c(emifilt,collapse="','"),"')
                   group by isoyear,isoweek,name,colourv
                   order by isoyear,isoweek ASC"
                   )
    
    weekly_emissions_data <- dbGetQuery(pgconn,query)
    
    plot_week_emi <- ggplot() +
      geom_col(
        position = "stack",
        data = weekly_emissions_data,
        aes(x = isoweek, y = value, fill = name)
      ) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x=element_blank()) +
      facet_wrap( ~ isoyear) + 
      scale_fill_manual(
        values=weekly_emissions_data$colourv,
        breaks=weekly_emissions_data$name,
        labels=weekly_emissions_data$name
      )
    
    pweek_emi <- ggplotly(plot_week_emi)
    
  })
  
  output$monthmap_emi <- renderPlotly(
    {
    
      #travel_lines_daily <- reload_lines_daily()
      #utilities_daily_4merge <- reload_daily_utilities()
      
    daterange_emi <- get_daterange_emi()
    emifilt <- get_emifilt()
    
    # daily_emissions_data <- rbind(travel_lines_daily,utilities_daily_4merge) %>%
    #   filter(as.Date(day) >= daterange_emi[1] &
    #            as.Date(day) <= daterange_emi[2] &
    #            name %in% emifilt)
    # 
    # monthly_emissions_data <- daily_emissions_data %>%
    #   group_by(name,colourv,month,year) %>%
    #   summarise(value = sum(value)) %>%
    #   mutate(month = as.numeric(substr(month,6,7)))
    
    query <- str_c("select name,colourv,month,year,sum(value) as value from emissions.emissions_daily 
    where day >= to_date('",daterange_emi[1],"','YYYY-MM-DD') and day <= to_date('",daterange_emi[2],"','YYYY-MM-DD') and 
                   name in ('",str_c(emifilt,collapse="','"),"')
                   group by year,month,name,colourv
                   order by year,month ASC"
    )
    
    monthly_emissions_data <- dbGetQuery(pgconn,query)
    
    
    
    plot_month_emi <- ggplot() +
      geom_col(
        position = "stack",
        data = monthly_emissions_data,
        aes(x = month, y = value, fill = name)
      ) +
      theme(axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )) +
      #theme(axis.text.x = element_blank()) +
      facet_wrap( ~ year) + 
      scale_fill_manual(
        values=monthly_emissions_data$colourv,
        breaks=monthly_emissions_data$name,
        labels=monthly_emissions_data$name
      ) +
      scale_x_continuous(
        breaks = seq(1,12),
        labels= levels(factor(seq(1,12)))
        #limits=seq(1,12)) 
      )
    
    pmonth_emi <- ggplotly(plot_month_emi)
    
  })
  
  output$annualmap_emi <- renderPlotly(
    {
    
      #travel_lines_daily <- reload_lines_daily()
      #utilities_daily_4merge <- reload_daily_utilities()
      
    daterange_emi <- get_daterange_emi()
    emifilt <- get_emifilt()
    
    # daily_emissions_data <- rbind(travel_lines_daily,utilities_daily_4merge) %>%
    #   filter(as.Date(day) >= daterange_emi[1] &
    #            as.Date(day) <= daterange_emi[2] &
    #            name %in% emifilt)
    # 
    # annual_emissions_data <- daily_emissions_data %>%
    #   group_by(name,colourv,year) %>%
    #   summarise(value = sum(value)) 
    
    query <- str_c("select name,colourv,year,sum(value) as value from emissions.emissions_daily 
    where day >= to_date('",daterange_emi[1],"','YYYY-MM-DD') and day <= to_date('",daterange_emi[2],"','YYYY-MM-DD') and 
                   name in ('",str_c(emifilt,collapse="','"),"')
                   group by year,name,colourv
                   order by year ASC"
    )
    
    annual_emissions_data <- dbGetQuery(pgconn,query)
    
    
    plot_year_emi <- ggplot() +
      geom_col(
        position = "stack",
        data = annual_emissions_data,
        aes(x = year, y = value, fill = name)
      ) +
      #theme(axis.text.x = element_blank()) +
      #facet_wrap( ~ year) + 
      scale_fill_manual(
        values=annual_emissions_data$colourv,
        breaks=annual_emissions_data$name,
        labels=annual_emissions_data$name
      ) + 
      scale_x_continuous(
        breaks = seq(min_yr_emi,max_yr_emi),
        labels= levels(factor(seq(min_yr_emi,max_yr_emi))))
    
    pyear_emi <- ggplotly(plot_year_emi)
    
  })
  
}

shinyApp(ui, server)




