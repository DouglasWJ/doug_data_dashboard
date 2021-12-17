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
library(leafgl)


units_options(group = c("(", ")") )

pgconn <-
  dbConnect(
    RPostgres::Postgres(),
    dbname = "postgres",
    host = "localhost",
    port = 5432,
    user = "u0_a312"
  )
dbExecute(pgconn,paste0("SET search_path = dougtracks,emissions,utilityusage,dobih, public"))


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
                    utility = 'energy' AND
                    time >= (CURRENT_DATE - 2)")
  #startd <- today() - 5
  #
  
  minute_elec_day <- dbGetQuery(pgconn,minquery) %>%
    as_tibble() %>%
    mutate(timecut = cut(.$time, breaks = "5 min",right=FALSE)) %>%
    #filter(time < today() & time > ymd(20200715)) %>%
    #filter(time > startd) %>%
    group_by(timecut) %>%
    summarise(value=max(value)) %>%
    #rename(minute = `lubridate::minute(time)`) %>%
    mutate(unaccum = across(.cols=c(value), ~ .-c(0,lag(.)[-1]))) %>%
    mutate(unaccum = .$unaccum$value) %>%
    mutate(time = as.POSIXlt(timecut,format='%F %X',tz="Etc/GMT") - 2.5*60) %>%
    mutate(day = as.Date(substr(as.character(.$time),1,10),format="%F")) %>%
    mutate(colourv = '#00999d') %>%
    #filter(time >= dayfilt & time <= dayfilt+1) %>%
    slice(-1) #remove the first value
  
  return(minute_elec_day)
  
}

reload_gas_data <- function() {
  
  minquery <- str_c("select time,utility,value from utilityusage.utilityrecordings_smartthings where 
                    time >= (CURRENT_DATE - 2)")
  #startd <- today() - 5
  #utility = 'gasMeter' AND
  
  minute_gas_day <- dbGetQuery(pgconn,minquery) %>%
    as_tibble() %>%
    mutate(timecut = cut(.$time, breaks = "30 min",right=FALSE)) %>%
    mutate(value2 = if_else(utility == 'gasMeter',value,0)) %>%
    #filter(time < today() & time > ymd(20200715)) %>%
    #filter(time > startd) %>%
    group_by(timecut) %>%
    summarise(value=max(value2)) %>%
    #rename(minute = `lubridate::minute(time)`) %>%
    mutate(unaccum = across(.cols=c(value), ~ .-c(0,lag(.)[-1]))) %>%
    mutate(unaccum = .$unaccum$value) %>%
    mutate(unaccum = if_else(value == unaccum,0,unaccum)) %>%
    mutate(unaccum = if_else(unaccum <= 0,0,unaccum)) %>%
    mutate(time = as.POSIXlt(timecut,format='%F %X',tz="Etc/GMT") - 15*60) %>%
    mutate(day = as.Date(substr(as.character(.$time),1,10),format="%F")) %>%
    mutate(colourv = '#880f07') %>%
    #filter(time >= dayfilt & time <= dayfilt+1) %>%
    slice(-1) #remove the first value
  
  return(minute_gas_day)
  
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

query <- str_c("select 
               min(climbed)::date as s,
               (max(climbed)::date + '30 days'::interval)::date as e,
               min(extract(year from climbed)) as s_yr,
               max(extract(year from climbed)) as e_yr
               from dobih.userlog")

choice_month_hilq <- dbGetQuery(pgconn,query)

choices_month_hil <- format(seq.Date(from = choice_month_hilq[1,1],to=choice_month_hilq[1,2], by = "month"), "%b-%Y")

min_yr_hil <- choice_month_hilq[1,3]
max_yr_hil <- choice_month_hilq[1,4]

travel_map_types <- c("Time (hours)" = "time_taken",
  "Distance (km)" = "length",
  "Emissions (kg CO2e)" = "kg_all_co2e",
  "Count of Journeys" = "count_journeys")

emissions_graph_types <- c("Usage" = "usage",
  "Emissions" = "emissions")

emissions_filters_types <- c("Aircraft" = "Aircraft",
                             "Boat" = "Boat",
                             "Bus" = "Bus",
                             "Car" = "Car",
                             "Train" = "Train",
                             "Gas" = "gas_emissions",
                             "Electricity" = "elec_emissions"
)

emissions_filters_types_df <- rownames_to_column(data.frame(t(data.frame(as.list(emissions_filters_types)))))
names(emissions_filters_types_df) <- c("displayname","name")

