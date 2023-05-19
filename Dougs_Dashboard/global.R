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
    dbname = "spatial_db",
    host = "192.168.1.126",
    port = 5433,
    user = "doug"
  )
dbExecute(pgconn,paste0("SET search_path = dougtracks,emissions,utilityusage,dobih, public"))


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
                             "Electricity" = "elec_emissions",
                             "Logs" = 'log_emissions'
)

emissions_filters_types_df <- rownames_to_column(data.frame(t(data.frame(as.list(emissions_filters_types)))))
names(emissions_filters_types_df) <- c("displayname","name")

