library(shiny)

options(shiny.host = "0.0.0.0")
options(shiny.port = 7322)
options(scipen = 999)
options(bitmapType='cairo')

function(input, output, session) {
  
  output$tbl <- renderTable(spacing="xs",striped=TRUE,bordered=TRUE,expr={
    dbGetQuery(pgconn,"select REPLACE(REPLACE(classname,' hill',''),' (all)','') as class,total::integer,cnt::integer as climb ,perc 
               from 
               dobih.climbed_summary 
               where 
               classname in ('Tump (all)','Munro','Corbett','Graham','Highland Five','Donald Dewey','400-499m hill','300-399m hill','200-299m hill','100-199m hill','0-99m hill')
               ORDER BY cnt DESC"
    )})
  
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
  
  get_daterange_hil <- function() {
    startd <- as.POSIXct(str_c(input$dateslide_hil[1],"-01"),format="%b-%Y-%d")
    endd <- as.POSIXct(timeLastDayInMonth(as.POSIXct(str_c(input$dateslide_hil[2],"-01"),format="%b-%Y-%d")))
    
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
  
  get_hilltypes <- function() {
    return(input$hills)
  }
  
  get_altfilt <- function() {
    return(input$haltitude)
  }
  
  get_dropfilt <- function() {
    return(input$hdrop)
  }
  
  get_climbedyn <- function() {
    return(input$climbed)
  }
  
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
                              
                              # popupsv <- paste0( "<b>Traveltype: </b>"
                              #                    , map_dta$traveltype
                              #                    ,"<br>"
                              #                    ,"<b>Start Time (UTC): </b>"
                              #                    ,map_dta$start_time_utc
                              #                    ,"<br>"
                              #                    ,"<b>Time Taken: </b>"
                              #                    ,map_dta$time_taken
                              #                    ,"<br>"
                              #                    ,"<b>Length (km): </b>"
                              #                    ,round(map_dta$length2d_km,2)
                              #                    ,"<br>"
                              #                    ,"<b>Emissions (kg co2e): </b>"
                              #                    ,round(map_dta$kg_all_co2e,2)
                              # )
                              
                              popupsv <- paste0("<table>",
                                                "<tr>",
                                                "<td>Traveltype: </td>",
                                                "<td>",map_dta$traveltype,"</td>",
                                                "</tr>",
                                                "<tr>",
                                                "<td>Start Time (UTC): </td>",
                                                "<td>",map_dta$start_time_utc,"</td>",
                                                "</tr>",
                                                "<tr>",
                                                "<td>Time Taken: </td>",
                                                "<td>",map_dta$time_taken,"</td>",
                                                "</tr>",
                                                "<tr>",
                                                "<td>Length (km): </td>",
                                                "<td>",round(map_dta$length2d_km,2),"</td>",
                                                "</tr>",
                                                "<tr>",
                                                "<td>Emissions (kg co2e): </td>",
                                                "<td>",round(map_dta$kg_all_co2e,2),"</td>",
                                                "</tr>",
                                                "</table>"
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
  
  output$myhillmap <- renderLeaflet(
    {
      
      #filtervs <- get_traveltypefilter()
      
      #daterange <- get_daterange()
      #map_dta <- filter(travel_lines,traveltype %in% filtervs & start_time_utc >= daterange[1] & end_time_utc <= daterange[2])
      
      query <- str_c("select hillname,classification,feature,metres,feet,drop,geom,first_asc,num_asc,COALESCE(color,'red') as color from (
select hillname,classification,feature,metres,feet,drop,geom,min(climbed) as first_asc,count(climbed) as num_asc,color 
from
(select hillnumber,feature,classification,hillname,metres,feet,drop,geom from dobih.hills where hillnumber IN (select hillnumber from dobih.classlink where classref = 'M')) a
full outer JOIN
(select *,'blue' as color from dobih.userlog where hillnumber IN (select hillnumber from dobih.classlink where classref = 'M')) b 
using (hillnumber)
group by hillname,feature,classification,metres,feet,drop,geom,color) subq
                     where num_asc > 0")
      
      map_dta <- st_read(dsn=pgconn,query=query)
      
      query2 <- str_c("select hillname,classification,feature,metres,feet,drop,geom,first_asc,num_asc,COALESCE(color,'red') as color from (
select hillname,classification,feature,metres,feet,drop,geom,min(climbed) as first_asc,count(climbed) as num_asc,color 
from
(select hillnumber,feature,classification,hillname,metres,feet,drop from dobih.hills where hillnumber IN (select hillnumber from dobih.classlink where classref = 'M')) a
full outer JOIN
(select *,'blue' as color from dobih.userlog_with_geom_mv where hillnumber IN (select hillnumber from dobih.classlink where classref = 'M')) b 
using (hillnumber)
group by hillname,feature,classification,metres,feet,drop,geom,color) subq
                     where num_asc > 0")
      map_dta2 <- st_zm(st_read(dsn=pgconn,query=query2))
      
      popupsv <- popupsv <- paste0("<h5>","<b>",map_dta$hillname,"</b>","</h5>",
                                   "<table>",
                                   "<tr>",
                                   "<td>Name:</td>",
                                   "<td>",map_dta$hillname,"</td>",
                                   "</tr>",
                                   "<tr>",
                                   "<td>Classification:</td>",
                                   "<td>",map_dta$classification,"</td>",
                                   "</tr>",
                                   "<tr>",
                                   "<td>Height (m):</td>",
                                   "<td>",map_dta$metres,"</td>",
                                   "</tr>",
                                   "<tr>",
                                   "<td>Height (ft):</td>",
                                   "<td>",map_dta$feet,"</td>",
                                   "</tr>",
                                   "<tr>",
                                   "<td>Feature:</td>",
                                   "<td>",map_dta$feature,"</td>",
                                   "</tr>",
                                   "<tr>",
                                   "<td>Drop (m):</td>",
                                   "<td>",map_dta$drop,"</td>",
                                   "</tr>",
                                   "<tr>",
                                   "<td>First Ascent: </td>",
                                   "<td>",map_dta$first_asc,"</td>",
                                   "</tr>",
                                   "<tr>",
                                   "<td>Num Ascents: </td>",
                                   "<td>",map_dta$num_asc,"</td>",
                                   "</tr>",
                                   "</table>"
      )
      
      
      #travel_lines <- reload_map_data()
      
      #print(map_dta)
      map <- leaflet() %>%
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$Esri.WorldImagery,group="Imagery (ESRI)") %>%
        addPolylines(data = map_dta2,
                     color = "brown",
                     group = "Tracks",
                     weight = 2,
                     stroke = TRUE,
                     fill = FALSE#,
                     #bringToFront = TRUE,
                     #sendToBack = FALSE
        ) %>%
        addCircles(
          data = map_dta,
          color =  ~ color,
          popup = popupsv,
          group = "Hills"
          #layerId = "travel_lines_id"
        ) %>%
        addLayersControl(
          baseGroups = c("OSM (default)", "Imagery (ESRI)"),
          overlayGroups = c("Hills","Tracks"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
    })
  
  rerenderhillmap <- observeEvent(input$rerunhill, 
                                  {
                                    #print("render map")
                                    filterhs <- get_hilltypes()
                                    
                                    daterange <- get_daterange_hil()
                                    
                                    altfilt <- get_altfilt()
                                    
                                    dropfilt <- get_dropfilt()
                                    
                                    climbed <- get_climbedyn()
                                    #print(climbed)
                                    
                                    if (is.null(climbed)) {
                                      ascq <- " where num_asc > 1000"
                                    } else if (all.equal(climbed,c("yes","no")) == TRUE) {
                                      ascq <- " where num_asc >= 0"
                                    } else if (climbed == "yes") {
                                      ascq <- " where num_asc > 0"
                                    } else if (climbed == "no") {
                                      ascq <- " where num_asc = 0"
                                    }
                                    
                                    altq <- str_c(" and metres between ",altfilt[1]," and ",altfilt[2])
                                    
                                    dropq <- str_c(" and drop between ",dropfilt[1]," and ",dropfilt[2])
                                    
                                    
                                    query <- str_c(
                                    "select * from (
select hillname,classification,feature,metres,feet,drop,geom,min(climbed) as first_asc,count(climbed) as num_asc,COALESCE(color,'red') as color
from
(select hillnumber,classification,feature,hillname,metres,feet,drop,geom from dobih.hills where hillnumber IN (select hillnumber from dobih.classlink where classref IN ('",str_c(filterhs,collapse="','"),"'))) a
full outer JOIN
(select *,'blue' as color from dobih.userlog where hillnumber IN (select hillnumber from dobih.classlink where classref IN ('",str_c(filterhs,collapse="','"),"')) AND climbed between to_date('",daterange[1],"','YYYY-MM-DD') and to_date('",daterange[2],"','YYYY-MM-DD')) b 
using (hillnumber)
group by hillname,feature,classification,metres,feet,drop,geom,color) subq",ascq,altq,dropq)
                                    
                                    
                                    map_dta <- st_read(dsn=pgconn,query=query)
                                    
                                    query2 <- str_c(
                                      "select * from (
select hillname,classification,feature,metres,feet,drop,geom,min(climbed) as first_asc,count(climbed) as num_asc,COALESCE(color,'red') as color
from
(select hillnumber,classification,feature,hillname,metres,feet,drop from dobih.hills where hillnumber IN (select hillnumber from dobih.classlink where classref IN ('",str_c(filterhs,collapse="','"),"'))) a
INNER JOIN
(select *,'blue' as color from dobih.userlog_with_geom_mv where hillnumber IN (select hillnumber from dobih.classlink where classref IN ('",str_c(filterhs,collapse="','"),"')) AND climbed between to_date('",daterange[1],"','YYYY-MM-DD') and to_date('",daterange[2],"','YYYY-MM-DD')) b 
using (hillnumber)
group by hillname,feature,classification,metres,feet,drop,geom,color) subq",ascq,altq,dropq)
                                    
                                    map_dta2 <- st_zm(st_read(dsn=pgconn,query=query2))
                                    
                                    
                                    # popupsv <- paste0( "<b>Hill Name: </b>"
                                    #                    , map_dta$hillname
                                    #                    ,"<br>"
                                    #                    ,"<b>Classification(s):  </b>"
                                    #                    ,map_dta$classification
                                    #                    ,"<br>"
                                    #                    ,"<b>Altitude (m): </b>"
                                    #                    ,map_dta$metres
                                    #                    ,"<br>"
                                    #                    ,"<b>Altitude (feet): </b>"
                                    #                    ,map_dta$feet
                                    #                    ,"<br>"
                                    #                    ,"<b>Drop (m): </b>"
                                    #                    ,map_dta$drop
                                    #                    ,"<br>"
                                    #                    ,"<b>First Ascent: </b>"
                                    #                    ,map_dta$first_asc
                                    #                    ,"<br>"
                                    #                    ,"<b>Number of Ascents: </b>"
                                    #                    ,map_dta$num_asc
                                    # )
                                    
                                    popupsv <- paste0("<h5>","<b>",map_dta$hillname,"</b>","</h5>",
                                                      "<table>",
                                                      "<tr>",
                                                      "<td>Name:</td>",
                                                      "<td>",map_dta$hillname,"</td>",
                                                      "</tr>",
                                                      "<tr>",
                                                      "<td>Classification:</td>",
                                                      "<td>",map_dta$classification,"</td>",
                                                      "</tr>",
                                                      "<tr>",
                                                      "<td>Height (m):</td>",
                                                      "<td>",map_dta$metres,"</td>",
                                                      "</tr>",
                                                      "<tr>",
                                                      "<td>Height (ft):</td>",
                                                      "<td>",map_dta$feet,"</td>",
                                                      "</tr>",
                                                      "<tr>",
                                                      "<td>Feature:</td>",
                                                      "<td>",map_dta$feature,"</td>",
                                                      "</tr>",
                                                      "<tr>",
                                                      "<td>Drop (m):</td>",
                                                      "<td>",map_dta$drop,"</td>",
                                                      "</tr>",
                                                      "<tr>",
                                                      "<td>First Ascent: </td>",
                                                      "<td>",map_dta$first_asc,"</td>",
                                                      "</tr>",
                                                      "<tr>",
                                                      "<td>Num Ascents: </td>",
                                                      "<td>",map_dta$num_asc,"</td>",
                                                      "</tr>",
                                                      "</table>"
                                    )
                                    
                                    #print(map_dta)
                                    leafletProxy("myhillmap") %>%
                                      clearShapes() %>%
                                      addPolylines(data = map_dta2,
                                                   color = "brown",
                                                   group = "Tracks",
                                                   weight = 2,
                                                   stroke = TRUE,
                                                   fill = FALSE#,
                                                   #bringToFront = TRUE,
                                                   #sendToBack = FALSE
                                      ) %>%
                                      addCircles(
                                        data = map_dta,
                                        color =  ~color,
                                        popup = popupsv,
                                        group = "Hills"
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
                     #where traveltype = 'Walk'")
      
      map_dta <- st_read(dsn=pgconn,query=query)
      
      popupsv <- paste0( "<table>",
                         "<tr>",
                         "<td>Traveltype: </td>",
                         "<td>",map_dta$traveltype,"</td>",
                         "</tr>",
                         "<tr>",
                         "<td>Start Time (UTC): </td>",
                         "<td>",map_dta$start_time_utc,"</td>",
                         "</tr>",
                         "<tr>",
                         "<td>Time Taken: </td>",
                         "<td>",map_dta$time_taken,"</td>",
                         "</tr>",
                         "<tr>",
                         "<td>Length (km): </td>",
                         "<td>",round(map_dta$length2d_km,2),"</td>",
                         "</tr>",
                         "<tr>",
                         "<td>Emissions (kg co2e): </td>",
                         "<td>",round(map_dta$kg_all_co2e,2),"</td>",
                         "</tr>",
                         "</table>"
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
        mutate(colourv = if_else(name == 'elec_usage' | name == 'elec_emissions','#00999d','#880f07')) %>%
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
          labels= levels(factor(seq(min_yr_uti,max_yr_uti)))) + 
        scale_fill_manual(
          values=dat4plot$colourv
          #breaks=dat4plot$name,
          #labels=dat4plot$name
        )
      
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
        mutate(colourv = if_else(name == 'elec_usage' | name == 'elec_emissions','#00999d','#880f07')) %>%
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
        ) + 
        scale_fill_manual(
          values=dat4plot$colourv,
          breaks=dat4plot$name,
          labels=dat4plot$name
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
        mutate(colourv = if_else(name == 'elec_usage' | name == 'elec_emissions','#00999d','#880f07')) %>%
        #mutate(colourv = if_else(name == 'elec_emissions','#00999d','#880f07')) %>%
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
        ylab(emiuseuti) +
        scale_fill_manual(
          values=dat4plot$colourv,
          breaks=dat4plot$name,
          labels=dat4plot$name
        )
      
      pweek_uti <- ggplotly(plot_week_uti)
    })
  
  output$daily_week_elec <- renderPlotly(
    {
      
      minute_elec_day <- reload_elec_data()
      
      plot5min <- ggplot() + 
        geom_col(data=minute_elec_day,aes(x=time,y=unaccum),fill='#00999d') +
        facet_wrap(~day,scales="free_x") +
        theme(axis.text.x=element_text(angle = 30,vjust=3,hjust=1),axis.title.y=element_blank(),axis.title.x=element_blank()) +
        #ylab("kWh over 5 min intervals") + 
        xlab("time") +
        scale_x_datetime(labels = time_format(format="%H:%M",tz="Europe/London")) +
        scale_y_continuous(labels = scales::number_format(accuracy=0.1)) 
      
      plot5minplotly <- ggplotly(plot5min)
    })
  
  output$daily_week_gas <- renderPlotly(
    {
      
      minute_gas_day <- reload_gas_data()
      
      plot5min <- ggplot() + 
        geom_col(data=minute_gas_day,aes(x=time,y=unaccum),fill='#880f07') +
        facet_wrap(~day,scales="free_x") +
        theme(axis.text.x=element_text(angle = 30,vjust=3,hjust=1),axis.title.y=element_blank(),axis.title.x=element_blank()) +
        #ylab("kWh over 30 min intervals") + 
        xlab("time") +
        scale_x_datetime(labels = time_format(format="%H:%M",tz="Europe/London")) +
        scale_y_continuous(labels = scales::number_format(accuracy=0.1))
      
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