library(sf)
library(RPostgres)
library(RSQLite)
library(magrittr)

pgconn1 <-
  dbConnect(
    RPostgres::Postgres(),
    dbname = "spatial_db",
    host = "192.168.1.126",
    port = 5433,
    user = "doug"
  )
dbExecute(pgconn1,paste0("SET search_path = dougtracks,emissions,utilityusage,dobih, public"))



#define output Spatialite location and filename:
filename <- paste0("db_prelim.db")
file_output_location <- ""

file.remove(filename)

dsn_val <- paste0(file_output_location,filename)

#import Postgres table(s):
spatial_tables <- c("userlog_with_geom_mv","dougtracks_lines_emi_mv_simple2")
output_spatial <- c("dobih.userlog_with_geom_mv","dougtracks.dougtracks_lines_emi_mv_simple2")

nonspatial_tables <- c("dobih.hills","dobih.classlink","dobih.userlog","dougtracks.dougtracks_lines_emi_mv_nogeom",
                       "utilityusage.dailyusage_emissions","emissions.emissions_daily","dobih.climbed_summary",
                       "utilityusage.utilityrecordings_smartthings","utilityusage.electricityusage","utilityusage.gasusage")

#spatial:
for (i in 1:length(spatial_tables)) {

  st_read(pgconn1,layer=spatial_tables[i]) %>% st_write(obj=.,dsn=dsn_val,layer=output_spatial[i],driver = "SQLite")   

}

sqconn1 <- dbConnect(RSQLite::SQLite(),dsn_val)

#non-spatial:
for (j in nonspatial_tables) {

  dbGetQuery(pgconn1,paste0("select * from ",j)) %>%  dbWriteTable(conn=sqconn1,name=j,value=.)

}

#disconnect:
dbDisconnect(sqconn1)
dbDisconnect(pgconn1)
rm(sqconn1)
rm(pgconn1)

#delete old sqlite file and replace with new file:

file.remove("data.db")
file.copy(from=filename,to="data.db")
#file.remove(filename)





