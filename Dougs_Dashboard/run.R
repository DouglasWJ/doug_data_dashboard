library(shiny)

source("global.R")
ui <- source("ui.R")[[1]]
server <- source("server.R")[[1]]

shinyApp(ui,server)