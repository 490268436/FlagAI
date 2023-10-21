source("ui.R")
source("server.R")

shinyApp(ui, server, options=list(launch.browser=TRUE))