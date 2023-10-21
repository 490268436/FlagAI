source("data_preparation.R")

ui_accommodation_foodie <- fluidPage(
  titlePanel("Melbourne Map"),
  tags$head(
    tags$style(HTML(
      ".btn-primary { background-color: #FF5733; border-color: #FF5733; }" # Modify button style
    ))
  ),
  fluidRow(
    column(width = 4,
           textInput("search_query", "Search by Name or Address", value = ""),
           radioButtons("type_selector", "Select Type", choices = c("Restaurants&Cafe", "Hotels"), selected = "Restaurants&Cafe"),
           conditionalPanel(
             condition = "input.type_selector == 'Hotels'",
             selectInput("star_rating", "Select Star Rating", choices = c("All", "3-star", "4-star", "5-star"), selected = "All"),
             checkboxGroupInput("price_range", "Select Price Range", 
                                choices = c("Below 150", "150-300", "Above 300"), selected = c("Below 150", "150-300", "Above 300"))
           ),
           conditionalPanel(
             condition = "input.type_selector == 'Restaurants'",
             sliderInput("seats_range", "Select Seats Range", min = 0, max = 100, value = c(0, 100))
           ),
           
           uiOutput("details")
    ),
    column(width = 8, leafletOutput("map", height = "600px")
    )
  )
)




ui <- fluidPage(
  useShinydashboard(),
  theme = bs_theme(version = 5, bootswatch = 'united'),
  navbarPage(
             header=setUpTableauInShiny(),
             tags$head(
               # Include our custom CSS
               includeCSS("custom.css"),
               includeScript("custom.js")
             ),
             title = "Melbourne Travel Guide",
             id = "navbar", 
            tabPanel("Main", 
                     h3("Melbourne has emerged as the top domestic holiday destination for Australians in the next two years. It has consistently been the first choice for tourists, boasting a wealth of attractions, convenient transportation, and a diverse range of accommodations and culinary delights."),
                     div(class = "row justify-content-center",
                         # Attraction button
                         div(class = "col-md-2 card mt-3 mx-3 adaptive-card", 
                             img(src = "attraction_main.jpg", class = "card-img-top", onclick = "Shiny.setInputValue('attraction_btn', true)"),
                             div(class = "card-body", 
                                 h5(class = "card-title", "Attraction"))
                         ),
                         
                         # Hotel&Foodie button
                         div(class = "col-md-2 card mt-3 mx-3 adaptive-card",
                             img(src = "foodie_main.jpg", class = "card-img-top", onclick = "Shiny.setInputValue('foodie_btn', true)"),
                             div(class = "card-body", 
                                 h5(class = "card-title", "Accomodation & Foodie"))
                         ),
                         
                         # Transport button
                         div(class = "col-md-2 card mt-3 mx-3 adaptive-card",
                             img(src = "transport_main.jpg", class = "card-img-top", onclick = "Shiny.setInputValue('transport_btn', true)"),
                             div(class = "card-body", 
                                 h5(class = "card-title", "Transport"))
                         ),
                     ),
                     div(class = "row justify-content-center",
                         tableauPublicViz(
                           id="tableau_tourism",
                           url="https://public.tableau.com/views/MainPageMelbourneVisiters/MostpopularAustralianholidaydestinations?:language=au-EN&publish=yes&:display_count=n&:origin=viz_share_link",
                           height="280px"
                         ),
                     )
            ),
             tabPanel("Attractions", 
                      value = 'tab-attraction',
                      sidebarLayout(
                        
                        sidebarPanel(
                          h3("Attractions in Melbourne City"),
                          
                          selectInput("poi_search_bar", "Choose a Point of Interest:",
                                      choices = c("Select" = "", landmarks$Feature.Name),
                                      selected = NULL,
                          ),
                          selectInput("attraction_theme", "Attraction Theme", 
                                      choices = unique(landmarks$Theme),
                                      multiple = TRUE, selectize = TRUE, selected = unique(landmarks$Theme)),
                        ),
                        mainPanel(
                          leafletOutput("mainMap", height = "48vh")  # adjust height as per your requirement
                        )
                      ),
                      splitLayout(
                        tableauPublicViz(
                                id="tableau_landmark",
                                url="https://public.tableau.com/views/NumberofNearbyAttractionsbyDistance/NumberofNearbyAttractionsbyDistance?:language=zh-CN&publish=yes&:display_count=n&:origin=viz_share_link",
                                height="400px"
                              ),
                        value="attraction-info",
                            uiOutput("attractionInfo"),
                      ),
                      absolutePanel(
                        id = "controls", class = "panel panel-default", fixed = TRUE,
                        draggable = TRUE, top = 0, left = "auto", right = 0, bottom = "auto",
                        width = 300, height = "auto",

                        valueBoxOutput("tempBox",width = 100),
                        valueBoxOutput("feelTempBox",width = 100),
                        valueBoxOutput("humidityBox",width = 100),
                        valueBoxOutput("weatherMainBox",width = 100),
                        valueBoxOutput("windBox",width = 100),
                        card(
                          height = 250,
                          full_screen = TRUE,
                          card_header("Temperature (°C) Forecast"),
                          card_body(
                            plotlyOutput("weather_forecast_plt")
                          )
                        )
                      ),
            ),
             tabPanel("Accomodation and Foodie", value = "tab-accomodation-foodie",ui_accommodation_foodie),
            tabPanel(title='Public Transport',
                     value = "tab-transport",
                     #h2('Public Transport'),
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           inputId='Type',
                           label='Type',
                           choices=c('Bike Share Dock',
                                     'Bus',
                                     'Shuttle Bus',
                                     'Tram',
                                     'Train'),
                           selected='Bike Share Dock'
                         )
                       ),
                       mainPanel(
                         leafletOutput('transportMap', height = "48vh")
                       )
                     ),
                     splitLayout(
                       tableauPublicViz(
                         id="tableau_transport",
                         url="https://public.tableau.com/views/NumberofDifferentTransportApproaches/NumberofDifferentTransportApproachesinMelbourneCity?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link",
                         height="400px"
                       ),
                       girafeOutput("transportBarChart", height = "400px"),
                       value="transport-info",
                       uiOutput("transportInfo"),
                     )),
  )
)
