source("data_preparation.R")


map <- leaflet() %>%
  addProviderTiles(providers$CartoDB, options = providerTileOptions(minZoom = 10, maxZoom = 20)) %>%
  setView(lng = 144.9561, lat = -37.8103, zoom = 14)


get_place_details <- function(place_name){
  # Build API request URL
  base_url <- "https://maps.googleapis.com/maps/api/place/findplacefromtext/json?"
  if(is.character(place_name) && length(place_name) == 1) {
    request_url <- paste0(base_url, 
                          "input=", URLencode(place_name),
                          "&inputtype=textquery",
                          "&fields=place_id",
                          "&key=AIzaSyD9tNHaD2CtEA6NOJk9QdnuT9ZKjWYKo2U")
  } else {
    stop("place_name must be a single string")
  }
  
  # Making the Request
  response <- tryCatch({
    res <- GET(request_url)
    json_text <- content(res, "text", encoding = "UTF-8")
    fromJSON(json_text)
  }, 
  error = function(e) {
    warning("Error in API Request: ", conditionMessage(e))
    NULL
  })

  # Get place_id and retrieve detailed information
  if(length(response$candidates) > 0){
    place_id <- response$candidates[[1]][1]
    
    details_url <- paste0("https://maps.googleapis.com/maps/api/place/details/json?",
                          "place_id=", place_id,
                          "&fields=opening_hours,rating,photos,formatted_address",
                          "&key=AIzaSyD9tNHaD2CtEA6NOJk9QdnuT9ZKjWYKo2U")
    details_response <- fromJSON(content(GET(details_url), "text", encoding = "UTF-8"))
    
    opening_hour_info <- details_response$result$opening_hours$weekday_text
    rating <- details_response$result$rating
    photos <- details_response$result$photos
    address <- details_response$result$formatted_address
    
    photo_urls <- vector("list", 3)  # Initialize a list to hold up to 3 photo URLs
    
    if(length(photos) > 0) {
      photo_base_url <- "https://maps.googleapis.com/maps/api/place/photo?"
      for (i in 1:min(length(photos), 3)) {
        photo_reference <- photos$photo_reference[i]
        if(!is.na(photo_reference)){
          photo_urls[[i]] <- paste0(photo_base_url,
                                    "maxwidth=400",
                                    "&photoreference=", photo_reference,
                                    "&key=AIzaSyD9tNHaD2CtEA6NOJk9QdnuT9ZKjWYKo2U")
        }
      }
    }
    
    if(is.null(opening_hour_info)){
      opening_hour_info = "N/A"
    }
    
    if(is.null(rating)){
      rating = "N/A"
    }
    
    return(list(
      name = place_name,
      hours_html = paste0("<strong>Opening Hours:</strong><br>", paste(opening_hour_info, collapse = "<br>")),
      rating = rating,
      address = address,
      photo_urls = photo_urls  # This now returns a list of up to 3 photo URLs
    ))
  } else {
    return(NULL)
  }
}

render_transport_marker <- function(map_id, type, marker_click, zoom) {
  map_proxy <- leafletProxy(map_id)
  
  # Prepare color palettes for bus/tram routes
  tram_palette <- viridis(24)
  tram_color_mapping <- colorFactor(palette = tram_palette, domain = tram_sf$ROUTE_ID)
  
  bus_palette <- viridis(36)
  bus_color_mapping <- colorFactor(palette = bus_palette, domain = bus_sf$ROUTE_ID)
  
  # Add markers and routes according to the selected transport type
  if (type == 'Bike Share Dock') {
    map_proxy %>%
      clearMarkers() %>% 
      clearShapes() %>%
      addMarkers(data = bike_data, ~lng, ~lat,
                 layerId = ~name,
                 icon = icons_list[["Bike"]])
  } 
  else if(type == 'Bus') {
    map_proxy %>%
      clearMarkers() %>% 
      clearShapes() %>%
      addPolylines(data = bus_sf, color = ~bus_color_mapping(ROUTE_ID), opacity = 0.75)
    
    if(!is.null(zoom) && zoom > 13.5) {
      map_proxy %>%
        addMarkers(data = bus_data, ~lng, ~lat, 
                   layerId = ~name,
                   icon = icons_list[["Bus"]])
    }
  } 
  else if(type == 'Shuttle Bus') {
    map_proxy %>%
      clearMarkers() %>% 
      clearShapes() %>%
      addMarkers(data = shuttle_bus_data, ~lng, ~lat,
                 layerId = ~name,
                 icon = icons_list[["Shuttle Bus"]])
  } 
  else if(type == 'Tram') {
    map_proxy %>%
      clearMarkers() %>% 
      clearShapes() %>%
      addPolylines(data = tram_sf, color = ~tram_color_mapping(ROUTE_ID), opacity = 0.75)
    
    if(!is.null(zoom) && zoom > 13.5) {
      map_proxy %>%
        addMarkers(data = tram_stops, ~lng, ~lat, 
                   layerId = ~name,
                   icon = icons_list[["Tram"]])
    }
  } 
  else if(type == 'Train') {
    map_proxy %>%
      clearMarkers() %>% 
      clearShapes() %>%
      addMarkers(data = train_data, ~lng, ~lat,
                 layerId = ~name,
                 icon = icons_list[["Train"]])
  }
  
  # Add a marker for the selected point of interest
  if (!is.null(marker_click)) {
    map_proxy %>%
      addMarkers(
        lng = marker_click$lng,
        lat = marker_click$lat,
        layerId = marker_click$id
      )
  }
}

server <- function(input, output, session) {
  # Reactive Filtered Data for landmarks
  data_attraction <- reactiveVal(NULL)
  
  
  # store google place data
  place_info <- reactiveVal(NULL)
  
  # store shared attraction data across tabs
  shared_data <- reactiveValues()
  
  # store clicked marker data
  selected_marker_data <- reactiveVal(NULL)
  
  
  generate_detail_page_res <- function(name, address, seating_type, number_of_seats, rate) {
    # Restaurant and cafe detail page
    
    get_star_rating <- function(rate) {
      full_stars <- floor(rate)
      half_star <- ifelse(rate - full_stars >= 0.5, 1, 0)
      empty_stars <- 5 - full_stars - half_star
      stars <- paste(rep("★", full_stars), collapse = "")  # full stars
      stars <- paste0(stars, ifelse(half_star == 1, "☆", ""))  # half star
      stars <- paste0(stars, rep("☆", empty_stars))  # empty stars
      return(stars)
    }
    
    sr <- get_star_rating(rate)
    
    content <- paste0(
      "<div style='font-size: 14px; font-family: Arial, sans-serif; margin-bottom: 10px;'>",
      "<strong>",address, "</strong><br>",
      "<strong>Customer Rating: </strong>", sr, "<br>"
    )
    page <- fluidPage(
      titlePanel(paste("", name)),
      mainPanel(
        HTML(content),
        p(paste("Seating Type: ", seating_type)),
        p(paste("Number of Seats: ", number_of_seats)),
        plotlyOutput("rate_plot"),
        plotlyOutput("seating_plot"),
        actionLink("return_map", "Return to Map")
      )
    )

    return(page)
  }
  

  
  
  
  generate_detail_page <- function(name, address, Prices, recent_prices, rate) {
    
    get_star_rating <- function(rate) {
      full_stars <- floor(rate)
      half_star <- ifelse(rate - full_stars >= 0.5, 1, 0)
      empty_stars <- 5 - full_stars - half_star
      stars <- paste(rep("★", full_stars), collapse = "")  # full stars
      stars <- paste0(stars, ifelse(half_star == 1, "☆", ""))  # half star
      stars <- paste0(stars, rep("☆", empty_stars))  # empty stars
      return(stars)
    }
    
    sr <- get_star_rating(rate)
    
    content <- paste0(
      "<div style='font-size: 14px; font-family: Arial, sans-serif; margin-bottom: 10px;'>",
      "<strong>",address, "</strong><br>",
      "<strong>Customer Rating: </strong>", sr, "<br>"
    )
    
    # Hotel detail page
    page <- fluidPage(
      titlePanel(paste("", name)),
      mainPanel(
        HTML(content),
        p(paste("Recent Average One-Night Rate ($): ", Prices)),
        plotlyOutput("seating_plot"),
        actionLink("return_map", "Return to Map")
      )
    )
    return(page)
  }
  
  
  observe({
    # Ensure input$attraction_theme is available
    req(input$attraction_theme)
    
    # Update data_attraction with the filtered data
    data_attraction(filter(landmarks, Theme %in% input$attraction_theme))
  })
  
  output$map <- renderLeaflet({
    map
  })
  

  output$rate_plot <- renderPlotly({
    click_info_res <- input$map_marker_click
    scores <- data_res$customer_rate

    clicked_lat <- click_info_res$lat
    clicked_lng <- click_info_res$lng
    selected_restaurant <- subset(data_res, latitude == clicked_lat & longitude == clicked_lng)
    current_score <- selected_restaurant$customer_rate

    p <- ggplot(data = data.frame(score = scores), aes(x = score)) +
      geom_histogram(binwidth = 0.2, fill = "blue") +
      
      geom_histogram(data = data.frame(score = scores[scores > (current_score - current_score %% 0.2-0.1) & scores <= (current_score - current_score %% 0.2 + 0.1)]), 
                     binwidth = 0.2, fill = "yellow") +
      labs(title = "Distribution of Ratings", x = "Customer rate") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(0, 2, by = 0.2), limits = c(3, 5))
  
    return(p)

  })

  
  output$seating_plot <- renderPlotly({
    if (show_detail()) {
      if (input$type_selector == "Restaurants&Cafe") {
        click_info_res <- input$map_marker_click
        if (!is.null(click_info_res) && nrow(data_res) > 0) {
          clicked_lat <- click_info_res$lat
          clicked_lng <- click_info_res$lng
          selected_restaurant <- subset(data_res, latitude == clicked_lat & longitude == clicked_lng)
          if (nrow(selected_restaurant) > 0) {
            seating_data <- data.frame(Type = selected_restaurant$seating_type, Seats = selected_restaurant$number_of_seats)
            p <- plot_ly(seating_data, x = ~Type, y = ~Seats, type = 'bar', height = 200)
            p <- p %>% layout(title = "Seating Type and Quantity")
            return(p)
          }
        }
      }
      
      if (input$type_selector == "Hotels") {
        click_info <- input$map_marker_click
        if (!is.null(click_info) && nrow(hotels) > 0) {
          clicked_lat <- click_info$lat
          clicked_lng <- click_info$lng
          selected_hotel <- subset(hotels, Lat == clicked_lat & Long == clicked_lng)
          if (nrow(selected_hotel) > 0) {
            price_data <- selected_hotel$Recent_Prices
            split_values <- strsplit(gsub("\\[|\\]", "", price_data), ",")
            price_recent_data <- as.numeric(unlist(split_values))
            price_recent_data <- price_recent_data[!is.na(price_recent_data)]
            df <- data.frame(Price = price_recent_data)
            p <- plot_ly(df, x = ~seq_along(Price), y = ~Price, type = 'scatter', mode = 'lines+markers', height = 200)
            p <- p %>% layout(
              title = "Recent Prices",
              xaxis = list(
                title = "Recent 7 days",
                tickmode = "array",
                tickvals = 1:7,
                ticktext = c("1", "2", "3", "4", "5", "6", "7")
              ),
              yaxis = list(title = "Price")
            )
            return(p)
          }
        }
      }
    }
  })
  
  
  observe({
    if (input$type_selector == "All" || input$type_selector == "Restaurants&Cafe") {
      # Process restaurant and cafe data
      filtered_data_res <- data_res
      search_query_res <- input$search_query
      
      if (!is.null(search_query_res) && nchar(search_query_res) > 0) {
        filtered_data_res <- filtered_data_res %>% 
          filter(grepl(search_query_res, clue_small_area, ignore.case = TRUE) | grepl(search_query_res, trading_name, ignore.case = TRUE) | grepl(search_query_res, building_address, ignore.case = TRUE))
      }
      
      filtered_data_res <- filtered_data_res %>% filter(number_of_seats >= input$seats_range[1] & number_of_seats <= input$seats_range[2])
      
      map <- leafletProxy("map")
      map <- map %>% clearMarkers()
      
      if (nrow(filtered_data_res) > 0) {
        for (i in 1:nrow(filtered_data_res)) {
          if (grepl("cafe", filtered_data_res$trading_name[i], ignore.case = TRUE)) {
            print(filtered_data_res$trading_name[i])
            temp <-hotel_icons[["Cafe"]]
          } else {
            print(filtered_data_res$trading_name[i])
            temp <-hotel_icons[["Restaurant"]]
          }
          
          map <- map %>%
            addMarkers(
              data = filtered_data_res[i, ],
              lng = ~longitude,
              lat = ~latitude,
              popup = as.character(filtered_data_res$trading_name[i]),
              icon = temp
              
            )
        }
      }
    }
    
    if (input$type_selector == "All" || input$type_selector == "Hotels") {
      # Process hotel data
      filtered_hotels <- hotels
      search_query_hotels <- input$search_query
      
      if (!is.null(search_query_hotels) && nchar(search_query_hotels) > 0) {
        filtered_hotels <- filtered_hotels %>% 
          filter(grepl(search_query_hotels, clue_small_area, ignore.case = TRUE) | grepl(search_query_hotels, Name, ignore.case = TRUE) | grepl(search_query_hotels, Address, ignore.case = TRUE))
      }
      
      selected_stars <- if (input$star_rating == "All") {
        c("3-star", "4-star", "5-star")
      } else {
        input$star_rating
      }
      
      result_df <- data.frame()
      
      if ("Below 150" %in% input$price_range) {
        empty_df_1 <- filtered_hotels %>%
          filter(Stars %in% selected_stars, Prices <= 150)
        result_df <- bind_rows(result_df, empty_df_1)
      }
      
      if ("150-300" %in% input$price_range) {
        empty_df_2 <- filtered_hotels %>%
          filter(Stars %in% selected_stars, Prices > 150 & Prices <= 200)
        result_df <- bind_rows(result_df, empty_df_2)
      }
      
      if ("Above 300" %in% input$price_range) {
        empty_df_3 <- filtered_hotels %>%
          filter(Stars %in% selected_stars, Prices > 300)
        result_df <- bind_rows(result_df, empty_df_3)
      }
      
      filtered_hotels <- result_df
      
      map <- leafletProxy("map")
      map <- map %>% clearMarkers()
      
      if (nrow(filtered_hotels) > 0) {
        for (i in 1:nrow(filtered_hotels)) {
          star_rating <- filtered_hotels$Stars[i]
          house_icon <- hotel_icons[[star_rating]]
          
          map <- map %>%
            addMarkers(
              data = filtered_hotels[i, ],
              lng = ~Long,
              lat = ~Lat,
              popup = as.character(filtered_hotels$Name[i]),
              icon = house_icon
            )
        }
      }
    }
  })
  
  show_detail <- reactiveVal(FALSE)
  
  output$details <- renderUI({
    
    
    if (show_detail()) {
      if (input$type_selector == "All" || input$type_selector == "Restaurants&Cafe") {
        click_info_res <- input$map_marker_click
        if (!is.null(click_info_res) && nrow(data_res) > 0) {
          clicked_lat <- click_info_res$lat
          clicked_lng <- click_info_res$lng
          selected_restaurant <- subset(data_res, latitude == clicked_lat & longitude == clicked_lng)
          if (nrow(selected_restaurant) > 0) {
            detail_page <- generate_detail_page_res(selected_restaurant$trading_name, selected_restaurant$building_address, selected_restaurant$seating_type, selected_restaurant$number_of_seats,selected_restaurant$customer_rate)
            return(detail_page)
          }
        }
      }
      
      if (input$type_selector == "All" || input$type_selector == "Hotels") {
        click_info <- input$map_marker_click
        if (!is.null(click_info) && nrow(hotels) > 0) {
          clicked_lat <- click_info$lat
          clicked_lng <- click_info$lng
          selected_hotel <- subset(hotels, Lat == clicked_lat & Long == clicked_lng)
          if (nrow(selected_hotel) > 0) {
            detail_page <- generate_detail_page(selected_hotel$Name, selected_hotel$Address, selected_hotel$Prices, selected_hotel$Recent_Prices,selected_hotel$customer_rate)
            return(detail_page)
          }
        }
      }
    }
  })
  
  observe({
    req(input$attraction_theme) 
    
    # Get the updated data based on the selected themes
    updated_data <- data_attraction()
    
    # Update the choices available in input$poi_search_bar
    updateSelectInput(
      session = session,
      inputId = "poi_search_bar",
      choices = c("Select" = "", updated_data$Feature.Name),
      selected = NULL 
    )
  })
  
  # Reactive coordinates of the selected POI
  searched_poi_coords <- reactive({

    # Check if input$poi_search_bar is non-empty without stopping execution
    if(is.null(input$poi_search_bar) || input$poi_search_bar == "") {
      return(NULL)
    } 

    # Find the row in the landmarks data frame that corresponds to the selected POI
    selected_row <- landmarks[landmarks$Feature.Name == input$poi_search_bar,]
    
    # If found, return the lng and lat as a named numeric vector
    if (nrow(selected_row) > 0) {
      return(c(lng = selected_row$lng, lat = selected_row$lat))
    } else {
      return(NULL)
    }
  })
  
  render_place <- function(place_name, lat, lng){
    deatils <- get_place_details(place_name)
    place_info(deatils) 
    
    runjs(sprintf('let viz = document.getElementById("tableau_landmark");
        let sheet = viz.workbook.activeSheet;
        sheet.applyFilterAsync("Place", ["%s"], FilterUpdateType.Replace);', place_name))
    
    content <- paste0(
      "<div style='font-size: 14px; font-family: Arial, sans-serif; margin-bottom: 10px;'>",
      "<strong>", place_name, "</strong><br>",
      place_info()$hours_html,
      "</div>",
      '<button class="btn btn-primary custom-button" id="', paste0(place_name, "-accomodation-foodie"),
      '" onclick="Shiny.onInputChange(\'foodie_accomodation_explore_button\',  this.id + new Date().toISOString());">',
      "More on Foodie and Accomodation Nearby",
      "</button><br><br>",  # <br> tags added for spacing
      '<button class="btn btn-secondary custom-button" id="', paste0(place_name, "-transport"),
      '" onclick="Shiny.onInputChange(\'transport_explore_button\',  this.id + new Date().toISOString());">',
      "More on Public Transport Nearby",
      "</button>"
    )
    
    
    leafletProxy("mainMap") %>%
      addPopups(
        lng = lng,
        lat = lat,
        content
      ) %>% 
      setView(lng = lng, lat = lat + 0.012, zoom = 14)
  }
  
  ### Main Tab ###
  observeEvent(input$attraction_btn, {
    updateNavbarPage(session, "navbar", selected = "tab-attraction")
  })
  
  observeEvent(input$transport_btn, {
    updateNavbarPage(session, "navbar", selected = "tab-transport")
  })
  
  observeEvent(input$foodie_btn, {
    updateNavbarPage(session, "navbar", selected = "tab-accomodation-foodie")
  })
  
  observeEvent(input$navbar, {
    runjs('dispartchEvent(new Event("resize"))')
  })
  
  ### Attraction Tab###
  # Render Leaflet Map
  output$mainMap <- renderLeaflet({
    # Default values for lng, lat, and zoom
    lng_default <- 144.962292  
    lat_default <- -37.812329  
    zoom_default <- 14
    
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB, options = providerTileOptions(minZoom = 10, maxZoom = 20))
    
    data_to_plot <- data_attraction()
    
    if (nrow(data_to_plot) > 0) {
      # Split the data by theme
      split_data <- split(data_to_plot, data_to_plot$Theme)
      
      # Loop through each theme and add new markers
      for(theme in names(split_data)) {
        if(theme %in% names(icons_list) && !is.null(split_data[[theme]])) {
          m <- addMarkers(m, data = split_data[[theme]], ~lng, ~lat,
                          icon = icons_list[[theme]], 
                          layerId = ~Feature.Name)
        }
      }
    }
    # Ensure the map has a view
    m <- m %>% 
      setView(lng = lng_default, lat = lat_default, zoom = zoom_default)
    return(m)
  })
  
  observe({
    # Ensure the reactive data is not NULL
    req(data_attraction())
    data_to_plot <- data_attraction()
    
    if (nrow(data_to_plot) > 0) {

      # Split the data by theme
      split_data <- split(data_to_plot, data_to_plot$Theme)
      
      # Loop through each theme and add new markers
      for(theme in names(split_data)) {
        if(theme %in% names(icons_list) && !is.null(split_data[[theme]])) {
          m <- leafletProxy("mainMap")
          leafletProxy("mainMap") %>%
            addMarkers(data = split_data[[theme]], ~lng, ~lat,
                       icon = icons_list[[theme]], 
                       layerId = ~Feature.Name)
          
        }
      }
      # Determine the bounding box
      min_lng <- min(data_to_plot$lng, na.rm = TRUE)
      max_lng <- max(data_to_plot$lng, na.rm = TRUE)
      min_lat <- min(data_to_plot$lat, na.rm = TRUE)
      max_lat <- max(data_to_plot$lat, na.rm = TRUE)
      
      # Set the map view to fit the bounding box
      leafletProxy("mainMap") %>%
        fitBounds(min_lng, min_lat, max_lng, max_lat)
    }
  })
  
  # when the attraction map's marker is clicked
  observeEvent(input$mainMap_marker_click, {
    marker_click <- input$mainMap_marker_click
    
    # store clicked marker for the transport tab
    selected_marker_data(marker_click)
    
    place_name <- marker_click$id
    render_place(place_name, marker_click$lat, marker_click$lng)
  })
  
  output$attractionInfo <- renderUI({
    info <- place_info()
    
    if (is.null(info)) {
      return(NULL)
    }
    
    # Check if there are any images
    if (length(info$photo_urls) == 0 || all(sapply(info$photo_urls, is.null))) {
      # No images available
      return(HTML(paste0(
        "<br><strong>Name:</strong> ", info$name,
        "<br><strong>Rating:</strong> ", info$rating,
        "<br><strong>Address:</strong> ", info$address
      )))
    }
    
    # Generating the carousel items
    carousel_items <- ""
    carousel_indicators <- ""
    for (i in 1:length(info$photo_urls)) {
      if (!is.null(info$photo_urls[[i]])) {
        active_class <- ifelse(i == 1, "active", "")  # The first item should be active
        carousel_items <- paste0(
          carousel_items,
          "<div class='carousel-item ", active_class, "'>",
          "<img src='", info$photo_urls[[i]],
          "' class='d-block w-100' style='height: 300px; object-fit: contain;' alt='Place Photo ", i, "'>",
          "</div>"
        )
        carousel_indicators <- paste0(
          carousel_indicators,
          "<button type='button' data-bs-target='#carouselExampleIndicators' data-bs-slide-to='", i - 1,
          "' class='", active_class, "' aria-current='", ifelse(i == 1, "true", "false"),
          "' aria-label='Slide ", i, "'></button>"
        )
      }
    }
    
    # Combining carousel items with other information
    carousel_html <- paste0(
      "<div id='carouselExampleIndicators' class='carousel slide' data-bs-ride='carousel'>",
      "<div class='carousel-indicators'>",
      carousel_indicators,
      "</div>",
      "<div class='carousel-inner'>",
      carousel_items,
      "</div>",
      "<button class='carousel-control-prev' type='button' data-bs-target='#carouselExampleIndicators' data-bs-slide='prev'>",
      "<span class='carousel-control-prev-icon' aria-hidden='true' style='background-color: rgba(0,0,0,0.5); border-radius: 50%;'></span>",
      "<span class='visually-hidden'>Previous</span>",
      "</button>",
      "<button class='carousel-control-next' type='button' data-bs-target='#carouselExampleIndicators' data-bs-slide='next'>",
      "<span class='carousel-control-next-icon' aria-hidden='true' style='background-color: rgba(0,0,0,0.5); border-radius: 50%;'></span>",
      "<span class='visually-hidden'>Next</span>",
      "</button>",
      "</div>"
    )
    
    HTML(paste0(
      carousel_html,
      "<br><strong>Name:</strong> ", info$name,
      "<br><strong>Rating:</strong> ", info$rating,
      "<br><strong>Address:</strong> ", info$address
    ))
  })
  
  # Create an observer event to modify reactive_data_attraction based on tableau_landmark_mark_selection_changed input
  observeEvent(input$tableau_landmark_mark_selection_changed, {
    # Get the new input value
    selected_data <- input$tableau_landmark_mark_selection_changed
    place_name <- place_info()$name
    last_selected_place <- landmarks[landmarks$Feature.Name == place_name,]
    lng <- last_selected_place$lng
    lat <- last_selected_place$lat
    # Check if the input is NULL or empty list
    if (is.null(selected_data) || length(selected_data) == 0) {
      # If so, revert to original behavior
      data_attraction(filter(landmarks, Theme %in% input$attraction_theme))
    } else {
      # Otherwise, extract PlaceNames, split into individual names, and use to filter data
      all_place_names <- unlist(strsplit(selected_data$PlaceNames, split = ", "))
      unique_place_names <- c(unique(all_place_names), place_name)
      data_attraction(filter(landmarks, Feature.Name %in% unique_place_names & Theme %in% input$attraction_theme))
    }
    # Whether the bar chart is selected or unselected, the ui should always click on the last selected point
    render_place(place_name, lat, lng)
  })
  
  # Redirect on main map marker button clicked
  observeEvent(input$foodie_accomodation_explore_button, {
    foodie_accomodation_explore_btn_id <- input$foodie_accomodation_explore_button
    if (!is.null(foodie_accomodation_explore_btn_id) && nzchar(foodie_accomodation_explore_btn_id)) {
      print("redirect to foodie")
      # trim the button id
      foodie_accomodation_explore_btn_id <- trimws(foodie_accomodation_explore_btn_id)
      
      place_name <- place_info()$name
      last_selected_place <- landmarks[landmarks$Feature.Name == place_name,]
      lng <- last_selected_place$lng
      lat <- last_selected_place$lat
      
      # Construct shared_data
      shared_data$google_data <- place_info()
      shared_data$lat <- lat
      shared_data$lng <- lng
      
      updateNavbarPage(session, "navbar", selected = "tab-accomodation-foodie")
    }
  })
  
  observeEvent(input$return_map, {
    show_detail(FALSE)
  })
  
  observe({
    click_info <- input$map_marker_click
    if (!is.null(click_info)) {
      show_detail(TRUE)
    }
  })
  
  observeEvent(input$transport_explore_button, {
    transport_explore_btn_id <- input$transport_explore_button
    if (!is.null(transport_explore_btn_id) && nzchar(transport_explore_btn_id)) {
      print("redirect to transport")
      
      # trim the button id
      transport_explore_btn_id <- trimws(transport_explore_btn_id)
      
      place_name <- place_info()$name
      last_selected_place <- landmarks[landmarks$Feature.Name == place_name,]
      lng <- last_selected_place$lng
      lat <- last_selected_place$lat
      
      # Construct shared_data
      shared_data$google_data <- place_info()
      shared_data$lat <- lat
      shared_data$lng <- lng
      
      updateNavbarPage(session, "navbar", selected = "tab-transport")
    }
  })
  
  # Zoom on search option selected
  observe({
    selected_coords <- searched_poi_coords()
    if (!is.null(selected_coords)) {
      lng_default <- selected_coords["lng"]
      lat_default <- selected_coords["lat"]
      zoom_default <- 20
      
      # Update the view of the existing leaflet map
      leafletProxy("mainMap") %>% 
        setView(lng = lng_default, lat = lat_default, zoom = zoom_default)
    }
  })
  
  owmr_settings("e77d21b7ddc8748be4429e32f45c63bf")
  lng_default <- 144.962292  
  lat_default <- -37.812329
  
  autoInvalidate <- reactiveTimer(600000)
  
  observe({
    autoInvalidate()
  })
  
  output$dateText <- renderText({
    autoInvalidate()
    paste("last update @", Sys.time())
  })
  
  data_current <- reactivePoll(
    intervalMillis = 600000, 
    session, 
    checkFunc = function(){
      Sys.time()
    }, 
    valueFunc = function(){
      get_current(lon=lng_default,lat=lat_default, units = "metric")
    }
  )
  
  data_forecast <- reactivePoll(
    intervalMillis = 600000, 
    session, 
    checkFunc = function(){
      Sys.time()
    }, 
    valueFunc = function(){
      get_forecast(lon=lng_default,lat=lat_default, units = "metric")
    }
  )
  
  output$tempBox <- renderValueBox({
    weather <- data_current()
    valueBox(
      paste0(format(weather$main$temp, digits = 2), " °C"), "temperature",
      color = "orange", icon = icon("thermometer-half")
    )
  })
  
  output$feelTempBox <- renderValueBox({
    weather <- data_current()
    valueBox(
      paste0(format(weather$main$feels_like, digits = 2), " °C"), "feels like",
      color = "orange", icon = icon("thermometer-half")
    )
  })
  
  output$humidityBox <- renderValueBox({
    weather <- data_current()
    valueBox(
      paste0(weather$main$humidity, " %"), "humidity",
      color = "orange", icon = icon("tint")
    )
  })
  
  output$weatherMainBox <- renderValueBox({
    weather <- data_current()
    weather_tibble <- as_tibble(weather$weather)
    
    weather_tibble <- weather_tibble %>% 
      mutate(description = case_when(
        description == "overcast clouds" ~ "overcast",
        description == "broken clouds" ~ "broken",
        TRUE ~ description 
      ))
    valueBox(
      paste0(weather_tibble$description), "cloudiness",
      color = "orange", icon = icon("cloud")
    )
  })
  output$windBox <- renderValueBox({
    weather <- data_current()
    valueBox(
      paste0(weather$wind$speed, " km/h"), "wind speed",
      color = "orange", icon = icon("tachometer-alt")
    )
  })
  
  output$weather_forecast_plt <- renderPlotly({
    weather_forecast_data <- data_forecast()
    weather_forecast <- as_tibble(weather_forecast_data$list)
    weather_forecast$new_date <- ymd_hms(weather_forecast$dt_txt)
    
    plot_ly(data = weather_forecast, 
            type = 'scatter', 
            mode = 'lines+markers',
            x = ~new_date, 
            y = ~main.temp, 
            line = list(width = 1.5),
            marker = list(size = 5, color = '#605BA8'),
            text = ~paste0("Temperature: ", main.temp, "°C"),
            hoverinfo = "text") %>%
      layout(title = "",
             xaxis = list(title = "",
                          tickformat = "%a-%d\n%H:%M",
                          tickangle = -45,
                          gridcolor = toRGB("grey"),
                          gridwidth = 1),
             yaxis = list(title = "",
                          gridcolor = toRGB("grey"),
                          gridwidth = 1),
             shapes = list(
               list(
                 type = "line",
                 y0 = 0, y1 = 0,
                 x0 = first(weather_forecast$new_date), x1 = last(weather_forecast$new_date),
                 line = list(dash = "dash")
               )
             ),
             hovermode = 'x unified'
      ) 
  })
  
  ### Transport Tab###
  render_transport <- function(place_name, lat, lng){
    # Get place details by calling the Google API
    deatils <- get_place_details(place_name)
    place_info(deatils) 
    
    content <- paste0(
      "<div style='font-size: 14px; font-family: Arial, sans-serif; margin-bottom: 10px;'>",
      "<strong>", place_name, "</strong><br>")
    
    leafletProxy("transportMap") %>%
      addPopups(
        lng = lng,
        lat = lat,
        content
      ) %>% 
      setView(lng = lng, lat = lat + 0.012, zoom = 14)
  }
  
  # Render the transport Map
  output$transportMap <- renderLeaflet({
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng=144.9631, lat=-37.8136, zoom=12.5) 
    
    map_proxy <- leafletProxy("transportMap") 
    
    # Bike is the default type
    if (input$Type == 'Bike Share Dock') {
      map_proxy %>%
        clearMarkers() %>% 
        clearShapes() %>%
        addMarkers(data = bike_data, ~lng, ~lat,
                   layerId = ~name,
                   icon = icons_list[["Bike"]])
    } 
    
    map
  })
  
  # Dynamically re-render the transport map
  observe({
    render_transport_marker("transportMap", input$Type, selected_marker_data(), input$transportMap_zoom)
  })
  
  observeEvent(input$transportMap_marker_click, {
    # Obtain the marker clicked
    marker_click <- input$transportMap_marker_click
    place_name <- marker_click$id
    place_lng <- marker_click$lng
    place_lat <- marker_click$lat
    
    # Render popup
    render_transport(place_name, place_lat, place_lng)
    
    # Obtain lat and lng, and calculate distances
    clicked_point <- c(place_lng, place_lat)
    distances <- distm(clicked_point, cbind(transport_data$lng, transport_data$lat), fun = distVincentySphere)
    max_break <- max(distances) + 1
    
    # Categorize distances
    distance_category <- cut(
      distances,
      breaks = c(0, 500, 1000, 5000, 10000, max_break),
      labels = c("Within 500m", "500m - 1km", "1km - 5km", "5km - 10km", "Beyond 10 km"),
      include.lowest = TRUE
    )
    
    # Combine the data
    result_dataframe <- data.frame(
      PlaceName = transport_data$name,
      Type = transport_data$label,
      Lat = transport_data$lat,
      Lng = transport_data$lng,
      Distance = distance_category
    )
    
    # Arrange the data by distance
    df_aggregated <- result_dataframe %>%
      group_by(Distance, Type) %>%
      summarise(Count = n()) %>%
      ungroup()
    
    output$transportBarChart <- renderGirafe({
      united_colors <- c("#409240", "#FFC425", "#FF8C00", "#E95420", "red")
      
      df_aggregated <- df_aggregated %>%
        complete(Type, Distance, fill = list(Count = 0))
      
      distances <- c("Within 500m", "500m - 1km", "1km - 5km", "5km - 10km", "Beyond 10 km")  
      
      # Sort the data by distance
      df_aggregated$Distance <- factor(df_aggregated$Distance, levels = distances)

      # Plotting
      p <- ggplot(df_aggregated, aes(x=Type, y=Count, fill=Distance)) + 
        geom_bar_interactive(stat="identity", position="dodge", 
                             aes(tooltip = paste("Distance: ", Distance, " <br/>Type: ", Type, " <br/>Count: ", Count)),
                             width = 0.8) +
        labs(title=paste("Traffic Facilities near", input$transportMap_marker_click$id),
             y="Count",
             x="Traffic Facility Type") +
        scale_fill_manual(values = united_colors) +
        theme_minimal() +
        theme(
          plot.title = element_text(size=12),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1),   
          axis.text.y = element_text(size=14)
        )
      
      girafe(ggobj = p, options = list(
        hover_opacity = 1, 
        tooltip_offx = 10, 
        tooltip_offy = -10
      ))
    })
    
  })
  
  # Generate Bar Chart / Clear markers and popups according to tab
  observeEvent(input$navbar, {
    if (input$navbar == "tab-transport") {
      
      if(!is.null(selected_marker_data())) {
        # Obtain the marker clicked
        marker_click <- selected_marker_data()
        place_name <- marker_click$id
        place_lng <- marker_click$lng
        place_lat <- marker_click$lat
        
        # Render popup
        render_transport(place_name, place_lat, place_lng)
        
        # Obtain lat and lng, and calculate distances
        clicked_point <- c(place_lng, place_lat)
        distances <- distm(clicked_point, cbind(transport_data$lng, transport_data$lat), fun = distVincentySphere)
        max_break <- max(distances) + 1
        
        # Categorize distances
        distance_category <- cut(
          distances,
          breaks = c(0, 500, 1000, 5000, 10000, max_break),
          labels = c("Within 500m", "500m - 1km", "1km - 5km", "5km - 10km", "Beyond 10 km"),
          include.lowest = TRUE
        )
        
        # Combine the data
        result_dataframe <- data.frame(
          PlaceName = transport_data$name,
          Type = transport_data$label,
          Lat = transport_data$lat,
          Lng = transport_data$lng,
          Distance = distance_category
        )
        
        # Arrange the data by distance
        df_aggregated <- result_dataframe %>%
          group_by(Distance, Type) %>%
          summarise(Count = n()) %>%
          ungroup()
        
        output$transportBarChart <- renderGirafe({
          united_colors <- c("#409240", "#FFC425", "#FF8C00", "#E95420", "red")
          
          df_aggregated <- df_aggregated %>%
            complete(Type, Distance, fill = list(Count = 0))
          
          distances <- c("Within 500m", "500m - 1km", "1km - 5km", "5km - 10km", "Beyond 10 km")  
          
          # Sort the data by distance
          df_aggregated$Distance <- factor(df_aggregated$Distance, levels = distances)
          
          # Plotting
          p <- ggplot(df_aggregated, aes(x=Type, y=Count, fill=Distance)) + 
            geom_bar_interactive(stat="identity", position="dodge", 
                                 aes(tooltip = paste("Distance: ", Distance, " <br/>Type: ", Type, " <br/>Count: ", Count)),
                                 width = 0.8) +
            labs(title=paste("Traffic Facilities near", marker_click$id),
                 y="Count",
                 x="Traffic Facility Type") +
            scale_fill_manual(values = united_colors) +
            theme_minimal() +
            theme(
              plot.title = element_text(size=12),
              axis.title.x = element_text(size=16),
              axis.title.y = element_text(size=16),
              axis.text.x = element_text(size=12, angle = 45, hjust = 1), 
              axis.text.y = element_text(size=14)
            )
          
          girafe(ggobj = p, options = list(
            hover_opacity = 1, 
            tooltip_offx = 10, 
            tooltip_offy = -10
          ))
        })
        
      }
    } 
    else {
      # Clear popups when leaving the transport tab
      leafletProxy("transportMap") %>%
        clearPopups()
      
      if (!is.null(selected_marker_data())) {
        # Remove markers when leaving the transport tab
        leafletProxy("transportMap") %>%
          removeMarker(layerId = selected_marker_data()$layerId) 
      }
    }
  })
  
  # Generate info for transport tab, basically the same as the previous function
  output$transportInfo <- renderUI({
    info <- place_info()
    
    if (is.null(info)) {
      return(NULL)
    }
    
    # Check if there are any images
    if (length(info$photo_urls) == 0 || all(sapply(info$photo_urls, is.null))) {
      # No images available
      return(HTML(paste0(
        "<br><strong>Name:</strong> ", info$name,
        "<br><strong>Rating:</strong> ", info$rating,
        "<br><strong>Address:</strong> ", info$address
      )))
    }
    
    # Generating the carousel items
    carousel_items <- ""
    carousel_indicators <- ""
    for (i in 1:length(info$photo_urls)) {
      if (!is.null(info$photo_urls[[i]])) {
        active_class <- ifelse(i == 1, "active", "")  # The first item should be active
        carousel_items <- paste0(
          carousel_items,
          "<div class='carousel-item ", active_class, "'>",
          "<img src='", info$photo_urls[[i]],
          "' class='d-block w-100' style='height: 300px; object-fit: contain;' alt='Place Photo ", i, "'>",
          "</div>"
        )
        carousel_indicators <- paste0(
          carousel_indicators,
          "<button type='button' data-bs-target='#carouselExampleIndicators' data-bs-slide-to='", i - 1,
          "' class='", active_class, "' aria-current='", ifelse(i == 1, "true", "false"),
          "' aria-label='Slide ", i, "'></button>"
        )
      }
    }
    
    # Combining carousel items with other information
    carousel_html <- paste0(
      "<div id='transport-carouselExampleIndicators' class='carousel slide' data-bs-ride='carousel'>",
      "<div class='carousel-indicators'>",
      carousel_indicators,
      "</div>",
      "<div class='carousel-inner'>",
      carousel_items,
      "</div>",
      "<button class='carousel-control-prev' type='button' data-bs-target='#transport-carouselExampleIndicators' data-bs-slide='prev'>",
      "<span class='carousel-control-prev-icon' aria-hidden='true' style='background-color: rgba(0,0,0,0.5); border-radius: 50%;'></span>",
      "<span class='visually-hidden'>Previous</span>",
      "</button>",
      "<button class='carousel-control-next' type='button' data-bs-target='#transport-carouselExampleIndicators' data-bs-slide='next'>",
      "<span class='carousel-control-next-icon' aria-hidden='true' style='background-color: rgba(0,0,0,0.5); border-radius: 50%;'></span>",
      "<span class='visually-hidden'>Next</span>",
      "</button>",
      "</div>"
    )
    
    HTML(paste0(
      carousel_html,
      "<br><strong>Name:</strong> ", info$name,
      "<br><strong>Rating:</strong> ", info$rating,
      "<br><strong>Address:</strong> ", info$address
    ))
  })
  
  
}
