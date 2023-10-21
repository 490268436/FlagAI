
library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(shinyWidgets)

# Load the datasets
data_res <- read.csv("../data/data_without_commas.csv")

# Load hotel data
hotels <- read.csv("../data/output_data_with_recent_prices.csv")

generate_detail_page_res <- function(name, address, seating_type, number_of_seats, rate) {
  # Restaurant and cafe detail page
  page <- fluidPage(
    titlePanel(paste("", name)),
    mainPanel(
      p(paste("Address: ", address)),
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
  
  # 函数将rate转换为星星字符串
  get_star_rating <- function(rate) {
    full_stars <- floor(rate)
    half_star <- ifelse(rate - full_stars >= 0.5, 1, 0)
    empty_stars <- 5 - full_stars - half_star
    stars <- paste(rep("★", full_stars), collapse = "")  # full stars
    stars <- paste0(stars, ifelse(half_star == 1, "☆", ""))  # half star
    stars <- paste0(stars, rep("☆", empty_stars))  # empty stars
    return(stars)
  }
  
  star_rating <- get_star_rating(rate)
  
  content <- paste0(
    "<div style='font-size: 14px; font-family: Arial, sans-serif; margin-bottom: 10px;'>",
    "<strong>", name, "</strong><br>",
    "<strong>Rating: </strong>", star_rating, "<br>"
  )
  
  # Hotel detail page
  page <- fluidPage(
    titlePanel(paste("Detailed Information -", name)),
    mainPanel(
      HTML(content),  # 使用HTML函数插入星星评分
      #p(paste("Rate:",rate)),
      p(paste("Address: ", address)),
      p(paste("Recent Average One-Night Rate ($): ", Prices)),
      plotlyOutput("seating_plot"),
      actionLink("return_map", "Return to Map")
    )
  )
  return(page)
}



hotel_icons <- list(
  "4-star" = makeIcon(
    iconUrl = "www/hotel4.png",
    iconWidth = 28, iconHeight = 28
  ),
  "5-star" = makeIcon(
    iconUrl = "www/hotel5.png",
    iconWidth = 28, iconHeight = 28
  ),
  "3-star" = makeIcon(
    iconUrl = "www/hotel3.png",
    iconWidth = 28, iconHeight = 28
  )
)

ui <- fluidPage(
  titlePanel("Melbourne Map"),
  tags$head(
    tags$style(HTML(
      ".btn-primary { background-color: #FF5733; border-color: #FF5733; }" # Modify button style
    ))
  ),
  fluidRow(
    column(width = 4,
           textInput("search_query", "Search by Name or Address", value = ""),
           radioButtons("type_selector", "Select Type", choices = c("Restaurants", "Hotels"), selected = "Restaurants"),
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


server <- function(input, output, session) {
  # Initialize the map
  map <- leaflet() %>%
    addProviderTiles(providers$CartoDB, options = providerTileOptions(minZoom = 10, maxZoom = 20)) %>%
    setView(lng = 144.9561, lat = -37.8103, zoom = 14)
  
  output$map <- renderLeaflet({
    map
  })
  
  observe({
    if (input$type_selector == "All" || input$type_selector == "Restaurants") {
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
          map <- map %>%
            addMarkers(
              data = filtered_data_res[i, ],
              lng = ~longitude,
              lat = ~latitude,
              popup = as.character(filtered_data_res$trading_name[i])
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
      if (input$type_selector == "All" || input$type_selector == "Restaurants") {
        click_info_res <- input$map_marker_click
        if (!is.null(click_info_res) && nrow(data_res) > 0) {
          clicked_lat <- click_info_res$lat
          clicked_lng <- click_info_res$lng
          selected_restaurant <- subset(data_res, latitude == clicked_lat & longitude == clicked_lng)
          if (nrow(selected_restaurant) > 0) {
            detail_page <- generate_detail_page_res(selected_restaurant$trading_name, selected_restaurant$building_address, selected_restaurant$seating_type, selected_restaurant$number_of_seats)
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
  
  output$rate_plot <- renderPlotly({
    n <- 100 # 假设有50个酒店
    
    # 生成随机评分数据，范围在0到5之间
    mean_score <- 4.2
    sd_score <- 1
    scores <- pmin(5, pmax(0, abs(rnorm(n, mean = mean_score, sd = sd_score))))
    
    current_score<-4.2
    p <- ggplot(data = data.frame(score = scores), aes(x = score)) +
      geom_histogram(binwidth = 1, fill = "blue") +
      
      geom_histogram(data = data.frame(score = scores[scores > (current_score - current_score %% 1-0.5) & scores < (current_score - current_score %% 1 + 0.5)]), 
                     binwidth = 1, fill = "yellow") +
      
      # 调整柱状图美学
      labs(title = "dis", x = "rate") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(0, 5, by = 0.5))
    
    return(p)
    
  })

  output$seating_plot <- renderPlotly({
    if (show_detail()) {
      if (input$type_selector == "Restaurants") {
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
  
  observeEvent(input$return_map, {
    show_detail(TRUE)
  })
  
  observe({
    click_info <- input$map_marker_click
    if (!is.null(click_info)) {
      show_detail(TRUE)
    }
  })
}

shinyApp(ui, server)

