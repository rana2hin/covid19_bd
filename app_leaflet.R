# Load the packages
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(googlesheets4)
library(tidyverse)

# de-authorize first, otherwise you have to login with a google account.
gs4_deauth()

# Load the shapefiles, layer="BGD_2" for zilla and "BGD_1" for divisions
# Using sf package for modern spatial data handling
zilla <- st_read("/home/rana2hin/ShinyApps/covid19_bd/data/gadm36_BGD_shp/gadm36_BGD_2.shp")
div <- st_read("/home/rana2hin/ShinyApps/covid19_bd/data/gadm36_BGD_shp/gadm36_BGD_1.shp")

# Google Sheets URLs
dist_data_url<- "https://docs.google.com/spreadsheets/d/1OejY671NcewEMf_kdsXDjgOC9l9kZy6QtRspm-WA1us/edit#gid=315257534"
div_data_url<- "https://docs.google.com/spreadsheets/d/1MTJ0pnZfCMd-eR98gxIWPLL4VAlhOgEls8pulHgt5yU/edit#gid=294514360"
values_url<- "https://docs.google.com/spreadsheets/d/1_8fd_yZRIf8h1IHMCqCNRKN5unHgt5FcCz1cVE1pkLQ/edit#gid=0"

# Read data from Google Sheets
districts <- read_sheet(dist_data_url)
districts <- districts[order(districts$confirmed_cases, decreasing = T), ]

divisions <- read_sheet(div_data_url)
value_data <- read_sheet(values_url)

# Join the attribute data with the spatial data
zilla_sf <- zilla %>%
  left_join(districts, by = c("NAME_2" = "district"))

div_sf <- div %>%
  left_join(divisions, by = c("NAME_1" = "division"))


ui<- dashboardPage(skin = "green",
    dashboardHeader(title = "COVID19-BD",
        dropdownMenu(type = "messages",
            messageItem(
                from = "Govt. Info.",
                message = "Please Stay Home!",
                time = "14:25",
                icon = icon("info")
            ),
            messageItem(
                from = "Tuhin Rana",
                message = "This App is under development!",
                time = "15:00",
                icon = icon("file-code")
            )
        )
    ),
    dashboardSidebar(
        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                          label = "Search..."),
        sidebarMenu(
            menuItem("View On Map", tabName = "map", icon = icon("map-marked-alt")),
            menuItem("Most Affected Cities", tabName = "cities", icon = icon("chart-bar")),
            menuItem("View Source Codes", href = "https://github.com/rana2hin/covid19_bd", badgeLabel = "new", badgeColor = "red", icon = icon("github")),
            menuItem("About Developer", icon = icon("info"),
                     menuSubItem("Rana Tuhin", href = "https://facebook.com/rana2hin", icon = icon("facebook")),
                     menuSubItem("Tuhin Rana", href = "https://www.linkedin.com/in/rana2hin/", icon = icon("linkedin")),
                     menuSubItem("R programming Book", href = "https://rbook.rana2hin.com", icon = icon("book-open")),
                     menuSubItem("Tuhin's Diary", href = "https://rana2hin.com", icon = icon("globe-asia"))
                     )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "map",
                fluidRow(
                  valueBoxOutput("cases"),
                  valueBoxOutput("deaths"),
                  valueBoxOutput("recovered")
                ),
                fluidRow(
                    box(status = "primary", title = "", solidHeader = T, width = 4,
                        radioButtons("div_dist_selector", "View by:",
                                     choices = c("Districts" = "dist",
                                                 "Divisions" = "divs"),
                                     )
                        ),
                    box(title = "Map", status = "success", solidHeader = T, collapsible = T, width = 8,
                        leafletOutput("leaflet_map", height = 400)) # Changed from plotOutput to leafletOutput
                ),
                fluidRow(
                    box(status = "danger", title = "Attention Please!", collapsible = T, width = 12,
                        h6("1. Dhaka City & Dhaka District are treated as same District."),
                        h6("2. Dhaka & Mymensingh are treated as same Division because of Incorrect Map Data."),
                        h6("3. App may response Slower because of Your poor connection Speed.")
                        )
                )
            ),
            tabItem(
                tabName = "cities",
                fluidRow(
                    valueBoxOutput("cases2"),
                    valueBoxOutput("deaths2"),
                    valueBoxOutput("recovered2")
                ),
                fluidRow(
                    box(status = "primary", title = "", solidHeader = T, width = 5,
                        sliderInput("n", "Number of cities:", min = 5, max = 15, value = 5)),
                    tabBox(
                        title = "COVID-19", width = 7,
                        id= "tabset1", height = "250px",
                        tabPanel("Bar Chart",
                                 plotOutput("plot2", height = 300)
                        ),
                        tabPanel("Summary",
                                 verbatimTextOutput("summary")
                        )
                    )
                )
            )
        )
    )
)

server<- function(input, output, session)  {
    # Value box outputs
    output$cases<- renderValueBox({
        valueBox(value_data$cases, "Confirmed Cases", icon = icon("virus"), color = "yellow")
    })

    output$deaths<- renderValueBox({
        valueBox(value_data$deaths, "Deaths", icon = icon("skull"), color = "red")
    })

    output$recovered<- renderValueBox({
        valueBox(value_data$recovered, "Recovered", icon = icon("hand-holding-heart"), color = "green")
    })

    output$cases2<- renderValueBox({
        valueBox(value_data$cases, "Confirmed Cases", icon = icon("virus"), color = "yellow")
    })

    output$deaths2<- renderValueBox({
        valueBox(value_data$deaths, "Deaths", icon = icon("skull"), color = "red")
    })

    output$recovered2<- renderValueBox({
        valueBox(value_data$recovered, "Recovered", icon = icon("hand-holding-heart"), color = "green")
    })

    # Reactive expression to switch between district and division data
    data_for_map <- reactive({
      if (input$div_dist_selector == "dist") {
        return(zilla_sf)
      } else {
        return(div_sf)
      }
    })

    # Create the base leaflet map
    output$leaflet_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 90.3563, lat = 23.6850, zoom = 7)
    })

    # Observer to update polygons and legend based on radio button selection
    observe({
      map_data <- data_for_map()
      
      pal <- colorNumeric(
        palette = "viridis",
        domain = map_data$confirmed_cases,
        reverse = TRUE
      )
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%d Confirmed Cases",
        map_data[[ifelse(input$div_dist_selector == 'dist', 'NAME_2', 'NAME_1')]], map_data$confirmed_cases
      ) %>% lapply(htmltools::HTML)

      leafletProxy("leaflet_map", data = map_data) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          fillColor = ~pal(confirmed_cases),
          weight = 1.5,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>%
        addLegend(pal = pal, values = ~confirmed_cases, opacity = 0.7, title = "Confirmed Cases",
                  position = "bottomright")
    })
    
    output$plot2<- renderPlot({
        x<- head(districts, input$n)
        ggplot(x, aes(x=reorder(district, -confirmed_cases), y=confirmed_cases))+ geom_bar(aes(fill=confirmed_cases), stat = "identity")+
            labs(title = "Most Affected cities (Bangladesh)", x= "Districts", caption = "Data Source: IEDCR")+
            scale_fill_viridis_c(option = "plasma", direction = -1, alpha = 0.95)+theme_minimal()+
            theme(axis.text.x=element_text(angle=45, hjust=1))
    })
    
    output$summary<- renderPrint({
        summary(data.frame("Districts"= districts$district, "Confirmed_Cases"= districts$confirmed_cases))
    })
}

shinyApp(ui, server)