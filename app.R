# Load the packages
library(shiny)
library(shinydashboard)
library(sp)
library(rgdal)
library(tabulizer)
library(tidyverse)
library(googlesheets4)
# de authorize first, otherwise you have to login with a google account.
gs4_deauth()

# Load the shapefiles, layer="BGD_2" for zilla
zilla<- readOGR("/home/rana2hin/ShinyApps/covid19_bd/data/gadm36_BGD_shp/", layer = "gadm36_BGD_2")
zilla_gg<- fortify(zilla)

# layer="BGD_1" for divisions. to know more and download the shapefiles from https://gadm.org
div<- readOGR("/home/rana2hin/ShinyApps/covid19_bd/data/gadm36_BGD_shp/", layer = "gadm36_BGD_1")
div_gg<- fortify(div)

dist_data_url<- "https://docs.google.com/spreadsheets/d/1OejY671NcewEMf_kdsXDjgOC9l9kZy6QtRspm-WA1us/edit#gid=315257534"
div_data_url<- "https://docs.google.com/spreadsheets/d/1MTJ0pnZfCMd-eR98gxIWPLL4VAlhOgEls8pulHgt5yU/edit#gid=294514360"
districts<- read_sheet(dist_data_url)
districts<- districts[order(districts$confirmed_cases, decreasing = T), ]

values_url<- "https://docs.google.com/spreadsheets/d/1_8fd_yZRIf8h1IHMCqCNRKN5unHgt5FcCz1cVE1pkLQ/edit#gid=0"

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
            menuItem("Most Affected Cities", tabName = "cities", icon = icon("chart-bar"))
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
                        plotOutput("plot1", height = 300))
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

server<- function(input, output)  {
    data_for_map <- reactive({
        data_url<- switch(input$div_dist_selector,
                          dist = dist_data_url,
                          divs = div_data_url,
                          dist_data_url)
        d<- read_sheet(data_url)
        if (input$div_dist_selector== "divs") {
            data_gg<- div_gg %>% mutate(id= as.numeric(id))
            data_gg<- data_gg %>% left_join(d, by="id")
            data_gg
        } else {
            data_gg <- zilla_gg %>% mutate(id = as.numeric(id))
            data_gg <-  data_gg %>% left_join(d, by = "id")
            data_gg
        }
    })
    
    value_data<- read_sheet(values_url)
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
    
    output$plot1<- renderPlot({
        ggplot(data_for_map())+ aes(long, lat, group=group, fill=confirmed_cases)+
            geom_polygon()+geom_path(color="white")+coord_equal()+
            labs(title = "COVID-19 Data Visualization (Bangladesh)", caption = "Data Source: IEDCR")+
            scale_fill_viridis_c(option = "plasma", direction = -1, alpha = 0.95)+ theme_minimal()
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