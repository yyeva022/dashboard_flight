#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)
library(shinydashboard)
library(tidyr)
library(formattable)
# Part 1. Data Prepartion
setwd("/Users/evayang/Google Drive/MSBA@UMN/_Portfolio/")
#flight <- read.csv("flights.csv")
airport <- read.csv("airports.csv")
airline <- read.csv("airlines.csv")
flight_sub <- read.csv("flight_sub.csv")
us <- map_data("state")
# Subset for easier computation
#flight_sub <- flight %>% filter(AIRLINE %in% c('VX','WN'))
#write.csv(flight_sub, file = "flight_sub.csv")

# Create new feature
flight_sub$DELAY <- ifelse(flight_sub$ARRIVAL_DELAY > 0, 1, 0)
# Join airport table with state
flight_all <- flight_sub %>% left_join(airport, by = c("ORIGIN_AIRPORT" = "IATA_CODE"))


# Part 2. UI Design

# Header
header <- dashboardHeader(title = "Flight Dashboard")

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Map", tabName = "map", icon = icon("map")),
    conditionalPanel("input.sidebarmenu === 'dashboard'",
                     selectInput("month", "Month:",
                                 choices=list("All year" = 99, "Jan" = 1,"Feb" = 2,"Mar" = 3,
                                              "Apr" = 4,"May" = 5, "Jun" = 6,  "Jul" = 7,
                                              "Aug" = 8,"Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12)),
                     selectInput("airline", "Airline:",
                                 choices=list("Virgin America" = "VX",
                                              "Southwest Airlines" = "WN"))),
    conditionalPanel("input.sidebarmenu === 'map'",
                     radioButtons("n","Figures need to be shown",
                                  choices=list("Number of Flight" = 2,
                                               "Cancel Rate" = 3,
                                               "Delay Rate" = 4)))
  )
)

# Content
frow1 <- fluidRow(
  valueBoxOutput("total_number")
  ,valueBoxOutput("delay_rate")
  ,valueBoxOutput("cancel_rate")
)

frow2 <- fluidRow( 
  box(
    title = "Delay Rate"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("montly_delay", click = "plot_click")),
  box(
    title = "Delay Reason"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("reason_delay", click = "plot_click"))
)

frow_map <- fluidRow(
    box(
      title = "Figures for Each State"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotOutput("map_state")
      ,height = 600, width = 1000
    )    
)

# Body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            frow1,frow2
    ),
    tabItem(tabName = "map",
            frow_map
    )
  )
)

# Put everything together
ui <- dashboardPage(title = 'Flight overview', header, sidebar, body, skin='blue')

# Part 3. Server logic 
server <- shinyServer(function(input, output) {
  
  data <- reactive({
    if (input$month ==99) {
      sub <- flight_all %>% filter(AIRLINE == input$airline)
    }
    else {
      sub <- flight_all %>% filter((AIRLINE == input$airline) & (MONTH == input$month))
    }
      return(sub)
  })
    
  output$total_number <- renderValueBox({
    total_number_value <- data() %>% nrow()
    valueBox(
      formatC(total_number_value, format = "d", big.mark = ',')
      ,subtitle = "Number of flights"
      ,color = 'olive'
    )
  })

  output$delay_rate <- renderValueBox({
      delay_rate_value <- as.numeric(data() %>% summarise(mean(DELAY, na.rm=TRUE)))
    valueBox(
      percent(delay_rate_value)
      ,subtitle = "Delay Rate"
      ,color = 'yellow'  
    )
  })
  
  output$cancel_rate <- renderValueBox({
      cancel_rate_value <- as.numeric(data() %>% summarise(mean(CANCELLED, na.rm=TRUE)))
    valueBox(
      percent(cancel_rate_value)
      ,subtitle = "Cancel Rate"
      ,color = 'red'  
    )
  })  
  
  output$montly_delay <- renderPlot({
    if (input$month ==99) {
      data() %>%
      group_by(MONTH) %>%
      summarise(p_delay = mean(DELAY, na.rm=TRUE)) %>%
      ggplot() +
      geom_line(aes(x=MONTH, y=p_delay),color="steelblue") +
      geom_point(aes(x=MONTH, y=p_delay), size=2, color="steelblue") +
      theme_bw() +
      labs(x="Month", y="Delay Rate") +
      scale_x_continuous(limits=c(1,12), breaks=c(1,3,5,7,9,11)) +
      scale_y_continuous(limits = c(0,1), breaks=c(0,0.25,0.5,0.75,1))
      }
    else {
      data() %>%
        group_by(DAY) %>%
        summarise(p_delay = mean(DELAY, na.rm=TRUE)) %>%
        ggplot() +
        geom_line(aes(x=DAY, y=p_delay),color="steelblue") +
        geom_point(aes(x=DAY, y=p_delay), size=2, color="steelblue") +
        theme_bw() +
        labs(x="Day", y="Delay Rate") +
        scale_x_continuous(limits=c(1,31), breaks=c(1,7,14,21,27)) +
        scale_y_continuous(limits = c(0,1), breaks=c(0,0.25,0.5,0.75,1))
    }
  })
  
  output$reason_delay <- renderPlot({
   data() %>% 
      select(AIR_SYSTEM_DELAY, SECURITY_DELAY, AIRLINE_DELAY, WEATHER_DELAY)  %>%  
      gather() %>% 
      filter(value > 0) %>% 
      group_by(key) %>%
      summarise(p_incidence = n()) %>%
      ggplot() + 
      geom_bar(aes(x = reorder(key,p_incidence), y = p_incidence/sum(p_incidence)), fill="steelblue",stat = "identity") + 
      coord_flip() + 
      theme_bw() +
      labs(x="Frequency", y="Delay Reason") 
  })

  output$map_state <- renderPlot({
    column_select = as.numeric(input$n)
    flight_geo <- flight_all %>% 
      group_by(STATE) %>% 
      summarise(n_flight = n(), p_cancel = mean(CANCELLED), p_delay = mean(DELAY, na.rm=TRUE)) %>%
      select(1, column_select)
    
    names(flight_geo) <- c("STATE","n")
    
    flight_geo$region <- tolower(state.name[match(flight_geo$STATE,state.abb)])
    
    ggplot() + 
      geom_map(data=us, map=us,
                        aes(x=long, y=lat, map_id=region),
                        fill="#ffffff", color="#ffffff", size=0.3) +
      geom_map(data=flight_geo, map=us,
                        aes(fill=n, map_id=region),
                        color="#ffffff", size=0.3) +
      scale_fill_continuous(low='thistle2', high='darkred', guide='colorbar', na.value = 'grey') +
      labs(x=NULL, y=NULL) +
      coord_map("mercator") +
      theme(panel.border = element_blank()) +
      theme(panel.background = element_blank()) +
      theme(axis.ticks = element_blank()) +
      theme(axis.text = element_blank())
  })
})

# Run the application 
shinyApp(ui = ui, server = server)
