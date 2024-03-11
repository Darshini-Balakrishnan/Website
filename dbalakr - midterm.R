poverty = read.csv("Poverty.csv")

library(tidyverse)

poverty$X..of.population <- gsub(",", ".", poverty$X..of.population)

mapdata <- map_data("world") %>% as_tibble()

poverty <- poverty %>% rename(region = Country)

mapdata <- left_join(mapdata, poverty, by="region")

mapdata <-mapdata %>% filter(!is.na(mapdata$Population))

str(mapdata)

mapdata$X..of.population <- as.numeric(as.character(mapdata$X..of.population))

library(plotly)
map1 <- ggplot(mapdata, aes(x = long, y = lat, group = group, text = paste(region, ": ", X..of.population, "%"))) +
  geom_polygon(aes(fill = X..of.population), color = "black") +
  scale_fill_gradient(low = "orange", high = "red", name = "Poverty %") +
  labs(title = "Poverty Percentage by Region") +
  theme_minimal()

map1_plotly <- ggplotly(map1, tooltip = "text")

map1_plotly

library(shiny)
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = "Poverty Data by Region", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    conditionalPanel(
      condition = "input.graphType === 'Map'",
      selectInput("regionDropdown", "Select Region:", choices = c("World", unique(mapdata$region)))
    ),
    selectInput("graphType", "Select Graph Type:", choices = c("Map", "Bar")),
    conditionalPanel(
      condition = "input.graphType === 'Bar'",
      selectizeInput("selectedValues", "Select Country(s):", choices = unique(mapdata$region), multiple = TRUE, selected = unique(mapdata$region)[1])
    ),
    actionButton("updateButton", "Update View"),
    tags$head(tags$style(HTML('
      .skin-blue .main-sidebar { background-color: #1f4d6e; }
      .skin-blue .main-header .navbar { background-color: #1f4d6e; }
      .skin-blue .main-header .logo { background-color: #1f4d6e; }
    ')))
  ),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("plotOutput"), width = 12)
    )
  )
)

server <- function(input, output) {
  selected_country <- reactive({
    if (length(input$selectedValues) == 0) {
      unique(mapdata$region)[1]
    } else {
      input$selectedValues
    }
  })
  output$plotOutput <- renderPlotly({
    if (input$regionDropdown == "World") {
      filteredData <- mapdata
    } else {
      filteredData <- mapdata[mapdata$region == input$regionDropdown, ]
    }
    
    if (input$graphType == "Map") {
      Map <- ggplot(filteredData, aes(x = long, y = lat, group = group, text = paste(region, ": ", Total.number.of.people.living.in.extreme.poverty))) +
        geom_polygon(aes(fill = X..of.population), color = "black") +
        scale_fill_gradient(low = "light green", high = "red", name = "Poverty %") +
        labs(title = paste("Poverty Percentage in", input$regionDropdown)) +
        theme_minimal() +
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
        coord_fixed(1.3)
      
      ggplotly(Map, tooltip = "text")
    } else if (input$graphType == "Bar") {
     
      povertybar <- poverty[poverty$region %in% input$selectedValues, ]
      povertybar$Total.number.of.people.living.in.extreme.poverty <- gsub(",", "", povertybar$Total.number.of.people.living.in.extreme.poverty)
      povertybar$Total.number.of.people.living.in.extreme.poverty <- as.numeric(povertybar$Total.number.of.people.living.in.extreme.poverty)
  
      Bargraph <- ggplot(povertybar, aes(x = region, y = Total.number.of.people.living.in.extreme.poverty, fill = region, text = paste("Population:", Total.number.of.people.living.in.extreme.poverty, "M"))) +
        geom_bar(stat = "identity") +
        scale_fill_discrete(name = "Region") + 
        labs(title = "Poverty Comparison among Regions",
             y = "Population (M)", x = "Country") +
        theme_minimal() +
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))+
        scale_y_continuous(labels = scales::comma)
      
      ggplotly(Bargraph, tooltip = "text")
    }
  })
}
shinyApp(ui, server)