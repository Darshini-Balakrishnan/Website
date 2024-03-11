poverty = read.csv("Poverty.csv")

poverty_year = read.csv("C:/Users/darsh/Desktop/STAT 515/Mid Project/Population in poverty.csv")

poverty_year$Entity <- gsub("\\(.*?\\)", "", poverty_year$Entity)

poverty_year <- poverty_year %>% rename(Country = Entity)

poverty_year <- poverty_year %>% rename(Poverty.Percentage = X.2.15.a.day...Share.of.population.in.poverty)

library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel("Country Poverty Percentage Over Years"),
  selectizeInput("countryInput", "Select Country(s):",
                 choices = unique(poverty_year$Country),
                 multiple = TRUE, selected = unique(poverty_year$Country)[1]),
  plotlyOutput("povertyPlot")
)

server <- function(input, output) {
  selected_country <- reactive({
    if (length(input$countryInput) == 0) {
      unique(poverty_year$Country)[1]
    } else {
      input$countryInput
    }
  })
  
  output$povertyPlot <- renderPlotly({
    filtered_data <- poverty_year[poverty_year$Country %in% selected_country(), ]
    
    p <- ggplot(filtered_data, aes(x = Year, y = Poverty.Percentage, color = Country)) +
      geom_line() +
      geom_point(aes(text = paste("Year:", Year, "<br>Population in Poverty (%):", Poverty.Percentage))) +
      theme_minimal() +
      labs(title = paste("Poverty Percentage Over Years"),
           y = "Poverty Percentage", x = "Year")
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)
