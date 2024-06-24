library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

# Load the data
data <- read.csv("data/results_by_directors.csv")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "IMDb Data Dashboard",
                  tags$li(class = "dropdown",
                          tags$a(href = "https://www.linkedin.com/in/tiagoadrianunes/", 
                                 target = "_blank", 
                                 icon("linkedin"),
                                 "LinkedIn", 
                                 style = "color: white; padding: 10px;")),
                  tags$li(class = "dropdown",
                          tags$a(href = "https://github.com/TiagoAdriaNunes", 
                                 target = "_blank", 
                                 icon("github"),
                                 "GitHub", 
                                 style = "color: white; padding: 10px;"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(HTML("Top 5000 Movies<br>Last Update: 06/21/2024"), tabName = "dashboard", icon = icon("dashboard")),
      textInput("primaryTitle", "Title", value = ""),
      textInput("director", "Director", value = ""),
      textInput("genre", "Genre", value = ""),
      sliderInput("year", "Year", 
                  min = min(data$startYear, na.rm = TRUE), 
                  max = max(data$startYear, na.rm = TRUE), 
                  value = c(min(data$startYear, na.rm = TRUE), max(data$startYear, na.rm = TRUE))),
      sliderInput("rank", "Rank", 
                  min = min(data$rank, na.rm = TRUE), 
                  max = max(data$rank, na.rm = TRUE), 
                  value = c(min(data$rank, na.rm = TRUE), max(data$rank, na.rm = TRUE))),
      sliderInput("rating", "Average Rating", 
                  min = min(data$averageRating, na.rm = TRUE), 
                  max = max(data$averageRating, na.rm = TRUE), 
                  value = c(min(data$averageRating, na.rm = TRUE), max(data$averageRating, na.rm = TRUE))),
      sliderInput("votes", "Number of Votes", 
                  min = min(data$numVotes, na.rm = TRUE), 
                  max = max(data$numVotes, na.rm = TRUE), 
                  value = c(min(data$numVotes, na.rm = TRUE), max(data$numVotes, na.rm = TRUE)))
    )
  ),
  dashboardBody(
    DTOutput("dataTable")
  )
)

# Define server logic
server <- function(input, output) {
  filteredData <- reactive({
    data %>%
      filter(grepl(input$director, directors, ignore.case = TRUE),
             grepl(input$genre, genres, ignore.case = TRUE),
             grepl(input$primaryTitle, primaryTitle, ignore.case = TRUE),
             startYear >= input$year[1] & startYear <= input$year[2],
             rank >= input$rank[1] & rank <= input$rank[2],
             averageRating >= input$rating[1] & averageRating <= input$rating[2],
             numVotes >= input$votes[1] & numVotes <= input$votes[2])
  })
  
  output$dataTable <- renderDT({
    datatable(filteredData(), escape = FALSE, options = list(pageLength = 10), 
              colnames = c('IMDb Link', 'Title', 'Year', 'Rank', 'Average Rating', 'Number of Votes', 'Directors', 'Genres'))
  })
}

# Run the app
shinyApp(ui, server)