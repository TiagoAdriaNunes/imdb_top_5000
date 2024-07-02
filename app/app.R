library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(shinyWidgets)
library(shinyBS)  # Load shinyBS library

# Load the data
data <- read.csv("data/results_by_directors.csv", stringsAsFactors = FALSE)

# Extract unique movie titles and director names for virtualSelectInput choices
unique_titles <- unique(as.character(data$primaryTitle))
unique_directors <- unique(unlist(strsplit(as.character(data$directors), ",\\s*")))

# Extract unique genres for virtualSelectInput choices and order them alphabetically
unique_genres <- sort(unique(unlist(strsplit(as.character(data$genres), ",\\s*"))))

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
                          tags$a(href = "https://github.com/TiagoAdriaNunes/imdb_top_5000", 
                                 target = "_blank", 
                                 icon("github"),
                                 "GitHub", 
                                 style = "color: white; padding: 10px;"))
  ),
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        .vscomp-dropbox-container {
          z-index: 99999 !important;
        }
      "))
    ),
    sidebarMenu(
      menuItem(HTML("Top 5000 Movies<br>Last Update: 07/01/2024"), tabName = "dashboard", icon = icon("dashboard")),
      fluidRow(
        column(
          width = 12,
          virtualSelectInput(
            inputId = "primaryTitle", 
            label = "Title", 
            choices = unique_titles, 
            multiple = TRUE, 
            search = TRUE,
            noOfDisplayValues = 3,
            optionsCount = 3,
            showValueAsTags = TRUE,
            showSelectedOptionsFirst = TRUE,
            position = "bottom left",
            placeholder = "Enter movie title..."
          ),
          
          virtualSelectInput(
            inputId = "director", 
            label = "Director", 
            choices = unique_directors, 
            multiple = TRUE, 
            search = TRUE,
            showSelectedOptionsFirst = TRUE,
            noOfDisplayValues = 3,
            optionsCount = 3,
            showValueAsTags = TRUE,
            position = "bottom left",
            placeholder = "Enter director name..."
          ),
          virtualSelectInput(
            inputId = "genre", 
            label = "Genre", 
            choices = unique_genres, 
            multiple = TRUE, 
            search = TRUE,
            showSelectedOptionsFirst = TRUE,
            showValueAsTags = TRUE,
            noOfDisplayValues = 3,
            optionsCount = 3,
            maxValues = 3,
            tooltipMaxWidth = 'Any movie just have 3 genres or less.',
            position = "bottom left",
            placeholder = "Select movie genres..."
          ),
          bsTooltip("genre", "Select genres (Max. 3).", placement = "right", options = list(container = "body"))
        )
      ),
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
                  value = c(min(data$numVotes, na.rm = TRUE), max(data$numVotes, na.rm = TRUE))),
      div(class = "reset-button-container", 
          actionButton("reset", "Reset Filters", icon = icon("redo")))
    )
  ),
  dashboardBody(
    DTOutput("dataTable")
  )
)

# Define server logic
server <- function(input, output, session) {
  filteredData <- reactive({
    filtered <- data
    
    # Filter by directors
    if (length(input$director) > 0) {
      filtered <- filtered %>% 
        filter(sapply(strsplit(as.character(directors), ",\\s*"), function(d) any(input$director %in% d)))
    }
    
    # Filter by movie titles
    if (length(input$primaryTitle) > 0) {
      filtered <- filtered %>% filter(primaryTitle %in% input$primaryTitle)
    }
    
    # Filter by other inputs
    filtered <- filtered %>%
      filter(startYear >= input$year[1] & startYear <= input$year[2],
             rank >= input$rank[1] & rank <= input$rank[2],
             averageRating >= input$rating[1] & averageRating <= input$rating[2],
             numVotes >= input$votes[1] & numVotes <= input$votes[2])
    
    # Filter by genres
    if (length(input$genre) > 0) {
      filtered <- filtered %>%
        filter(sapply(strsplit(as.character(genres), ",\\s*"), function(g) all(input$genre %in% g)))
    }
    
    filtered
  })
  
  output$dataTable <- renderDT({
    datatable(filteredData() %>% 
                select(Title_IMDb_Link, startYear, rank, averageRating, numVotes, directors, genres), 
              escape = FALSE, options = list(pageLength = 10, searchHighlight = TRUE),
              colnames = c('Title/IMDb Link', 'Year', 'Rank', 'Average Rating', 'Number of Votes', 'Directors', 'Genres'))
  })
  
  observeEvent(input$reset, {
    updateSliderInput(session, "year", value = c(min(data$startYear, na.rm = TRUE), max(data$startYear, na.rm = TRUE)))
    updateSliderInput(session, "rank", value = c(min(data$rank, na.rm = TRUE), max(data$rank, na.rm = TRUE)))
    updateSliderInput(session, "rating", value = c(min(data$averageRating, na.rm = TRUE), max(data$averageRating, na.rm = TRUE)))
    updateSliderInput(session, "votes", value = c(min(data$numVotes, na.rm = TRUE), max(data$numVotes, na.rm = TRUE)))
  })
}

# Run the app
shinyApp(ui, server)
