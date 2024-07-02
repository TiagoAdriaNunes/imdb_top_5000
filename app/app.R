library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

# Load the data
data <- read.csv("data/results_by_directors.csv")

# Extract unique movie titles and director names for selectizeInput choices
unique_titles <- unique(data$primaryTitle)
unique_directors <- unique(unlist(strsplit(as.character(data$directors), ",\\s*")))

# Extract unique genres for selectizeInput choices and order them alphabetically
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
        .reset-button-container {
          display: flex;
          justify-content: flex-end;
          padding: 10px;
        }
        .sidebar-menu .shiny-input-container {
          margin-bottom: 5px;  /* Reduce space between inputs */
        }
        .sidebar-menu .form-group {
          margin-bottom: 5px;  /* Reduce space between input groups */
        }
      "))
    ),
    sidebarMenu(
      menuItem(HTML("Top 5000 Movies<br>Last Update: 07/01/2024"), tabName = "dashboard", icon = icon("dashboard")),
      selectizeInput(
        inputId = "primaryTitle", 
        label = "Title", 
        choices = unique_titles, 
        multiple = TRUE, 
        options = list(
          create = FALSE,
          maxItems = '1',
          placeholder = "Enter movie title...",
          plugins = list('remove_button'),
          onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
          onType = I("function (str) {if (str === '') {this.close();}}")
        )
      ),
      selectizeInput(
        inputId = "director", 
        label = "Director", 
        choices = unique_directors, 
        multiple = TRUE, 
        options = list(
          create = FALSE,
          maxItems = '1',
          placeholder = "Enter director name...",
          plugins = list('remove_button'),
          onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
          onType = I("function (str) {if (str === '') {this.close();}}")
        )
      ),
      selectizeInput("genre", "Genre", choices = unique_genres, multiple = TRUE, 
                     options = list(create = FALSE, maxItems = '3', placeholder = "Select movie genres...", plugins = list('remove_button'))),
      sliderInput("year", "Year", 
                  min = min(data$startYear, na.rm = TRUE), 
                  max = max(data$startYear, na.rm = TRUE), 
                  value = c(min(data$startYear, na.rm = TRUE), max(data$startYear, na.rm = TRUE))),
      sliderInput("rank", "Rank", 
                  min = min(data$rank, na.rm = TRUE), 
                  max = max(data$rank, na.rm = TRUE), 
                  value = c(min(data$rank, na.rm = TRUE), max(data$rank, na.rm = TRUE)),
                  step = 500),
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
      filtered <- filtered %>% filter(sapply(strsplit(directors, ",\\s*"), function(d) any(input$director %in% d)))
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
        filter(sapply(strsplit(genres, ",\\s*"), function(g) all(input$genre %in% g)))
    }
    
    filtered
  })
  
  output$dataTable <- renderDT({
    datatable(filteredData() %>% select(Title_IMDb_Link, startYear, rank, averageRating, numVotes, directors, genres), 
              escape = FALSE, options = list(pageLength = 10, searchHighlight = TRUE),
              colnames = c('Title/IMDb Link', 'Year', 'Rank', 'Average Rating', 'Number of Votes', 'Directors', 'Genres'))
  })
  
  observeEvent(input$reset, {
    updateSelectizeInput(session, "primaryTitle", selected = character(0))
    updateSelectizeInput(session, "director", selected = character(0))
    updateSelectizeInput(session, "genre", selected = character(0))
    updateSliderInput(session, "year", value = c(min(data$startYear, na.rm = TRUE), max(data$startYear, na.rm = TRUE)))
    updateSliderInput(session, "rank", value = c(min(data$rank, na.rm = TRUE), max(data$rank, na.rm = TRUE)))
    updateSliderInput(session, "rating", value = c(min(data$averageRating, na.rm = TRUE), max(data$averageRating, na.rm = TRUE)))
    updateSliderInput(session, "votes", value = c(min(data$numVotes, na.rm = TRUE), max(data$numVotes, na.rm = TRUE)))
  })
}

# Run the app
shinyApp(ui, server)