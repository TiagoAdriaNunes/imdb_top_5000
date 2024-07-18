library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyWidgets)
library(reactable)
library(tidyr)
library(plotly)
library(shinycssloaders)

# Load the data
data <- tryCatch({
  read.csv("data/results_by_directors.csv", stringsAsFactors = FALSE)
}, error = function(e) {
  message("Error loading data: ", e)
  NULL
})

# Ensure data is loaded
if (is.null(data)) {
  stop("Failed to load data.")
}

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
        .sidebar-menu > .menu-item {
          margin-bottom: 0px !important;
        }
        .form-group {
          margin-bottom: 0px !important;
        }
      "))
    ),
    sidebarMenu(
      menuItem(HTML("Top 5000 Movies<br>Last Update: 07/18/2024"), tabName = "dashboard", icon = icon("dashboard")),
      fluidRow(
        column(
          width = 12,
          virtualSelectInput(
            inputId = "primaryTitle", 
            label = "Title",
            choices = unique_titles, 
            multiple = TRUE, 
            search = TRUE,
            zIndex = 0,
            noOfDisplayValues = 3,
            optionsCount = 5,
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
            zIndex = 0,
            noOfDisplayValues = 3,
            optionsCount = 5,
            showValueAsTags = TRUE,
            position = "bottom left",
            placeholder = "Enter director name..."
          ),
          
          virtualSelectInput(
            inputId = "genre", 
            label = "Genre (Max: 3)", 
            choices = unique_genres, 
            multiple = TRUE, 
            search = TRUE,
            showSelectedOptionsFirst = TRUE,
            showValueAsTags = TRUE,
            zIndex = 0,
            noOfDisplayValues = 3,
            optionsCount = 5,
            maxValues = 3,
            position = "bottom left",
            placeholder = "Select genres (Max: 3)..."
          )
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
      sliderInput("num_results", "Number of Rows to Display in Graph", 
                  min = 1, max = 20, value = 10),
      div(class = "reset-button-container", 
          actionButton("reset", "Reset Sliders", icon = icon("redo")))
    )
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Best Directors by Movies",
        width = 6,
        shinycssloaders::withSpinner(
          plotlyOutput("plot_directors_by_movies"), 
          type = 4, 
          color = "#427ea6"
        )
      ),
      box(
        title = "Best Genres by Movies",
        width = 6,
        shinycssloaders::withSpinner(
          plotlyOutput("plot_genres_by_movies"), 
          type = 4, 
          color = "#427ea6"
        )
      )
    ),
    reactableOutput("dataTable")
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Function to filter data
  filter_data <- function() {
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
  }
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    filter_data()
  })
  
  output$dataTable <- renderReactable({
    reactable(filteredData() %>% 
                select(Title_IMDb_Link, startYear, rank, averageRating, numVotes, directors, genres), 
              columns = list(
                Title_IMDb_Link = colDef(name = "Title/IMDb Link", html = TRUE),
                startYear = colDef(name = "Year", minWidth = 60, width = 60),
                rank = colDef(name = "Rank", minWidth = 60, width = 60),
                averageRating = colDef(name = "Average Rating", minWidth = 100, width = 100),
                numVotes = colDef(name = "Number of Votes", minWidth = 100, width = 100),
                directors = colDef(name = "Directors", minWidth = 200, width = 200),
                genres = colDef(name = "Genres", minWidth = 200, width = 200)
              ),
              searchable = FALSE,
              compact = TRUE,
              defaultPageSize = 10,
              pageSizeOptions = c(10, 25, 50, 100),
              showPageSizeOptions = TRUE,
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE)
  })
  
  # Function to create plots
  create_plot <- function(data, x, y, x_title, y_title) {
    if (nrow(data) == 0) {
      plot_ly() %>%
        layout(
          title = "No data found, try other filter selection.",
          xaxis = list(showline = FALSE, showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showline = FALSE, showticklabels = FALSE, zeroline = FALSE)
        )
    } else {
      plot_ly(
        data = data,
        x = x,
        y = y,
        type = "bar",
        marker = list(color = "#427ea6"),
        orientation = "h"
      ) %>%
        layout(
          xaxis = list(title = x_title),
          yaxis = list(title = y_title)
        ) %>%
        config(displayModeBar = FALSE)
    }
  }
  
  # Plot: Best Directors by Movies
  output$plot_directors_by_movies <- renderPlotly({
    plot_data <- filteredData() %>%
      separate_rows(directors, sep = ",\\s*") %>%
      group_by(directors) %>%
      summarise(movie_count = n()) %>%
      arrange(desc(movie_count)) %>%
      head(input$num_results) %>%
      mutate(directors = factor(directors, levels = rev(unique(directors))))
    
    create_plot(plot_data, ~movie_count, ~directors, "Number of Movies", "Director")
  })
  
  # Plot: Best Genres by Movies
  output$plot_genres_by_movies <- renderPlotly({
    plot_data <- filteredData() %>%
      separate_rows(genres, sep = ",\\s*") %>%
      group_by(genres) %>%
      summarise(movie_count = n()) %>%
      arrange(desc(movie_count)) %>%
      head(input$num_results) %>%
      mutate(genres = factor(genres, levels = rev(unique(genres))))
    
    create_plot(plot_data, ~movie_count, ~genres, "Number of Movies", "Genre")
  })
  
  observeEvent(input$reset, {
    updateSliderInput(session, "year", value = c(min(data$startYear, na.rm = TRUE), max(data$startYear, na.rm = TRUE)))
    updateSliderInput(session, "rank", value = c(min(data$rank, na.rm = TRUE), max(data$rank, na.rm = TRUE)))
    updateSliderInput(session, "rating", value = c(min(data$averageRating, na.rm = TRUE), max(data$averageRating, na.rm = TRUE)))
    updateSliderInput(session, "votes", value = c(min(data$numVotes, na.rm = TRUE), max(data$numVotes, na.rm = TRUE)))
    updateNumericInput(session, "num_results", value = 10)
  })
}

# Run the app
shinyApp(ui, server)