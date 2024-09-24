# IMDb Data Dashboard - Movies

This GitHub repository contains the code for a Shiny dashboard written in R that allows users to explore IMDb data, specifically the top 5000 movies. The dashboard provides various filters to help users search for movies based on title, director, writer, genre, year, rank, average rating, and number of votes. Check the dashboard: [https://tiagoadrianunes.shinyapps.io/IMDB_TOP_5000/](https://tiagoadrianunes.shinyapps.io/IMDB_TOP_5000/)

## Features

- **Interactive Shiny App**: Visualize and interact with the movie data.
- **Data Filtering**: Pre-import filtering of data to ensure better memory handling.
- **Detailed Analysis**: Various analyses including genre distribution, ratings, and more.

## Installation

To run this dashboard locally, follow these steps:

1.  **Clone the repository:**
```R
git clone https://github.com/TiagoAdriaNunes/imdb_top_5000.git
cd imdb_top_5000
```
2.  **Install the required packages:**
```R
install.packages(c("R.utils","data.table", "shiny", "shinydashboard", "dplyr", "shinyWidgets", "reactable", "tidyr", "plotly", "shinycssloaders"))
```
Or Alternatively using Renv:
```R
install.packages("renv")
```
In the terminal after clonning the repository to install all packages and restore the project:
```R
renv::restore()
```
3. **To perform the analysis, run the** `imdb_analysis.R` **script:**
```R
source("imdb_analysis.R")
```

4.  **Run the Shiny app:**
```R
shiny::runApp("./app/app.r")
```

## Usage

-   **Title Filter:** Enter a movie title to search for specific movies.

-   **Director Filter:** Enter a director's name to find movies directed by them.

-   **Genre Filter:** Enter a genre to search for movies of that genre.

-   **Year, Rank, Rating, and Votes Filters:** Adjust the sliders to filter movies based on these criteria.

## Code overview

### Detailed Code Explanation

### Data import and transformation

1. Garbage Collection and Library Loading:
```R
# Free memory by running garbage collection
gc()

# Load necessary libraries
library(data.table)

# Set maximum number of threads data.table can use to the maximum available
setDTthreads(parallel::detectCores())

# Start time measurement
start_time <- Sys.time()
```
This section initializes the script by performing garbage collection, loading required libraries, setting the maximum number of threads for data.table, and starting the time measurement for performance tracking.

2. Directory Creation and File Definitions:
```R
# Create a directory for data storage if it doesn't exist
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}

# Define file paths and URLs in a list - https://developer.imdb.com/non-commercial-datasets/
files <- list(
  title_crew     = "https://datasets.imdbws.com/title.crew.tsv.gz",
  name_basics    = "https://datasets.imdbws.com/name.basics.tsv.gz",
  title_ratings  = "https://datasets.imdbws.com/title.ratings.tsv.gz",
  title_basics   = "https://datasets.imdbws.com/title.basics.tsv.gz"
)
```
This section creates a directory for data storage if it doesn't already exist and defines the URLs for the IMDb datasets in a list.

3. Function for Downloading and Filtering Data:
```R
# Function to download and read files with conditions
read_and_filter <- function(url, path, select_cols, na.strings = "\\N", filters = NULL, id_filter = NULL, id_col = "tconst") {
  if (!file.exists(path)) {
    download.file(url, path, mode = "wb")
  }
  dt <- fread(path, select = select_cols, na.strings = na.strings, quote = "")
  
  if (!is.null(id_filter)) {
    dt <- dt[get(id_col) %in% id_filter]
  }
  
  if (!is.null(filters)) {
    for (filter in filters) {
      dt <- dt[eval(parse(text=filter))]  
    }
  }
  
  return(dt)
}
```
This function downloads and reads the data files, applying filters to ensure that only relevant data is loaded into memory.

4. Loading and Filtering Datasets:
```R
title_basics <- read_and_filter(
  files$title_basics,
  "data/title.basics.tsv.gz",
  c("tconst", "titleType", "primaryTitle", "startYear", "runtimeMinutes", "genres"),
  filters = c("!(runtimeMinutes == '0' | is.na(runtimeMinutes))", "(titleType == 'movie' | titleType == 'tvMovie')")
)

title_ratings <- read_and_filter(
  files$title_ratings,
  "data/title.ratings.tsv.gz",
  c("tconst", "averageRating", "numVotes"),
  filters = c("!is.na(numVotes) & numVotes > 0")
)
```
These lines load the title_basics and title_ratings datasets, applying filters to remove entries with missing or zero runtime and to include only movies and TV movies with valid ratings and vote counts.

5. Merging and Cleaning Data:
```R
# Merge, filter, and rank the data
title_basics_ratings <- merge(title_basics, title_ratings, by = "tconst")
common_tconst <- unique(title_basics_ratings$tconst)

# Remove unused data frames from memory
rm(title_basics, title_ratings)
gc()
```
This section merges the title_basics and title_ratings datasets on the tconst column and removes the original datasets from memory to optimize performance.

6. Loading and Merging Additional Datasets:
```R
title_crew <- read_and_filter(
  files$title_crew,
  "data/title.crew.tsv.gz",
  c("tconst", "directors"),
  id_filter = common_tconst,
  id_col = "tconst"
)

name_basics <- read_and_filter(
  files$name_basics,
  "data/name.basics.tsv.gz",
  c("nconst", "primaryName"),
  id_filter = unique(unlist(strsplit(title_crew$directors, ","))),
  id_col = "nconst"
)
```
These lines load and filter the title_principals and name_basics datasets, ensuring that only relevant records are included based on the common tconst and nconst values.

7. Extracting and Aggregating Director and Editor Data:
```R
# Extract directors data
title_crew_long <- title_crew[, .(tconst, directors = unlist(strsplit(directors, ","))), by = tconst]
setnames(title_crew_long, "tconst", "crew_tconst")
directors_name <- merge(title_crew_long, name_basics, by.x = "directors", by.y = "nconst", all.x = TRUE)
directors_name <- directors_name[, .(tconst = crew_tconst, directors = paste(primaryName, collapse = ", ")), by = crew_tconst]

# Remove title_crew data frame from memory
rm(title_crew_long)
gc()
```
8. Ranking and Finalizing Results:
```R
# Ensure unique ranks by using tconst as a secondary criterion
title_basics_ratings <- title_basics_ratings[order(-averageRating * numVotes, tconst)]
title_basics_ratings[, rank := .I]
ranks <- title_basics_ratings[rank <= 5000]

result <- ranks[order(rank)][, .(tconst, primaryTitle, startYear, rank, averageRating, numVotes, genres)]
result$genres <- gsub(",([^ ])", ", \\1", result$genres)

# Remove title_basics_ratings data frame from memory
rm(title_basics_ratings, ranks)
gc()

# Remove any duplicates and ensure only necessary columns are present
directors_name <- unique(directors_name[, .(tconst, directors)])

# Merge with the result data frame
results_by_directors <- merge(result, directors_name, by = "tconst", all.x = TRUE)

# Create the new Title/IMDb Link column
results_by_directors$IMDbLink <- paste0(
  '<a href="https://www.imdb.com/title/',
  results_by_directors$tconst,
  '" target="_blank">',
  results_by_directors$tconst,
  '</a>'
)

results_by_directors$Title_IMDb_Link <- paste0(
  '<a href="https://www.imdb.com/title/',
  results_by_directors$tconst,
  '" target="_blank">',
  results_by_directors$primaryTitle,
  '</a>'
)

# Order and select columns
results_by_directors <- results_by_directors[order(rank)][, .(tconst, primaryTitle, startYear, rank, averageRating, numVotes, directors, genres, IMDbLink, Title_IMDb_Link)]

# Save results to CSV
output_dir <- "app/data"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
write.csv(results_by_directors, file.path(output_dir, "results_by_directors.csv"), row.names = FALSE)
print(paste("File saved to:", file.path(output_dir, "results_by_directors.csv")))

# Free memory by running garbage collection
gc()

# End measuring time
end_time <- Sys.time()

# Calculate and print the time taken in minutes and seconds
time_taken <- end_time - start_time
total_seconds <- as.numeric(time_taken, units = "secs")
minutes <- floor(total_seconds / 60)
seconds <- total_seconds %% 60

print(paste("Time taken:", minutes, "minutes and", round(seconds, 2), "seconds"))
"))
```
### Shiny Dashboard

1. Data loading
```R
data <- read.csv("data/results_by_directors.csv")
```
2. User Interface (UI)

The UI is defined using dashboardPage, which creates a structured dashboard layout. The layout consists of three main parts:

2.1 Header
```R
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
)
```
2.2 Sidebar
```R
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
    menuItem(HTML("Top 5000 Movies<br>Last Update: 07/12/2024"), tabName = "dashboard", icon = icon("dashboard")),
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
        
        tags$div(
          virtualSelectInput(
            inputId = "genre", 
            label = "Genre", 
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
            placeholder = "Select movie genres..."
          ),
          `data-toggle` = "tooltip",
          `data-placement` = "right",
          title = "Select genres (Max.: 3)."
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
    div(class = "reset-button-container", 
        actionButton("reset", "Reset Sliders", icon = icon("redo")))
  )
)
```
2.3 Main Body:
```R
dashboardBody(
  reactableOutput("dataTable")
)
```
3. Server Logic
```R
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
              searchable = TRUE,
              compact = TRUE,
              defaultPageSize = 10,
              pageSizeOptions = c(10, 25, 50, 100),
              showPageSizeOptions = TRUE,
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE)
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

```

## Author

-   [Tiago Adrian Nunes](https://www.linkedin.com/in/tiagoadrianunes/)

## License

This project is licensed under the MIT License. See the `LICENSE` file for more details.

## Contributing

Contributions are welcome! Please fork this repository and submit pull requests for any enhancements or bug fixes.
