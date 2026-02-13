box::use(
  shiny[NS, fluidRow, column, moduleServer],
  shinydashboard[box],
  shinycssloaders[withSpinner],
  plotly[plotlyOutput, renderPlotly, plot_ly, add_annotations, layout, config],
  tidyr[separate_rows],
  dplyr[filter, group_by, summarise, n, arrange, desc, mutate],
  utils[head],
  logger[log_error],
  ../utils/helpers[create_empty_plot]
)

#' @export
chartsUI <- function(id, SPINNER_TYPE = 3, SPINNER_COLOR = "#427ea6", SPINNER_BG_COLOR = "#FFFFFF") {
  ns <- NS(id)

  fluidRow(
    box(
      title = "Best Directors, Writers, and Genres by Movies",
      width = 12,
      fluidRow(
        column(
          width = 4,
          withSpinner(
            plotlyOutput(ns("plot_directors_by_movies")),
            type = SPINNER_TYPE,
            color = SPINNER_COLOR,
            color.background = SPINNER_BG_COLOR
          )
        ),
        column(
          width = 4,
          withSpinner(
            plotlyOutput(ns("plot_writers_by_movies")),
            type = SPINNER_TYPE,
            color = SPINNER_COLOR,
            color.background = SPINNER_BG_COLOR
          )
        ),
        column(
          width = 4,
          withSpinner(
            plotlyOutput(ns("plot_genres_by_movies")),
            type = SPINNER_TYPE,
            color = SPINNER_COLOR,
            color.background = SPINNER_BG_COLOR
          )
        )
      )
    )
  )
}

#' @export
chartsServer <- function(id, filteredData, num_results, CHART_COLOR = "#427ea6") {
  moduleServer(id, function(input, output, session) {

    # Plot: Best Directors by Movies
    output$plot_directors_by_movies <- renderPlotly({
      # Get filtered data
      plot_data <- tryCatch({
        filteredData() |>
          separate_rows(directors, sep = ",\\s*") |>
          filter(!is.na(directors), directors != "", directors != "-", directors != "NA") |>
          group_by(directors) |>
          summarise(movie_count = n()) |>
          arrange(desc(movie_count)) |>
          head(num_results()) |>
          mutate(directors = factor(directors, levels = rev(unique(directors))))
      }, error = function(e) {
        log_error("Error in directors plot: {e$message}")
        NULL
      })

      # Check if data exists and has rows
      if (is.null(plot_data) || nrow(plot_data) == 0) {
        return(create_empty_plot("No Director Data Available"))
      }

      # Create plot with data
      plot_ly(
        data = plot_data,
        x = ~ movie_count,
        y = ~ directors,
        type = "bar",
        marker = list(color = CHART_COLOR),
        orientation = "h"
      ) |>
        layout(
          xaxis = list(title = "Number of Movies"),
          yaxis = list(title = "Director")
        ) |>
        config(displayModeBar = FALSE)
    })

    # Plot: Best Writers by Movies
    output$plot_writers_by_movies <- renderPlotly({
      # Get filtered data
      plot_data <- tryCatch({
        filteredData() |>
          separate_rows(writers, sep = ",\\s*") |>
          filter(!is.na(writers), writers != "", writers != "-", writers != "NA") |>
          group_by(writers) |>
          summarise(movie_count = n()) |>
          arrange(desc(movie_count)) |>
          head(num_results()) |>
          mutate(writers = factor(writers, levels = rev(unique(writers))))
      }, error = function(e) {
        log_error("Error in writers plot: {e$message}")
        NULL
      })

      # Check if data exists and has rows
      if (is.null(plot_data) || nrow(plot_data) == 0) {
        return(create_empty_plot("No Writer Data Available"))
      }

      # Create plot with data
      plot_ly(
        data = plot_data,
        x = ~ movie_count,
        y = ~ writers,
        type = "bar",
        marker = list(color = CHART_COLOR),
        orientation = "h"
      ) |>
        layout(
          xaxis = list(title = "Number of Movies"),
          yaxis = list(title = "Writer")
        ) |>
        config(displayModeBar = FALSE)
    })

    # Plot: Best Genres by Movies
    output$plot_genres_by_movies <- renderPlotly({
      # Get filtered data
      plot_data <- tryCatch({
        filteredData() |>
          separate_rows(genres, sep = ",\\s*") |>
          filter(!is.na(genres), genres != "", genres != "-") |>
          group_by(genres) |>
          summarise(movie_count = n()) |>
          arrange(desc(movie_count)) |>
          head(num_results()) |>
          mutate(genres = factor(genres, levels = rev(unique(genres))))
      }, error = function(e) {
        log_error("Error in genres plot: {e$message}")
        NULL
      })

      # Check if data exists and has rows
      if (is.null(plot_data) || nrow(plot_data) == 0) {
        return(create_empty_plot("No Genre Data Available"))
      }

      # Create plot with data
      plot_ly(
        data = plot_data,
        x = ~ movie_count,
        y = ~ genres,
        type = "bar",
        marker = list(color = CHART_COLOR),
        orientation = "h"
      ) |>
        layout(
          xaxis = list(title = "Number of Movies"),
          yaxis = list(title = "Genre")
        ) |>
        config(displayModeBar = FALSE)
    })
  })
}
