box::use(
  dplyr[filter],
  plotly[plot_ly, add_annotations, layout, config]
)

#' @export
filter_data <- function(data, director, writer, primaryTitle, year, rank, rating, votes, runtime, genre) {
  filtered <- data

  # Filter by directors
  if (length(director) > 0) {
    filtered <- filtered |>
      filter(sapply(strsplit(as.character(directors), ",\\s*"), function(d)
        any(director %in% d)))
  }

  # Filter by writers
  if (length(writer) > 0) {
    filtered <- filtered |>
      filter(sapply(strsplit(as.character(writers), ",\\s*"), function(w)
        any(writer %in% w)))
  }

  # Filter by movie titles
  if (length(primaryTitle) > 0) {
    filtered <- filtered |> filter(.data$primaryTitle %in% primaryTitle)
  }

  # Extract range values to avoid variable name collisions with column names
  year_min <- year[1]
  year_max <- year[2]
  rank_min <- rank[1]
  rank_max <- rank[2]
  rating_min <- rating[1]
  rating_max <- rating[2]
  votes_min <- votes[1]
  votes_max <- votes[2]
  runtime_min <- runtime[1]
  runtime_max <- runtime[2]

  # Filter by range inputs
  filtered <- filtered |>
    filter(
      .data$startYear >= year_min & .data$startYear <= year_max,
      .data$rank >= rank_min & .data$rank <= rank_max,
      .data$averageRating >= rating_min & .data$averageRating <= rating_max,
      .data$numVotes >= votes_min & .data$numVotes <= votes_max,
      .data$runtimeMinutes >= runtime_min & .data$runtimeMinutes <= runtime_max
    )

  # Filter by genres (must match ALL selected genres)
  if (length(genre) > 0) {
    filtered <- filtered |>
      filter(sapply(strsplit(as.character(genres), ",\\s*"), function(g)
        all(genre %in% g)))
  }

  return(filtered)
}

#' @export
create_empty_plot <- function(message = "No Data Available", CHART_COLOR = "#427ea6") {
  plot_ly() |>
    add_annotations(
      text = message,
      x = 0.5,
      y = 0.5,
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      font = list(size = 20, color = CHART_COLOR)
    ) |>
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    ) |>
    config(displayModeBar = FALSE)
}
