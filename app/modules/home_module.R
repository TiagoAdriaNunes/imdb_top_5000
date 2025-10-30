# Home Module UI
homeUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Sidebar content (filters)
    sidebar = filtersUI(ns("filters")),

    # Body content (charts and table)
    body = tagList(
      useBusyIndicators(
        spinners = TRUE,
        pulse = TRUE,
        fade = TRUE
      ),
      chartsUI(ns("charts")),
      tableUI(ns("table"))
    )
  )
}

# Home Module Server
homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Call filters module and get reactive filter values
    filters <- filtersServer("filters")

    # Create filtered data reactive based on filter values
    filteredData <- reactive({
      filter_values <- filters()
      filter_data(
        data = data,
        director = filter_values$director,
        writer = filter_values$writer,
        primaryTitle = filter_values$primaryTitle,
        year = filter_values$year,
        rank = filter_values$rank,
        rating = filter_values$rating,
        votes = filter_values$votes,
        runtime = filter_values$runtime,
        genre = filter_values$genre
      )
    })

    # Get num_results reactive
    num_results <- reactive({
      filters()$num_results
    })

    # Call charts module
    chartsServer("charts", filteredData, num_results)

    # Call table module
    tableServer("table", filteredData)
  })
}
