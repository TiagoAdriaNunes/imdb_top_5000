# Home Module UI
home_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Sidebar content (filters)
    sidebar = filters_ui(ns("filters")),

    # Body content (charts and table)
    body = tagList(
      useBusyIndicators(
        spinners = TRUE,
        pulse = TRUE,
        fade = TRUE
      ),
      charts_ui(ns("charts")),
      table_ui(ns("table"))
    )
  )
}

# Home Module Server
home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Call filters module and get reactive filter values
    filters <- filters_server("filters")

    # Create filtered data reactive based on filter values
    filtered_data <- reactive({
      filter_values <- filters()
      filter_data(
        data = data,
        director = filter_values$director,
        writer = filter_values$writer,
        primary_title = filter_values$primaryTitle,
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
    charts_server("charts", filtered_data, num_results)

    # Call table module
    table_server("table", filtered_data)
  })
}
