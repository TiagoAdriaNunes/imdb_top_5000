# Import modules using box
box::use(
  shiny[reactive]
)

box::use(
  . / utils / helpers[filter_data],
  . / modules / filters[filters_server],
  . / modules / charts[charts_server],
  . / modules / table[table_server]
)

# Define server logic
server <- function(input, output, session) {
  # Call filters module and get reactive filter values
  filters <- filters_server("home-filters", data)

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
  charts_server("home-charts", filtered_data, num_results)

  # Call table module
  table_server("home-table", filtered_data)
}
