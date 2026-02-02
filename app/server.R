# Import modules using box
box::use(
  shiny[reactive]
)

box::use(
  utils/helpers[filter_data],
  modules/filters[filtersServer],
  modules/charts[chartsServer],
  modules/table[tableServer]
)

# Define server logic
server <- function(input, output, session) {
  # Call filters module and get reactive filter values
  filters <- filtersServer("home-filters", data)

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
  chartsServer("home-charts", filteredData, num_results)

  # Call table module
  tableServer("home-table", filteredData)
}
