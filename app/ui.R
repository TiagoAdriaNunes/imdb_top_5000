# Import modules using box
box::use(
  shiny[tags, icon, useBusyIndicators],
  shinydashboard[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody]
)

box::use(
  modules/filters[filtersUI],
  modules/charts[chartsUI],
  modules/table[tableUI]
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "IMDb Data Dashboard",
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://www.linkedin.com/in/tiagoadrianunes/",
        target = "_blank",
        icon("linkedin"),
        "LinkedIn",
        style = "color: white; padding: 10px;"
      )
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://github.com/TiagoAdriaNunes/imdb_top_5000",
        target = "_blank",
        icon("github"),
        "GitHub",
        style = "color: white; padding: 10px;"
      )
    )
  ),
  dashboardSidebar(
    filtersUI("home-filters", file_date, data, unique_titles, unique_directors, unique_writers, unique_genres)
  ),
  dashboardBody(
    useBusyIndicators(
      spinners = TRUE,
      pulse = TRUE,
      fade = TRUE
    ),
    chartsUI("home-charts"),
    tableUI("home-table")
  )
)
