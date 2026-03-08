# Import modules using box
box::use(
  sass[sass_file],
  shiny[icon, tags, useBusyIndicators],
  shinydashboard[dashboardBody, dashboardHeader, dashboardPage, dashboardSidebar],
)

box::use(
  . / modules / filters[filters_ui],
  . / modules / charts[charts_ui],
  . / modules / table[table_ui]
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
        class = "header-link",
        icon("linkedin"),
        "LinkedIn"
      )
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://github.com/TiagoAdriaNunes/imdb_top_5000",
        target = "_blank",
        class = "header-link",
        icon("github"),
        "GitHub"
      )
    )
  ),
  dashboardSidebar(
    filters_ui(
      "home-filters",
      file_date,
      data,
      unique_titles,
      unique_directors,
      unique_writers,
      unique_genres
    )
  ),
  dashboardBody(
    tags$head(tags$style(sass_file("static/custom.scss"))),
    useBusyIndicators(
      spinners = TRUE,
      pulse = TRUE,
      fade = TRUE
    ),
    charts_ui("home-charts"),
    table_ui("home-table")
  )
)
