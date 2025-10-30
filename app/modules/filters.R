box::use(
  shiny[NS, tagList, tags, HTML, fluidRow, column, sliderInput, div, actionButton, icon, moduleServer, observeEvent, updateSliderInput, reactive],
  shinydashboard[sidebarMenu, menuItem],
  shinyWidgets[virtualSelectInput, updateVirtualSelect]
)

#' @export
filtersUI <- function(id, file_date, data, unique_titles, unique_directors, unique_writers, unique_genres) {
  ns <- NS(id)

  # Debug: print data ranges
  print(paste("Data rows in filtersUI:", nrow(data)))
  print(paste("Votes range:", min(data$numVotes, na.rm = TRUE), "to", max(data$numVotes, na.rm = TRUE)))

  tagList(
    tags$head(tags$style(
      HTML(
        "
        .vscomp-dropbox-container {
          z-index: 99999 !important;
        }
        .sidebar-menu > .menu-item {
          margin-bottom: 0px !important;
        }
        .form-group {
          margin-bottom: 0px !important;
        }
      "
      )
    )),
    sidebarMenu(
      menuItem(
        HTML(paste0("Top 5000 Movies<br>Last Update: ", file_date)),
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      fluidRow(
        column(
          width = 12,
          virtualSelectInput(
            inputId = ns("primaryTitle"),
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
            inputId = ns("director"),
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
            inputId = ns("writer"),
            label = "Writer",
            choices = unique_writers,
            multiple = TRUE,
            search = TRUE,
            showSelectedOptionsFirst = TRUE,
            zIndex = 0,
            noOfDisplayValues = 3,
            optionsCount = 5,
            showValueAsTags = TRUE,
            position = "bottom left",
            placeholder = "Enter writer name..."
          ),
          virtualSelectInput(
            inputId = ns("genre"),
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
      sliderInput(
        ns("rank"),
        "Rank",
        min = min(data$rank, na.rm = TRUE),
        max = max(data$rank, na.rm = TRUE),
        value = c(min(data$rank, na.rm = TRUE), max(data$rank, na.rm = TRUE))
      ),
      sliderInput(
        ns("year"),
        "Year",
        min = min(data$startYear, na.rm = TRUE),
        max = max(data$startYear, na.rm = TRUE),
        value = c(
          min(data$startYear, na.rm = TRUE),
          max(data$startYear, na.rm = TRUE)
        )
      ),
      sliderInput(
        ns("runtime"),
        "Runtime (minutes)",
        min = min(data$runtimeMinutes, na.rm = TRUE),
        max = max(data$runtimeMinutes, na.rm = TRUE),
        value = c(
          min(data$runtimeMinutes, na.rm = TRUE),
          max(data$runtimeMinutes, na.rm = TRUE)
        )
      ),
      sliderInput(
        ns("rating"),
        "Average Rating",
        min = min(data$averageRating, na.rm = TRUE),
        max = max(data$averageRating, na.rm = TRUE),
        value = c(
          min(data$averageRating, na.rm = TRUE),
          max(data$averageRating, na.rm = TRUE)
        )
      ),
      sliderInput(
        ns("votes"),
        "Number of Votes",
        min = min(data$numVotes, na.rm = TRUE),
        max = max(data$numVotes, na.rm = TRUE),
        value = c(
          min(data$numVotes, na.rm = TRUE),
          max(data$numVotes, na.rm = TRUE)
        )
      ),
      sliderInput(
        ns("num_results"),
        "Number of results to Display in Charts",
        min = 1,
        max = 20,
        value = 10
      ),
      div(class = "reset-button-container", actionButton(ns("reset"), "Reset filters", icon = icon("redo"))),
      div(
        style = "padding: 15px; text-align: left; font-size: 0.8em; color: #fff;",
        HTML(
          "Information courtesy of<br>
          <strong>IMDb</strong><br>
          (<a href='https://www.imdb.com' target='_blank'>https://www.imdb.com</a>).<br>
          Used with permission."
        )
      )
    )
  )
}

#' @export
filtersServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # Reset button observer
    observeEvent(input$reset, {
      updateSliderInput(session = session, "year", value = c(
        min(data$startYear, na.rm = TRUE),
        max(data$startYear, na.rm = TRUE)
      ))
      updateSliderInput(session = session, "rank", value = c(
        min(data$rank, na.rm = TRUE),
        max(data$rank, na.rm = TRUE)
      ))
      updateSliderInput(session = session, "rating", value = c(
        min(data$averageRating, na.rm = TRUE),
        max(data$averageRating, na.rm = TRUE)
      ))
      updateSliderInput(session = session, "votes", value = c(
        min(data$numVotes, na.rm = TRUE),
        max(data$numVotes, na.rm = TRUE)
      ))
      updateSliderInput(session = session, "runtime", value = c(
        min(data$runtimeMinutes, na.rm = TRUE),
        max(data$runtimeMinutes, na.rm = TRUE)
      ))
      updateSliderInput(session = session, "num_results", value = 10)
      updateVirtualSelect(session = session, "primaryTitle", selected = character(0))
      updateVirtualSelect(session = session, "director", selected = character(0))
      updateVirtualSelect(session = session, "writer", selected = character(0))
      updateVirtualSelect(session = session, "genre", selected = character(0))
    })

    # Return reactive list of filter values
    return(
      reactive({
        list(
          director = input$director,
          writer = input$writer,
          primaryTitle = input$primaryTitle,
          year = input$year,
          rank = input$rank,
          rating = input$rating,
          votes = input$votes,
          runtime = input$runtime,
          genre = input$genre,
          num_results = input$num_results
        )
      })
    )
  })
}
