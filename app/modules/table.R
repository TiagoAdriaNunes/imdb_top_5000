box::use(
  shiny[NS, moduleServer],
  reactable[reactableOutput, renderReactable, reactable, colDef, colFormat],
  dplyr[select],
  utils[head]
)

#' @export
tableUI <- function(id) {
  ns <- NS(id)

  reactableOutput(ns("dataTable"))
}

#' @export
tableServer <- function(id, filteredData) {
  moduleServer(id, function(input, output, session) {

    output$dataTable <- renderReactable({
      data_to_display <- filteredData()
      print(paste("Table: filteredData has", nrow(data_to_display), "rows"))

      data_selected <- data_to_display |>
        select(
          rank,
          Title_IMDb_Link,
          startYear,
          runtimeMinutes,
          averageRating,
          numVotes,
          directors,
          writers,
          genres
        )

      print(paste("Table: after select has", nrow(data_selected), "rows"))

      reactable(
        data_selected,
        columns = list(
          rank = colDef(name = "Rank", minWidth = 50),
          Title_IMDb_Link = colDef(
            name = "Title/IMDb Link",
            html = TRUE,
            minWidth = 220
          ),
          startYear = colDef(name = "Year", minWidth = 50),
          runtimeMinutes = colDef(
            name = "Runtime",
            minWidth = 80,
            cell = function(value) {
              if (is.na(value) || value == 0) {
                return("-")
              } else {
                hours <- floor(value / 60)
                minutes <- value %% 60
                if (hours > 0) {
                  if (minutes > 0) {
                    return(paste(hours, "h", minutes, "m"))
                  } else {
                    return(paste(hours, "h"))
                  }
                } else {
                  return(paste(minutes, "m"))
                }
              }
            }
          ),
          averageRating = colDef(name = "Average Rating", minWidth = 70),
          numVotes = colDef(
            name = "Number of Votes",
            minWidth = 80,
            format = colFormat(
              separators = TRUE,
              digits = 0,
              locales = "en-US"
            )
          ),
          directors = colDef(name = "Directors", minWidth = 150, na = "-"),
          writers = colDef(name = "Writers", minWidth = 200, na = "-"),
          genres = colDef(name = "Genres", minWidth = 180)
        ),
        searchable = FALSE,
        compact = TRUE,
        defaultPageSize = 10,
        pageSizeOptions = c(10, 25, 50, 100),
        showPageSizeOptions = TRUE,
        bordered = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })
  })
}
