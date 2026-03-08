# IMDb Data Dashboard - Entry Point
# This file loads the necessary components and starts the Shiny app

# Import using box
box::use(
  shiny[shinyApp]
)

# Set box module search path to the app directory so local modules resolve correctly
options(box.path = getwd())


# Source global first (data loading and constants)
source("global.R")

# Source ui and server
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui, server)
