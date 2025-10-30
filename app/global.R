# Load required libraries using box
box::use(
  DBI[dbConnect, dbExecute, dbGetQuery],
  duckdb[duckdb]
)

# Connect to DuckDB
con <- dbConnect(duckdb(), dbdir = ":memory:")

# Remove existing file if it exists
results_file <- "data/results_with_crew.csv"
if (file.exists(results_file)) {
  file.remove(results_file)
  print(paste("Removed existing file:", results_file))
}

# Download the file from GitHub
github_url <- "https://raw.githubusercontent.com/TiagoAdriaNunes/imdb_top_5000/main/app/data/results_with_crew.csv"
dir.create(dirname(results_file), showWarnings = FALSE, recursive = TRUE)
download.file(github_url, results_file, mode = "wb")
print(paste("File downloaded from GitHub to:", results_file))

# Get file creation/modification date for Last Update display
file_date <- format(file.mtime(results_file), "%Y-%m-%d")
print(paste("File last modified on:", file_date))

# Load the data into DuckDB
dbExecute(
  con,
  "CREATE TABLE IF NOT EXISTS results_with_crew
AS SELECT * FROM 'data/results_with_crew.csv'"
)

# Retrieve data from DuckDB
data <- dbGetQuery(con, "SELECT * FROM results_with_crew")

# Verify the columns in the loaded data
required_columns <- c(
  "primaryTitle",
  "startYear",
  "rank",
  "averageRating",
  "numVotes",
  "runtimeMinutes",
  "directors",
  "writers",
  "genres",
  "Title_IMDb_Link"
)
missing_columns <- setdiff(required_columns, names(data))
if (length(missing_columns) > 0) {
  stop(
    "The following required columns are missing from the data: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Extract unique values for filter choices
unique_titles <- unique(as.character(data$primaryTitle))
unique_directors <- unique(unlist(strsplit(as.character(data$directors), ",\\s*")))
unique_writers <- unique(unlist(strsplit(as.character(data$writers), ",\\s*")))
unique_genres <- sort(unique(unlist(strsplit(
  as.character(data$genres), ",\\s*"
))))

# Constants
CHART_COLOR <- "#427ea6"
SPINNER_TYPE <- 3
SPINNER_COLOR <- "#427ea6"
SPINNER_BG_COLOR <- "#FFFFFF"
