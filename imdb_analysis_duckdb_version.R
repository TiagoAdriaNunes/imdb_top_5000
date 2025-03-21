# Free memory by running garbage collection
gc()

# Load necessary libraries
library(duckdb)
library(duckplyr)
library(tidyr)
library(dbplyr)

# Initialize DuckDB connection
con <- dbConnect(duckdb())

# Start time measurement
start_time <- Sys.time()

# Create a directory for data storage if it doesn't exist
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
} else {
  # Get current date
  today <- Sys.Date()
  
  # Clean up existing .gz files that are not from today
  gz_files <- list.files(data_dir, pattern = "\\.gz$", full.names = TRUE)
  if (length(gz_files) > 0) {
    # Get file modification dates
    file_dates <- as.Date(file.info(gz_files)$mtime)
    
    # Find files that are not from today
    old_files <- gz_files[file_dates != today]
    
    if (length(old_files) > 0) {
      file.remove(old_files)
      print(paste("Removed", length(old_files), "outdated .gz files"))
    }
  }
}

# Define file paths and URLs in a list - https://developer.imdb.com/non-commercial-datasets/
files <- list(
  title_crew     = "https://datasets.imdbws.com/title.crew.tsv.gz",
  name_basics    = "https://datasets.imdbws.com/name.basics.tsv.gz",
  title_ratings  = "https://datasets.imdbws.com/title.ratings.tsv.gz",
  title_basics   = "https://datasets.imdbws.com/title.basics.tsv.gz"
)

# Function to download and read files with conditions
read_and_filter <- function(url, path, select_cols, na.strings = "\\N", filters = NULL, id_filter = NULL, id_col = "tconst") {
  # Download file if it doesn't exist
  if (!file.exists(path)) {
    tryCatch({
      download.file(url, path, mode = "wb")
    }, error = function(e) {
      stop(paste("Failed to download file:", e$message))
    })
  }
  
  # Safely create SQL for column selection
  cols_sql <- paste(dbQuoteIdentifier(con, select_cols), collapse = ", ")
  
  # Create and execute query with proper error handling
  tryCatch({
    query <- sprintf(
      "SELECT %s FROM read_csv_auto('%s', delim='\t', nullstr='\\N', ignore_errors=true, sample_size=-1)",
      cols_sql,
      path
    )
    dt <- tbl(con, sql(query))
    
    # Apply filters
    if (!is.null(id_filter)) {
      dt <- dt %>% filter(!!sym(id_col) %in% id_filter)
    }
    
    if (!is.null(filters)) {
      for (filter in filters) {
        dt <- dt %>% filter(eval(parse(text=filter)))
      }
    }
    
    return(dt)
  }, error = function(e) {
    stop(paste("Failed to process file:", e$message))
  })
}

# Load and filter initial datasets first
title_basics <- read_and_filter(
  files$title_basics,
  "data/title.basics.tsv.gz",
  c("tconst", "titleType", "primaryTitle", "startYear", "runtimeMinutes", "genres")
) %>%
  filter(
    !is.na(runtimeMinutes),
    runtimeMinutes != '0',
    titleType %in% c('movie', 'tvMovie')
  )

# Define minimum vote threshold - IMDb uses different thresholds for different lists
# For Top 250, they use around 25,000 votes as minimum
m <- 25000

# Calculate ratings from all titles
title_ratings <- read_and_filter(
  files$title_ratings,
  "data/title.ratings.tsv.gz",
  c("tconst", "averageRating", "numVotes")
) %>%
  filter(!is.na(numVotes), numVotes > 0)

# Calculate C (global weighted average)
# First collect the data to perform the calculation in R
title_ratings_collected <- title_ratings %>% collect()

# Calculate the weighted average in R
C <- weighted.mean(title_ratings_collected$averageRating, 
                   title_ratings_collected$numVotes)

print(paste("Global weighted average (C):", C))

# Create title_basics_ratings with the IMDb weighted formula
title_basics_ratings <- title_basics %>%
  inner_join(title_ratings, by = "tconst") %>%
  # Filter to ensure each title has at least m votes
  filter(numVotes >= m) %>%
  # Apply the IMDb Bayesian weighted average formula
  # WR = (v/(v+m)) × R + (m/(v+m)) × C
  # Where:
  # WR = Weighted Rating
  # R = Average Rating for the movie
  # v = Number of votes for the movie
  # m = Minimum votes required (25,000)
  # C = Mean vote across the whole report (currently 7.0)
  mutate(
    score = ((numVotes / (numVotes + m)) * averageRating) +
            ((m / (numVotes + m)) * C),
    # Round score to 1 decimal place for comparison purposes
    score_rounded = round(score, 1)
  ) %>%
  # Sort by the weighted score in descending order
  # For ties (same score_rounded), use multiple criteria:
  # 1. Exact score (not rounded)
  # 2. Number of votes (more votes is better)
  # 3. Average rating (higher rating is better
  arrange(
    desc(score_rounded),
    desc(numVotes),
    desc(score),
    desc(averageRating),
    tconst
  ) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 5000) %>%
  compute()  # Create temporary table in DuckDB

# Get the filtered tconst list
common_tconst <- title_basics_ratings %>%
  select(tconst) %>%
  collect() %>%
  pull(tconst)

# Now load other files using the filtered tconst list
title_crew <- read_and_filter(
  files$title_crew,
  "data/title.crew.tsv.gz",
  c("tconst", "directors", "writers"),
  id_filter = common_tconst
) %>%
  collect()

# Get crew IDs only from the filtered movies
crew_ids <- unique(c(
  unlist(strsplit(title_crew$directors, ",")),
  unlist(strsplit(title_crew$writers, ","))
))

# Load name_basics with only relevant crew members
name_basics <- read_and_filter(
  files$name_basics,
  "data/name.basics.tsv.gz",
  c("nconst", "primaryName"),
  id_col = "nconst",
  id_filter = crew_ids
) %>%
  collect()

# Extract directors and writers data
title_crew_long <- title_crew %>%
  separate_rows(directors, sep = ",") %>%
  separate_rows(writers, sep = ",")

# Combine directors and writers into one dataframe with appropriate roles
title_crew_long_directors <- title_crew_long %>%
  filter(!is.na(directors)) %>%
  select(tconst, nconst = directors) %>%
  mutate(role = "directors")

title_crew_long_writers <- title_crew_long %>%
  filter(!is.na(writers)) %>%
  select(tconst, nconst = writers) %>%
  mutate(role = "writers")

title_crew_long_combined <- bind_rows(title_crew_long_directors, title_crew_long_writers)

# Ensure unique ranks by using tconst as a secondary criterion
title_basics_ratings <- title_basics_ratings %>%
  select(tconst, primaryTitle, startYear, rank, averageRating, numVotes, runtimeMinutes, genres) %>%
  collect() %>%  # Materialize the data first
  mutate(genres = gsub(",([^ ])", ", \\1", genres))  # Format genres after collecting

# Merge with name_basics to get names of directors and writers
crew_names <- title_crew_long_combined %>%
  left_join(name_basics, by = "nconst") %>%  # Changed from inner_join to left_join
  mutate(primaryName = ifelse(is.na(primaryName), "Unknown", primaryName)) %>%  # Handle missing names
  group_by(tconst, role) %>%
  summarise(names = paste(unique(primaryName), collapse = ", "), .groups = 'drop') %>%
  pivot_wider(
    names_from = role,
    values_from = names,
    values_fill = list(names = NA_character_)
  )

# Merge directors and writers names with the result data frame
results_with_crew <- title_basics_ratings %>%
  left_join(crew_names, by = "tconst")

# Create the new Title/IMDb Link column
results_with_crew <- results_with_crew %>%
  mutate(
    IMDbLink = paste0('<a href="https://www.imdb.com/title/', tconst, '" target="_blank">', tconst, '</a>'),
    Title_IMDb_Link = paste0('<a href="https://www.imdb.com/title/', tconst, '" target="_blank">', primaryTitle, '</a>')
  )

# Order and select columns
results_with_crew <- results_with_crew %>%
  arrange(rank) %>%
  select(tconst, primaryTitle, startYear, rank, averageRating, numVotes, runtimeMinutes, directors, writers, genres, IMDbLink, Title_IMDb_Link)

# Save results to CSV
output_dir <- "app/data"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
write.csv(results_with_crew, file.path(output_dir, "results_with_crew.csv"), row.names = FALSE)
print(paste("File saved to:", file.path(output_dir, "results_with_crew.csv")))

# Free memory by running garbage collection
gc()

# End measuring time
end_time <- Sys.time()

# Calculate and print the time taken in minutes and seconds
time_taken <- end_time - start_time
total_seconds <- as.numeric(time_taken, units = "secs")
minutes <- floor(total_seconds / 60)
seconds <- total_seconds %% 60

print(paste("Time taken:", minutes, "minutes and", round(seconds, 2), "seconds"))

# Close DuckDB connection at the end
dbDisconnect(con, shutdown = TRUE)
