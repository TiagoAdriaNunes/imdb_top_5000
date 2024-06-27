# Free memory by running garbage collection
gc()

# Load necessary libraries
library(data.table)

# Set maximum number of threads data.table can use to the maximum available
setDTthreads(0)

# Start time measurement
start_time <- Sys.time()

# Create a directory for data storage if it doesn't exist
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}

# Define file paths and URLs in a list
files <- list(
  title_crew     = "https://datasets.imdbws.com/title.crew.tsv.gz",
  name_basics    = "https://datasets.imdbws.com/name.basics.tsv.gz",
  title_ratings  = "https://datasets.imdbws.com/title.ratings.tsv.gz",
  title_basics   = "https://datasets.imdbws.com/title.basics.tsv.gz"
)

# Function to download and read files with conditions
read_and_filter <- function(url, path, select_cols, na.strings = "\\N", filters = NULL, id_filter = NULL, id_col = "tconst") {
  if (!file.exists(path)) {
    download.file(url, path, mode = "wb")
  }
  dt <- fread(path, select = select_cols, na.strings = na.strings, quote = "", showProgress = TRUE, nThread = setDTthreads(0))
  
  if (!is.null(id_filter)) {
    dt <- dt[get(id_col) %in% id_filter]
  }
  
  if (!is.null(filters)) {
    for (filter in filters) {
      dt <- dt[eval(parse(text=filter))]  
    }
  }
  
  return(dt)
}

# Load and filter datasets
title_basics <- read_and_filter(
  files$title_basics,
  "data/title.basics.tsv.gz",
  c("tconst", "titleType", "primaryTitle", "startYear", "runtimeMinutes", "genres"),
  filters = c("!(runtimeMinutes == '0' | is.na(runtimeMinutes))", "(titleType == 'movie' | titleType == 'tvMovie')")
)

title_ratings <- read_and_filter(
  files$title_ratings,
  "data/title.ratings.tsv.gz",
  c("tconst", "averageRating", "numVotes"),
  filters = c("!is.na(numVotes) & numVotes > 0")
)

# Merge, filter, and rank the data
title_basics_ratings <- merge(title_basics, title_ratings, by = "tconst")
common_tconst <- unique(title_basics_ratings$tconst)

# Remove unused data frames from memory
rm(title_basics, title_ratings)
gc()

# Continue processing other files with the list of common tconsts
title_crew <- read_and_filter(
  files$title_crew,
  "data/title.crew.tsv.gz",
  c("tconst", "directors"),
  id_filter = common_tconst,
  id_col = "tconst"
)

name_basics <- read_and_filter(
  files$name_basics,
  "data/name.basics.tsv.gz",
  c("nconst", "primaryName"),
  id_filter = unique(unlist(strsplit(title_crew$directors, ","))),
  id_col = "nconst"
)

# Extract directors data
title_crew_long <- title_crew[, .(tconst, directors = unlist(strsplit(directors, ","))), by = tconst]
setnames(title_crew_long, "tconst", "crew_tconst")
directors_name <- merge(title_crew_long, name_basics, by.x = "directors", by.y = "nconst", all.x = TRUE)
directors_name <- directors_name[, .(tconst = crew_tconst, directors = paste(primaryName, collapse = ", ")), by = crew_tconst]

# Remove title_crew data frame from memory
rm(title_crew_long)
gc()

# Ensure unique ranks by using tconst as a secondary criterion
title_basics_ratings <- title_basics_ratings[order(-averageRating * numVotes, tconst)]
title_basics_ratings[, rank := .I]
ranks <- title_basics_ratings[rank <= 5000]

result <- ranks[order(rank)][, .(tconst, primaryTitle, startYear, rank, averageRating, numVotes, genres)]
result$genres <- gsub(",([^ ])", ", \\1", result$genres)

# Remove title_basics_ratings data frame from memory
rm(title_basics_ratings, ranks)
gc()

# Remove any duplicates and ensure only necessary columns are present
directors_name <- unique(directors_name[, .(tconst, directors)])

# Merge with the result data frame
results_by_directors <- merge(result, directors_name, by = "tconst", all.x = TRUE)

# Order and select columns
results_by_directors <- results_by_directors[order(rank)][, .(tconst, primaryTitle, startYear, rank, averageRating, numVotes, directors, genres)]

# Transform tconst to clickable links that open in a new tab
results_by_directors$IMDbLink <- paste0(
  '<a href="https://www.imdb.com/title/',
  results_by_directors$tconst,
  '/" target="_blank">',
  results_by_directors$tconst,
  '</a>'
)

# Reorder columns to make IMDbLink the first column
results_by_directors <- results_by_directors[, c("IMDbLink", "primaryTitle", "startYear", "rank", "averageRating", "numVotes", "directors", "genres")]

# Save results to CSV
output_dir <- "app/data"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
write.csv(results_by_directors, file.path(output_dir, "results_by_directors.csv"), row.names = FALSE)
print(paste("File saved to:", file.path(output_dir, "results_by_directors.csv")))

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
