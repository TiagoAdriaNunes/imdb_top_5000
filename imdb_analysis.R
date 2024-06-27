# Free memory by running garbage collection
gc()

# Load necessary libraries
library(data.table)

# Set maximum number of threads data.table can use to the maximum available
setDTthreads(parallel::detectCores())

# Start time measurement
start_time <- Sys.time()

# Create a directory for data storage if it doesn't exist
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}

# Define file paths and URLs in a list
files <- list(
  title_principals = "https://datasets.imdbws.com/title.principals.tsv.gz",
  name_basics      = "https://datasets.imdbws.com/name.basics.tsv.gz",
  title_ratings    = "https://datasets.imdbws.com/title.ratings.tsv.gz",
  title_basics     = "https://datasets.imdbws.com/title.basics.tsv.gz"
)

# Function to download and read files with conditions
read_and_filter <- function(url, path, select_cols, na.strings = "\\N", filters = NULL, id_filter = NULL, id_col = "tconst") {
  if (!file.exists(path)) {
    download.file(url, path, mode = "wb")
  }
  dt <- fread(path, select = select_cols, na.strings = na.strings, quote = "")
  
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
title_principals <- read_and_filter(
  files$title_principals,
  "data/title.principals.tsv.gz",
  c("tconst", "nconst", "category"),
  id_filter = common_tconst,
  id_col = "tconst"
)

name_basics <- read_and_filter(
  files$name_basics,
  "data/name.basics.tsv.gz",
  c("nconst", "primaryName"),
  id_filter = unique(title_principals$nconst),
  id_col = "nconst"
)

# Extract directors data
directors <- title_principals[category == "director", .(tconst, nconst)]

# Extract editors data
editors <- title_principals[category == "editor", .(tconst, nconst)]

# Remove title_principals data frame from memory
rm(title_principals)
gc()

# Merge directors and editors with name_basics
directors_name <- merge(directors, name_basics, by = "nconst", all.x = TRUE)
editors_name <- merge(editors, name_basics, by = "nconst", all.x = TRUE)

# Replace NA values in primaryName with "-"
directors_name[is.na(primaryName), primaryName := "-"]
editors_name[is.na(primaryName), primaryName := "-"]

# Aggregate director names
directors_name_aggregated <- directors_name[, .(directors = paste(primaryName, collapse = ", ")), by = tconst]

# Find tconsts without directors and use editors for them
missing_directors_tconst <- setdiff(unique(title_basics_ratings$tconst), directors_name_aggregated$tconst)
editors_name_aggregated <- editors_name[tconst %in% missing_directors_tconst, .(directors = paste(primaryName, collapse = ", ")), by = tconst]

# Combine directors and editors
combined_directors <- rbindlist(list(directors_name_aggregated, editors_name_aggregated), fill = TRUE)

# Remove temporary data frames from memory
rm(directors, editors, directors_name, editors_name, directors_name_aggregated, editors_name_aggregated, name_basics)
gc()

# Rankings: Ensure unique ranks by using tconst as a secondary criterion
title_basics_ratings <- title_basics_ratings[order(-averageRating * numVotes, tconst)]
title_basics_ratings[, rank := .I]
ranks <- title_basics_ratings[rank <= 5000]

result <- ranks[order(rank)][, .(tconst, primaryTitle, startYear, rank, averageRating, numVotes, genres)]
result$genres <- gsub(",([^ ])", ", \\1", result$genres)

# Remove title_basics_ratings data frame from memory
rm(title_basics_ratings, ranks)
gc()

# Merge with the result data frame
results_by_directors <- merge(result, combined_directors, by = "tconst", all.x = TRUE)

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

# Optionally remove the original tconst column
# results_by_directors$tconst <- NULL

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

# Calculate and print the time taken
time_taken <- end_time - start_time
print(paste("Time taken: ", time_taken))