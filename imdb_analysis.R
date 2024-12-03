# Free memory by running garbage collection
gc()

# Load necessary libraries
library(data.table)
library(dplyr)
library(tidyr)

# Set maximum number of threads data.table can use to the maximum available
setDTthreads(0)

# Start time measurement
start_time <- Sys.time()

# Create a directory for data storage if it doesn't exist
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
} else {
  # Clean up existing .gz files
  gz_files <- list.files(data_dir, pattern = "\\.gz$", full.names = TRUE)
  if (length(gz_files) > 0) {
    file.remove(gz_files)
    print(paste("Removed", length(gz_files), "existing .gz files"))
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

# Merge, filter, and rank the data using dplyr
title_basics_ratings <- title_basics %>%
  inner_join(title_ratings, by = "tconst") %>%
  arrange(desc(averageRating * numVotes), tconst) %>%
  mutate(rank = row_number())

common_tconst <- unique(title_basics_ratings$tconst)

# Remove unused data frames from memory
rm(title_basics, title_ratings)
gc()

# Continue processing other files with the list of common tconsts
title_crew <- read_and_filter(
  files$title_crew,
  "data/title.crew.tsv.gz",
  c("tconst", "directors", "writers"),
  id_filter = common_tconst,
  id_col = "tconst"
)

# Load name_basics data
name_basics <- read_and_filter(
  files$name_basics,
  "data/name.basics.tsv.gz",
  c("nconst", "primaryName"),
  id_filter = unique(c(unlist(strsplit(title_crew$directors, ",")), unlist(strsplit(title_crew$writers, ",")))),
  id_col = "nconst"
)

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

# Merge with name_basics to get names of directors and writers
crew_names <- title_crew_long_combined %>%
  inner_join(name_basics, by = "nconst") %>%
  group_by(tconst, role) %>%
  summarise(names = paste(unique(primaryName), collapse = ", "), .groups = 'drop') %>%
  pivot_wider(names_from = role, values_from = names, values_fill = list(names = NA_character_))

# Ensure unique ranks by using tconst as a secondary criterion
title_basics_ratings <- title_basics_ratings %>%
  filter(rank <= 5000) %>%
  select(tconst, primaryTitle, startYear, rank, averageRating, numVotes, genres) %>%
  mutate(genres = gsub(",([^ ])", ", \\1", genres))

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
  select(tconst, primaryTitle, startYear, rank, averageRating, numVotes, directors, writers, genres, IMDbLink, Title_IMDb_Link)

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

