# Free memory by running garbage collection
gc()

# Load necessary libraries
library(duckdb)
library(duckplyr)
library(tidyr)
library(dbplyr)
library(logger)
library(glue)
library(curl)

# Override dplyr methods with duckplyr implementations
duckplyr::methods_overwrite()

# Initialize DuckDB connection
con <- dbConnect(duckdb())

# Start time measurement
start_time <- Sys.time()
log_info("Script started")

# Create a directory for data storage if it doesn't exist
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
} else {
  gz_files <- list.files(data_dir, pattern = "\\.gz$", full.names = TRUE)
  old_files <- gz_files[as.Date(file.info(gz_files)$mtime) < Sys.Date()]
  if (length(old_files) > 0) {
    file.remove(old_files)
    log_info("Removed {length(old_files)} outdated .gz file(s) from previous days")
  }
}

# Helper to log elapsed time for a step
elapsed <- function(t0) round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)

# Define file paths and URLs in a list
# https://developer.imdb.com/non-commercial-datasets/
files <- list(
  title_crew = "https://datasets.imdbws.com/title.crew.tsv.gz",
  name_basics = "https://datasets.imdbws.com/name.basics.tsv.gz",
  title_ratings = "https://datasets.imdbws.com/title.ratings.tsv.gz",
  title_basics = "https://datasets.imdbws.com/title.basics.tsv.gz"
)

# Validate a downloaded TSV.gz file by reading its header via DuckDB.
# Deletes the file and stops if corrupt or missing expected columns.
validate_tsv_gz <- function(path, expected_cols) {
  result <- tryCatch(
    dbGetQuery(con, glue(
      "SELECT * FROM read_csv_auto('{path}', delim='\\t', nullstr='\\N') LIMIT 0"
    )),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    file.remove(path)
    stop(glue("Corrupt file removed ({basename(path)}): {result$message}"))
  }
  missing <- setdiff(expected_cols, names(result))
  if (length(missing) > 0) {
    file.remove(path)
    stop(glue(
      "Unexpected structure in {basename(path)}, missing columns: {paste(missing, collapse=', ')}. File removed."
    ))
  }
  log_info("  Structure OK: {basename(path)} has all expected columns")
}

# Returns the Content-Length of a remote URL via a HEAD request, or NA on failure.
remote_size <- function(url) {
  tryCatch({
    resp <- curl_fetch_memory(url, handle = new_handle(nobody = TRUE, followlocation = TRUE))
    hdrs <- parse_headers(rawToChar(resp$headers), multiple = FALSE)
    cl   <- hdrs[grepl("^content-length:", hdrs, ignore.case = TRUE)]
    if (length(cl) == 0) return(NA_real_)
    as.numeric(trimws(sub("(?i)content-length:\\s*", "", cl[length(cl)], perl = TRUE)))
  }, error = function(e) NA_real_)
}

# Function to download and create a lazy DuckDB tbl from an IMDb TSV.gz file
read_and_filter <- function(
  url,
  path,
  select_cols,
  id_filter = NULL,
  id_col = "tconst"
) {
  if (!file.exists(path)) {
    log_info("Downloading {basename(path)}...")
    tryCatch(
      curl_download(
        url, path,
        handle = new_handle(timeout = 600, connecttimeout = 30),
        quiet = FALSE
      ),
      error = function(e) stop(paste("Failed to download:", e$message))
    )
    validate_tsv_gz(path, select_cols)
  } else {
    remote <- remote_size(url)
    local  <- file.info(path)$size
    if (!is.na(remote) && local != remote) {
      log_warn(
        "Size mismatch for {basename(path)}: local={local} bytes, remote={remote} bytes — consider deleting and re-running"
      )
    } else {
      log_info("  Size OK: {basename(path)} ({local} bytes)")
    }
  }

  cols_sql <- paste(dbQuoteIdentifier(con, select_cols), collapse = ", ")
  dt <- tbl(con, sql(glue(
    "SELECT {cols_sql} FROM read_csv_auto('{path}', delim='\\t', nullstr='\\N', ignore_errors=true)"
  )))

  if (!is.null(id_filter)) {
    dt <- dt |> filter(!!sym(id_col) %in% id_filter)
  }

  dt
}

# Define minimum vote threshold
# IMDb uses different thresholds for different lists
# For Top 250, they use around 25,000 votes as minimum
m <- 25000

# [1] Load title_basics and title_ratings (lazy — no scan yet)
t <- Sys.time()
log_info("[1/7] Building title_basics + title_ratings lazy queries...")
title_basics <- read_and_filter(
  files$title_basics,
  "data/title.basics.tsv.gz",
  c(
    "tconst",
    "titleType",
    "primaryTitle",
    "startYear",
    "runtimeMinutes",
    "genres"
  )
) |>
  filter(
    !is.na(runtimeMinutes),
    runtimeMinutes != "0",
    titleType %in% c("movie", "tvMovie")
  )

title_ratings <- read_and_filter(
  files$title_ratings,
  "data/title.ratings.tsv.gz",
  c("tconst", "averageRating", "numVotes")
) |>
  filter(!is.na(numVotes), numVotes > 0)
log_info("[1/7] Done in {elapsed(t)}s")

# [2] Compute global weighted average in DuckDB
t <- Sys.time()
log_info("[2/7] Computing global weighted average (C)...")
global_avg <- title_ratings |>
  summarise(global_avg = sum(averageRating * numVotes) / sum(numVotes)) |>
  collect() |>
  pull(global_avg)
log_info("[2/7] Done in {elapsed(t)}s — C = {round(global_avg, 4)}")

# [3] Build and materialise top-5000 rankings in DuckDB
# IMDb Bayesian weighted formula: WR = (v/(v+m)) * R + (m/(v+m)) * C
t <- Sys.time()
log_info("[3/7] Computing top-5000 rankings (join + sort + compute)...")
title_basics_ratings <- title_basics |>
  inner_join(title_ratings, by = "tconst") |>
  filter(numVotes >= m) |>
  mutate(
    score = ((numVotes / (numVotes + m)) * averageRating) +
      ((m / (numVotes + m)) * global_avg),
    score_rounded = round(score, 1)
  ) |>
  arrange(
    desc(score_rounded),
    desc(numVotes),
    desc(score),
    desc(averageRating),
    tconst
  ) |>
  mutate(rank = row_number()) |>
  filter(rank <= 5000) |>
  compute()
log_info("[3/7] Done in {elapsed(t)}s")

# [4] Load title_crew for the top-5000 titles
t <- Sys.time()
log_info("[4/7] Loading title_crew for top-5000 titles...")
# title_basics_ratings is already in DuckDB — join directly instead of
# collecting tconsts to R and passing back as a giant IN (...) clause
title_crew <- read_and_filter(
  files$title_crew,
  "data/title.crew.tsv.gz",
  c("tconst", "directors", "writers")
) |>
  inner_join(title_basics_ratings |> select(tconst), by = "tconst") |>
  collect()
log_info("[4/7] Done in {elapsed(t)}s — {nrow(title_crew)} crew rows")

# [5] Load name_basics filtered to relevant crew members
t <- Sys.time()
log_info("[5/7] Loading name_basics for crew members...")
crew_ids <- unique(c(
  unlist(strsplit(title_crew$directors, ",")),
  unlist(strsplit(title_crew$writers, ","))
))

name_basics <- read_and_filter(
  files$name_basics,
  "data/name.basics.tsv.gz",
  c("nconst", "primaryName"),
  id_col = "nconst",
  id_filter = crew_ids
) |>
  collect()
log_info("[5/7] Done in {elapsed(t)}s — {length(crew_ids)} unique crew IDs, {nrow(name_basics)} names loaded")

# [6] Aggregate crew names in DuckDB using string_split/unnest/string_agg
t <- Sys.time()
log_info("[6/7] Aggregating crew names in DuckDB...")
duckdb::duckdb_register(con, "title_crew_db", title_crew)
duckdb::duckdb_register(con, "name_basics_db", name_basics)

crew_names <- tbl(con, sql("
  WITH crew_expanded AS (
    SELECT tconst, 'directors' AS role,
           unnest(string_split(directors, ',')) AS nconst
    FROM title_crew_db WHERE directors IS NOT NULL
    UNION ALL
    SELECT tconst, 'writers' AS role,
           unnest(string_split(writers, ',')) AS nconst
    FROM title_crew_db WHERE writers IS NOT NULL
  ),
  crew_with_names AS (
    SELECT DISTINCT c.tconst, c.role,
           COALESCE(n.primaryName, 'Unknown') AS primaryName
    FROM crew_expanded c
    LEFT JOIN name_basics_db n ON c.nconst = n.nconst
  )
  SELECT tconst, role, string_agg(primaryName, ', ') AS names
  FROM crew_with_names
  GROUP BY tconst, role
")) |>
  collect() |>
  pivot_wider(
    names_from = role,
    values_from = names,
    values_fill = list(names = NA_character_)
  )

duckdb::duckdb_unregister(con, "title_crew_db")
duckdb::duckdb_unregister(con, "name_basics_db")
log_info("[6/7] Done in {elapsed(t)}s")

# [7] Collect rankings, join crew, build links, write CSV
t <- Sys.time()
log_info("[7/7] Collecting rankings, joining crew, writing CSV...")
title_basics_ratings <- title_basics_ratings |>
  select(
    tconst,
    primaryTitle,
    startYear,
    rank,
    averageRating,
    numVotes,
    runtimeMinutes,
    genres
  ) |>
  mutate(genres = regexp_replace(genres, ",([^ ])", ", \\1", "g")) |>
  collect()

results_with_crew <- title_basics_ratings |>
  left_join(crew_names, by = "tconst") |>
  mutate(
    IMDbLink = paste0(
      '<a href="https://www.imdb.com/title/',
      tconst,
      '" target="_blank">',
      tconst,
      "</a>"
    ),
    Title_IMDb_Link = paste0(
      '<a href="https://www.imdb.com/title/',
      tconst,
      '" target="_blank">',
      primaryTitle,
      "</a>"
    )
  ) |>
  arrange(rank) |>
  select(
    tconst,
    primaryTitle,
    startYear,
    rank,
    averageRating,
    numVotes,
    runtimeMinutes,
    directors,
    writers,
    genres,
    IMDbLink,
    Title_IMDb_Link
  )

output_dir <- "app/data"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
write.csv(
  results_with_crew,
  file.path(output_dir, "results_with_crew.csv"),
  row.names = FALSE
)
log_info("[7/7] Done in {elapsed(t)}s — saved to {file.path(output_dir, 'results_with_crew.csv')}")

gc()

# Total time
total_seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
log_info("Total time: {floor(total_seconds / 60)}m {round(total_seconds %% 60, 2)}s")

# Close DuckDB connection at the end
dbDisconnect(con, shutdown = TRUE)
