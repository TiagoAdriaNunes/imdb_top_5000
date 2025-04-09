# IMDb Top 5000 Movies Dashboard

[![IMDb Movies Pipeline](https://github.com/TiagoAdriaNunes/imdb_top_5000/actions/workflows/imdb-pipeline.yml/badge.svg)](https://github.com/TiagoAdriaNunes/imdb_top_5000/actions/workflows/imdb-pipeline.yml)

This GitHub repository contains the code for a Shiny dashboard written in R that allows users to explore IMDb data, specifically the top 5000 movies. The dashboard provides various filters to help users search for movies based on title, director, writer, genre, year, rank, average rating, and number of votes. Check the dashboard: [https://tiagoadrianunes.shinyapps.io/IMDB_TOP_5000/](https://tiagoadrianunes.shinyapps.io/IMDB_TOP_5000/)

## Features

- **Interactive Data Exploration**: Filter and visualize movie data
- **Performance Optimized**: Uses DuckDB for efficient data processing
- **Visualizations**: View top directors, writers, and genres with interactive charts
- **IMDb Weighted Ranking**: Implements IMDb's Bayesian weighted average formula

## Quick Start

```bash
# Clone repository
git clone https://github.com/TiagoAdriaNunes/imdb_top_5000.git
cd imdb_top_5000

# Install dependencies
install.packages("renv")
renv::restore()

# Run analysis script (DuckDB version)
source("imdb_analysis_duckdb_version.R")

# Launch Shiny app
shiny::runApp("./app/app.R")
```

## Data Processing

The `imdb_analysis_duckdb_version.R` script:
1. Downloads and processes IMDb datasets (title basics, ratings, crew info)
2. Applies the IMDb weighted rating formula
3. Creates relationships between movies, directors, and writers
4. Exports processed data for the Shiny dashboard

## Dashboard Features

- **Smart Filtering**: Find movies by any combination of criteria
- **Interactive Charts**: Visualize directors, writers, and genres by movie count
- **Detailed Movie Information**: View comprehensive data including runtime, ratings, and links to IMDb

## Author

- [Tiago Adrian Nunes](https://www.linkedin.com/in/tiagoadrianunes/)

## License

MIT License. See the `LICENSE` file for details.

Information courtesy of IMDb (https://www.imdb.com). Used with permission.

## Contributing

Contributions are welcome! Please fork this repository and submit pull requests for any enhancements or bug fixes.
