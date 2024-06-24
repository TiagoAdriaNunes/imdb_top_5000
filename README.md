# IMDb Data Dashboard

This GitHub repository contains the code for a Shiny dashboard written in R that allows users to explore IMDb data, specifically the top 5000 movies. The dashboard provides various filters to help users search for movies based on title, director, genre, year, rank, average rating, and number of votes.

## Features

-   **LinkedIn and GitHub Integration:** Quick access to the developer's LinkedIn and GitHub profiles.
-   **Dynamic Filters:** Users can filter movies by title, director, genre, year, rank, average rating, and number of votes.
-   **Data Table:** Displays the filtered results in a user-friendly data table.

## Installation

To run this dashboard locally, follow these steps:

1.  **Clone the repository:**

    `git clone https://github.com/TiagoAdriaNunes/imdb_top_5000.git cd imdb_top_500`

2.  **Install the required packages:**

    `install.packages(c("shiny", "shinydashboard", "DT", "dplyr"))`

3.  **Run the Shiny app:**

    `shiny::runApp()`

## Usage

-   **Title Filter:** Enter a movie title to search for specific movies.

-   **Director Filter:** Enter a director's name to find movies directed by them.

-   **Genre Filter:** Enter a genre to search for movies of that genre.

-   **Year, Rank, Rating, and Votes Filters:** Adjust the sliders to filter movies based on these criteria.

## Author

-   [Tiago Adrian Nunes](https://www.linkedin.com/in/tiagoadrianunes/)

## License

This project is licensed under the MIT License. See the `LICENSE` file for more details.

## Contributing

Contributions are welcome! Please fork this repository and submit pull requests for any enhancements or bug fixes.
