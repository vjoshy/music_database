# Music Database Analysis Shiny App

## Overview

This Shiny app provides an interactive analysis of a music database using various visualizations. The app is designed to explore sales data, employee performance, and customer metrics. This dashboard can also be found [here](https://ddfr4c-unmaskedcrusader.shinyapps.io/Music_Db_Analysis/)

## Features

-   **Top Albums in USA**: Displays a bar chart of the top genres in the USA based on the percentage of tracks sold.

-   **Employee Sales Performance**: Shows a bar chart of the total albums sold by the top three employees.

-   **Sales by Country**:

    -   Total Sales: Bar chart depicting total sales in each country.
    -   Number of Customers: Polar bar chart illustrating the number of customers in each country.
    -   Customer Lifetime Value: Scatter plot of customer lifetime value in each country.

-   **Albums vs Tracks**: Compares the percentage of album purchases versus individual track purchases.

## Data Source

The app uses the Chinook Database, which is a sample database representing a digital media store.

![<https://dq-content.s3.amazonaws.com/374/chinook-schema.svg>](chinook-schema.svg)

## How to Run the App

1.  Install required packages (if not already installed) by running the following command in R:

    ``` r
       install.packages(c("shiny", "ggplot2", "dplyr", "RSQLite", "plotly"))
    ```

2.  Run app with following code:

    ``` r_2
       library(shiny)
       runApp("path/to/app/directory")
    ```
