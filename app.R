# Install required packages if not already installed
# install.packages(c("shiny", "ggplot2", "dplyr", "RSQLite"))

library(shiny)
library(ggplot2)
library(dplyr)
library(RSQLite)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Music Database Analysis"),
  
  tabsetPanel(
    tabPanel("Top Albums in USA",
             plotlyOutput("genre_plot")
    ),
    
    tabPanel("Employee Sales Performance",
             plotlyOutput("employee_sales_plot")
    ),
    
    tabPanel("Sales by Country",
             tabsetPanel(
               tabPanel("Total Sales",
                        plotlyOutput("country_metrics_plot_1")
               ),
               tabPanel("Number of Customers",
                        plotOutput("country_metrics_plot_2")
               ),
               tabPanel("Customer Lifetime Value",
                        plotlyOutput("country_metrics_plot_3")
               )
             )
    ),
    
    tabPanel("Albums vs Tracks",
             plotlyOutput("albums_vs_tracks_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Load Chinook Database
  db <- 'chinook.db'
  
  # Function to run queries
  run_query <- function(query){
    conn <- dbConnect(SQLite(), db)
    result <- dbGetQuery(conn, query)
    dbDisconnect(conn)
    
    return(result)
  }
  
  albums_to_purchase = ' WITH usa_tracks_sold AS(
                            SELECT il.* FROM invoice_line il
                            INNER JOIN invoice i on il.invoice_id = i.invoice_id
                            INNER JOIN customer c on i.customer_id = c.customer_id
                            WHERE c.country = "USA")
                        SELECT 
                            g.name genre, 
                            count(uts.invoice_line_id) tracks_sold, 
                            cast(count(uts.invoice_line_id) AS FLOAT)/(
                              SELECT COUNT(*) FROM usa_tracks_sold
                            ) percentage_sold
                          FROM usa_tracks_sold uts
                          INNER JOIN track t on t.track_id = uts.track_id
                          INNer JOIN genre g on g.genre_id = t.genre_id
                          GROUP BY 1
                          ORDER BY 2 DESC
                          LIMIT 10;
                          '
  employee_sales_performance = 'WITH customer_support_rep_sales AS
                                  (
                                   SELECT
                                       i.customer_id,
                                       c.support_rep_id,
                                       SUM(i.total) total
                                   FROM invoice i
                                   INNER JOIN customer c ON i.customer_id = c.customer_id
                                   GROUP BY 1,2
                                  )
                              
                              SELECT
                                  e.first_name || " " || e.last_name employee,
                                  e.hire_date,
                                  SUM(csrs.total) total_sales
                              FROM customer_support_rep_sales csrs
                              INNER JOIN employee e ON e.employee_id = csrs.support_rep_id
                              GROUP BY 1;
                              '
  
  sales_by_country = 'WITH country_or_other AS
                          (
                           SELECT
                             CASE
                                 WHEN (
                                       SELECT count(*)
                                       FROM customer
                                       where country = c.country
                                      ) = 1 THEN "Other"
                                 ELSE c.country
                             END AS country,
                             c.customer_id,
                             il.*
                           FROM invoice_line il
                           INNER JOIN invoice i ON i.invoice_id = il.invoice_id
                           INNER JOIN customer c ON c.customer_id = i.customer_id
                          )
                      SELECT
                          country,
                          customers,
                          total_sales,
                          average_order,
                          customer_lifetime_value
                      FROM
                          (
                          SELECT
                              country,
                              count(distinct customer_id) customers,
                              SUM(unit_price) total_sales,
                              SUM(unit_price) / count(distinct customer_id) customer_lifetime_value,
                              SUM(unit_price) / count(distinct invoice_id) average_order,
                              CASE
                                  WHEN country = "Other" THEN 1
                                  ELSE 0
                              END AS sort
                          FROM country_or_other
                          GROUP BY country
                          ORDER BY sort ASC, total_sales DESC
                          );
                      '
  
  albums_vs_tracks = 'WITH invoice_first_track AS
                              (
                               SELECT
                                   il.invoice_id invoice_id,
                                   MIN(il.track_id) first_track_id
                               FROM invoice_line il
                               GROUP BY 1
                              )
                          
                          SELECT
                              album_purchase,
                              COUNT(invoice_id) number_of_invoices,
                              CAST(count(invoice_id) AS FLOAT) / (
                                                                   SELECT COUNT(*) FROM invoice
                                                                ) percent
                          FROM
                              (
                              SELECT
                                  ifs.*,
                                  CASE
                                      WHEN
                                           (
                                            SELECT t.track_id FROM track t
                                            WHERE t.album_id = (
                                                                SELECT t2.album_id FROM track t2
                                                                WHERE t2.track_id = ifs.first_track_id
                                                               ) 
                          
                                            EXCEPT 
                          
                                            SELECT il2.track_id FROM invoice_line il2
                                            WHERE il2.invoice_id = ifs.invoice_id
                                           ) IS NULL
                                       AND
                                           (
                                            SELECT il2.track_id FROM invoice_line il2
                                            WHERE il2.invoice_id = ifs.invoice_id
                          
                                            EXCEPT 
                          
                                            SELECT t.track_id FROM track t
                                            WHERE t.album_id = (
                                                                SELECT t2.album_id FROM track t2
                                                                WHERE t2.track_id = ifs.first_track_id
                                                               ) 
                                           ) IS NULL
                                       THEN "yes"
                                       ELSE "no"
                                   END AS "album_purchase"
                               FROM invoice_first_track ifs
                              )
                          GROUP BY album_purchase;
                          '
  
  # Top Albums in USA
  output$genre_plot <- renderPlotly({
    genre_sales <- run_query(albums_to_purchase)
    ggplot(genre_sales, aes(x = reorder(genre, -percentage_sold), 
                            y = percentage_sold, fill = genre)) +
      geom_bar(stat = "identity") + labs(x = "Genre", y = "Percentage Sold") + 
      theme_bw() + scale_fill_discrete()
  })
  
  # Employee Sales Performance
  output$employee_sales_plot <- renderPlotly({
    employee_sales <- run_query(employee_sales_performance)
    ggplot(employee_sales, aes(x = reorder(employee, -total_sales),
          y = total_sales, fill = employee)) +
      theme_bw() + labs( x = "Top Three Employees", y = "Total Albums Sold") + 
      geom_bar(stat = "identity") + scale_fill_viridis_d()
  })
  
  # Sales by Country
  output$country_metrics_plot_1 <- renderPlotly({
    country_metrics <- run_query(sales_by_country)
    ggplot(country_metrics, aes(x = reorder(country, -total_sales), 
                                y = total_sales, fill = country)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Total sales by country",
        x = "Country",
        y = "Total Sales"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$country_metrics_plot_2 <- renderPlot({
    country_metrics <- run_query(sales_by_country)
    ggplot(country_metrics, aes(x = reorder(country, -customers),
                                y = customers, fill = country)) +
      geom_bar(stat = "identity") +
      coord_polar("y") +
      labs(
        title = "Number of customers by country",
        x = "Country",
        y = "Customers"
      ) +
      theme_bw()
  })
  
  output$country_metrics_plot_3 <- renderPlotly({
    country_metrics <- run_query(sales_by_country)
    ggplot(country_metrics, aes(x = reorder(country, -customer_lifetime_value), 
                  y = customer_lifetime_value, color = country, size = 10)) +
      geom_point(stat = "identity") +
      labs(
        title = "Customer lifetime value by country",
        x = "Country",
        y = "Customer Lifetime Value"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_bw()
  })
  
  # Albums vs Tracks
  output$albums_vs_tracks_plot <- renderPlotly({
    albums_vs_tracks_data <- run_query(albums_vs_tracks)
    ggplot(albums_vs_tracks_data, aes(x = album_purchase, y = percent,
                                      fill = album_purchase)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Albums vs Tracks",
        x = "Album Purchase",
        y = "Percentage"
      ) +
      theme_bw() + scale_fill_brewer()
  })
}

# Run the Shiny app
shinyApp(ui, server)
