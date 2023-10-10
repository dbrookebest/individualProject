library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(highcharter)
library(wordcloud2)

# Read the Spotify data
spotify_data <- read.csv("spotify-2023.csv")
# Data frame for streams over time
streams_over_time <- data.frame(
  Date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "days"),
  Streams = cumsum(rnorm(365, mean = 10000, sd = 5000))  # Simulated stream data
)
# Define the UI
ui <- fluidPage(
  titlePanel("Spotify Data Visualization"),
  
  # Navigation bar
  navbarPage(
    "Spotify 2023",
    
    # Home tab
    tabPanel("Home",
             fluidPage(
               tags$style(HTML("
          body {
            background-color: #28A745; /* Green */
            color: #FFFFFF; /* White */
          }
        "))
             ),
             h1("Welcome to Spotify 2023 Data Visualization"),
             p("Explore the visualizations under different tabs.")
    ),
    
    # Visualization tabs
    tabPanel("Song Counts by Release Year",
             plotOutput("bar_chart")
    ),
    
    tabPanel("Mode Distribution",
             plotlyOutput("pie_chart")
    ),
    
    tabPanel("BPM Distribution",
             plotOutput("histogram")
    ),
    
    tabPanel("Valence vs. Energy",
             plotlyOutput("scatter_plot")
    ),
    
    tabPanel("Spotify Playlist Presence",
             plotOutput("stacked_bar_chart")
    ),
    
    tabPanel("Daily Streams Over Time",
             plotlyOutput("line_chart")
    ),
    
    tabPanel("Acousticness Distribution",
             plotOutput("box_plot")
    ),
    
    tabPanel("Top Artists by Song Count",
             plotOutput("top_artists_bar_chart")
    ),
    
    tabPanel("Correlations Between Audio Features",
             highchartOutput("heatmap")
    ),
    
    tabPanel("Word Cloud of Song Titles",
             wordcloud2Output("wordcloud")
    )
  )
)

# Define the server
server <- function(input, output) {
  # Song Counts by Release Year
  output$bar_chart <- renderPlot({
    ggplot(spotify_data, aes(x = released_year)) +
      geom_bar() +
      labs(title = "Song Counts by Release Year", x = "Year", y = "Number of Songs")
  })
  
    # Mode Distribution
    output$pie_chart <- renderPlotly({
      plot_ly(spotify_data) %>%
        add_trace(type = "pie", labels = ~mode) %>%
        layout(title = "Mode Distribution")
    })
  
  # BPM Distribution
  output$histogram <- renderPlot({
    ggplot(spotify_data, aes(x = bpm)) +
      geom_histogram(binwidth = 5, fill = "#28A745", color = "black") +
      labs(title = "BPM Distribution", x = "BPM", y = "Number of Songs")
  })
  
  # Valence vs. Energy
  output$scatter_plot <- renderPlotly({
    plot_ly(spotify_data, x = ~'valence_%', y = ~'energy_%', mode = "markers") %>%
      layout(title = "Valence vs. Energy", xaxis = list(title = "Valence (%)"), yaxis = list(title = "Energy (%)"))
  })
  
  # Spotify Playlist Presence
  output$stacked_bar_chart <- renderPlot({
    ggplot(spotify_data, aes(x = track_name, fill = factor(in_spotify_playlists))) +
      geom_bar() +
      labs(title = "Spotify Playlist Presence", x = "Song", y = "Count", fill = "In Playlists")
  })
  
  # Daily Streams Over Time
  output$scatterPlot <- renderPlotly({
    scatter_plot <- plot_ly(data = streams_over_time, x = ~Date, y = ~Streams, type = "scatter", mode = "lines+markers") %>%
      layout(title = "Streams Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Streams"))
    
    scatter_plot
  })
  
  # Acousticness Distribution
  output$box_plot <- renderPlot({
    ggplot(spotify_data, aes(x = "", y = 'acousticness_%')) +
      geom_boxplot(fill = "#28A745", color = "black") +
      labs(title = "Acousticness Distribution", x = "", y = "Acousticness (%)")
  })

  
  # Top Artists
  output$top_artists <- renderPlotly({
    top_artists_data <- spotify_data %>%
      count('artist(s)_name', sort = TRUE) %>%
      top_n(5)
    
    plot_ly(data = top_artists_data, x = ~n, y = ~'artist(s)_name', type = "bar", orientation = "h") %>%
      layout(title = "Top 5 Artists by Song Count")
  })
  
  
  output$correlation_heatmap <- renderPlotly({
    # Subset the data and select the audio features columns
    audio_features <- spotify_data[, c("danceability_%", "valence_%", "energy_%", "acousticness_%", "instrumentalness_%")]
    
    # Calculate the correlation matrix
    correlation_matrix <- cor(audio_features, use = "complete.obs")
    
    # Create a heatmap
    plot_ly(
      x = colnames(correlation_matrix),
      y = colnames(correlation_matrix),
      z = correlation_matrix,
      type = "heatmap"
    )
  })
  
  # Word Cloud of Song Titles
  output$song_titles_wordcloud <- renderWordcloud2({
    # Extract song titles from the dataset
    song_titles <- spotify_data$track_name
    wordcloud2(data = song_titles)
  })
}

# Run the Shiny app
shinyApp(ui, server)
