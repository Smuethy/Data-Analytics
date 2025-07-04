library(shiny)
library(shinydashboard)
library(rvest) 
library(dplyr)
library(plotly)
library(DT)

# scrape data ONCE when the app starts
url <- "https://en.wikipedia.org/wiki/List_of_Spotify_streaming_records"
webpage <- read_html(url)

tables <- html_nodes(webpage, "table")
streamed_table <- html_table(tables[[1]], fill = TRUE)

# Clean column names
names(streamed_table) <- trimws(names(streamed_table))

# Find the column name that contains "Stream"
stream_col <- grep("Stream", names(streamed_table), value = TRUE)

# Clean the table
streamed_table <- streamed_table %>%
  mutate(
    Streams = readr::parse_number(.[[stream_col]])
  ) %>%
  filter(!is.na(Streams)) %>%
  mutate(
    Rank = row_number()
  )

ui <- dashboardPage(
  dashboardHeader(title = "🎵 Spotify Streams Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Table", tabName = "datatable", icon = icon("table")),
      menuItem("Spotify Most Streamed Songs 2023", icon = icon("external-link-alt"), 
               href = "https://en.wikipedia.org/wiki/List_of_Spotify_streaming_records")
    )
  ),
  dashboardBody(
    tags$head(
      # Google Fonts link + custom CSS
      tags$link(rel = "stylesheet", 
                href = "https://fonts.googleapis.com/css2?family=Lato:wght@700&display=swap"),
      tags$style(HTML("
        body, h1, h2, h3, h4, h5, h6 {
          font-family: 'Lato', sans-serif;
        }
        .box-title {
          font-weight: 700;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Histogram", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotlyOutput("hist_plot", height = 300),
                  sliderInput("bins", "Number of bins:", min = 5, max = 30, value = 10)
                ),
                box(
                  title = "Scatter Plot", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotlyOutput("scatter_plot", height = 300)
                )
              ),
              fluidRow(
                box(
                  title = "Controls & Filters", status = "info", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  sliderInput("slider", "Number of observations:", 
                              min = 1, max = nrow(streamed_table), value = nrow(streamed_table)),
                  
                  selectInput("artist_filter", "Filter by Artist:", 
                              choices = c("All", sort(unique(streamed_table$`Artist(s)`))),
                              selected = "All"),
                  
                  selectInput("song_filter", "Filter by Song:", 
                              choices = c("All", sort(unique(streamed_table$Song))),
                              selected = "All"),
                  
                  checkboxInput("dark_mode", "Dark Mode", FALSE),
                  
                  downloadButton("download", "Download Data")
                )
              )
      ),
      tabItem(tabName = "datatable",
              h2("Scraped Spotify Data Table"),
              DTOutput("data_table")
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$artist_filter, {
    filtered_songs <- streamed_table
    if (input$artist_filter != "All") {
      filtered_songs <- filtered_songs %>% filter(`Artist(s)` == input$artist_filter)
    }
    updateSelectInput(session, "song_filter", 
                      choices = c("All", sort(unique(filtered_songs$Song))),
                      selected = "All")
  })
  
  data_subset <- reactive({
    df <- streamed_table %>%
      slice_head(n = input$slider)
    
    if (input$artist_filter != "All") {
      df <- df %>% filter(`Artist(s)` == input$artist_filter)
    }
    if (input$song_filter != "All") {
      df <- df %>% filter(Song == input$song_filter)
    }
    df
  })
  
  output$hist_plot <- renderPlotly({
    df <- data_subset()
    p <- ggplot(df, aes(x = Streams)) +
      geom_histogram(bins = input$bins, fill = "steelblue", color = "white") +
      labs(title = "Histogram of Streams", x = "Streams (Billions)", y = "Count") +
      theme_minimal(base_family = "Lato") +
      theme(plot.title = element_text(face = "bold"))
    ggplotly(p) %>% 
      layout(paper_bgcolor = if (input$dark_mode) '#222' else '#fff',
             plot_bgcolor  = if (input$dark_mode) '#222' else '#fff',
             font = list(color = if (input$dark_mode) '#fff' else '#000'))
  })
  
  output$scatter_plot <- renderPlotly({
    df <- data_subset()
    p <- ggplot(df, aes(x = Rank, y = Streams, text = paste("Song:", Song, "<br>Artist:", `Artist(s)`))) +
      geom_point(color = "tomato", size = 3) +
      labs(title = "Scatter Plot of Streams", x = "Rank", y = "Streams (Billions)") +
      theme_minimal(base_family = "Lato") +
      theme(plot.title = element_text(face = "bold"))
    ggplotly(p, tooltip = "text") %>% 
      layout(paper_bgcolor = if (input$dark_mode) '#222' else '#fff',
             plot_bgcolor  = if (input$dark_mode) '#222' else '#fff',
             font = list(color = if (input$dark_mode) '#fff' else '#000'))
  })
  
  output$data_table <- renderDT({
    datatable(
      data_subset(),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("spotify_streams_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data_subset(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
