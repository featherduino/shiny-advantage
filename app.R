# ==============================
# 📊 Sector Sentiment Studio (Enhanced Shiny Dashboard)
# ==============================

library(shiny)
library(DBI)
library(RPostgres)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(DT)
library(lubridate)

# -----------------------------------------
# DB Connection
# -----------------------------------------
connect_db <- function() {
  tryCatch({
    dbConnect(
      RPostgres::Postgres(),
      host = Sys.getenv("PGHOST", unset = "localhost"),
      port = Sys.getenv("PGPORT", unset = "5432"),
      user = Sys.getenv("PGUSER", unset = "cmg"),
      password = Sys.getenv("PGPASSWORD", unset = "cmg123"),
      dbname = Sys.getenv("PGDATABASE", unset = "cmg")
    )
  }, error = function(e) {
    message("⚠️ Offline mode — DB not connected.")
    return(NULL)
  })
}

# -----------------------------------------
# Load Bullish + Bearish Detail Data
# -----------------------------------------
load_data <- function(con) {
  if (!is.null(con)) {
    bullish <- dbGetQuery(con, "SELECT * FROM bullish_detail_flat;")
    bearish <- dbGetQuery(con, "SELECT * FROM bearish_detail_flat;")
  } else {
    bullish <- read.csv("bullish_detail_flat.csv")
    bearish <- read.csv("bearish_detail_flat.csv")
  }
  
  # clean column names
  names(bullish) <- gsub("\\.", "_", names(bullish))
  names(bearish) <- gsub("\\.", "_", names(bearish))
  
  bullish$created_date <- as.Date(bullish$created_date)
  bearish$created_date <- as.Date(bearish$created_date)
  
  list(bullish = bullish, bearish = bearish)
}

# -----------------------------------------
# UI
# -----------------------------------------
ui <- fluidPage(
  
  titlePanel("📈 Sector Sentiment Studio — Market Intelligence"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Date Range:",
                     start = Sys.Date() - 7, end = Sys.Date()),
      
      selectInput("sector", "Sector:", choices = NULL, multiple = TRUE),
      selectInput("industry", "Industry:", choices = NULL, multiple = TRUE),
      
      sliderInput("epsFilter", "EPS (Latest):", min = 0, max = 200, value = c(0, 200)),
      sliderInput("promHoldFilter", "Promoter Holding %:", min = 0, max = 100, value = c(0, 100)),
      sliderInput("volSpikeFilter", "Volume Spike:", min = 0, max = 200, value = c(0, 200)),
      
      selectInput("biasFilter", "Trend Bias:",
                  choices = c("All", "BULLISH", "NEUTRAL", "BEARISH"), selected = "All"),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        
        # ---------------- Overview ----------------
        tabPanel("📊 Overview",
                 plotOutput("macroPlot", height = "520px"),
                 DTOutput("summaryTable")
        ),
        
        # ---------------- Sector Focus ----------------
        tabPanel("🔍 Sector Focus",
                 plotOutput("sectorBar", height = "400px"),
                 fluidRow(
                   column(6, h4("🔥 Top Bullish"), DTOutput("topBullish")),
                   column(6, h4("❄️ Top Bearish"), DTOutput("topBearish"))
                 )
        ),
        
        # ---------------- Insights ----------------
        tabPanel("🧠 AI Insights",
                 h3("Market Commentary"),
                 verbatimTextOutput("aiInsights")
        )
      )
    )
  )
)

# -----------------------------------------
# SERVER
# -----------------------------------------
server <- function(input, output, session) {
  
  con <- connect_db()
  radar_data <- load_data(con)
  
  # combine bullish + bearish
  df_combined <- reactive({
    bullish <- radar_data$bullish %>% mutate(sentiment = "Bullish")
    bearish <- radar_data$bearish %>% mutate(sentiment = "Bearish")
    
    # --- Force common types to avoid bind_rows() errors ---
    common_numeric_cols <- c(
      "fundamentals_latestEPS",
      "fundamentals_promoterHold",
      "volSpike",
      "changePct",
      "rsi",
      "score",
      "ltp",
      "pChange",
      "weekHigh",
      "weekLow"
    )
    
    common_char_cols <- c(
      "trendBias",
      "fundamentals_latestAction",
      "companyName",
      "sector",
      "industry",
      "macro",
      "basicIndustry",
      "isin",
      "listingDate"
    )
    
    # Convert numeric columns safely
    for (col in common_numeric_cols) {
      if (col %in% names(bullish)) bullish[[col]] <- suppressWarnings(as.numeric(bullish[[col]]))
      if (col %in% names(bearish)) bearish[[col]] <- suppressWarnings(as.numeric(bearish[[col]]))
    }
    
    # Convert character columns safely
    for (col in common_char_cols) {
      if (col %in% names(bullish)) bullish[[col]] <- as.character(bullish[[col]])
      if (col %in% names(bearish)) bearish[[col]] <- as.character(bearish[[col]])
    }
    
    bind_rows(bullish, bearish)
  })
  
  # detect which column refers to sector
  get_sector_col <- function(df) {
    possible_cols <- c("sector", "sector_name", "industry")
    intersect(possible_cols, names(df))[1]
  }
  
  # -----------------------------------------
  # Dropdown population
  # -----------------------------------------
  observe({
    df <- df_combined()
    sec <- get_sector_col(df)
    if (!is.null(sec)) {
      updateSelectInput(session, "sector", choices = sort(unique(df[[sec]])))
    }
    
    if ("industry" %in% names(df)) {
      updateSelectInput(session, "industry", choices = sort(unique(df$industry)))
    }
  })
  
  # -----------------------------------------
  # Filter Data
  # -----------------------------------------
  filtered_data <- reactive({
    df <- df_combined()
    
    sec_col <- get_sector_col(df)
    
    df %>%
      filter(
        created_date >= input$dateRange[1],
        created_date <= input$dateRange[2],
        if (length(input$sector) > 0) .data[[sec_col]] %in% input$sector else TRUE,
        if (length(input$industry) > 0) industry %in% input$industry else TRUE,
        fundamentals_latestEPS >= input$epsFilter[1],
        fundamentals_latestEPS <= input$epsFilter[2],
        fundamentals_promoterHold >= input$promHoldFilter[1],
        fundamentals_promoterHold <= input$promHoldFilter[2],
        volSpike >= input$volSpikeFilter[1],
        volSpike <= input$volSpikeFilter[2],
        if (input$biasFilter != "All") trendBias == input$biasFilter else TRUE
      )
  })
  
  # -----------------------------------------
  # Macro Scatter Plot
  # -----------------------------------------
  output$macroPlot <- renderPlot({
    df <- filtered_data() %>%
      group_by(symbol, sentiment) %>%
      summarise(
        avg_score = mean(score),
        avg_change = mean(changePct),
        avg_rsi = mean(rsi),
        .groups = "drop"
      )
    
    ggplot(df, aes(avg_change, avg_score, color = sentiment, label = symbol)) +
      geom_point(size = 4) +
      geom_text_repel(max.overlaps = 25) +
      scale_color_manual(values = c("Bearish" = "#D73027", "Bullish" = "#1A9850")) +
      theme_minimal(base_size = 14) +
      labs(title = "Macro Strength vs Momentum",
           x = "Avg % Change",
           y = "Avg Score")
  })
  
  # -----------------------------------------
  # Summary Table
  # -----------------------------------------
  output$summaryTable <- renderDT({
    df <- filtered_data() %>%
      group_by(sentiment) %>%
      summarise(
        avg_score = mean(score),
        avg_change = mean(changePct),
        n = n()
      ) %>%
      arrange(desc(avg_score))
    
    datatable(df)
  })
  
  # -----------------------------------------
  # Sector Strength Bar Chart
  # -----------------------------------------
  output$sectorBar <- renderPlot({
    df <- filtered_data()
    sec_col <- get_sector_col(df)
    if (is.null(sec_col)) return(NULL)
    
    df %>% 
      mutate(sector_clean = stringr::str_wrap(.data[[sec_col]], width = 20)) %>%
      group_by(sector_clean, sentiment) %>% 
      summarise(
        mean_score = mean(score, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      ggplot(aes(x = reorder(sector_clean, mean_score), y = mean_score, fill = sentiment)) +
      geom_col(position = "dodge", width = 0.7) +
      coord_flip() +
      facet_wrap(~ sentiment, ncol = 1, scales = "free_y") +
      scale_fill_manual(values = c("Bearish" = "#D73027", "Bullish" = "#1A9850")) +
      labs(title = "Sector Sentiment Strength", x = "", y = "Mean Score") +
      theme_minimal(base_size = 14) +
      theme(
        strip.text = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 11),
        plot.margin = margin(10, 20, 10, 20)
      )
  })
  
  # -----------------------------------------
  # Top Bullish / Bearish Tables
  # -----------------------------------------
  output$topBullish <- renderDT({
    df <- filtered_data()
    sec_col <- get_sector_col(df)
    if (is.null(sec_col)) return(NULL)
    
    df %>%
      filter(sentiment == "Bullish") %>%
      arrange(desc(score)) %>%
      distinct(symbol, .keep_all = TRUE) %>%           # only unique companies
      head(10) %>%
      select(created_date, symbol, score, changePct, rsi, sector, industry) %>%
      datatable(options = list(dom = 't', scrollX = TRUE))
  })
  
  output$topBearish <- renderDT({
    df <- filtered_data()
    sec_col <- get_sector_col(df)
    if (is.null(sec_col)) return(NULL)
    
    df %>%
      filter(sentiment == "Bearish") %>%
      arrange(desc(score)) %>%
      distinct(symbol, .keep_all = TRUE) %>%           # only unique companies
      head(10) %>%
      select(created_date, symbol, score, changePct, rsi, sector, industry) %>%
      datatable(options = list(dom = 't', scrollX = TRUE))
  })
  
  
  # -----------------------------------------
  # AI Insights (pseudo-AI text generation)
  # -----------------------------------------
  output$aiInsights <- renderText({
    df <- filtered_data()
    
    sectors <- df %>%
      group_by(sector) %>%
      summarise(
        avg_score = mean(score),
        avg_change = mean(changePct),
        n = n()
      )
    
    bullish_sector <- sectors %>% arrange(desc(avg_score)) %>% slice(1)
    bearish_sector <- sectors %>% arrange(avg_score) %>% slice(1)
    
    paste0(
      "The strongest sector this week is **", bullish_sector$sector,
      "** with an average score of ", round(bullish_sector$avg_score, 2), ".\n\n",
      "Weakness is visible in **", bearish_sector$sector,
      "** where momentum and score both remain soft.\n\n",
      "Overall market breadth currently shows ",
      nrow(df %>% filter(sentiment == 'Bullish')), " bullish vs ",
      nrow(df %>% filter(sentiment == 'Bearish')), " bearish signals."
    )
  })
}

shinyApp(ui, server)
