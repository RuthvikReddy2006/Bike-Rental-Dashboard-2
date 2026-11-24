install.packages(c(
#  "shiny",
 # "shinydashboard",
#  "dplyr",
#  "ggplot2",
#  "DT"
))

# Load required packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

# Load dataset
bike <- read.csv("bike_cleaned.csv")

# Check dataset
head(bike)

# Check dimensions
dim(bike)
# Load required packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

# Load dataset
bike <- read.csv("bike_cleaned.csv")

# Check dataset
head(bike)

# Check dimensions
dim(bike)

# Check structure
str(bike)

# Check missing values
colSums(is.na(bike))

# Convert date variable
bike$dteday <- as.Date(bike$dteday)

# Check dataset again
str(bike)
# Check structure
str(bike)

# Check missing values
colSums(is.na(bike))

# Convert date variable
bike$dteday <- as.Date(bike$dteday)

# Check dataset again
str(bike)

# Convert categorical variables into factors

bike$season <- factor(
  bike$season,
  levels = c("Spring", "Summer", "Fall", "Winter")
)

bike$yr <- factor(
  bike$yr,
  levels = c(0, 1),
  labels = c("2011", "2012")
)

bike$holiday <- factor(bike$holiday)

bike$workingday <- factor(bike$workingday)

bike$weathersit <- factor(
  bike$weathersit,
  levels = c(
    "Clear",
    "Mist and Cloudy",
    "Light Rain/Snow",
    "Heavy Rain/Snow"
  )
)

# Check the updated dataset
str(bike)

summary(bike)

levels(bike$season)
levels(bike$weathersit)


library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

# Load and prepare dataset
bike <- read.csv("bike_cleaned.csv", stringsAsFactors = FALSE)

bike$dteday <- as.Date(bike$dteday)

bike$season <- factor(
  bike$season,
  levels = c("Spring", "Summer", "Fall", "Winter")
)

bike$yr <- factor(bike$yr, levels = c(2011, 2012))
bike$holiday <- factor(bike$holiday)
bike$workingday <- factor(bike$workingday)

bike$weathersit <- factor(
  bike$weathersit,
  levels = c(
    "Clear",
    "Mist and Cloudy",
    "Light Rain/Snow",
    "Heavy Rain/Snow"
  )
)

# Dashboard user interface
ui <- dashboardPage(
  
  dashboardHeader(title = "Bike Rental Demand Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard",
               icon = icon("dashboard")),
      menuItem("Data Table", tabName = "data",
               icon = icon("table"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # Main dashboard tab
      tabItem(
        tabName = "dashboard",
        
        h2("Bike Rental Demand Overview"),
        
        fluidRow(
          valueBox(
            value = nrow(bike),
            subtitle = "Total Records",
            icon = icon("database"),
            color = "blue"
          ),
          
          valueBox(
            value = round(mean(bike$cnt)),
            subtitle = "Average Daily Rentals",
            icon = icon("bicycle"),
            color = "green"
          ),
          
          valueBox(
            value = max(bike$cnt),
            subtitle = "Maximum Daily Rentals",
            icon = icon("chart-line"),
            color = "yellow"
          )
        ),
        
        fluidRow(
          box(
            title = "Daily Bike Rental Trend",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("trendPlot")
          )
        )
      ),
      
      # Data table tab
      tabItem(
        tabName = "data",
        
        h2("Bike Rental Dataset"),
        
        box(
          width = 12,
          DTOutput("bikeTable")
        )
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  output$trendPlot <- renderPlot({
    
    ggplot(bike, aes(x = dteday, y = cnt)) +
      geom_line(color = "steelblue") +
      labs(
        title = "Daily Bike Rentals (2011–2012)",
        x = "Date",
        y = "Total Bike Rentals"
      ) +
      theme_minimal()
    
  })
  
  output$bikeTable <- renderDT({
    datatable(bike, options = list(pageLength = 10))
  })
  
}

# Run dashboard
shinyApp(ui = ui, server = server)


# ==========================================
# BIKE RENTAL DEMAND DASHBOARD
# ==========================================

# Load packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

# ==========================================
# 1. LOAD AND PREPARE DATA
# ==========================================

bike <- read.csv(
  "bike_cleaned.csv",
  stringsAsFactors = FALSE
)

bike$dteday <- as.Date(bike$dteday)

bike$season <- factor(
  bike$season,
  levels = c("Spring", "Summer", "Fall", "Winter")
)

bike$yr <- factor(
  as.character(bike$yr),
  levels = c("2011", "2012")
)

bike$weathersit <- factor(
  bike$weathersit,
  levels = c(
    "Clear",
    "Mist and Cloudy",
    "Light Rain/Snow",
    "Heavy Rain/Snow"
  )
)

bike$holiday <- factor(bike$holiday)
bike$workingday <- factor(bike$workingday)

# ==========================================
# 2. DASHBOARD USER INTERFACE
# ==========================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Bike Rental Demand"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Data Table",
        tabName = "data",
        icon = icon("table")
      )
    ),
    
    br(),
    
    h4("Dashboard Filters", style = "padding-left: 15px;"),
    
    selectInput(
      "year",
      "Select Year:",
      choices = c("All", levels(bike$yr)),
      selected = "All"
    ),
    
    selectInput(
      "season",
      "Select Season:",
      choices = c("All", levels(bike$season)),
      selected = "All"
    ),
    
    selectInput(
      "weather",
      "Select Weather:",
      choices = c(
        "All",
        levels(droplevels(bike$weathersit))
      ),
      selected = "All"
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # ======================================
      # DASHBOARD TAB
      # ======================================
      
      tabItem(
        tabName = "dashboard",
        
        h2("Bike Rental Demand Overview"),
        
        # KPI cards
        fluidRow(
          
          valueBoxOutput(
            "totalRecords",
            width = 4
          ),
          
          valueBoxOutput(
            "averageRentals",
            width = 4
          ),
          
          valueBoxOutput(
            "maximumRentals",
            width = 4
          )
        ),
        
        # Daily trend
        fluidRow(
          box(
            title = "Daily Bike Rental Trend",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput(
              "trendPlot",
              height = "350px"
            )
          )
        ),
        
        # Season and weather charts
        fluidRow(
          
          box(
            title = "Average Rentals by Season",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput(
              "seasonPlot",
              height = "300px"
            )
          ),
          
          box(
            title = "Average Rentals by Weather",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput(
              "weatherPlot",
              height = "300px"
            )
          )
        ),
        
        # Temperature chart
        fluidRow(
          box(
            title = "Temperature vs Bike Rentals",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput(
              "temperaturePlot",
              height = "350px"
            )
          )
        ),
        
        # Business insights
        fluidRow(
          box(
            title = "Key Business Insights",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            uiOutput("businessInsights")
          )
        )
      ),
      
      # ======================================
      # DATA TABLE TAB
      # ======================================
      
      tabItem(
        tabName = "data",
        
        h2("Bike Rental Dataset"),
        
        box(
          title = "Dataset Records",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          
          DTOutput("bikeTable")
        )
      )
    )
  )
)

# ==========================================
# 3. SERVER LOGIC
# ==========================================

server <- function(input, output) {
  
  # ----------------------------------------
  # Filter dataset
  # ----------------------------------------
  
  filteredBike <- reactive({
    
    data <- bike
    
    if (input$year != "All") {
      data <- data %>%
        filter(as.character(yr) == input$year)
    }
    
    if (input$season != "All") {
      data <- data %>%
        filter(as.character(season) == input$season)
    }
    
    if (input$weather != "All") {
      data <- data %>%
        filter(as.character(weathersit) == input$weather)
    }
    
    data
  })
  
  # ----------------------------------------
  # KPI CARD 1: TOTAL RECORDS
  # ----------------------------------------
  
  output$totalRecords <- renderValueBox({
    
    valueBox(
      value = nrow(filteredBike()),
      subtitle = "Total Records",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  # ----------------------------------------
  # KPI CARD 2: AVERAGE RENTALS
  # ----------------------------------------
  
  output$averageRentals <- renderValueBox({
    
    data <- filteredBike()
    
    avg <- if (nrow(data) > 0) {
      round(mean(data$cnt))
    } else {
      0
    }
    
    valueBox(
      value = avg,
      subtitle = "Average Daily Rentals",
      icon = icon("bicycle"),
      color = "green"
    )
  })
  
  # ----------------------------------------
  # KPI CARD 3: MAXIMUM RENTALS
  # ----------------------------------------
  
  output$maximumRentals <- renderValueBox({
    
    data <- filteredBike()
    
    maximum <- if (nrow(data) > 0) {
      max(data$cnt)
    } else {
      0
    }
    
    valueBox(
      value = maximum,
      subtitle = "Maximum Daily Rentals",
      icon = icon("line-chart"),
      color = "yellow"
    )
  })
  
  # ----------------------------------------
  # CHART 1: DAILY RENTAL TREND
  # ----------------------------------------
  
  
  # ----------------------------------------
  # CHART 1: IMPROVED DAILY RENTAL TREND
  # ----------------------------------------
  
  output$trendPlot <- renderPlot({
    
    data <- filteredBike() %>%
      arrange(dteday)
    
    validate(
      need(nrow(data) > 0, "No records for these filters.")
    )
    
    p <- ggplot(
      data,
      aes(x = dteday, y = cnt)
    )
    
    if (nrow(data) >= 30) {
      
      p <- p +
        geom_line(
          color = "steelblue",
          linewidth = 0.7
        ) +
        geom_point(
          color = "steelblue",
          size = 1.3,
          alpha = 0.6
        )
      
    } else {
      
      p <- p +
        geom_point(
          color = "steelblue",
          size = 2.5,
          alpha = 0.8
        )
    }
    
    p +
      labs(
        title = "Daily Bike Rental Demand",
        x = "Date",
        y = "Total Bike Rentals"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 15
        ),
        axis.title = element_text(
          face = "bold"
        )
      )
    
  })
  
  
  # ----------------------------------------
  # CHART 2: RENTALS BY SEASON
  # ----------------------------------------
  
  
  # ----------------------------------------
  # CHART 2: IMPROVED RENTALS BY SEASON
  # ----------------------------------------
  
  output$seasonPlot <- renderPlot({
    
    data <- filteredBike()
    
    validate(
      need(nrow(data) > 0, "No records for these filters.")
    )
    
    season_data <- data %>%
      group_by(season) %>%
      summarise(
        Average_Rentals = mean(cnt),
        .groups = "drop"
      )
    
    ggplot(
      season_data,
      aes(
        x = season,
        y = Average_Rentals,
        fill = season
      )
    ) +
      
      geom_col(width = 0.65) +
      
      geom_text(
        aes(label = round(Average_Rentals)),
        vjust = -0.5,
        size = 4
      ) +
      
      scale_fill_manual(
        values = c(
          "Spring" = "#F4A261",
          "Summer" = "#2A9D8F",
          "Fall" = "#457B9D",
          "Winter" = "#8E7DBE"
        ),
        drop = TRUE
      ) +
      
      labs(
        title = "Average Daily Rentals by Season",
        x = "Season",
        y = "Average Daily Rentals"
      ) +
      
      theme_minimal(base_size = 13) +
      
      theme(
        legend.position = "none",
        plot.title = element_text(
          face = "bold"
        )
      )
  })
  
  # ----------------------------------------
  # CHART 3: RENTALS BY WEATHER
  # ----------------------------------------
  
  
  # ----------------------------------------
  # CHART 3: IMPROVED RENTALS BY WEATHER
  # ----------------------------------------
  
  output$weatherPlot <- renderPlot({
    
    data <- filteredBike()
    
    validate(
      need(nrow(data) > 0, "No records for these filters.")
    )
    
    weather_data <- data %>%
      group_by(weathersit) %>%
      summarise(
        Average_Rentals = mean(cnt),
        .groups = "drop"
      )
    
    ggplot(
      weather_data,
      aes(
        x = weathersit,
        y = Average_Rentals,
        fill = weathersit
      )
    ) +
      
      geom_col(width = 0.65) +
      
      geom_text(
        aes(label = round(Average_Rentals)),
        vjust = -0.5,
        size = 4
      ) +
      
      scale_fill_manual(
        values = c(
          "Clear" = "#2A9D8F",
          "Mist and Cloudy" = "#E9C46A",
          "Light Rain/Snow" = "#E76F51",
          "Heavy Rain/Snow" = "#8E7DBE"
        ),
        drop = TRUE
      ) +
      
      labs(
        title = "Average Daily Rentals by Weather",
        x = "Weather Condition",
        y = "Average Daily Rentals"
      ) +
      
      theme_minimal(base_size = 13) +
      
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(
          angle = 20,
          hjust = 1
        )
      )
  })
  
  
  # ----------------------------------------
  # CHART 4: TEMPERATURE VS RENTALS
  # ----------------------------------------
  
  
  # ----------------------------------------
  # CHART 4: IMPROVED TEMPERATURE VS RENTALS
  # ----------------------------------------
  
  output$temperaturePlot <- renderPlot({
    
    data <- filteredBike()
    
    validate(
      need(nrow(data) > 0, "No records for these filters.")
    )
    
    ggplot(
      data,
      aes(x = temp, y = cnt)
    ) +
      
      geom_point(
        color = "#2585C7",
        size = 2.5,
        alpha = 0.65
      ) +
      
      geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = TRUE,
        color = "#E76F51",
        fill = "#F4A261",
        linewidth = 1
      ) +
      
      labs(
        title = "Temperature vs Bike Rental Demand",
        x = "Temperature (Normalised)",
        y = "Total Daily Rentals"
      ) +
      
      theme_minimal(base_size = 13) +
      
      theme(
        plot.title = element_text(
          face = "bold"
        )
      )
  })
  
  # ----------------------------------------
  # KEY BUSINESS INSIGHTS
  # ----------------------------------------
  
  
  # ----------------------------------------
  # KEY BUSINESS INSIGHTS
  # ----------------------------------------
  
  output$businessInsights <- renderUI({
    
    data <- filteredBike()
    
    if (nrow(data) == 0) {
      return(p("No data available for the selected filters."))
    }
    
    season_data <- data %>%
      group_by(season) %>%
      summarise(
        average = mean(cnt),
        .groups = "drop"
      ) %>%
      arrange(desc(average))
    
    weather_data <- data %>%
      group_by(weathersit) %>%
      summarise(
        average = mean(cnt),
        .groups = "drop"
      ) %>%
      arrange(desc(average))
    
    temp_cor <- if (
      n_distinct(data$temp) > 1 &&
      n_distinct(data$cnt) > 1
    ) {
      cor(data$temp, data$cnt)
    } else {
      NA_real_
    }
    
    tagList(
      
      div(
        class = "alert alert-info",
        tags$strong("Seasonal demand: "),
        if (nrow(season_data) > 0) {
          paste0(
            season_data$season[1],
            " has the highest average rentals (",
            round(season_data$average[1]),
            ")."
          )
        }
      ),
      
      div(
        class = "alert alert-success",
        tags$strong("Weather impact: "),
        if (nrow(weather_data) > 0) {
          paste0(
            weather_data$weathersit[1],
            " has the highest average rentals (",
            round(weather_data$average[1]),
            ")."
          )
        }
      ),
      
      div(
        class = "alert alert-warning",
        tags$strong("Temperature relationship: "),
        if (!is.na(temp_cor)) {
          paste0(
            "The correlation between temperature and rentals is ",
            round(temp_cor, 2),
            "."
          )
        } else {
          "Not enough variation to calculate correlation."
        }
      )
    )
  })
  
  # ----------------------------------------
  # SEARCHABLE DATA TABLE
  # ----------------------------------------
  
  
  # ----------------------------------------
  # IMPROVED SEARCHABLE DATA TABLE
  # ----------------------------------------
  
  output$bikeTable <- renderDT({
    
    datatable(
      filteredBike(),
      
      filter = "top",
      
      extensions = "Buttons",
      
      options = list(
        
        pageLength = 10,
        
        lengthMenu = c(10, 20, 50, 100),
        
        scrollX = TRUE,
        
        autoWidth = FALSE,
        
        columnDefs = list(
          list(width = "110px", targets = "_all")
        ),
        
        searching = TRUE,
        
        ordering = TRUE,
        
        dom = "Bfrtip",
        
        buttons = c("copy", "csv", "excel"),
        
        language = list(
          search = "Search dataset:",
          emptyTable = "No records available"
        )
      ),
      
      rownames = FALSE
    )
    
  })
}

# ==========================================
# 4. RUN DASHBOARD
# ==========================================

shinyApp(
  ui = ui,
  server = server
) 

