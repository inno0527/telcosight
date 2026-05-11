# tab_drilldown.R - Drill-down tab with interactive filters

library(shiny)
library(plotly)
library(dplyr)

drilldown_ui <- tabPanel(
  "Drill-Down",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filter Customers"),
      selectInput("dd_contract", "Contract Type",
                  choices = c("All", "Month-to-month", "One year", "Two year"),
                  selected = "All"),
      selectInput("dd_internet", "Internet Service",
                  choices = c("All", "DSL", "Fiber optic", "No"),
                  selected = "All"),
      selectInput("dd_security", "Online Security",
                  choices = c("All", "Yes", "No"),
                  selected = "All"),
      selectInput("dd_support", "Tech Support",
                  choices = c("All", "Yes", "No"),
                  selected = "All"),
      selectInput("dd_streaming", "Streaming TV",
                  choices = c("All", "Yes", "No"),
                  selected = "All"),
      sliderInput("dd_tenure_range", "Tenure (months)",
                  min = 0, max = 72, value = c(0, 72))
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(6,
          wellPanel(
            h3("Filtered Churn Rate"),
            h4(textOutput("dd_churn_rate"), style = "color: #d9534f;")
          )
        ),
        column(6,
          wellPanel(
            h3("Customers in Segment"),
            h4(textOutput("dd_customer_count"))
          )
        )
      ),
      fluidRow(
        column(12,
          wellPanel(
            h4("Churn by Internet Service"),
            plotlyOutput("dd_internet_chart")
          )
        )
      ),
      fluidRow(
        column(12,
          wellPanel(
            h4("Add-on Comparison"),
            radioButtons("dd_addon_select", "Select Add-on:",
                         choices = c("Online Security" = "OnlineSecurity",
                                    "Tech Support" = "TechSupport",
                                    "Streaming TV" = "StreamingTV"),
                         inline = TRUE),
            plotlyOutput("dd_addon_chart")
          )
        )
      ),
      fluidRow(
        column(12,
          wellPanel(
            h4("Customer Map (Tenure vs Monthly Charges)"),
            htmlOutput("dd_scatter_placeholder")
          )
        )
      )
    )
  )
)

drilldown_server <- function(input, output, session, df) {
  
  # Reactive filtered dataset
  filtered_df <- reactive({
    data <- df
    
    # Apply contract filter
    if (input$dd_contract != "All") {
      data <- data %>% filter(Contract == input$dd_contract)
    }
    
    # Apply internet service filter
    if (input$dd_internet != "All") {
      data <- data %>% filter(InternetService == input$dd_internet)
    }
    
    # Apply security filter
    if (input$dd_security != "All") {
      data <- data %>% filter(OnlineSecurity == input$dd_security)
    }
    
    # Apply support filter
    if (input$dd_support != "All") {
      data <- data %>% filter(TechSupport == input$dd_support)
    }
    
    # Apply streaming filter
    if (input$dd_streaming != "All") {
      data <- data %>% filter(StreamingTV == input$dd_streaming)
    }
    
    # Apply tenure range
    data <- data %>% filter(tenure >= input$dd_tenure_range[1],
                           tenure <= input$dd_tenure_range[2])
    
    data
  })
  
  # Check if any filter is applied (not all "All" and full tenure range)
  any_filter_applied <- reactive({
    input$dd_contract != "All" |
    input$dd_internet != "All" |
    input$dd_security != "All" |
    input$dd_support != "All" |
    input$dd_streaming != "All" |
    input$dd_tenure_range[1] > 0 |
    input$dd_tenure_range[2] < 72
  })
  
  # KPI outputs
  output$dd_churn_rate <- renderText({
    data <- filtered_df()
    if (nrow(data) == 0) return("0%")
    churn_rate <- round(100 * sum(data$Churn == "Yes") / nrow(data), 1)
    paste0(churn_rate, "%")
  })
  
  output$dd_customer_count <- renderText({
    nrow(filtered_df())
  })
  
  # Internet service chart
  output$dd_internet_chart <- renderPlotly({
    data <- filtered_df()
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No data available with current filters"))
    }
    
    chart_data <- data %>%
      group_by(InternetService, Churn) %>%
      summarise(Count = n()) %>%
      group_by(InternetService) %>%
      mutate(Percentage = round(100 * Count / sum(Count), 1))
    
    plot_ly(
      data = chart_data,
      x = ~InternetService,
      y = ~Count,
      color = ~Churn,
      type = "bar",
      colors = c("#5cb85c", "#d9534f"),
      text = ~paste("Count:", Count, "<br>%:", Percentage, "%"),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Churn by Internet Service Type",
        xaxis = list(title = "Internet Service"),
        yaxis = list(title = "Number of Customers"),
        barmode = "stack"
      )
  })
  
  # Add-on comparison chart
  output$dd_addon_chart <- renderPlotly({
    data <- filtered_df()
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No data available with current filters"))
    }
    
    selected_addon <- input$dd_addon_select
    
    # Filter out "No internet service" rows for add-on analysis
    analysis_data <- data %>% filter(InternetService != "No")
    
    if (nrow(analysis_data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No internet service customers in this segment"))
    }
    
    # Calculate churn rates with and without the add-on
    addon_data <- analysis_data %>%
      group_by(!!sym(selected_addon)) %>%
      summarise(
        Churn_Rate = round(100 * sum(Churn == "Yes") / n(), 1),
        Count = n()
      ) %>%
      filter(!!sym(selected_addon) %in% c("Yes", "No"))
    
    # Clean label for display
    addon_label <- switch(selected_addon,
                         "OnlineSecurity" = "Online Security",
                         "TechSupport" = "Tech Support",
                         "StreamingTV" = "Streaming TV")
    
    plot_ly(
      data = addon_data,
      x = ~get(selected_addon),
      y = ~Churn_Rate,
      type = "bar",
      marker = list(color = c("#5cb85c", "#d9534f")),
      text = ~paste("Rate:", Churn_Rate, "%", "<br>Customers:", Count),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste("Churn Rate:", addon_label, "Adoption"),
        xaxis = list(title = paste("Has", addon_label)),
        yaxis = list(title = "Churn Rate (%)", range = c(0, 100))
      )
  })
  
  # Scatter plot with placeholder
  output$dd_scatter_placeholder <- renderUI({
    if (any_filter_applied()) {
      # Render the actual scatter plot
      output$dd_scatter_plot <- renderPlotly({
        data <- filtered_df()
        
        if (nrow(data) == 0) {
          return(plotly_empty(type = "scatter", mode = "markers") %>%
                   layout(title = "No data available with current filters"))
        }
        
        plot_ly(
          data = data,
          x = ~tenure,
          y = ~MonthlyCharges,
          color = ~Churn,
          type = "scatter",
          mode = "markers",
          colors = c("#5cb85c", "#d9534f"),
          marker = list(size = 8, opacity = 0.6),
          text = ~paste("Tenure:", tenure, "months",
                       "<br>Monthly Bill: $", MonthlyCharges,
                       "<br>Churned:", Churn),
          hoverinfo = "text"
        ) %>%
          layout(
            title = "Customer Distribution: Tenure vs Monthly Charges",
            xaxis = list(title = "Tenure (months)"),
            yaxis = list(title = "Monthly Charges ($)")
          )
      })
      
      plotlyOutput("dd_scatter_plot", height = "500px")
    } else {
      div(
        class = "alert alert-info",
        style = "text-align: center; padding: 40px;",
        h4("Apply a filter to explore the customer map"),
        p("Use the filters on the left to see customer distribution patterns")
      )
    }
  })
}