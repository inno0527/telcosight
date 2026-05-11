# tab_services.R - Services tab analyzing add-ons and internet tiers

library(shiny)
library(plotly)
library(dplyr)

services_ui <- tabPanel(
  "Services",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filter by Monthly Charges"),
      sliderInput("svc_charges", "Monthly Charges Range ($)",
                  min = 0, max = 120, value = c(0, 120), step = 5),
      hr(),
      p("Select a charge range to see how spending patterns relate to churn")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(4,
          wellPanel(
            h4("Filtered Churn Rate"),
            h3(textOutput("svc_churn_rate"), style = "color: #d9534f;")
          )
        ),
        column(4,
          wellPanel(
            h4("Customers in Segment"),
            h3(textOutput("svc_customer_count"))
          )
        ),
        column(4,
          wellPanel(
            h4("Segment Revenue Risk"),
            h3(textOutput("svc_revenue_risk"), style = "color: #f0ad4e;")
          )
        )
      ),
      fluidRow(
        column(12,
          wellPanel(
            h4("Churn by Internet Service"),
            plotlyOutput("svc_internet_chart")
          )
        )
      ),
      fluidRow(
        column(12,
          wellPanel(
            h4("Add-on Impact on Churn"),
            radioButtons("svc_addon_select", "Select Add-on:",
                         choices = c("Online Security" = "OnlineSecurity",
                                    "Tech Support" = "TechSupport",
                                    "Streaming TV" = "StreamingTV"),
                         inline = TRUE),
            plotlyOutput("svc_addon_chart")
          )
        )
      ),
      fluidRow(
        column(12,
          wellPanel(
            h4("Average Charges by Internet Service"),
            plotlyOutput("svc_charges_chart")
          )
        )
      ),
      fluidRow(
        column(12,
          wellPanel(
            h4("Tenure Distribution by Service Type"),
            plotlyOutput("svc_boxplot")
          )
        )
      )
    )
  )
)

services_server <- function(input, output, session, df) {
  
  # Reactive filtered dataset by monthly charges
  filtered_df <- reactive({
    df %>%
      filter(MonthlyCharges >= input$svc_charges[1],
             MonthlyCharges <= input$svc_charges[2])
  })
  
  # KPI outputs
  output$svc_churn_rate <- renderText({
    data <- filtered_df()
    if (nrow(data) == 0) return("0%")
    churn_rate <- round(100 * sum(data$Churn == "Yes") / nrow(data), 1)
    paste0(churn_rate, "%")
  })
  
  output$svc_customer_count <- renderText({
    nrow(filtered_df())
  })
  
  output$svc_revenue_risk <- renderText({
    data <- filtered_df()
    revenue <- sum(data$MonthlyCharges[data$Churn == "Yes"])
    paste0("$", formatC(revenue, format = "f", digits = 0, big.mark = ","))
  })
  
  # Internet service chart
  output$svc_internet_chart <- renderPlotly({
    data <- filtered_df()
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No data available"))
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
        title = "Churn Rate by Internet Service Type",
        xaxis = list(title = "Internet Service"),
        yaxis = list(title = "Number of Customers"),
        barmode = "stack"
      )
  })
  
  # Add-on comparison chart
  output$svc_addon_chart <- renderPlotly({
    data <- filtered_df() %>%
      filter(InternetService != "No")
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No internet service customers in this segment"))
    }
    
    selected_addon <- input$svc_addon_select
    
    addon_data <- data %>%
      group_by(!!sym(selected_addon)) %>%
      summarise(
        Churn_Rate = round(100 * sum(Churn == "Yes") / n(), 1),
        Count = n()
      ) %>%
      filter(!!sym(selected_addon) %in% c("Yes", "No"))
    
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
  
  # Average charges chart
  output$svc_charges_chart <- renderPlotly({
    data <- filtered_df()
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No data available"))
    }
    
    charges_data <- data %>%
      group_by(InternetService, Churn) %>%
      summarise(Avg_Charge = round(mean(MonthlyCharges), 2))
    
    plot_ly(
      data = charges_data,
      x = ~InternetService,
      y = ~Avg_Charge,
      color = ~Churn,
      type = "bar",
      colors = c("#5cb85c", "#d9534f"),
      text = ~paste("Churn:", Churn, "<br>Avg Charge: $", Avg_Charge),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Average Monthly Charges by Service Type",
        xaxis = list(title = "Internet Service"),
        yaxis = list(title = "Average Monthly Charges ($)"),
        barmode = "group"
      )
  })
  
  # Box plot for tenure distribution
  output$svc_boxplot <- renderPlotly({
    data <- filtered_df()
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No data available"))
    }
    
    plot_ly(
      data = data,
      x = ~InternetService,
      y = ~tenure,
      color = ~Churn,
      type = "box",
      colors = c("#5cb85c", "#d9534f"),
      text = ~paste("Service:", InternetService,
                   "<br>Churned:", Churn,
                   "<br>Tenure:", tenure, "months"),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Tenure Distribution by Internet Service",
        xaxis = list(title = "Internet Service"),
        yaxis = list(title = "Tenure (months)")
      )
  })
}