# tab_overview.R - Overview tab with high-level metrics

library(shiny)
library(plotly)
library(dplyr)

overview_ui <- tabPanel(
  "Overview",
  fluidRow(
    column(6,
      wellPanel(
        h3("Churn Rate"),
        h2(textOutput("overview_churn_rate"), style = "color: #d9534f;"),
        p("Percentage of customers who have churned")
      )
    ),
    column(6,
      wellPanel(
        h3("Revenue at Risk"),
        h2(textOutput("overview_revenue_risk"), style = "color: #f0ad4e;"),
        p("Monthly charges lost to churn")
      )
    )
  ),
  fluidRow(
    column(12,
      wellPanel(
        h4("Churn Distribution"),
        plotlyOutput("overview_donut_chart", height = "400px")
      )
    )
  ),
  fluidRow(
    column(12,
      wellPanel(
        h4("Churn Breakdown"),
        tabsetPanel(
          tabPanel("By Contract Type", plotlyOutput("overview_contract_chart")),
          tabPanel("By Payment Method", plotlyOutput("overview_payment_chart"))
        )
      )
    )
  )
)

overview_server <- function(input, output, session, df) {
  
  # Calculate overall metrics
  overall_churn_rate <- reactive({
    churned <- sum(df$Churn == "Yes")
    paste0(round(100 * churned / nrow(df), 1), "%")
  })
  
  overall_revenue_risk <- reactive({
    revenue <- sum(df$MonthlyCharges[df$Churn == "Yes"])
    paste0("$", formatC(revenue, format = "f", digits = 0, big.mark = ","))
  })
  
  output$overview_churn_rate <- renderText({
    overall_churn_rate()
  })
  
  output$overview_revenue_risk <- renderText({
    overall_revenue_risk()
  })
  
  # Donut chart
  output$overview_donut_chart <- renderPlotly({
    churn_summary <- df %>%
      group_by(Churn) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = round(100 * Count / sum(Count), 1))
    
    plot_ly(
      data = churn_summary,
      labels = ~Churn,
      values = ~Count,
      type = "pie",
      hole = 0.6,
      marker = list(colors = c("#5cb85c", "#d9534f")),
      textinfo = "label+percent",
      hoverinfo = "text",
      text = ~paste(Churn, "<br>Count:", Count, "<br>Percentage:", Percentage, "%")
    ) %>%
      layout(
        title = "Customer Churn Breakdown",
        showlegend = TRUE,
        annotations = list(
          list(
            text = paste0("Total<br>", nrow(df)),
            font = list(size = 14),
            showarrow = FALSE
          )
        )
      )
  })
  
  # Contract type bar chart
  output$overview_contract_chart <- renderPlotly({
    contract_data <- df %>%
      group_by(Contract, Churn) %>%
      summarise(Count = n()) %>%
      group_by(Contract) %>%
      mutate(Percentage = round(100 * Count / sum(Count), 1))
    
    plot_ly(
      data = contract_data,
      x = ~Contract,
      y = ~Count,
      color = ~Churn,
      type = "bar",
      colors = c("#5cb85c", "#d9534f"),
      text = ~paste("Count:", Count, "<br>%:", Percentage, "%"),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Churn by Contract Type",
        xaxis = list(title = "Contract Type"),
        yaxis = list(title = "Number of Customers"),
        barmode = "stack"
      )
  })
  
  # Payment method bar chart
  output$overview_payment_chart <- renderPlotly({
    # Calculate churn rate by payment method
    payment_data <- df %>%
      group_by(PaymentMethod) %>%
      summarise(
        Churn_Rate = round(100 * sum(Churn == "Yes") / n(), 1),
        Total = n()
      ) %>%
      arrange(desc(Churn_Rate))
    
    plot_ly(
      data = payment_data,
      x = ~Churn_Rate,
      y = ~reorder(PaymentMethod, Churn_Rate),
      type = "bar",
      orientation = "h",
      marker = list(color = ~Churn_Rate, 
                    colorscale = list(c(0, "#5cb85c"), c(0.5, "#f0ad4e"), c(1, "#d9534f")),
                    showscale = TRUE),
      text = ~paste("Rate:", Churn_Rate, "%", "<br>Customers:", Total),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Churn Rate by Payment Method",
        xaxis = list(title = "Churn Rate (%)"),
        yaxis = list(title = "")
      )
  })
}