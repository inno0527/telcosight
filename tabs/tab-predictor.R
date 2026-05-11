# tab_predictor.R - Churn predictor tab with real-time predictions

library(shiny)
library(plotly)
library(dplyr)

predictor_ui <- tabPanel(
  "Churn Predictor",
  fluidRow(
    column(4,
      wellPanel(
        h4("Customer Profile"),
        selectInput("pred_contract", "Contract Type:",
                    choices = c("Month-to-month", "One year", "Two year")),
        selectInput("pred_internet", "Internet Service:",
                    choices = c("DSL", "Fiber optic", "No")),
        radioButtons("pred_security", "Online Security:",
                     choices = c("Yes", "No"), inline = TRUE),
        radioButtons("pred_support", "Tech Support:",
                     choices = c("Yes", "No"), inline = TRUE),
        radioButtons("pred_streaming", "Streaming TV:",
                     choices = c("Yes", "No"), inline = TRUE),
        sliderInput("pred_charges", "Monthly Charges ($):",
                    min = 18, max = 118, value = 70, step = 1),
        sliderInput("pred_tenure", "Tenure (months):",
                    min = 0, max = 72, value = 12, step = 1),
        actionButton("predict_btn", "Predict Churn Risk",
                     class = "btn-primary btn-lg",
                     style = "width: 100%; margin-top: 15px;")
      )
    ),
    column(8,
      wellPanel(
        h4("Prediction Result"),
        fluidRow(
          column(6,
            h5("Churn Probability"),
            plotlyOutput("pred_gauge", height = "300px")
          ),
          column(6,
            h5("Risk Category"),
            htmlOutput("pred_risk_badge")
          )
        ),
        hr(),
        div(
          class = "panel panel-default",
          div(
            class = "panel-heading",
            h5("What drove this prediction?",
               style = "cursor: pointer;",
               onclick = "$('#importance_panel').toggle()")
          ),
          div(
            id = "importance_panel",
            class = "panel-collapse collapse",
            style = "display: none;",
            div(
              class = "panel-body",
              plotlyOutput("pred_importance", height = "300px")
            )
          )
        )
      )
    )
  )
)

predictor_server <- function(input, output, session, df, churn_model) {
  
  # Observe predict button and generate prediction
  prediction_result <- eventReactive(input$predict_btn, {
    # Create new data frame for prediction
    new_customer <- data.frame(
      Contract = factor(input$pred_contract, levels = c("Month-to-month", "One year", "Two year")),
      tenure = input$pred_tenure,
      MonthlyCharges = input$pred_charges,
      InternetService = factor(input$pred_internet, levels = c("DSL", "Fiber optic", "No")),
      OnlineSecurity = factor(input$pred_security, levels = c("Yes", "No")),
      TechSupport = factor(input$pred_support, levels = c("Yes", "No")),
      StreamingTV = factor(input$pred_streaming, levels = c("Yes", "No"))
    )
    
    # Get prediction
    predict_churn(churn_model, new_customer)
  })
  
  # Gauge chart
  output$pred_gauge <- renderPlotly({
    result <- prediction_result()
    prob <- result$probability
    
    # Determine color based on probability
    gauge_color <- ifelse(prob < 30, "#5cb85c",
                         ifelse(prob < 60, "#f0ad4e", "#d9534f"))
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = prob,
      title = list(text = "Risk Score (%)"),
      gauge = list(
        axis = list(range = list(0, 100), tickvals = c(0, 30, 60, 100)),
        bar = list(color = gauge_color),
        steps = list(
          list(range = c(0, 30), color = "#dff0d8"),
          list(range = c(30, 60), color = "#fcf8e3"),
          list(range = c(60, 100), color = "#f2dede")
        ),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = prob
        )
      ),
      number = list(suffix = "%", font = list(size = 40))
    ) %>%
      layout(margin = list(t = 50, b = 50))
  })
  
  # Risk badge
  output$pred_risk_badge <- renderUI({
    result <- prediction_result()
    risk <- result$risk_label
    prob <- result$probability
    
    badge_color <- switch(risk,
                         "Low Risk" = "success",
                         "Medium Risk" = "warning",
                         "High Risk" = "danger")
    
    div(
      style = "text-align: center;",
      span(
        risk,
        class = paste0("label label-", badge_color),
        style = "font-size: 24px; padding: 10px 20px;"
      ),
      p(style = "margin-top: 15px;",
        paste0("Based on the customer profile, there is a ", prob, "% likelihood of churn."))
    )
  })
  
  # Feature importance chart
  output$pred_importance <- renderPlotly({
    result <- prediction_result()
    factors <- result$importance
    
    if (length(factors) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No importance data available"))
    }
    
    # Create mock importance scores (in a real implementation, you'd use actual coefficients)
    importance_data <- data.frame(
      Factor = factors,
      Importance = c(0.8, 0.5, 0.3)[1:length(factors)]  # Simplified for demo
    )
    
    plot_ly(
      data = importance_data,
      x = ~Importance,
      y = ~reorder(Factor, Importance),
      type = "bar",
      orientation = "h",
      marker = list(color = "#337ab7"),
      text = ~paste("Impact:", round(Importance * 100), "%"),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Top Factors Driving This Prediction",
        xaxis = list(title = "Relative Impact", range = c(0, 1)),
        yaxis = list(title = "")
      )
  })
}