# tab_about.R - About tab with context and documentation

library(shiny)
library(shiny)

about_ui <- tabPanel(
  "About",
  fluidRow(
    column(12,
      wellPanel(
        h3("About TelcoSight"),
        p("TelcoSight is an interactive dashboard designed to help telecommunications companies 
          understand and predict customer churn. By analyzing customer behavior, service adoption, 
          and billing patterns, the dashboard provides actionable insights for retention strategies."),
        p("Built with R Shiny and Plotly, this dashboard transforms raw data into clear, 
          business-friendly visualizations that answer real operational questions.")
      )
    )
  ),
  fluidRow(
    column(12,
      wellPanel(
        h4("Real-World Business Problems Addressed"),
        div(class = "row",
          div(class = "col-md-6",
            div(class = "panel panel-default",
              div(class = "panel-heading", "Problem 1: Revenue Protection"),
              div(class = "panel-body",
                p("Monthly recurring revenue is at risk when customers churn. TelcoSight quantifies 
                  'Revenue at Risk' as a dollar figure, enabling finance teams to prioritize 
                  retention investments.")
              )
            ),
            div(class = "panel panel-default",
              div(class = "panel-heading", "Problem 2: Early Warning Signs"),
              div(class = "panel-body",
                p("Month-to-month contracts show significantly higher churn rates. The dashboard 
                  reveals which contract types and tenure lengths indicate flight risk before 
                  cancellation.")
              )
            ),
            div(class = "panel panel-default",
              div(class = "panel-heading", "Problem 3: Service Quality Gaps"),
              div(class = "panel-body",
                p("Fiber optic customers churn at different rates than DSL users. Service teams 
                  can identify which internet tiers need improvement based on churn patterns.")
              )
            )
          ),
          div(class = "col-md-6",
            div(class = "panel panel-default",
              div(class = "panel-heading", "Problem 4: Add-on Value Assessment"),
              div(class = "panel-body",
                p("Customers without online security or tech support churn more frequently. 
                  Product teams can evaluate which add-ons most effectively reduce churn.")
              )
            ),
            div(class = "panel panel-default",
              div(class = "panel-heading", "Problem 5: Proactive Retention"),
              div(class = "panel-body",
                p("The Churn Predictor enables retention teams to identify at-risk customers 
                  before they leave, shifting from reactive to predictive customer management.")
              )
            )
          )
        )
      )
    )
  ),
  fluidRow(
    column(12,
      wellPanel(
        h4("Dataset Information"),
        p("This dashboard uses the IBM Telco Customer Churn dataset, available on Kaggle."),
        tags$a(href = "https://www.kaggle.com/datasets/blastchar/telco-customer-churn",
               target = "_blank",
               class = "btn btn-primary",
               "View Dataset on Kaggle"),
        hr(),
        h5("Dataset Summary"),
        tags$ul(
          tags$li("Source: IBM / Kaggle — blastchar/telco-customer-churn"),
          tags$li("Total Records: 7,043 customer rows"),
          tags$li("Timeframe: Synthetic data representing modern telecom operations"),
          tags$li("License: Publicly available for analysis and learning")
        )
      )
    )
  ),
  fluidRow(
    column(12,
      wellPanel(
        h4("How to Use This Dashboard"),
        tags$ol(
          tags$li("Overview Tab: Start here to see overall churn metrics and high-level patterns"),
          tags$li("Drill-Down Tab: Apply filters to explore specific customer segments"),
          tags$li("Services Tab: Analyze how internet service and add-ons affect churn"),
          tags$li("Churn Predictor Tab: Create customer profiles to assess churn risk"),
          tags$li("About Tab: Reference documentation and business context")
        ),
        hr(),
        p(class = "text-muted",
          "Built with R Shiny v4.3+ | Visualization: Plotly | Data Processing: dplyr, tidyr")
      )
    )
  )
)

# No server function needed for About tab
about_server <- function(input, output, session) {
  # Empty server function
}