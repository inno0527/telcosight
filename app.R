# app.R - Main Shiny application for TelcoSight

# Load required libraries
library(shiny)
library(plotly)
library(dplyr)
library(tidyr)

# Source all data and model files (these are in the root directory)
source("data.R")
source("model.R")

# Source all tab module files from the 'tab' folder
source("tabs/tab-overview.R")
source("tabs/tab-drilldown.R")
source("tabs/tab-services.R")
source("tabs/tab-predictor.R")
source("tabs/tab-about.R")

# Define UI
ui <- navbarPage(
  title = div(
    img(src = "", height = "30px", style = "margin-right: 10px;"),
    "TelcoSight - Customer Churn Intelligence"
  ),
  windowTitle = "TelcoSight | Customer Churn Dashboard",
  
  # Include custom CSS for better styling
  tags$head(
    tags$style(HTML("
      .well {
        background-color: #f9f9f9;
        border-radius: 8px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      .panel-heading {
        font-weight: bold;
        background-color: #337ab7;
        color: white;
      }
      .label {
        font-size: 18px;
        padding: 8px 15px;
        display: inline-block;
      }
      h2, h3, h4 {
        margin-top: 5px;
        margin-bottom: 15px;
      }
      .alert-info {
        background-color: #d9edf7;
        border-color: #bce8f1;
        color: #31708f;
      }
      .btn-primary {
        background-color: #337ab7;
        border-color: #2e6da4;
      }
    "))
  ),
  
  # Include all tab modules
  overview_ui,
  drilldown_ui,
  services_ui,
  predictor_ui,
  about_ui
)

# Define server
server <- function(input, output, session) {
  
  # Call each module's server function
  overview_server(input, output, session, df)
  drilldown_server(input, output, session, df)
  services_server(input, output, session, df)
  predictor_server(input, output, session, df, churn_model)
  about_server(input, output, session)
  
}

# Run the application
shinyApp(ui = ui, server = server)