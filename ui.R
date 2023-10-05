
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT)

header <- dashboardHeader(
  title = "EEP Data Dashboard"
)

body <- dashboardBody(
  fluidRow(
    h2("Location Based Filtering"),
    column(
      3,
      uiOutput("state_selecter")
    ),
    column(
      3,
      uiOutput("county_selecter")
    ),
    column(
      3,
      uiOutput("location_var_selecter"),
    )
  ),
  fluidRow(
    h2("Location Results"),
    column(
      6,
      materialSwitch(
        inputId = "boxplot_outliers_toggle",
        label = "Include Outliers", 
        value = FALSE,
        status = "primary"
      ),
      plotOutput("location_boxplot", height = "100%")
    ),
    column(
      6,
      plotOutput("location_ranked", height = "100%")
    )
  ),
  fluidRow(
    h2("Variable Filtering Options"),
    column(
      3,
      uiOutput("data_state_filter"),
      uiOutput("data_county_filter"),
    ),
    column(
      3,
      uiOutput("add_variable_filter_name")
    ),
    column(
      3,
      textInput(
        inputId = "filter_lower_limit",
        label = "Enter a lower limit:",
        placeholder = "Lower limit..."
      ),
      textInput(
        inputId = "filter_upper_limit",
        label = "Enter an upper limit:",
        placeholder = "Upper limit..."
      )
    ),
    column(
      3,
      br(),
      actionButton(inputId = "add_filter_row", "Add a filter criteria")
    )
  ),
  fluidRow(
    column(
      3,
      uiOutput("delete_variable_filter_name")
    ),
    column(
      6,
      br(),
      actionButton(inputId = "delete_filter_row", "Delete a filter criteria")
    )
  ),
  fluidRow(
    column(
      12,
      actionButton(inputId = "filter_data", "Execute filter"),
      DTOutput("table1")
    )
  ),
  fluidRow(
    column(
      12,
      h2("Preview Data:"),
      DTOutput("table2"),
      downloadButton("download_filtered_data")
    )
  ),
  fluidRow(
    column(
      12,
      h1("Analysis"),
      uiOutput("variable_selecter"),
      uiOutput("group_selecter"),
      plotOutput("national_hist")
    )
  ),
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)