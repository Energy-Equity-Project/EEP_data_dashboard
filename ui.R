
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(reactable)

header <- dashboardHeader(
  title = "EEP Data Dashboard"
)

body <- dashboardBody(
  fluidRow(
    box(
      title = "Location Exploration",
      width = 12,
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
    )
  ),
  fluidRow(
    box(
      width = 6,
      plotOutput("location_boxplot", height = "100%"),
      column(
        6,
        br(),
        materialSwitch(
          inputId = "boxplot_outliers_toggle",
          label = "Include Outliers", 
          value = FALSE,
          status = "primary"
        )
      ),
      column(
        6
      )
    ),
    box(
      width = 6,
      plotOutput("location_ranked", height = "100%"),
      column(
        6,
        br(),
        tags$div(
          materialSwitch(
            inputId = "ranked_range",
            label = "Q1 and Q3",
            value = FALSE,
            status = "primary",
            inline = TRUE
          ),
          tags$span("Min and Max")
        )
      ),
      column(
        6,
        br(),
        tags$div(
          materialSwitch(
            inputId = "ranked_center",
            label = "Median",
            value = FALSE,
            status = "primary",
            inline = TRUE
          ),
          tags$span("Mean")
        )
      )
    )
  ),
  fluidRow(
    box(
      title = "Variable Filtering Options",
      width = 12,
      column(
        3,
        uiOutput("data_state_filter"),
        uiOutput("data_county_filter"),
        uiOutput("delete_variable_filter_name"),
        actionButton(inputId = "filter_data", "Execute filter")
      ),
      column(
        3,
        uiOutput("add_variable_filter_name"),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        actionButton(inputId = "delete_filter_row", "Delete a filter criteria")
      ),
      column(
        3,
        uiOutput("bounds_slider")
      ),
      column(
        3,
        br(),
        actionButton(inputId = "add_filter_row", "Add a filter criteria")
      )
    ),
    box(
      width = 12,
      reactableOutput("table1")
    )
  ),
  fluidRow(
    box(
      title = "Preview Data",
      width = 12,
      uiOutput("preview_var_selecter"),
      reactableOutput("table2"),
      downloadButton("download_filtered_data")
    )
  ),
  fluidRow(
    box(
      width = 12,
      uiOutput("variable_selecter"),
      uiOutput("group_selecter"),
      plotOutput("national_hist")
    )
  )
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)