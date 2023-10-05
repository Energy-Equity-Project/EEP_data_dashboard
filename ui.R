
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT)

header <- dashboardHeader(
  title = "EEP Data Dashboard"
)

body <- dashboardBody(
  
  box(
    title = "Location Based Filtering",
    width = 12,
    fluidRow(
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
  box(
    title = "Location Results",
    width = 12,
    fluidRow(
      column(
        6,
        plotOutput("location_boxplot", height = "100%")
      ),
      column(
        6,
        plotOutput("location_ranked", height = "100%")
      )
    ),
    fluidRow(
      column(
        3,
        br(),
        materialSwitch(
          inputId = "boxplot_outliers_toggle",
          label = "Include Outliers", 
          value = FALSE,
          status = "primary"
        )
      ),
      column(
        3
      ),
      column(
        3,
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
        3,
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
  box(
    title = "Variable Filtering Options",
    width = 12,
    fluidRow(
      
      column(
        3,
        uiOutput("data_state_filter"),
        uiOutput("data_county_filter"),
      ),
      column(
        3,
        uiOutput("add_variable_filter_name"),
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
        br(),
        DTOutput("table1")
      )
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