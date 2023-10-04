
library(tidyverse)
library(janitor)
library(shiny)
library(shinyWidgets)
library(DT)

cejst <- read.csv("data/1.0-communities.csv") %>%
  clean_names() %>%
  mutate(energy_burden = as.numeric(energy_burden)) %>%
  mutate(energy_burden = case_when(
    energy_burden > 100 ~ 100,
    TRUE ~ energy_burden
  ))

variable_list <- colnames(cejst)[!(colnames(cejst) %in% c("census_tract_2010_id", "county_name", "state_territory"))]
variable_list <- cejst %>%
  select(where(is.numeric)) %>%
  select(-c(census_tract_2010_id)) %>%
  colnames()

group_list <- cejst %>%
  select(!where(is.numeric)) %>%
  colnames()

group_list <- c("", group_list)


create_histogram <- function(df, variable, group_in = NA) {

  if (is.na(variable) | is.null(variable)) { return(NULL) }
  
  keep_cols <- c(variable)
  
  if (!is.na(group_in) & str_length(group_in) > 0) {
    keep_cols <- c(keep_cols, group_in)
  }
  
  df <- df %>%
    select(keep_cols) %>%
    filter(!is.na(.data[[variable]]))
  
  if (is.na(group_in) | str_length(group_in) == 0) {
    df %>%
      ggplot() +
      geom_histogram(aes_string(variable), color = "#145DA0", fill = "#145DA0", alpha = 0.7) +
      theme_bw()
  } else {
    
    df <- df %>%
      filter(!is.na(.data[[group_in]]))
    
    df %>%
      ggplot(aes_string(variable, color = group_in, fill = group_in)) +
      geom_histogram(alpha = 0.7, position = "dodge") +
      theme_bw() +
      theme(legend.position = "bottom")
  }
}

execute_filter <- function(df, filter_df, state, county) {
  
  print(paste("here", state, county))
  
  if (nrow(filter_df) == 0 & state == "" & county == "") { return(df) }
  
  if (state != "") { df <- df %>% filter(state_territory == state) }
  
  if (county != "") { df <- df %>% filter(county_name == county) }
  
  if (nrow(filter_df) > 0) {
    for (i in 1:nrow(filter_df)) {
      filter_variable_name <- filter_df[i, 1][[1]]
      lower_limit <- filter_df[i, 2][[1]]
      upper_limit <- filter_df[i, 3][[1]]
      
      df <- df %>%
        filter(.data[[filter_variable_name]] >= as.numeric(lower_limit) &
                 .data[[filter_variable_name]] <= as.numeric(upper_limit))
    }
  }
  
  
  return(df)
}

area_boxplot <- function(df, state, county, variable) {
  
  area_selected_df <- df %>%
    filter(state_territory == state & county_name == county) %>%
    mutate(area_summary = county)
  
  area_df <- df %>%
    mutate(area_summary = case_when(
      state_territory == state ~ state,
      TRUE ~ "National"
    )) %>%
    bind_rows(area_selected_df)
  
  area_order <- c(county, state, "National")
  area_df$area_summary <- factor(area_df$area_summary, levels = area_order)
  
  # create_histogram(area_df, variable, group_in = "county_summary")
  area_df %>%
    ggplot() +
    # geom_boxplot(aes_string("county_summary", "variable")) +
    geom_boxplot(aes_string("area_summary", variable, fill = "area_summary")) +
    scale_fill_manual(values = c("#f7776c", "#689cfc", "#08bc3c")) +
    theme_bw() +
    theme(
      legend.position = "bottom"
    )
}

area_ranked <- function(df, state, county, variable) {
  
  area_df <- df %>%
    filter(state_territory == state) %>%
    group_by(county_name) %>%
    summarize(q1 = quantile(.data[[variable]], probs = c(0.25), na.rm = TRUE),
              q3 = quantile(.data[[variable]], probs = c(0.75), na.rm = TRUE),
              avg_var = mean(.data[[variable]], na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(area_selected = case_when(
      county_name == county ~ county,
      TRUE ~ paste("Other county in", state)
    )) %>%
    bind_rows(
      df %>%
        summarize(q1 = quantile(.data[[variable]], probs = c(0.25), na.rm = TRUE),
                  q3 = quantile(.data[[variable]], probs = c(0.75), na.rm = TRUE),
                  avg_var = mean(.data[[variable]], na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(area_selected = "National",
               county_name = "National")
    )
  
  area_order <- c(county, paste("Other county in", state), "National")
  
  area_df$area_selected = factor(area_df$area_selected, levels = area_order)
  
  area_df %>%
    ggplot() +
    geom_point(aes(x = q1, y = reorder(county_name, avg_var), color = area_selected), size = 3) +
    geom_point(aes(x = q3, y = reorder(county_name, avg_var), color = area_selected), size = 3) +
    geom_segment(aes(x = q1, xend = q3, y = county_name, yend = county_name, color = area_selected), size = 1) +
    scale_color_manual(values = c("#f7776c", "#689cfc", "#08bc3c")) +
    geom_point(aes(x = avg_var, y = reorder(county_name, avg_var)), color = "orange", size = 3) +
    theme_bw() +
    labs(x = variable, y = "", color = "Areas") +
    theme(
      legend.position = "bottom"
    )
}

area_summary <- function(df, state, county, variable, operation) {
  
  area_df <- df %>%
    filter(state_territory == state &
           county_name == county) %>%
    summarize(var_count = .data[[variable]])
  
  # Numeric variables
  
  # distribution
  
  # min, max, mean
  
  # sum
  
  # Categorical variables
  
}

server <- function(input, output, session) {
  
  react_vals <- reactiveValues(
    filter_criteria = data.frame(
      variable = c(),
      lower_limit = c(),
      upper_limit = c()
      ),
    df_filtered = cejst
  )
  
  output$state_selecter <- renderUI({
    pickerInput(
      inputId = "state_selected",
      label = "Select a state you are interested in",
      choices = unique(cejst$state_territory),
      options = list(
        `live-search` = TRUE)
    )
  })
  
  output$county_selecter <- renderUI({
    
    possible_counties <- cejst %>%
      filter(state_territory == input$state_selected) %>%
      pull(county_name) %>%
      unique()
    
    pickerInput(
      inputId = "county_selected",
      label = "Select a county in the state you are interested in",
      choices = possible_counties,
      options = list(
        `live-search` = TRUE)
    )
  })
  
  output$location_var_selecter <- renderUI({
    pickerInput(
      inputId = "location_var_selected",
      label = "Select a variable you would like to compare",
      choices = variable_list,
      options = list(
        `live-search` = TRUE)
    )
  })
  
  output$location_boxplot <- renderPlot({
    area_boxplot(cejst, input$state_selected, input$county_selected, input$location_var_selected)
  }, height = 600)
  
  output$location_ranked <- renderPlot({
    area_ranked(cejst, input$state_selected, input$county_selected, input$location_var_selected)
  }, height = 600)
  
  output$variable_selecter <- renderUI({
    pickerInput(
      inputId = "variable_selecter",
      label = "Select a variable to explore", 
      choices = variable_list,
      options = list(
        `live-search` = TRUE)
    )
  })
  
  output$group_selecter <- renderUI({
    pickerInput(
      inputId = "group_selecter",
      label = "Select a group to disaggregate data:", 
      choices = group_list,
      selected = "",
      options = list(
        `live-search` = TRUE)
    )
  })
  
  output$data_state_filter <- renderUI({
    pickerInput(
      inputId = "data_state_selected",
      label = "Choose a state (if blank will include all states):",
      choices = c("", unique(cejst$state_territory)),
      options = list(
        `live-search` = TRUE)
    )
  })
  
  output$data_county_filter <- renderUI({
    
    possible_counties <- cejst %>%
      filter(state_territory == input$data_state_selected) %>%
      pull(county_name) %>%
      unique()
    
    
    pickerInput(
      inputId = "data_county_selected",
      label = "Choose a county (if blank include all counties within a state):",
      choices = c("", possible_counties),
      options = list(
        `live-search` = TRUE)
    )
  })
  
  output$add_variable_filter_name <- renderUI({
    pickerInput(
      inputId = "filter_variable_name",
      label = "Select a variable to explore", 
      choices = variable_list,
      options = list(
        `live-search` = TRUE)
    )
  })
  
  output$delete_variable_filter_name <- renderUI({
    pickerInput(
      inputId = "filter_variable_name_delete",
      label = "Select a variable to explore", 
      choices = variable_list,
      options = list(
        `live-search` = TRUE)
    )
  })
  
  output$national_hist <- renderPlot({
    create_histogram(react_vals$filtered_df, input$variable_selecter, input$group_selecter)
  })
  
  output$table1 <- renderDT({
    datatable(react_vals$filter_criteria, editable = TRUE)
  })
  
  output$table2 <- renderDT({
    datatable(react_vals$filtered_df,
              options = list(
                scrollX = TRUE
              ))
  })
  
  
  observeEvent(input$add_filter_row, {
    react_vals$filter_criteria <- react_vals$filter_criteria %>%
      bind_rows(
        data.frame(
          variable = c(input$filter_variable_name),
          lower_limit = c(as.numeric(input$filter_lower_limit)),
          upper_limit = c(as.numeric(input$filter_upper_limit))
        )
      )
  })
  
  observeEvent(input$delete_filter_row, {
    react_vals$filter_criteria <- react_vals$filter_criteria %>%
      filter(variable != input$filter_variable_name_delete)
  })
  
  observeEvent(input$filter_data, {
    if(is.null(input$filter_data)) { return(cejst) }
    react_vals$filtered_df <- execute_filter(cejst, react_vals$filter_criteria, input$data_state_selected, input$data_county_selected)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      paste("cejst_processed_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(react_vals$filtered_df, row.names = FALSE, file)
    }
  )
}