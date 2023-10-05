
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
    p <- df %>%
      ggplot() +
      geom_histogram(aes_string(variable), color = "#145DA0", fill = "#145DA0", alpha = 0.7) +
      theme_bw()
  } else {
    
    df <- df %>%
      filter(!is.na(.data[[group_in]]))
    
    p <- df %>%
      ggplot(aes_string(variable, color = group_in, fill = group_in)) +
      geom_histogram(alpha = 0.7, position = "dodge") +
      theme_bw() +
      theme(legend.position = "bottom")
  }
  
  p <- p +
    labs(y = "Census Tract Count", x = str_to_sentence(gsub("_", " ", variable))) +
    theme(
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
  
  if (str_length(group_in) > 0) {
    p <- p +
      labs(fill = str_to_sentence(gsub("_", " ", group_in)), color = str_to_sentence(gsub("_", " ", group_in))) +
      theme(
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "top"
      )
  }
  
  
  p
}

execute_filter <- function(df, filter_df, state, county) {
  
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

area_boxplot <- function(df, state, county, variable, include.outliers = FALSE) {
  
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
  
  if (!include.outliers) {
    
    y_boundaries <- area_df %>%
      group_by(area_summary) %>%
      summarize(q1 = quantile(.data[[variable]], probs = c(0.25), na.rm = TRUE),
                q3 = quantile(.data[[variable]], probs = c(0.75), na.rm = TRUE)) %>%
      mutate(iqr = q3-q1) %>%
      mutate(whisker_length = iqr * 1.5) %>%
      mutate(lower_limit = max(c(0, q1-iqr)),
             upper_limit = q3 + iqr) %>%
      ungroup()
    
    y_max <- max(y_boundaries$upper_limit)
    y_min <- min(y_boundaries$lower_limit)
    
    p <- area_df %>%
      ggplot() +
      geom_boxplot(aes_string("area_summary", variable, fill = "area_summary"), outlier.shape = NA) +
      scale_y_continuous(limits = c(y_min, y_max))
    
  } else {
    p <- area_df %>%
      ggplot() +
      geom_boxplot(aes_string("area_summary", variable, fill = "area_summary"))
  }
  
  p +
    scale_fill_manual(values = c("#f7776c", "#689cfc", "#08bc3c")) +
    labs(x = "", y = str_to_sentence(gsub("_", " ", variable))) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 15),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 15)
    )
}

area_ranked <- function(df, state, county, variable, range_type = FALSE, center_type = FALSE) {
  
  area_df <- df %>%
    filter(state_territory == state)
  
  if (!range_type) {
    area_df <- area_df %>%
      group_by(county_name) %>%
      summarize(q1 = quantile(.data[[variable]], probs = c(0.25), na.rm = TRUE),
                q3 = quantile(.data[[variable]], probs = c(0.75), na.rm = TRUE)) %>%
      ungroup()
    
    national_df <- df %>%
      summarize(q1 = quantile(.data[[variable]], probs = c(0.25), na.rm = TRUE),
                q3 = quantile(.data[[variable]], probs = c(0.75), na.rm = TRUE)) %>%
      ungroup()
      
  } else {
    area_df <- area_df %>%
      group_by(county_name) %>%
      summarize(q1 = min(.data[[variable]], na.rm = TRUE),
                q3 = max(.data[[variable]], na.rm = TRUE)) %>%
      ungroup()
    
    national_df <- df %>%
      summarize(q1 = min(.data[[variable]], na.rm = TRUE),
                q3 = max(.data[[variable]], na.rm = TRUE)) %>%
      ungroup()
  }
  

  if (!center_type) {
    area_df <- area_df %>%
      left_join(
        df %>%
          filter(state_territory == state) %>%
          group_by(county_name) %>%
          summarize(avg_var = mean(.data[[variable]], na.rm = TRUE)) %>%
          ungroup(),
        by = c("county_name")
      )
    
    national_df <- national_df %>%
      cbind(
        df %>%
          summarize(avg_var = mean(.data[[variable]], na.rm = TRUE)) %>%
          ungroup()
      )
      
  } else {
    area_df <- area_df %>%
      left_join(
        df %>%
          filter(state_territory == state) %>%
          group_by(county_name) %>%
          summarize(avg_var = quantile(.data[[variable]], probs = c(0.5), na.rm = TRUE)) %>%
          ungroup(),
        by = c("county_name")
      )
    
    national_df <- national_df %>%
      cbind(
        df %>%
          summarize(avg_var = quantile(.data[[variable]], probs = c(0.5), na.rm = TRUE)) %>%
          ungroup()
      )
  }
  
  area_df <- area_df %>%
    mutate(area_selected = case_when(
      county_name == county ~ county,
      TRUE ~ paste("Other county in", state)
    )) %>%
    bind_rows(
      national_df %>%
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
    labs(x = str_to_sentence(gsub("_", " ", variable)), y = "", color = "Areas") +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 15)
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

create_scatter <- function(df, var1, var2, color_var, include.trendline = FALSE) {
  p <- df %>%
    filter(!is.na(.data[[var1]]) & !is.na(.data[[var2]])) %>%
    ggplot(aes_string(x = var1, y = var2))
  
  if (str_length(color_var) == 0) {
    p <- p +
      geom_point()
  } else {
    p <- p +
      geom_point(aes_string(color = color_var))
  }
  
  if (include.trendline & str_length(color_var) == 0) {
    p <- p +
      geom_smooth(method=lm)
  } else if (include.trendline & str_length(color_var) > 0) {
    p <- p +
      geom_smooth(aes_string(color = color_var), method=lm)
  }
  
  p <- p +
    scale_y_continuous(limits = c(0, max(df[[var2]]))) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15)
    ) +
    labs(x = str_to_sentence(gsub("_", " ", var1)), y = str_to_sentence(gsub("_", " ", var2)))
  
  if (str_length(color_var) > 0) {
    p <- p +
      theme(
        legend.position = "top",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)
      ) +
      labs(color = str_to_sentence(gsub("_", " ", color_var)))
  }
  
  p
}

server <- function(input, output, session) {
  
  react_vals <- reactiveValues(
    filter_criteria = data.frame(
      variable = as.character(),
      lower_limit = as.numeric(),
      upper_limit = as.numeric()
      ),
    filtered_df = cejst
  )
  
  # isolate(react_vals$filter_criteria)
  # isolate(react_vals$df_filtered)
  
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
  
  output$preview_var_selecter <- renderUI({
    pickerInput(
      inputId = "preview_var_selected",
      label = "Select a variable you would like to preview",
      choices = variable_list,
      options = list(
        `live-search` = TRUE)
    )
  })
  
  output$location_boxplot <- renderPlot({
    
    area_boxplot(cejst, input$state_selected, input$county_selected, input$location_var_selected, input$boxplot_outliers_toggle)
  }, height = reactive(500 + 7 * cejst %>%
                         filter(state_territory == input$state_selected) %>%
                         select(county_name) %>%
                         distinct() %>%
                         nrow()))
  
  output$location_ranked <- renderPlot({
    area_ranked(cejst, input$state_selected, input$county_selected, input$location_var_selected, input$ranked_range, input$ranked_center)
  }, height = reactive(500 + 7 * cejst %>%
                         filter(state_territory == input$state_selected) %>%
                         select(county_name) %>%
                         distinct() %>%
                         nrow()))
  
  output$variable_selecter <- renderUI({
    pickerInput(
      inputId = "variable_selecter",
      label = "Select a variable to explore", 
      choices = variable_list,
      options = list(
        `live-search` = TRUE)
    )
  })
  
  output$scatter_var1_selected <- renderUI({
    pickerInput(
      inputId = "scatter_var1_selected",
      label = "Select a X-axis variable:",
      choices = variable_list,
      options = list(
        `live-search` = TRUE)
    )
  })
  
  output$scatter_var2_selected <- renderUI({
    pickerInput(
      inputId = "scatter_var2_selected",
      label = "Select a Y-axis variable:",
      choices = variable_list,
      options = list(
        `live-search` = TRUE)
    )
  })
  
  output$scatter_color_selected <- renderUI({
    pickerInput(
      inputId = "scatter_color_selected",
      label = "Select a color variable:",
      choices = c("", group_list),
      options = list(
        `live-search` = TRUE)
    )
  })
  
  output$scatterplot_explore <- renderPlot({
    create_scatter(react_vals$filtered_df, input$scatter_var1_selected, input$scatter_var2_selected, input$scatter_color_selected, input$scatter_trend_selected)
  }, height = 500)
  
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
  
  output$bounds_slider <- renderUI({
    upper_limit <- max(cejst[[input$filter_variable_name]], na.rm = TRUE)
    lower_limit <- min(c(0, cejst[[input$filter_variable_name]]), na.rm = TRUE)
    sliderInput(
      inputId = "bounds_selected",
      label = "Select a lower and upper limit:",
      min = lower_limit,
      max = upper_limit,
      value = c(lower_limit, upper_limit)
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
      label = "Select a variable to delete", 
      choices = variable_list,
      options = list(
        `live-search` = TRUE)
    )
  })
  
  output$national_hist <- renderPlot({
    create_histogram(react_vals$filtered_df, input$variable_selecter, input$group_selecter)
  }, height = 500)
  
  output$table1 <- renderReactable({
    if (is.null(react_vals$filter_criteria)) {
      reactable(cejst, minRows = 0)
    } else {
      reactable(react_vals$filter_criteria, minRows = 0)
    }
  })
  
  output$table2 <- renderReactable({
    
    preview_cols <- c("state_territory", "county_name", "census_tract_2010_id", input$preview_var_selected)
    reactable(react_vals$filtered_df %>%
                select(preview_cols) %>%
                filter(!is.na(.data[[input$preview_var_selected]])),
              minRows = 0)
  })
  
  
  observeEvent(input$add_filter_row, {

    react_vals$filter_criteria <- react_vals$filter_criteria %>%
      filter(variable != input$filter_variable_name) %>%
      bind_rows(
        data.frame(
          variable = c(input$filter_variable_name),
          lower_limit = c(as.numeric(input$bounds_selected[1])),
          upper_limit = c(as.numeric(input$bounds_selected[2]))
        )
      )
  })
  
  observeEvent(input$delete_filter_row, {
    react_vals$filter_criteria <- react_vals$filter_criteria %>%
      filter(variable != input$filter_variable_name_delete)
  })
  
  observeEvent(input$filter_data, {
    if(is.null(input$filter_data)) { return() }
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