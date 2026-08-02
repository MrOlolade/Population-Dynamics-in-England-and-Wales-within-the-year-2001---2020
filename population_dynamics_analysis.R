# LOADING LIBRARIES
library(tidyverse)
library(janitor)
library(skimr)
library(naniar)

library(broom)
library(car)

library(caret)
library(randomForest)
library(xgboost)
library(nnet)

library(cluster)
library(factoextra)

library(scales)
library(corrplot)

set.seed(42)
# IMPORTING DATASETS
project_data_directory <- "data"

import_population_datasets <- function(data_directory) {
  
  detailed_population_estimates <- read.csv(
    file.path(
      data_directory,
      "MYEB1_detailed_population_estimates_series_UK_(2020_geog21).csv"
    ),
    stringsAsFactors = FALSE
  ) %>%
    clean_names()
  
  detailed_components_of_change <- read.csv(
    file.path(
      data_directory,
      "MYEB2_detailed_components_of_change_series_EW_(2020_geog21).csv"
    ),
    stringsAsFactors = FALSE
  ) %>%
    clean_names()
  
  summary_components_of_change <- read.csv(
    file.path(
      data_directory,
      "MYEB3_summary_components_of_change_series_UK_(2020_geog21).csv"
    ),
    stringsAsFactors = FALSE
  ) %>%
    clean_names()
  
  imported_datasets <- list(
    detailed_population_estimates = detailed_population_estimates,
    detailed_components_of_change = detailed_components_of_change,
    summary_components_of_change = summary_components_of_change
  )
  
  print("Datasets imported successfully")
  
  return(imported_datasets)
}

population_datasets <- import_population_datasets(project_data_directory)

detailed_population_estimates <- population_datasets$detailed_population_estimates
detailed_components_of_change <- population_datasets$detailed_components_of_change
summary_components_of_change <- population_datasets$summary_components_of_change
# DATASET OVERVIEW
create_dataset_overview <- function(detailed_population_estimates,
                                    detailed_components_of_change,
                                    summary_components_of_change) {
  
  dataset_overview <- tibble(
    dataset_name = c(
      "MYEB1 Detailed Population Estimates",
      "MYEB2 Detailed Components of Change",
      "MYEB3 Summary Components of Change"
    ),
    renamed_object = c(
      "detailed_population_estimates",
      "detailed_components_of_change",
      "summary_components_of_change"
    ),
    rows = c(
      nrow(detailed_population_estimates),
      nrow(detailed_components_of_change),
      nrow(summary_components_of_change)
    ),
    columns = c(
      ncol(detailed_population_estimates),
      ncol(detailed_components_of_change),
      ncol(summary_components_of_change)
    )
  )
  
  print(dataset_overview)
  return(dataset_overview)
}

dataset_overview <- create_dataset_overview(
  detailed_population_estimates,
  detailed_components_of_change,
  summary_components_of_change
)
# INITIAL DATASET INSPECTION
inspect_dataset <- function(dataset) {
  
  dataset_inspection <- list(
    shape = dim(dataset),
    first_five_rows = as_tibble(head(dataset, 5)),
    data_type_summary = table(sapply(dataset, class)),
    top_missing_values = sort(colSums(is.na(dataset)), decreasing = TRUE)[1:10]
  )
  
  print(dataset_inspection)
  return(dataset_inspection)
}

detailed_population_inspection <- inspect_dataset(
  detailed_population_estimates
)

detailed_components_inspection <- inspect_dataset(
  detailed_components_of_change
)

summary_components_inspection <- inspect_dataset(
  summary_components_of_change
)
# COVERAGE SUMMARY
create_coverage_summary <- function(detailed_population_estimates,
                                    detailed_components_of_change,
                                    summary_components_of_change) {
  
  coverage_summary <- tibble(
    dataset = c(
      "detailed_population_estimates",
      "detailed_components_of_change",
      "summary_components_of_change"
    ),
    countries_present = c(
      paste(sort(unique(detailed_population_estimates$country)), collapse = ", "),
      paste(sort(unique(detailed_components_of_change$country)), collapse = ", "),
      paste(sort(unique(summary_components_of_change$country)), collapse = ", ")
    ),
    number_of_local_authorities = c(
      n_distinct(detailed_population_estimates$ladcode21),
      n_distinct(detailed_components_of_change$ladcode21),
      n_distinct(summary_components_of_change$ladcode21)
    ),
    age_range = c(
      paste(
        min(detailed_population_estimates$age, na.rm = TRUE),
        "to",
        max(detailed_population_estimates$age, na.rm = TRUE)
      ),
      paste(
        min(detailed_components_of_change$age, na.rm = TRUE),
        "to",
        max(detailed_components_of_change$age, na.rm = TRUE)
      ),
      "Not age-disaggregated"
    ),
    sex_categories = c(
      paste(sort(unique(detailed_population_estimates$sex)), collapse = ", "),
      paste(sort(unique(detailed_components_of_change$sex)), collapse = ", "),
      "Not sex-disaggregated"
    )
  )
  
  print(coverage_summary)
  return(coverage_summary)
}

coverage_summary <- create_coverage_summary(
  detailed_population_estimates,
  detailed_components_of_change,
  summary_components_of_change
)
# YEAR COVERAGE
extract_years_from_columns <- function(dataset) {
  
  years <- names(dataset) %>%
    str_extract("\\d{4}$") %>%
    na.omit() %>%
    as.integer() %>%
    unique() %>%
    sort()
  
  return(years)
}

create_year_coverage <- function(detailed_population_estimates,
                                 detailed_components_of_change,
                                 summary_components_of_change) {
  
  population_years <- extract_years_from_columns(detailed_population_estimates)
  detailed_component_years <- extract_years_from_columns(detailed_components_of_change)
  summary_component_years <- extract_years_from_columns(summary_components_of_change)
  
  year_coverage <- tibble(
    dataset = c(
      "detailed_population_estimates",
      "detailed_components_of_change",
      "summary_components_of_change"
    ),
    first_year = c(
      min(population_years),
      min(detailed_component_years),
      min(summary_component_years)
    ),
    last_year = c(
      max(population_years),
      max(detailed_component_years),
      max(summary_component_years)
    ),
    number_of_years = c(
      length(population_years),
      length(detailed_component_years),
      length(summary_component_years)
    )
  )
  
  print(year_coverage)
  return(year_coverage)
}

year_coverage <- create_year_coverage(
  detailed_population_estimates,
  detailed_components_of_change,
  summary_components_of_change
)
# COLUMN COMPARISON BETWEEN DATASETS
compare_dataset_columns <- function(detailed_population_estimates,
                                    detailed_components_of_change,
                                    summary_components_of_change) {
  
  population_columns <- names(detailed_population_estimates)
  detailed_component_columns <- names(detailed_components_of_change)
  summary_component_columns <- names(summary_components_of_change)
  
  column_comparison <- list(
    population_not_in_detailed_components = sort(
      setdiff(population_columns, detailed_component_columns)
    ),
    detailed_components_not_in_population = sort(
      setdiff(detailed_component_columns, population_columns)
    ),
    summary_not_in_detailed_components = sort(
      setdiff(summary_component_columns, detailed_component_columns)
    ),
    detailed_components_not_in_summary = sort(
      setdiff(detailed_component_columns, summary_component_columns)
    )
  )
  
  print(column_comparison)
  return(column_comparison)
}

column_comparison <- compare_dataset_columns(
  detailed_population_estimates,
  detailed_components_of_change,
  summary_components_of_change
)
# COLUMN COMPARISON COUNT SUMMARY
summarise_column_comparison <- function(column_comparison) {
  
  column_comparison_summary <- tibble(
    comparison = c(
      "Columns in population dataset but not in detailed components dataset",
      "Columns in detailed components dataset but not in population dataset",
      "Columns in summary dataset but not in detailed components dataset",
      "Columns in detailed components dataset but not in summary dataset"
    ),
    number_of_columns = c(
      length(column_comparison$population_not_in_detailed_components),
      length(column_comparison$detailed_components_not_in_population),
      length(column_comparison$summary_not_in_detailed_components),
      length(column_comparison$detailed_components_not_in_summary)
    )
  )
  
  print(column_comparison_summary)
  return(column_comparison_summary)
}

column_comparison_summary <- summarise_column_comparison(
  column_comparison
)
# DATASET ROLE SUMMARY
create_dataset_role_summary <- function() {
  
  dataset_role_summary <- tibble(
    dataset = c(
      "detailed_population_estimates",
      "detailed_components_of_change",
      "summary_components_of_change"
    ),
    coverage = c(
      "United Kingdom",
      "England and Wales",
      "United Kingdom"
    ),
    level_of_detail = c(
      "Age and sex population estimates",
      "Age, sex, population and components of change",
      "Summary components of change"
    ),
    main_use_in_project = c(
      "Supporting dataset for population coverage comparison",
      "Main analytical dataset for feature engineering and modelling",
      "Validation dataset for checking summary totals"
    )
  )
  
  print(dataset_role_summary)
  return(dataset_role_summary)
}

dataset_role_summary <- create_dataset_role_summary()
# VALIDATING DETAILED DATASET AGAINST SUMMARY DATASET
validate_detailed_against_summary <- function(detailed_components_of_change,
                                              summary_components_of_change) {
  
  validation_results <- list()
  
  for (year in 2002:2020) {
    
    detailed_summary <- detailed_components_of_change %>%
      group_by(ladcode21, laname21, country) %>%
      summarise(
        detailed_population = sum(.data[[paste0("population_", year)]], na.rm = TRUE),
        detailed_births = sum(.data[[paste0("births_", year)]], na.rm = TRUE),
        detailed_deaths = sum(.data[[paste0("deaths_", year)]], na.rm = TRUE),
        detailed_internal_net = sum(.data[[paste0("internal_net_", year)]], na.rm = TRUE),
        detailed_international_net = sum(.data[[paste0("international_net_", year)]], na.rm = TRUE),
        .groups = "drop"
      )
    
    summary_data <- summary_components_of_change %>%
      select(
        ladcode21,
        laname21,
        country,
        all_of(paste0("population_", year)),
        all_of(paste0("births_", year)),
        all_of(paste0("deaths_", year)),
        all_of(paste0("internal_net_", year)),
        all_of(paste0("international_net_", year))
      )
    
    validation_check <- detailed_summary %>%
      inner_join(
        summary_data,
        by = c("ladcode21", "laname21", "country")
      )
    
    validation_results[[as.character(year)]] <- tibble(
      year = year,
      population_abs_diff = sum(
        abs(
          validation_check$detailed_population -
            validation_check[[paste0("population_", year)]]
        ),
        na.rm = TRUE
      ),
      births_abs_diff = sum(
        abs(
          validation_check$detailed_births -
            validation_check[[paste0("births_", year)]]
        ),
        na.rm = TRUE
      ),
      deaths_abs_diff = sum(
        abs(
          validation_check$detailed_deaths -
            validation_check[[paste0("deaths_", year)]]
        ),
        na.rm = TRUE
      ),
      internal_net_abs_diff = sum(
        abs(
          validation_check$detailed_internal_net -
            validation_check[[paste0("internal_net_", year)]]
        ),
        na.rm = TRUE
      ),
      international_net_abs_diff = sum(
        abs(
          validation_check$detailed_international_net -
            validation_check[[paste0("international_net_", year)]]
        ),
        na.rm = TRUE
      )
    )
  }
  
  summary_validation <- bind_rows(validation_results)
  
  print(head(summary_validation))
  return(summary_validation)
}

summary_validation <- validate_detailed_against_summary(
  detailed_components_of_change,
  summary_components_of_change
)
# MAXIMUM VALIDATION DIFFERENCE
summarise_validation_differences <- function(summary_validation) {
  
  validation_difference_summary <- summary_validation %>%
    select(-year) %>%
    summarise(across(everything(), max, na.rm = TRUE))
  
  print(validation_difference_summary)
  return(validation_difference_summary)
}

validation_difference_summary <- summarise_validation_differences(
  summary_validation
)
# VALIDATION PLOT
plot_validation_differences <- function(summary_validation) {
  
  summary_validation_long <- summary_validation %>%
    pivot_longer(
      cols = -year,
      names_to = "variable",
      values_to = "absolute_difference"
    )
  
  validation_plot <- ggplot(
    summary_validation_long,
    aes(
      x = year,
      y = absolute_difference,
      colour = variable
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Validation of Detailed Dataset Against Summary Dataset",
      x = "Year",
      y = "Absolute Difference",
      colour = "Variable"
    ) +
    theme_minimal()
  
  print(validation_plot)
  return(validation_plot)
}

validation_plot <- plot_validation_differences(
  summary_validation
)
# MAIN DATA QUALITY CHECK
create_main_data_quality_summary <- function(detailed_components_of_change) {
  
  main_data_quality <- tibble(
    check = c(
      "Rows",
      "Columns",
      "Duplicate rows",
      "Missing values",
      "Unique local authorities",
      "Countries present",
      "Age minimum",
      "Age maximum",
      "Sex categories"
    ),
    result = c(
      nrow(detailed_components_of_change),
      ncol(detailed_components_of_change),
      sum(duplicated(detailed_components_of_change)),
      sum(is.na(detailed_components_of_change)),
      n_distinct(detailed_components_of_change$ladcode21),
      paste(sort(unique(detailed_components_of_change$country)), collapse = ", "),
      min(detailed_components_of_change$age, na.rm = TRUE),
      max(detailed_components_of_change$age, na.rm = TRUE),
      paste(sort(unique(detailed_components_of_change$sex)), collapse = ", ")
    )
  )
  
  print(main_data_quality)
  return(main_data_quality)
}

main_data_quality <- create_main_data_quality_summary(
  detailed_components_of_change
)
# IDENTIFYING VARIABLE GROUPS
identify_variable_groups <- function(detailed_components_of_change) {
  
  variable_groups <- list(
    population = names(detailed_components_of_change)[str_detect(names(detailed_components_of_change), "^population_")],
    births = names(detailed_components_of_change)[str_detect(names(detailed_components_of_change), "^births_")],
    deaths = names(detailed_components_of_change)[str_detect(names(detailed_components_of_change), "^deaths_")],
    internal_in = names(detailed_components_of_change)[str_detect(names(detailed_components_of_change), "^internal_in_")],
    internal_out = names(detailed_components_of_change)[str_detect(names(detailed_components_of_change), "^internal_out_")],
    internal_net = names(detailed_components_of_change)[str_detect(names(detailed_components_of_change), "^internal_net_")],
    international_in = names(detailed_components_of_change)[str_detect(names(detailed_components_of_change), "^international_in_")],
    international_out = names(detailed_components_of_change)[str_detect(names(detailed_components_of_change), "^international_out_")],
    international_net = names(detailed_components_of_change)[str_detect(names(detailed_components_of_change), "^international_net_")],
    special_change = names(detailed_components_of_change)[str_detect(names(detailed_components_of_change), "^special_change_")],
    unattrib = names(detailed_components_of_change)[str_detect(names(detailed_components_of_change), "^unattrib_")],
    other_adjust = names(detailed_components_of_change)[str_detect(names(detailed_components_of_change), "^other_adjust_")]
  )
  
  variable_group_summary <- tibble(
    variable_group = names(variable_groups),
    number_of_columns = sapply(variable_groups, length),
    first_column = sapply(variable_groups, function(x) ifelse(length(x) > 0, x[1], NA)),
    last_column = sapply(variable_groups, function(x) ifelse(length(x) > 0, x[length(x)], NA))
  )
  
  print(variable_group_summary)
  
  return(list(
    variable_groups = variable_groups,
    variable_group_summary = variable_group_summary
  ))
}

variable_group_output <- identify_variable_groups(
  detailed_components_of_change
)

variable_groups <- variable_group_output$variable_groups
variable_group_summary <- variable_group_output$variable_group_summary
# RESHAPING MAIN DATASET FROM WIDE TO LONG PANEL FORMAT
create_population_panel <- function(detailed_components_of_change) {
  
  id_columns <- c(
    "ladcode21",
    "laname21",
    "country",
    "age",
    "sex"
  )
  
  population_panel_long <- detailed_components_of_change %>%
    pivot_longer(
      cols = -all_of(id_columns),
      names_to = "variable_year",
      values_to = "value"
    ) %>%
    separate(
      variable_year,
      into = c("variable", "year"),
      sep = "_(?=\\d{4}$)",
      convert = TRUE
    )
  
  population_panel <- population_panel_long %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    arrange(
      ladcode21,
      age,
      sex,
      year
    )
  
  print(head(population_panel, 10))
  print(dim(population_panel))
  glimpse(population_panel)
  
  return(list(
    population_panel_long = population_panel_long,
    population_panel = population_panel
  ))
}

population_panel_output <- create_population_panel(
  detailed_components_of_change
)

population_panel_long <- population_panel_output$population_panel_long
population_panel <- population_panel_output$population_panel
# VALIDATING WIDE-TO-LONG TRANSFORMATION
validate_population_panel_transformation <- function(detailed_components_of_change,
                                                     population_panel) {
  
  variables_to_check <- c(
    "population",
    "births",
    "deaths",
    "internal_in",
    "internal_out",
    "internal_net",
    "international_in",
    "international_out",
    "international_net",
    "special_change",
    "other_adjust",
    "unattrib"
  )
  
  transformation_validation_results <- list()
  result_index <- 1
  
  for (year in 2001:2020) {
    
    for (variable in variables_to_check) {
      
      original_column <- paste0(variable, "_", year)
      
      if (
        original_column %in% names(detailed_components_of_change) &&
        variable %in% names(population_panel)
      ) {
        
        original_total <- sum(
          detailed_components_of_change[[original_column]],
          na.rm = TRUE
        )
        
        panel_total <- population_panel %>%
          filter(year == !!year) %>%
          summarise(
            total_value = sum(.data[[variable]], na.rm = TRUE)
          ) %>%
          pull(total_value)
        
        transformation_validation_results[[result_index]] <- tibble(
          year = year,
          variable = variable,
          original_total = original_total,
          panel_total = panel_total,
          absolute_difference = abs(original_total - panel_total)
        )
        
        result_index <- result_index + 1
      }
    }
  }
  
  transformation_validation <- bind_rows(transformation_validation_results)
  
  print(head(transformation_validation, 20))
  print(max(transformation_validation$absolute_difference, na.rm = TRUE))
  
  return(transformation_validation)
}

transformation_validation <- validate_population_panel_transformation(
  detailed_components_of_change,
  population_panel
)
# PLOTTING WIDE-TO-LONG TRANSFORMATION VALIDATION
plot_transformation_validation <- function(transformation_validation) {
  
  transformation_validation_plot <- ggplot(
    transformation_validation,
    aes(
      x = year,
      y = absolute_difference,
      colour = variable
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Validation of Wide-to-Long Transformation",
      x = "Year",
      y = "Absolute Difference",
      colour = "Variable"
    ) +
    theme_minimal()
  
  print(transformation_validation_plot)
  
  return(transformation_validation_plot)
}

transformation_validation_plot <- plot_transformation_validation(
  transformation_validation
)
# PANEL MISSING VALUE SUMMARY
check_population_panel_missing_values <- function(population_panel) {
  
  panel_missing_summary <- sort(
    colSums(is.na(population_panel)),
    decreasing = TRUE
  )
  
  print(panel_missing_summary)
  
  return(panel_missing_summary)
}

panel_missing_summary <- check_population_panel_missing_values(
  population_panel
)
# CLEANING POPULATION PANEL
clean_population_panel <- function(population_panel) {
  
  component_columns <- c(
    "births",
    "deaths",
    "internal_in",
    "internal_out",
    "internal_net",
    "international_in",
    "international_out",
    "international_net",
    "special_change",
    "other_adjust",
    "unattrib"
  )
  
  available_component_columns <- intersect(
    component_columns,
    names(population_panel)
  )
  
  population_panel_clean <- population_panel %>%
    mutate(
      across(
        all_of(available_component_columns),
        ~ replace_na(.x, 0)
      )
    ) %>%
    mutate(
      sex = case_when(
        sex == 1 ~ "Male",
        sex == 2 ~ "Female",
        TRUE ~ as.character(sex)
      )
    ) %>%
    arrange(
      ladcode21,
      age,
      sex,
      year
    )
  
  print(head(population_panel_clean, 10))
  print(sort(colSums(is.na(population_panel_clean)), decreasing = TRUE))
  
  return(population_panel_clean)
}

population_panel_clean <- clean_population_panel(
  population_panel
)
# CREATING DEMOGRAPHIC FEATURES
create_demographic_panel_features <- function(population_panel_clean) {
  
  population_panel_features <- population_panel_clean %>%
    mutate(
      natural_change = births - deaths,
      
      total_net_migration = internal_net + international_net,
      
      total_inflow = internal_in + international_in,
      
      total_outflow = internal_out + international_out,
      
      total_adjustment = other_adjust + special_change + unattrib
    )
  
  print(
    head(
      population_panel_features %>%
        select(
          births,
          deaths,
          natural_change,
          internal_net,
          international_net,
          total_net_migration,
          total_inflow,
          total_outflow,
          total_adjustment
        ),
      10
    )
  )
  
  return(population_panel_features)
}

population_panel_features <- create_demographic_panel_features(
  population_panel_clean
)
# CREATING AGE GROUPS
assign_age_group <- function(age) {
  
  case_when(
    age <= 14 ~ "Children_0_14",
    age <= 24 ~ "Young_15_24",
    age <= 64 ~ "Working_25_64",
    TRUE ~ "Older_65_plus"
  )
}

create_age_group_variable <- function(population_panel_features) {
  
  population_panel_features <- population_panel_features %>%
    mutate(
      age_group = assign_age_group(age)
    )
  
  age_group_check <- population_panel_features %>%
    select(age, age_group) %>%
    distinct() %>%
    arrange(age)
  
  print(head(age_group_check, 20))
  
  return(population_panel_features)
}

population_panel_features <- create_age_group_variable(
  population_panel_features
)
# AGE GROUP SUMMARY
summarise_age_groups <- function(population_panel_features) {
  
  age_group_summary <- population_panel_features %>%
    group_by(age_group) %>%
    summarise(
      number_of_records = n(),
      total_population = sum(population, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(age_group_summary)
  
  return(age_group_summary)
}

age_group_summary <- summarise_age_groups(
  population_panel_features
)
# CREATING LOCAL-AUTHORITY-YEAR FEATURES
create_local_authority_year_features <- function(population_panel_features) {
  
  la_year_features <- population_panel_features %>%
    group_by(
      ladcode21,
      laname21,
      country,
      year
    ) %>%
    summarise(
      total_population = sum(population, na.rm = TRUE),
      births = sum(births, na.rm = TRUE),
      deaths = sum(deaths, na.rm = TRUE),
      natural_change = sum(natural_change, na.rm = TRUE),
      internal_net = sum(internal_net, na.rm = TRUE),
      international_net = sum(international_net, na.rm = TRUE),
      total_net_migration = sum(total_net_migration, na.rm = TRUE),
      total_inflow = sum(total_inflow, na.rm = TRUE),
      total_outflow = sum(total_outflow, na.rm = TRUE),
      total_adjustment = sum(total_adjustment, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(head(la_year_features, 10))
  print(dim(la_year_features))
  
  return(la_year_features)
}

la_year_features <- create_local_authority_year_features(
  population_panel_features
)
# CREATING POPULATION CHANGE AND RATE VARIABLES
create_local_authority_year_rates <- function(la_year_features) {
  
  la_year_features <- la_year_features %>%
    arrange(
      ladcode21,
      year
    ) %>%
    group_by(ladcode21) %>%
    mutate(
      population_change = total_population - lag(total_population),
      
      population_growth_rate = (
        population_change / lag(total_population)
      ) * 100,
      
      birth_rate_per_1000 = (
        births / total_population
      ) * 1000,
      
      death_rate_per_1000 = (
        deaths / total_population
      ) * 1000,
      
      migration_rate_per_1000 = (
        total_net_migration / total_population
      ) * 1000,
      
      natural_change_rate_per_1000 = (
        natural_change / total_population
      ) * 1000
    ) %>%
    ungroup()
  
  rate_columns <- c(
    "birth_rate_per_1000",
    "death_rate_per_1000",
    "migration_rate_per_1000",
    "natural_change_rate_per_1000"
  )
  
  la_year_features <- la_year_features %>%
    mutate(
      across(
        all_of(rate_columns),
        ~ if_else(year == 2001, NA_real_, .x)
      ),
      population_change = replace_na(population_change, 0),
      population_growth_rate = replace_na(population_growth_rate, 0)
    )
  
  print(head(la_year_features, 10))
  print(sort(colSums(is.na(la_year_features)), decreasing = TRUE))
  
  return(la_year_features)
}

la_year_features <- create_local_authority_year_rates(
  la_year_features
)
# CREATING AGE STRUCTURE DATASET
create_age_structure_dataset <- function(population_panel_features) {
  
  age_structure <- population_panel_features %>%
    group_by(
      ladcode21,
      laname21,
      country,
      year,
      age_group
    ) %>%
    summarise(
      age_group_population = sum(population, na.rm = TRUE),
      .groups = "drop"
    )
  
  age_structure_wide <- age_structure %>%
    pivot_wider(
      names_from = age_group,
      values_from = age_group_population,
      values_fill = 0
    )
  
  required_age_group_columns <- c(
    "Children_0_14",
    "Young_15_24",
    "Working_25_64",
    "Older_65_plus"
  )
  
  for (age_group_column in required_age_group_columns) {
    if (!(age_group_column %in% names(age_structure_wide))) {
      age_structure_wide[[age_group_column]] <- 0
    }
  }
  
  age_structure_wide <- age_structure_wide %>%
    mutate(
      age_total_population =
        Children_0_14 +
        Young_15_24 +
        Working_25_64 +
        Older_65_plus,
      
      Children_0_14_pct = (
        Children_0_14 / age_total_population
      ) * 100,
      
      Young_15_24_pct = (
        Young_15_24 / age_total_population
      ) * 100,
      
      Working_25_64_pct = (
        Working_25_64 / age_total_population
      ) * 100,
      
      Older_65_plus_pct = (
        Older_65_plus / age_total_population
      ) * 100,
      
      dependency_ratio = (
        (Children_0_14 + Older_65_plus) / Working_25_64
      ) * 100
    )
  
  print(head(age_structure_wide, 10))
  
  return(list(
    age_structure = age_structure,
    age_structure_wide = age_structure_wide
  ))
}

age_structure_output <- create_age_structure_dataset(
  population_panel_features
)

age_structure <- age_structure_output$age_structure
age_structure_wide <- age_structure_output$age_structure_wide
# MERGING AGE STRUCTURE FEATURES INTO LA-YEAR DATASET
merge_age_structure_with_la_year_features <- function(la_year_features,
                                                      age_structure_wide) {
  
  la_year_features <- la_year_features %>%
    left_join(
      age_structure_wide %>%
        select(
          ladcode21,
          year,
          Children_0_14_pct,
          Young_15_24_pct,
          Working_25_64_pct,
          Older_65_plus_pct,
          dependency_ratio
        ),
      by = c("ladcode21", "year")
    )
  
  print(head(la_year_features, 10))
  print(dim(la_year_features))
  print(sort(colSums(is.na(la_year_features)), decreasing = TRUE))
  
  return(la_year_features)
}

la_year_features <- merge_age_structure_with_la_year_features(
  la_year_features,
  age_structure_wide
)
# CREATING LOCAL-AUTHORITY MODELLING DATASET
create_local_authority_model_features <- function(la_year_features) {
  
  la_model_features <- la_year_features %>%
    group_by(
      ladcode21,
      laname21,
      country
    ) %>%
    summarise(
      population_2001 = first(total_population),
      population_2020 = last(total_population),
      
      total_births = sum(births, na.rm = TRUE),
      total_deaths = sum(deaths, na.rm = TRUE),
      total_natural_change = sum(natural_change, na.rm = TRUE),
      
      total_internal_net = sum(internal_net, na.rm = TRUE),
      total_international_net = sum(international_net, na.rm = TRUE),
      total_net_migration = sum(total_net_migration, na.rm = TRUE),
      
      total_inflow = sum(total_inflow, na.rm = TRUE),
      total_outflow = sum(total_outflow, na.rm = TRUE),
      total_adjustment = sum(total_adjustment, na.rm = TRUE),
      
      avg_birth_rate = mean(birth_rate_per_1000, na.rm = TRUE),
      avg_death_rate = mean(death_rate_per_1000, na.rm = TRUE),
      avg_migration_rate = mean(migration_rate_per_1000, na.rm = TRUE),
      avg_natural_change_rate = mean(natural_change_rate_per_1000, na.rm = TRUE),
      
      avg_children_pct = mean(Children_0_14_pct, na.rm = TRUE),
      avg_young_pct = mean(Young_15_24_pct, na.rm = TRUE),
      avg_working_pct = mean(Working_25_64_pct, na.rm = TRUE),
      avg_older_pct = mean(Older_65_plus_pct, na.rm = TRUE),
      avg_dependency_ratio = mean(dependency_ratio, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    mutate(
      absolute_growth = population_2020 - population_2001,
      
      growth_rate_2001_2020 = (
        absolute_growth / population_2001
      ) * 100,
      
      migration_contribution_pct = (
        total_net_migration / if_else(absolute_growth == 0, NA_real_, absolute_growth)
      ) * 100,
      
      natural_contribution_pct = (
        total_natural_change / if_else(absolute_growth == 0, NA_real_, absolute_growth)
      ) * 100,
      
      growth_class = ntile(growth_rate_2001_2020, 3),
      
      growth_class = case_when(
        growth_class == 1 ~ "Low_growth",
        growth_class == 2 ~ "Medium_growth",
        growth_class == 3 ~ "High_growth"
      ),
      
      growth_class = factor(
        growth_class,
        levels = c("Low_growth", "Medium_growth", "High_growth")
      ),
      
      main_growth_driver = case_when(
        abs(total_net_migration) > abs(total_natural_change) ~ "Migration_driven",
        TRUE ~ "Natural_change_driven"
      )
    )
  
  print(head(la_model_features, 10))
  print(dim(la_model_features))
  print(sort(colSums(is.na(la_model_features)), decreasing = TRUE))
  
  return(la_model_features)
}

la_model_features <- create_local_authority_model_features(
  la_year_features
)
# CHECKING GROWTH CLASS DISTRIBUTION
check_growth_class_distribution <- function(la_model_features) {
  
  growth_class_distribution <- la_model_features %>%
    count(
      growth_class,
      name = "number_of_local_authorities"
    )
  
  print(growth_class_distribution)
  
  return(growth_class_distribution)
}

growth_class_distribution <- check_growth_class_distribution(
  la_model_features
)
# PLOTTING GROWTH CLASS DISTRIBUTION
plot_growth_class_distribution <- function(la_model_features) {
  
  growth_class_distribution_plot <- ggplot(
    la_model_features,
    aes(x = growth_class)
  ) +
    geom_bar() +
    geom_text(
      stat = "count",
      aes(label = after_stat(count)),
      vjust = -0.3
    ) +
    labs(
      title = "Distribution of Growth Classes",
      x = "Growth Class",
      y = "Number of Local Authorities"
    ) +
    theme_minimal()
  
  print(growth_class_distribution_plot)
  
  return(growth_class_distribution_plot)
}

growth_class_distribution_plot <- plot_growth_class_distribution(
  la_model_features
)
# DISTRIBUTION OF POPULATION GROWTH RATES
plot_growth_rate_distribution <- function(la_model_features) {
  
  growth_rate_distribution_plot <- ggplot(
    la_model_features,
    aes(x = growth_rate_2001_2020)
  ) +
    geom_histogram(
      bins = 30,
      colour = "black",
      fill = "grey70"
    ) +
    geom_density(
      aes(y = after_stat(count)),
      linewidth = 1
    ) +
    geom_vline(
      xintercept = mean(la_model_features$growth_rate_2001_2020, na.rm = TRUE),
      linetype = "dashed"
    ) +
    geom_vline(
      xintercept = median(la_model_features$growth_rate_2001_2020, na.rm = TRUE),
      linetype = "dotted"
    ) +
    labs(
      title = "Distribution of Population Growth Rates Across England and Wales",
      x = "Population Growth Rate 2001–2020 (%)",
      y = "Number of Local Authorities"
    ) +
    theme_minimal()
  
  print(growth_rate_distribution_plot)
  
  growth_rate_summary <- la_model_features %>%
    summarise(
      mean_growth_rate = mean(growth_rate_2001_2020, na.rm = TRUE),
      median_growth_rate = median(growth_rate_2001_2020, na.rm = TRUE),
      minimum_growth_rate = min(growth_rate_2001_2020, na.rm = TRUE),
      maximum_growth_rate = max(growth_rate_2001_2020, na.rm = TRUE),
      standard_deviation = sd(growth_rate_2001_2020, na.rm = TRUE)
    )
  
  print(growth_rate_summary)
  
  return(list(
    plot = growth_rate_distribution_plot,
    summary = growth_rate_summary
  ))
}

growth_rate_distribution_output <- plot_growth_rate_distribution(
  la_model_features
)
# TOTAL POPULATION TREND
create_population_trend <- function(la_year_features) {
  
  population_trend <- la_year_features %>%
    group_by(year) %>%
    summarise(
      total_population = sum(total_population, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(population_trend)
  
  return(population_trend)
}

population_trend <- create_population_trend(
  la_year_features
)
plot_population_trend <- function(population_trend) {
  
  population_trend_plot <- ggplot(
    population_trend,
    aes(x = year, y = total_population)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(
      labels = label_number(scale = 1e-6, suffix = "M")
    ) +
    scale_x_continuous(
      breaks = population_trend$year
    ) +
    labs(
      title = "Total Population Trend in England and Wales",
      x = "Year",
      y = "Total Population"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(population_trend_plot)
  
  return(population_trend_plot)
}

population_trend_plot <- plot_population_trend(
  population_trend
)
# BIRTHS, DEATHS AND NATURAL CHANGE OVER TIME
create_natural_change_trend <- function(la_year_features) {
  
  natural_change_trend <- la_year_features %>%
    filter(year >= 2002) %>%
    group_by(year) %>%
    summarise(
      total_births = sum(births, na.rm = TRUE),
      total_deaths = sum(deaths, na.rm = TRUE),
      total_natural_change = sum(natural_change, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(head(natural_change_trend))
  print(tail(natural_change_trend))
  
  return(natural_change_trend)
}

natural_change_trend <- create_natural_change_trend(
  la_year_features
)
plot_natural_change_trend <- function(natural_change_trend) {
  
  natural_change_trend_long <- natural_change_trend %>%
    pivot_longer(
      cols = c(
        total_births,
        total_deaths,
        total_natural_change
      ),
      names_to = "component",
      values_to = "value"
    )
  
  natural_change_trend_plot <- ggplot(
    natural_change_trend_long,
    aes(x = year, y = value, colour = component)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(
      labels = comma
    ) +
    scale_x_continuous(
      breaks = natural_change_trend$year
    ) +
    labs(
      title = "Births, Deaths and Natural Change Over Time",
      x = "Year",
      y = "Count",
      colour = "Component"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(natural_change_trend_plot)
  
  return(natural_change_trend_plot)
}

natural_change_trend_plot <- plot_natural_change_trend(
  natural_change_trend
)
# MIGRATION COMPONENTS OVER TIME
create_migration_trend <- function(la_year_features) {
  
  migration_trend <- la_year_features %>%
    filter(year >= 2002) %>%
    group_by(year) %>%
    summarise(
      internal_net = sum(internal_net, na.rm = TRUE),
      international_net = sum(international_net, na.rm = TRUE),
      total_net_migration = sum(total_net_migration, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(head(migration_trend))
  print(tail(migration_trend))
  
  return(migration_trend)
}

migration_trend <- create_migration_trend(
  la_year_features
)
plot_migration_trend <- function(migration_trend) {
  
  migration_trend_long <- migration_trend %>%
    pivot_longer(
      cols = c(
        internal_net,
        international_net,
        total_net_migration
      ),
      names_to = "migration_component",
      values_to = "value"
    )
  
  migration_trend_plot <- ggplot(
    migration_trend_long,
    aes(x = year, y = value, colour = migration_component)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(
      labels = comma
    ) +
    scale_x_continuous(
      breaks = migration_trend$year
    ) +
    labs(
      title = "Migration Components Over Time",
      x = "Year",
      y = "Net Migration",
      colour = "Migration Component"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(migration_trend_plot)
  
  return(migration_trend_plot)
}

migration_trend_plot <- plot_migration_trend(
  migration_trend
)
# NATURAL CHANGE AND NET MIGRATION OVER TIME
create_growth_driver_trend <- function(la_year_features) {
  
  growth_driver_trend <- la_year_features %>%
    filter(year >= 2002) %>%
    group_by(year) %>%
    summarise(
      total_natural_change = sum(natural_change, na.rm = TRUE),
      total_net_migration = sum(total_net_migration, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(head(growth_driver_trend))
  print(tail(growth_driver_trend))
  
  return(growth_driver_trend)
}

growth_driver_trend <- create_growth_driver_trend(
  la_year_features
)
plot_growth_driver_trend <- function(growth_driver_trend) {
  
  growth_driver_trend_long <- growth_driver_trend %>%
    pivot_longer(
      cols = c(
        total_natural_change,
        total_net_migration
      ),
      names_to = "growth_driver",
      values_to = "value"
    )
  
  growth_driver_trend_plot <- ggplot(
    growth_driver_trend_long,
    aes(x = year, y = value, colour = growth_driver)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(
      labels = comma
    ) +
    scale_x_continuous(
      breaks = growth_driver_trend$year
    ) +
    labs(
      title = "Natural Change and Net Migration Over Time",
      x = "Year",
      y = "Count",
      colour = "Growth Driver"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(growth_driver_trend_plot)
  
  return(growth_driver_trend_plot)
}

growth_driver_trend_plot <- plot_growth_driver_trend(
  growth_driver_trend
)
# POPULATION TREND BY COUNTRY
create_country_population_trend <- function(la_year_features) {
  
  country_population_trend <- la_year_features %>%
    group_by(country, year) %>%
    summarise(
      total_population = sum(total_population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      country_name = case_when(
        country == "E" ~ "England",
        country == "W" ~ "Wales",
        TRUE ~ country
      )
    )
  
  print(head(country_population_trend, 10))
  print(tail(country_population_trend, 10))
  
  return(country_population_trend)
}

country_population_trend <- create_country_population_trend(
  la_year_features
)

plot_country_population_trend <- function(country_population_trend) {
  
  country_population_trend_plot <- ggplot(
    country_population_trend,
    aes(
      x = year,
      y = total_population,
      colour = country_name
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(
      labels = label_number(scale = 1e-6, suffix = "M")
    ) +
    scale_x_continuous(
      breaks = sort(unique(country_population_trend$year))
    ) +
    labs(
      title = "Population Trend by Country",
      x = "Year",
      y = "Total Population",
      colour = "Country"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(country_population_trend_plot)
  
  return(country_population_trend_plot)
}

country_population_trend_plot <- plot_country_population_trend(
  country_population_trend
)
# INDEXED POPULATION GROWTH BY COUNTRY
create_country_population_index <- function(country_population_trend) {
  
  country_population_index <- country_population_trend %>%
    group_by(country_name) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      population_index_2001 = (
        total_population / first(total_population)
      ) * 100
    ) %>%
    ungroup()
  
  print(head(country_population_index, 10))
  print(tail(country_population_index, 10))
  
  return(country_population_index)
}

country_population_index <- create_country_population_index(
  country_population_trend
)

plot_country_population_index <- function(country_population_index) {
  
  country_population_index_plot <- ggplot(
    country_population_index,
    aes(
      x = year,
      y = population_index_2001,
      colour = country_name
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 100, linetype = "dashed") +
    scale_x_continuous(
      breaks = sort(unique(country_population_index$year))
    ) +
    labs(
      title = "Indexed Population Growth by Country",
      x = "Year",
      y = "Population Index (2001 = 100)",
      colour = "Country"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(country_population_index_plot)
  
  return(country_population_index_plot)
}

country_population_index_plot <- plot_country_population_index(
  country_population_index
)
# COUNTRY GROWTH SUMMARY
create_country_growth_summary <- function(country_population_index) {
  
  country_growth_summary <- country_population_index %>%
    group_by(country_name) %>%
    summarise(
      population_2001 = first(total_population),
      population_2020 = last(total_population),
      .groups = "drop"
    ) %>%
    mutate(
      absolute_growth = population_2020 - population_2001,
      growth_rate_2001_2020 = (
        absolute_growth / population_2001
      ) * 100
    )
  
  print(country_growth_summary)
  
  return(country_growth_summary)
}

country_growth_summary <- create_country_growth_summary(
  country_population_index
)

plot_country_growth_summary <- function(country_growth_summary) {
  
  country_growth_summary_plot <- ggplot(
    country_growth_summary,
    aes(
      x = country_name,
      y = growth_rate_2001_2020
    )
  ) +
    geom_col() +
    geom_text(
      aes(label = paste0(round(growth_rate_2001_2020, 2), "%")),
      vjust = -0.3
    ) +
    labs(
      title = "Population Growth Rate by Country, 2001–2020",
      x = "Country",
      y = "Growth Rate (%)"
    ) +
    theme_minimal()
  
  print(country_growth_summary_plot)
  
  return(country_growth_summary_plot)
}

country_growth_summary_plot <- plot_country_growth_summary(
  country_growth_summary
)
# LOCAL AUTHORITY SUMMARY
create_local_authority_summary <- function(la_model_features) {
  
  local_authority_summary <- tibble(
    dataset = "la_model_features",
    level_of_analysis = "One row per local authority",
    number_of_local_authorities = n_distinct(la_model_features$ladcode21),
    rows = nrow(la_model_features),
    columns = ncol(la_model_features),
    countries_included = paste(
      sort(unique(la_model_features$country)),
      collapse = ", "
    )
  )
  
  print(local_authority_summary)
  
  return(local_authority_summary)
}

local_authority_summary <- create_local_authority_summary(
  la_model_features
)

# LOCAL AUTHORITY COUNTRY SUMMARY
create_local_authority_country_summary <- function(la_model_features) {
  
  la_country_summary <- la_model_features %>%
    mutate(
      country_name = case_when(
        country == "E" ~ "England",
        country == "W" ~ "Wales",
        TRUE ~ country
      )
    ) %>%
    group_by(country_name) %>%
    summarise(
      number_of_local_authorities = n_distinct(ladcode21),
      mean_growth_rate = mean(growth_rate_2001_2020, na.rm = TRUE),
      median_growth_rate = median(growth_rate_2001_2020, na.rm = TRUE),
      mean_dependency_ratio = mean(avg_dependency_ratio, na.rm = TRUE),
      mean_older_pct = mean(avg_older_pct, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(la_country_summary)
  
  return(la_country_summary)
}

la_country_summary <- create_local_authority_country_summary(
  la_model_features
)

plot_local_authority_count_by_country <- function(la_model_features) {
  
  la_country_count <- la_model_features %>%
    mutate(
      country_name = case_when(
        country == "E" ~ "England",
        country == "W" ~ "Wales",
        TRUE ~ country
      )
    )
  
  la_country_count_plot <- ggplot(
    la_country_count,
    aes(x = country_name)
  ) +
    geom_bar() +
    geom_text(
      stat = "count",
      aes(label = after_stat(count)),
      vjust = -0.3
    ) +
    labs(
      title = "Number of Local Authorities by Country",
      x = "Country",
      y = "Number of Local Authorities"
    ) +
    theme_minimal()
  
  print(la_country_count_plot)
  
  return(la_country_count_plot)
}

local_authority_count_by_country_plot <- plot_local_authority_count_by_country(
  la_model_features
)
# LOCAL AUTHORITY DEMOGRAPHIC SUMMARY
create_local_authority_demographic_summary <- function(la_model_features) {
  
  local_authority_demographic_summary <- la_model_features %>%
    select(
      ladcode21,
      laname21,
      country,
      population_2001,
      population_2020,
      absolute_growth,
      growth_rate_2001_2020,
      total_net_migration,
      total_natural_change,
      avg_birth_rate,
      avg_death_rate,
      avg_older_pct,
      avg_dependency_ratio,
      growth_class,
      main_growth_driver
    ) %>%
    arrange(desc(growth_rate_2001_2020))
  
  print(head(local_authority_demographic_summary, 20))
  
  return(local_authority_demographic_summary)
}

local_authority_demographic_summary <- create_local_authority_demographic_summary(
  la_model_features
)

# TOP AND BOTTOM LOCAL AUTHORITIES BY GROWTH RATE
create_top_bottom_growth_local_authorities <- function(la_model_features) {
  
  top_growth_local_authorities <- la_model_features %>%
    arrange(desc(growth_rate_2001_2020)) %>%
    slice_head(n = 10) %>%
    mutate(growth_group = "Highest growth")
  
  bottom_growth_local_authorities <- la_model_features %>%
    arrange(growth_rate_2001_2020) %>%
    slice_head(n = 10) %>%
    mutate(growth_group = "Lowest growth")
  
  top_bottom_growth_local_authorities <- bind_rows(
    top_growth_local_authorities,
    bottom_growth_local_authorities
  ) %>%
    arrange(growth_rate_2001_2020)
  
  print(
    top_bottom_growth_local_authorities %>%
      select(
        laname21,
        country,
        growth_rate_2001_2020,
        absolute_growth,
        growth_group
      )
  )
  
  return(list(
    top_growth_local_authorities = top_growth_local_authorities,
    bottom_growth_local_authorities = bottom_growth_local_authorities,
    top_bottom_growth_local_authorities = top_bottom_growth_local_authorities
  ))
}

top_bottom_growth_output <- create_top_bottom_growth_local_authorities(
  la_model_features
)

top_growth_local_authorities <- top_bottom_growth_output$top_growth_local_authorities
bottom_growth_local_authorities <- top_bottom_growth_output$bottom_growth_local_authorities
top_bottom_growth_local_authorities <- top_bottom_growth_output$top_bottom_growth_local_authorities

# PLOTTING TOP AND BOTTOM LOCAL AUTHORITIES
plot_top_bottom_growth_local_authorities <- function(top_bottom_growth_local_authorities) {
  
  top_bottom_growth_plot <- ggplot(
    top_bottom_growth_local_authorities,
    aes(
      x = growth_rate_2001_2020,
      y = reorder(laname21, growth_rate_2001_2020),
      fill = growth_group
    )
  ) +
    geom_col() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      title = "Top and Bottom 10 Local Authorities by Population Growth Rate",
      x = "Population Growth Rate 2001–2020 (%)",
      y = "Local Authority",
      fill = "Growth Group"
    ) +
    theme_minimal()
  
  print(top_bottom_growth_plot)
  
  return(top_bottom_growth_plot)
}

top_bottom_growth_plot <- plot_top_bottom_growth_local_authorities(
  top_bottom_growth_local_authorities
)

# RELATIONSHIP BETWEEN NET MIGRATION AND POPULATION GROWTH
analyse_migration_growth_relationship <- function(la_model_features) {
  
  migration_growth_correlation <- cor(
    la_model_features$total_net_migration,
    la_model_features$absolute_growth,
    use = "complete.obs"
  )
  
  migration_growth_correlation_table <- tibble(
    relationship = "Total net migration vs absolute population growth",
    correlation = migration_growth_correlation
  )
  
  migration_growth_plot <- ggplot(
    la_model_features,
    aes(
      x = total_net_migration,
      y = absolute_growth
    )
  ) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Relationship Between Net Migration and Population Growth",
      x = "Total Net Migration, 2002–2020",
      y = "Absolute Population Growth, 2001–2020"
    ) +
    theme_minimal()
  
  print(migration_growth_correlation_table)
  print(migration_growth_plot)
  
  return(list(
    correlation_table = migration_growth_correlation_table,
    plot = migration_growth_plot
  ))
}

migration_growth_relationship <- analyse_migration_growth_relationship(
  la_model_features
)

# RELATIONSHIP BETWEEN OLDER POPULATION SHARE AND GROWTH RATE
analyse_ageing_growth_relationship <- function(la_model_features) {
  
  ageing_growth_correlation <- cor(
    la_model_features$avg_older_pct,
    la_model_features$growth_rate_2001_2020,
    use = "complete.obs"
  )
  
  ageing_growth_correlation_table <- tibble(
    relationship = "Average older population share vs population growth rate",
    correlation = ageing_growth_correlation
  )
  
  ageing_growth_plot <- ggplot(
    la_model_features,
    aes(
      x = avg_older_pct,
      y = growth_rate_2001_2020
    )
  ) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
      title = "Relationship Between Older Population Share and Growth Rate",
      x = "Average Older Population Share (%)",
      y = "Population Growth Rate 2001–2020 (%)"
    ) +
    theme_minimal()
  
  print(ageing_growth_correlation_table)
  print(ageing_growth_plot)
  
  return(list(
    correlation_table = ageing_growth_correlation_table,
    plot = ageing_growth_plot
  ))
}

ageing_growth_relationship <- analyse_ageing_growth_relationship(
  la_model_features
)
# RELATIONSHIP BETWEEN DEPENDENCY RATIO AND GROWTH RATE
analyse_dependency_growth_relationship <- function(la_model_features) {
  
  dependency_growth_correlation <- cor(
    la_model_features$avg_dependency_ratio,
    la_model_features$growth_rate_2001_2020,
    use = "complete.obs"
  )
  
  dependency_growth_correlation_table <- tibble(
    relationship = "Average dependency ratio vs population growth rate",
    correlation = dependency_growth_correlation
  )
  
  dependency_growth_plot <- ggplot(
    la_model_features,
    aes(
      x = avg_dependency_ratio,
      y = growth_rate_2001_2020
    )
  ) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
      title = "Relationship Between Dependency Ratio and Growth Rate",
      x = "Average Dependency Ratio",
      y = "Population Growth Rate 2001–2020 (%)"
    ) +
    theme_minimal()
  
  print(dependency_growth_correlation_table)
  print(dependency_growth_plot)
  
  return(list(
    correlation_table = dependency_growth_correlation_table,
    plot = dependency_growth_plot
  ))
}

dependency_growth_relationship <- analyse_dependency_growth_relationship(
  la_model_features
)

# POPULATION TREND BY SEX
create_sex_population_trend <- function(population_panel_features) {
  
  sex_population_trend <- population_panel_features %>%
    group_by(
      sex,
      year
    ) %>%
    summarise(
      total_population = sum(population, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(head(sex_population_trend, 10))
  print(tail(sex_population_trend, 10))
  
  return(sex_population_trend)
}

sex_population_trend <- create_sex_population_trend(
  population_panel_features
)

plot_sex_population_trend <- function(sex_population_trend) {
  
  sex_population_trend_plot <- ggplot(
    sex_population_trend,
    aes(
      x = year,
      y = total_population,
      colour = sex
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(
      labels = label_number(scale = 1e-6, suffix = "M")
    ) +
    scale_x_continuous(
      breaks = sort(unique(sex_population_trend$year))
    ) +
    labs(
      title = "Population Trend by Sex",
      x = "Year",
      y = "Total Population",
      colour = "Sex"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(sex_population_trend_plot)
  
  return(sex_population_trend_plot)
}

sex_population_trend_plot <- plot_sex_population_trend(
  sex_population_trend
)

# BIRTHS BY SEX OVER TIME
create_births_by_sex <- function(population_panel_features) {
  
  births_by_sex <- population_panel_features %>%
    filter(year >= 2002) %>%
    group_by(
      sex,
      year
    ) %>%
    summarise(
      total_births = sum(births, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(head(births_by_sex, 10))
  print(tail(births_by_sex, 10))
  
  return(births_by_sex)
}

births_by_sex <- create_births_by_sex(
  population_panel_features
)

plot_births_by_sex <- function(births_by_sex) {
  
  births_by_sex_plot <- ggplot(
    births_by_sex,
    aes(
      x = year,
      y = total_births,
      colour = sex
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(
      breaks = sort(unique(births_by_sex$year))
    ) +
    labs(
      title = "Births by Sex Over Time",
      x = "Year",
      y = "Total Births",
      colour = "Sex"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(births_by_sex_plot)
  
  return(births_by_sex_plot)
}

births_by_sex_plot <- plot_births_by_sex(
  births_by_sex
)

# DEATHS BY SEX OVER TIME
create_deaths_by_sex <- function(population_panel_features) {
  
  deaths_by_sex <- population_panel_features %>%
    filter(year >= 2002) %>%
    group_by(
      sex,
      year
    ) %>%
    summarise(
      total_deaths = sum(deaths, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(head(deaths_by_sex, 10))
  print(tail(deaths_by_sex, 10))
  
  return(deaths_by_sex)
}

deaths_by_sex <- create_deaths_by_sex(
  population_panel_features
)

plot_deaths_by_sex <- function(deaths_by_sex) {
  
  deaths_by_sex_plot <- ggplot(
    deaths_by_sex,
    aes(
      x = year,
      y = total_deaths,
      colour = sex
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(
      breaks = sort(unique(deaths_by_sex$year))
    ) +
    labs(
      title = "Deaths by Sex Over Time",
      x = "Year",
      y = "Total Deaths",
      colour = "Sex"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(deaths_by_sex_plot)
  
  return(deaths_by_sex_plot)
}

deaths_by_sex_plot <- plot_deaths_by_sex(
  deaths_by_sex
)

# NATURAL CHANGE BY SEX OVER TIME
create_natural_change_by_sex <- function(population_panel_features) {
  
  natural_change_by_sex <- population_panel_features %>%
    filter(year >= 2002) %>%
    group_by(
      sex,
      year
    ) %>%
    summarise(
      total_births = sum(births, na.rm = TRUE),
      total_deaths = sum(deaths, na.rm = TRUE),
      total_natural_change = sum(natural_change, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(head(natural_change_by_sex, 10))
  print(tail(natural_change_by_sex, 10))
  
  return(natural_change_by_sex)
}

natural_change_by_sex <- create_natural_change_by_sex(
  population_panel_features
)

plot_natural_change_by_sex <- function(natural_change_by_sex) {
  
  natural_change_by_sex_plot <- ggplot(
    natural_change_by_sex,
    aes(
      x = year,
      y = total_natural_change,
      colour = sex
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(
      breaks = sort(unique(natural_change_by_sex$year))
    ) +
    labs(
      title = "Natural Change by Sex Over Time",
      x = "Year",
      y = "Natural Change",
      colour = "Sex"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(natural_change_by_sex_plot)
  
  return(natural_change_by_sex_plot)
}

natural_change_by_sex_plot <- plot_natural_change_by_sex(
  natural_change_by_sex
)

# TOTAL NET MIGRATION BY SEX OVER TIME
create_migration_by_sex <- function(population_panel_features) {
  
  migration_by_sex <- population_panel_features %>%
    filter(year >= 2002) %>%
    group_by(
      sex,
      year
    ) %>%
    summarise(
      internal_net = sum(internal_net, na.rm = TRUE),
      international_net = sum(international_net, na.rm = TRUE),
      total_net_migration = sum(total_net_migration, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(head(migration_by_sex, 10))
  print(tail(migration_by_sex, 10))
  
  return(migration_by_sex)
}

migration_by_sex <- create_migration_by_sex(
  population_panel_features
)

plot_migration_by_sex <- function(migration_by_sex) {
  
  migration_by_sex_plot <- ggplot(
    migration_by_sex,
    aes(
      x = year,
      y = total_net_migration,
      colour = sex
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(
      breaks = sort(unique(migration_by_sex$year))
    ) +
    labs(
      title = "Total Net Migration by Sex Over Time",
      x = "Year",
      y = "Total Net Migration",
      colour = "Sex"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(migration_by_sex_plot)
  
  return(migration_by_sex_plot)
}

migration_by_sex_plot <- plot_migration_by_sex(
  migration_by_sex
)

# AGE STRUCTURE BY SEX
create_age_sex_structure <- function(population_panel_features) {
  
  age_sex_structure <- population_panel_features %>%
    group_by(
      sex,
      age_group
    ) %>%
    summarise(
      total_population = sum(population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(sex) %>%
    mutate(
      percentage_within_sex = (
        total_population / sum(total_population, na.rm = TRUE)
      ) * 100
    ) %>%
    ungroup()
  
  print(age_sex_structure)
  
  return(age_sex_structure)
}

age_sex_structure <- create_age_sex_structure(
  population_panel_features
)

plot_age_sex_structure <- function(age_sex_structure) {
  
  age_sex_structure_plot <- ggplot(
    age_sex_structure,
    aes(
      x = age_group,
      y = percentage_within_sex,
      fill = sex
    )
  ) +
    geom_col(position = "dodge") +
    scale_x_discrete(
      limits = c(
        "Children_0_14",
        "Young_15_24",
        "Working_25_64",
        "Older_65_plus"
      )
    ) +
    labs(
      title = "Age Structure by Sex",
      x = "Age Group",
      y = "Percentage Within Sex (%)",
      fill = "Sex"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
  
  print(age_sex_structure_plot)
  
  return(age_sex_structure_plot)
}

age_sex_structure_plot <- plot_age_sex_structure(
  age_sex_structure
)

# SEX SUMMARY TABLE
create_sex_summary <- function(population_panel_features) {
  
  sex_summary <- population_panel_features %>%
    filter(year >= 2002) %>%
    group_by(sex) %>%
    summarise(
      population_person_years = sum(population, na.rm = TRUE),
      total_births = sum(births, na.rm = TRUE),
      total_deaths = sum(deaths, na.rm = TRUE),
      total_natural_change = sum(natural_change, na.rm = TRUE),
      total_net_migration = sum(total_net_migration, na.rm = TRUE),
      .groups = "drop"
    )
  
  weighted_age_by_sex <- population_panel_features %>%
    filter(year >= 2002) %>%
    mutate(
      weighted_age = age * population
    ) %>%
    group_by(sex) %>%
    summarise(
      weighted_age_total = sum(weighted_age, na.rm = TRUE),
      population_total = sum(population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      population_weighted_average_age = weighted_age_total / population_total
    ) %>%
    select(
      sex,
      population_weighted_average_age
    )
  
  sex_summary <- sex_summary %>%
    left_join(
      weighted_age_by_sex,
      by = "sex"
    ) %>%
    mutate(
      birth_rate_per_1000 = (
        total_births / population_person_years
      ) * 1000,
      
      death_rate_per_1000 = (
        total_deaths / population_person_years
      ) * 1000,
      
      migration_rate_per_1000 = (
        total_net_migration / population_person_years
      ) * 1000
    )
  
  print(sex_summary)
  
  return(sex_summary)
}

sex_summary <- create_sex_summary(
  population_panel_features
)

# AVERAGE BIRTH AND DEATH RATES OVER TIME
create_rate_trend <- function(la_year_features) {
  
  rate_trend <- la_year_features %>%
    filter(year >= 2002) %>%
    group_by(year) %>%
    summarise(
      avg_birth_rate = mean(birth_rate_per_1000, na.rm = TRUE),
      avg_death_rate = mean(death_rate_per_1000, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(head(rate_trend))
  print(tail(rate_trend))
  
  return(rate_trend)
}

rate_trend <- create_rate_trend(
  la_year_features
)

plot_rate_trend <- function(rate_trend) {
  
  rate_trend_long <- rate_trend %>%
    pivot_longer(
      cols = c(
        avg_birth_rate,
        avg_death_rate
      ),
      names_to = "rate_type",
      values_to = "rate_per_1000"
    )
  
  rate_trend_plot <- ggplot(
    rate_trend_long,
    aes(
      x = year,
      y = rate_per_1000,
      colour = rate_type
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(
      breaks = sort(unique(rate_trend$year))
    ) +
    labs(
      title = "Average Birth and Death Rates Over Time",
      x = "Year",
      y = "Rate per 1,000 Population",
      colour = "Rate Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(rate_trend_plot)
  
  return(rate_trend_plot)
}

rate_trend_plot <- plot_rate_trend(
  rate_trend
)

# AVERAGE MIGRATION RATE OVER TIME
create_migration_rate_trend <- function(la_year_features) {
  
  migration_rate_trend <- la_year_features %>%
    filter(year >= 2002) %>%
    group_by(year) %>%
    summarise(
      avg_migration_rate = mean(migration_rate_per_1000, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(head(migration_rate_trend))
  print(tail(migration_rate_trend))
  
  return(migration_rate_trend)
}

migration_rate_trend <- create_migration_rate_trend(
  la_year_features
)

plot_migration_rate_trend <- function(migration_rate_trend) {
  
  migration_rate_trend_plot <- ggplot(
    migration_rate_trend,
    aes(
      x = year,
      y = avg_migration_rate
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(
      breaks = sort(unique(migration_rate_trend$year))
    ) +
    labs(
      title = "Average Migration Rate Over Time",
      x = "Year",
      y = "Migration Rate per 1,000 Population"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(migration_rate_trend_plot)
  
  return(migration_rate_trend_plot)
}

migration_rate_trend_plot <- plot_migration_rate_trend(
  migration_rate_trend
)

# DEMOGRAPHIC PROFILE BY GROWTH CLASS
create_growth_class_profile <- function(la_model_features) {
  
  growth_class_profile <- la_model_features %>%
    group_by(growth_class) %>%
    summarise(
      avg_growth_rate = mean(growth_rate_2001_2020, na.rm = TRUE),
      avg_birth_rate = mean(avg_birth_rate, na.rm = TRUE),
      avg_death_rate = mean(avg_death_rate, na.rm = TRUE),
      avg_migration_rate = mean(avg_migration_rate, na.rm = TRUE),
      avg_natural_change_rate = mean(avg_natural_change_rate, na.rm = TRUE),
      avg_older_pct = mean(avg_older_pct, na.rm = TRUE),
      avg_dependency_ratio = mean(avg_dependency_ratio, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(growth_class_profile)
  
  return(growth_class_profile)
}

growth_class_profile <- create_growth_class_profile(
  la_model_features
)

plot_growth_class_profile <- function(growth_class_profile) {
  
  growth_class_profile_long <- growth_class_profile %>%
    pivot_longer(
      cols = -growth_class,
      names_to = "feature",
      values_to = "average_value"
    )
  
  growth_class_profile_plot <- ggplot(
    growth_class_profile_long,
    aes(
      x = feature,
      y = average_value,
      fill = growth_class
    )
  ) +
    geom_col(position = "dodge") +
    labs(
      title = "Demographic Profile by Growth Class",
      x = "Feature",
      y = "Average Value",
      fill = "Growth Class"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(growth_class_profile_plot)
  
  return(growth_class_profile_plot)
}

growth_class_profile_plot <- plot_growth_class_profile(
  growth_class_profile
)

# MAIN GROWTH DRIVER DISTRIBUTION
summarise_main_growth_driver <- function(la_model_features) {
  
  main_growth_driver_summary <- la_model_features %>%
    count(
      main_growth_driver,
      name = "number_of_local_authorities"
    )
  
  print(main_growth_driver_summary)
  
  return(main_growth_driver_summary)
}

main_growth_driver_summary <- summarise_main_growth_driver(
  la_model_features
)

plot_main_growth_driver_distribution <- function(la_model_features) {
  
  main_growth_driver_plot <- ggplot(
    la_model_features,
    aes(x = main_growth_driver)
  ) +
    geom_bar() +
    geom_text(
      stat = "count",
      aes(label = after_stat(count)),
      vjust = -0.3
    ) +
    labs(
      title = "Main Growth Driver Across Local Authorities",
      x = "Main Growth Driver",
      y = "Number of Local Authorities"
    ) +
    theme_minimal()
  
  print(main_growth_driver_plot)
  
  return(main_growth_driver_plot)
}

main_growth_driver_plot <- plot_main_growth_driver_distribution(
  la_model_features
)

# POPULATION GROWTH RATE BY MAIN GROWTH DRIVER
plot_growth_rate_by_main_driver <- function(la_model_features) {
  
  growth_rate_by_driver_plot <- ggplot(
    la_model_features,
    aes(
      x = main_growth_driver,
      y = growth_rate_2001_2020
    )
  ) +
    geom_boxplot() +
    labs(
      title = "Population Growth Rate by Main Growth Driver",
      x = "Main Growth Driver",
      y = "Growth Rate 2001–2020 (%)"
    ) +
    theme_minimal()
  
  print(growth_rate_by_driver_plot)
  
  return(growth_rate_by_driver_plot)
}

growth_rate_by_driver_plot <- plot_growth_rate_by_main_driver(
  la_model_features
)

# GROWTH CLASS DISTRIBUTION BY COUNTRY
create_country_growth_class_table <- function(la_model_features) {
  
  country_growth_class <- la_model_features %>%
    mutate(
      country_name = case_when(
        country == "E" ~ "England",
        country == "W" ~ "Wales",
        TRUE ~ country
      )
    )
  
  country_growth_class_table <- table(
    country_growth_class$country_name,
    country_growth_class$growth_class
  )
  
  print(country_growth_class_table)
  
  return(list(
    country_growth_class = country_growth_class,
    country_growth_class_table = country_growth_class_table
  ))
}

country_growth_class_output <- create_country_growth_class_table(
  la_model_features
)

country_growth_class <- country_growth_class_output$country_growth_class
country_growth_class_table <- country_growth_class_output$country_growth_class_table

plot_country_growth_class_distribution <- function(country_growth_class) {
  
  country_growth_class_plot <- ggplot(
    country_growth_class,
    aes(
      x = country_name,
      fill = growth_class
    )
  ) +
    geom_bar(position = "dodge") +
    geom_text(
      stat = "count",
      aes(label = after_stat(count)),
      position = position_dodge(width = 0.9),
      vjust = -0.3
    ) +
    labs(
      title = "Growth Class Distribution by Country",
      x = "Country",
      y = "Number of Local Authorities",
      fill = "Growth Class"
    ) +
    theme_minimal()
  
  print(country_growth_class_plot)
  
  return(country_growth_class_plot)
}

country_growth_class_plot <- plot_country_growth_class_distribution(
  country_growth_class
)

# DISTRIBUTION OF AVERAGE DEPENDENCY RATIO
plot_dependency_ratio_distribution <- function(la_model_features) {
  
  dependency_ratio_distribution_plot <- ggplot(
    la_model_features,
    aes(x = avg_dependency_ratio)
  ) +
    geom_histogram(
      bins = 30,
      colour = "black",
      fill = "grey70"
    ) +
    geom_density(
      aes(y = after_stat(count)),
      linewidth = 1
    ) +
    geom_vline(
      xintercept = mean(la_model_features$avg_dependency_ratio, na.rm = TRUE),
      linetype = "dashed"
    ) +
    labs(
      title = "Distribution of Average Dependency Ratio",
      x = "Average Dependency Ratio",
      y = "Number of Local Authorities"
    ) +
    theme_minimal()
  
  print(dependency_ratio_distribution_plot)
  
  dependency_ratio_summary <- la_model_features %>%
    summarise(
      mean_dependency_ratio = mean(avg_dependency_ratio, na.rm = TRUE),
      median_dependency_ratio = median(avg_dependency_ratio, na.rm = TRUE),
      minimum_dependency_ratio = min(avg_dependency_ratio, na.rm = TRUE),
      maximum_dependency_ratio = max(avg_dependency_ratio, na.rm = TRUE),
      standard_deviation = sd(avg_dependency_ratio, na.rm = TRUE)
    )
  
  print(dependency_ratio_summary)
  
  return(list(
    plot = dependency_ratio_distribution_plot,
    summary = dependency_ratio_summary
  ))
}

dependency_ratio_distribution_output <- plot_dependency_ratio_distribution(
  la_model_features
)

# GROWTH DECOMPOSITION
create_growth_decomposition <- function(la_model_features) {
  
  growth_decomposition <- la_model_features %>%
    mutate(
      natural_change_share = (
        total_natural_change / if_else(absolute_growth == 0, NA_real_, absolute_growth)
      ) * 100,
      
      migration_share = (
        total_net_migration / if_else(absolute_growth == 0, NA_real_, absolute_growth)
      ) * 100,
      
      adjustment_share = (
        total_adjustment / if_else(absolute_growth == 0, NA_real_, absolute_growth)
      ) * 100
    )
  
  decomposition_summary <- growth_decomposition %>%
    summarise(
      natural_change_share_mean = mean(natural_change_share, na.rm = TRUE),
      natural_change_share_median = median(natural_change_share, na.rm = TRUE),
      migration_share_mean = mean(migration_share, na.rm = TRUE),
      migration_share_median = median(migration_share, na.rm = TRUE),
      adjustment_share_mean = mean(adjustment_share, na.rm = TRUE),
      adjustment_share_median = median(adjustment_share, na.rm = TRUE)
    )
  
  print(decomposition_summary)
  
  return(list(
    growth_decomposition = growth_decomposition,
    decomposition_summary = decomposition_summary
  ))
}

growth_decomposition_output <- create_growth_decomposition(
  la_model_features
)

growth_decomposition <- growth_decomposition_output$growth_decomposition
decomposition_summary <- growth_decomposition_output$decomposition_summary
# OVERALL CONTRIBUTION TO POPULATION GROWTH
create_overall_decomposition <- function(la_model_features) {
  
  overall_decomposition <- tibble(
    component = c(
      "Natural Change",
      "Net Migration",
      "Adjustments"
    ),
    total = c(
      sum(la_model_features$total_natural_change, na.rm = TRUE),
      sum(la_model_features$total_net_migration, na.rm = TRUE),
      sum(la_model_features$total_adjustment, na.rm = TRUE)
    )
  ) %>%
    mutate(
      percentage_contribution = (
        total / sum(la_model_features$absolute_growth, na.rm = TRUE)
      ) * 100
    )
  
  print(overall_decomposition)
  
  return(overall_decomposition)
}

overall_decomposition <- create_overall_decomposition(
  la_model_features
)

plot_overall_decomposition <- function(overall_decomposition) {
  
  overall_decomposition_plot <- ggplot(
    overall_decomposition,
    aes(
      x = component,
      y = percentage_contribution
    )
  ) +
    geom_col() +
    geom_text(
      aes(label = paste0(round(percentage_contribution, 1), "%")),
      vjust = -0.3
    ) +
    labs(
      title = "Overall Contribution to Population Growth",
      x = "Growth Component",
      y = "Contribution to Total Growth (%)"
    ) +
    theme_minimal()
  
  print(overall_decomposition_plot)
  
  return(overall_decomposition_plot)
}

overall_decomposition_plot <- plot_overall_decomposition(
  overall_decomposition
)

# UNUSUAL LOCAL AUTHORITIES / OUTLIER SCORE
identify_unusual_local_authorities <- function(la_model_features) {
  
  unusual_feature_matrix <- la_model_features %>%
    select(
      total_net_migration,
      total_natural_change,
      avg_older_pct,
      avg_dependency_ratio,
      growth_rate_2001_2020
    )
  
  unusual_scaled <- scale(unusual_feature_matrix)
  
  la_model_features <- la_model_features %>%
    mutate(
      eda_outlier_score = sqrt(rowSums(unusual_scaled^2, na.rm = TRUE))
    )
  
  unusual_local_authorities <- la_model_features %>%
    arrange(desc(eda_outlier_score)) %>%
    slice_head(n = 15) %>%
    select(
      laname21,
      country,
      growth_rate_2001_2020,
      total_net_migration,
      total_natural_change,
      avg_older_pct,
      avg_dependency_ratio,
      eda_outlier_score
    )
  
  print(unusual_local_authorities)
  
  return(list(
    la_model_features = la_model_features,
    unusual_local_authorities = unusual_local_authorities
  ))
}

unusual_authority_output <- identify_unusual_local_authorities(
  la_model_features
)

la_model_features <- unusual_authority_output$la_model_features
unusual_local_authorities <- unusual_authority_output$unusual_local_authorities

plot_unusual_local_authorities <- function(la_model_features) {
  
  unusual_authorities_plot <- ggplot(
    la_model_features,
    aes(
      x = total_net_migration,
      y = total_natural_change,
      size = eda_outlier_score,
      colour = growth_class
    )
  ) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Unusual Local Authorities Based on Migration and Natural Change",
      x = "Total Net Migration",
      y = "Total Natural Change",
      size = "Outlier Score",
      colour = "Growth Class"
    ) +
    theme_minimal()
  
  print(unusual_authorities_plot)
  
  return(unusual_authorities_plot)
}

unusual_authorities_plot <- plot_unusual_local_authorities(
  la_model_features
)

# GROWTH CLASS TRAJECTORY
create_growth_class_trajectory <- function(la_year_features,
                                           la_model_features) {
  
  la_year_with_class <- la_year_features %>%
    left_join(
      la_model_features %>%
        select(ladcode21, growth_class),
      by = "ladcode21"
    )
  
  growth_class_trajectory <- la_year_with_class %>%
    group_by(growth_class, year) %>%
    summarise(
      avg_population_growth_rate = mean(population_growth_rate, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(head(growth_class_trajectory, 10))
  print(tail(growth_class_trajectory, 10))
  
  return(list(
    la_year_with_class = la_year_with_class,
    growth_class_trajectory = growth_class_trajectory
  ))
}

growth_class_trajectory_output <- create_growth_class_trajectory(
  la_year_features,
  la_model_features
)

la_year_with_class <- growth_class_trajectory_output$la_year_with_class
growth_class_trajectory <- growth_class_trajectory_output$growth_class_trajectory

plot_growth_class_trajectory <- function(growth_class_trajectory) {
  
  growth_class_trajectory_plot <- ggplot(
    growth_class_trajectory %>% filter(year >= 2002),
    aes(
      x = year,
      y = avg_population_growth_rate,
      colour = growth_class
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(
      breaks = sort(unique(growth_class_trajectory$year))
    ) +
    labs(
      title = "Average Yearly Growth Trajectory by Growth Class",
      x = "Year",
      y = "Average Yearly Growth Rate (%)",
      colour = "Growth Class"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(growth_class_trajectory_plot)
  
  return(growth_class_trajectory_plot)
}

growth_class_trajectory_plot <- plot_growth_class_trajectory(
  growth_class_trajectory
)

# AGE STRUCTURE TREND
create_age_structure_trend <- function(la_year_features) {
  
  age_structure_trend <- la_year_features %>%
    group_by(year) %>%
    summarise(
      avg_children_pct = mean(Children_0_14_pct, na.rm = TRUE),
      avg_young_pct = mean(Young_15_24_pct, na.rm = TRUE),
      avg_working_pct = mean(Working_25_64_pct, na.rm = TRUE),
      avg_older_pct = mean(Older_65_plus_pct, na.rm = TRUE),
      avg_dependency_ratio = mean(dependency_ratio, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(head(age_structure_trend))
  print(tail(age_structure_trend))
  
  return(age_structure_trend)
}

age_structure_trend <- create_age_structure_trend(
  la_year_features
)

plot_age_structure_trend <- function(age_structure_trend) {
  
  age_structure_trend_long <- age_structure_trend %>%
    pivot_longer(
      cols = c(
        avg_children_pct,
        avg_young_pct,
        avg_working_pct,
        avg_older_pct
      ),
      names_to = "age_structure_feature",
      values_to = "average_percentage"
    )
  
  age_structure_trend_plot <- ggplot(
    age_structure_trend_long,
    aes(
      x = year,
      y = average_percentage,
      colour = age_structure_feature
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(
      breaks = sort(unique(age_structure_trend$year))
    ) +
    labs(
      title = "Average Age Structure Over Time",
      x = "Year",
      y = "Average Population Share (%)",
      colour = "Age Structure Feature"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(age_structure_trend_plot)
  
  return(age_structure_trend_plot)
}

age_structure_trend_plot <- plot_age_structure_trend(
  age_structure_trend
)

# DEPENDENCY RATIO BY GROWTH CLASS
plot_dependency_ratio_by_growth_class <- function(la_model_features) {
  
  dependency_ratio_by_growth_class_plot <- ggplot(
    la_model_features,
    aes(
      x = growth_class,
      y = avg_dependency_ratio
    )
  ) +
    geom_boxplot() +
    labs(
      title = "Dependency Ratio by Growth Class",
      x = "Growth Class",
      y = "Average Dependency Ratio"
    ) +
    theme_minimal()
  
  print(dependency_ratio_by_growth_class_plot)
  
  return(dependency_ratio_by_growth_class_plot)
}

dependency_ratio_by_growth_class_plot <- plot_dependency_ratio_by_growth_class(
  la_model_features
)

# MIGRATION RELIANCE INDEX
create_migration_reliance_index <- function(la_model_features) {
  
  la_model_features <- la_model_features %>%
    mutate(
      migration_reliance_index = total_net_migration /
        if_else(
          abs(total_net_migration) + abs(total_natural_change) == 0,
          NA_real_,
          abs(total_net_migration) + abs(total_natural_change)
        )
    )
  
  migration_reliance_summary <- la_model_features %>%
    summarise(
      mean_migration_reliance_index = mean(migration_reliance_index, na.rm = TRUE),
      median_migration_reliance_index = median(migration_reliance_index, na.rm = TRUE),
      minimum_migration_reliance_index = min(migration_reliance_index, na.rm = TRUE),
      maximum_migration_reliance_index = max(migration_reliance_index, na.rm = TRUE)
    )
  
  print(migration_reliance_summary)
  
  return(la_model_features)
}

la_model_features <- create_migration_reliance_index(
  la_model_features
)

plot_migration_reliance_index <- function(la_model_features) {
  
  migration_reliance_index_plot <- ggplot(
    la_model_features,
    aes(x = migration_reliance_index)
  ) +
    geom_histogram(
      bins = 30,
      colour = "black",
      fill = "grey70"
    ) +
    geom_density(
      aes(y = after_stat(count)),
      linewidth = 1
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      title = "Distribution of Migration Reliance Index",
      x = "Migration Reliance Index",
      y = "Number of Local Authorities"
    ) +
    theme_minimal()
  
  print(migration_reliance_index_plot)
  
  return(migration_reliance_index_plot)
}

migration_reliance_index_plot <- plot_migration_reliance_index(
  la_model_features
)

# DEMOGRAPHIC TYPOLOGY
assign_demographic_typology <- function(total_natural_change,
                                        total_net_migration) {
  
  case_when(
    total_natural_change >= 0 & total_net_migration >= 0 ~ "Dual growth",
    total_natural_change < 0 & total_net_migration >= 0 ~ "Migration offsetting natural decline",
    total_natural_change >= 0 & total_net_migration < 0 ~ "Natural growth offsetting migration loss",
    TRUE ~ "Dual decline"
  )
}

create_demographic_typology <- function(la_model_features) {
  
  la_model_features <- la_model_features %>%
    mutate(
      demographic_typology = assign_demographic_typology(
        total_natural_change,
        total_net_migration
      )
    )
  
  demographic_typology_summary <- la_model_features %>%
    count(
      demographic_typology,
      name = "number_of_local_authorities"
    )
  
  print(demographic_typology_summary)
  
  return(la_model_features)
}

la_model_features <- create_demographic_typology(
  la_model_features
)

plot_demographic_typology <- function(la_model_features) {
  
  demographic_typology_plot <- ggplot(
    la_model_features,
    aes(
      x = total_net_migration,
      y = total_natural_change,
      colour = demographic_typology,
      size = growth_rate_2001_2020
    )
  ) +
    geom_point(alpha = 0.75) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Demographic Typology Based on Migration and Natural Change",
      x = "Total Net Migration",
      y = "Total Natural Change",
      colour = "Demographic Typology",
      size = "Growth Rate"
    ) +
    theme_minimal()
  
  print(demographic_typology_plot)
  
  return(demographic_typology_plot)
}

demographic_typology_plot <- plot_demographic_typology(
  la_model_features
)
# CONCENTRATION OF POPULATION GROWTH
create_growth_concentration <- function(la_model_features) {
  
  growth_concentration <- la_model_features %>%
    arrange(desc(absolute_growth)) %>%
    mutate(
      cumulative_growth = cumsum(absolute_growth),
      cumulative_growth_share = (
        cumulative_growth / sum(absolute_growth, na.rm = TRUE)
      ) * 100,
      local_authority_rank = row_number(),
      cumulative_la_share = (
        local_authority_rank / n()
      ) * 100
    )
  
  print(head(growth_concentration, 10))
  
  return(growth_concentration)
}

growth_concentration <- create_growth_concentration(
  la_model_features
)

plot_growth_concentration <- function(growth_concentration) {
  
  growth_concentration_plot <- ggplot(
    growth_concentration,
    aes(
      x = cumulative_la_share,
      y = cumulative_growth_share
    )
  ) +
    geom_line(linewidth = 1) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed"
    ) +
    labs(
      title = "Concentration of Population Growth Across Local Authorities",
      x = "Cumulative Share of Local Authorities (%)",
      y = "Cumulative Share of Population Growth (%)"
    ) +
    theme_minimal()
  
  print(growth_concentration_plot)
  
  return(growth_concentration_plot)
}

growth_concentration_plot <- plot_growth_concentration(
  growth_concentration
)
# CORRELATION HEATMAP
create_eda_correlation_matrix <- function(la_model_features) {
  
  eda_correlation_variables <- c(
    "growth_rate_2001_2020",
    "absolute_growth",
    "total_net_migration",
    "total_natural_change",
    "avg_birth_rate",
    "avg_death_rate",
    "avg_migration_rate",
    "avg_natural_change_rate",
    "avg_children_pct",
    "avg_working_pct",
    "avg_older_pct",
    "avg_dependency_ratio"
  )
  
  eda_correlation_matrix <- la_model_features %>%
    select(all_of(eda_correlation_variables)) %>%
    cor(use = "complete.obs")
  
  print(round(eda_correlation_matrix, 2))
  
  return(eda_correlation_matrix)
}

eda_correlation_matrix <- create_eda_correlation_matrix(
  la_model_features
)

plot_eda_correlation_heatmap <- function(eda_correlation_matrix) {
  
  variable_names <- c(
    "growth_rate_2001_2020" = "Growth rate",
    "absolute_growth" = "Absolute growth",
    "total_net_migration" = "Net migration",
    "total_natural_change" = "Natural change",
    "avg_birth_rate" = "Birth rate",
    "avg_death_rate" = "Death rate",
    "avg_migration_rate" = "Migration rate",
    "avg_natural_change_rate" = "Natural change rate",
    "avg_children_pct" = "Children %",
    "avg_working_pct" = "Working-age %",
    "avg_older_pct" = "Older %",
    "avg_dependency_ratio" = "Dependency ratio"
  )
  
  correlation_heatmap_data <- as.data.frame(as.table(eda_correlation_matrix)) %>%
    rename(
      variable_x = Var1,
      variable_y = Var2,
      correlation = Freq
    ) %>%
    mutate(
      variable_x = as.character(variable_x),
      variable_y = as.character(variable_y),
      variable_x = variable_names[variable_x],
      variable_y = variable_names[variable_y],
      correlation_label = round(correlation, 2),
      variable_x = factor(variable_x, levels = variable_names),
      variable_y = factor(variable_y, levels = rev(variable_names))
    )
  
  correlation_heatmap_plot <- ggplot(
    correlation_heatmap_data,
    aes(
      x = variable_x,
      y = variable_y,
      fill = correlation
    )
  ) +
    geom_tile(colour = "white") +
    geom_text(
      aes(label = correlation_label),
      size = 3
    ) +
    scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "steelblue",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Correlation"
    ) +
    labs(
      title = "Correlation Heatmap of Demographic Indicators",
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 9
      ),
      axis.text.y = element_text(
        size = 9
      ),
      plot.title = element_text(
        size = 13,
        face = "bold"
      ),
      panel.grid = element_blank()
    ) +
    coord_fixed()
  
  print(correlation_heatmap_plot)
  
  return(correlation_heatmap_plot)
}

eda_correlation_heatmap <- plot_eda_correlation_heatmap(
  eda_correlation_matrix
)
# ANOVA TEST: DEPENDENCY RATIO BY GROWTH CLASS
run_dependency_ratio_anova <- function(la_model_features) {
  
  anova_data <- la_model_features %>%
    select(
      growth_class,
      avg_dependency_ratio
    ) %>%
    drop_na()
  
  dependency_ratio_anova_model <- aov(
    avg_dependency_ratio ~ growth_class,
    data = anova_data
  )
  
  anova_summary <- summary(dependency_ratio_anova_model)
  
  dependency_ratio_group_summary <- anova_data %>%
    group_by(growth_class) %>%
    summarise(
      mean_dependency_ratio = mean(avg_dependency_ratio, na.rm = TRUE),
      median_dependency_ratio = median(avg_dependency_ratio, na.rm = TRUE),
      standard_deviation = sd(avg_dependency_ratio, na.rm = TRUE),
      count = n(),
      .groups = "drop"
    )
  
  print(anova_summary)
  print(dependency_ratio_group_summary)
  
  return(list(
    anova_data = anova_data,
    dependency_ratio_anova_model = dependency_ratio_anova_model,
    anova_summary = anova_summary,
    dependency_ratio_group_summary = dependency_ratio_group_summary
  ))
}

dependency_ratio_anova_output <- run_dependency_ratio_anova(
  la_model_features
)

anova_data <- dependency_ratio_anova_output$anova_data
dependency_ratio_anova_model <- dependency_ratio_anova_output$dependency_ratio_anova_model
dependency_ratio_group_summary <- dependency_ratio_anova_output$dependency_ratio_group_summary

# PLOT ANOVA RESULT: DEPENDENCY RATIO BY GROWTH CLASS
plot_dependency_ratio_anova <- function(anova_data) {
  
  dependency_ratio_anova_plot <- ggplot(
    anova_data,
    aes(
      x = growth_class,
      y = avg_dependency_ratio
    )
  ) +
    geom_boxplot() +
    labs(
      title = "Average Dependency Ratio by Growth Class",
      x = "Growth Class",
      y = "Average Dependency Ratio"
    ) +
    theme_minimal()
  
  print(dependency_ratio_anova_plot)
  
  return(dependency_ratio_anova_plot)
}

dependency_ratio_anova_plot <- plot_dependency_ratio_anova(
  anova_data
)

# CORRELATION TESTS
run_correlation_tests <- function(la_model_features) {
  
  correlation_tests <- tibble(
    relationship = c(
      "Migration rate vs population growth rate",
      "Natural change rate vs population growth rate",
      "Birth rate vs natural change rate",
      "Death rate vs population growth rate",
      "Older population share vs population growth rate",
      "Dependency ratio vs population growth rate"
    ),
    variable_x = c(
      "avg_migration_rate",
      "avg_natural_change_rate",
      "avg_birth_rate",
      "avg_death_rate",
      "avg_older_pct",
      "avg_dependency_ratio"
    ),
    variable_y = c(
      "growth_rate_2001_2020",
      "growth_rate_2001_2020",
      "avg_natural_change_rate",
      "growth_rate_2001_2020",
      "growth_rate_2001_2020",
      "growth_rate_2001_2020"
    )
  )
  
  correlation_results <- map_dfr(
    1:nrow(correlation_tests),
    function(i) {
      
      test_data <- la_model_features %>%
        select(
          x = all_of(correlation_tests$variable_x[i]),
          y = all_of(correlation_tests$variable_y[i])
        ) %>%
        drop_na()
      
      pearson_test <- cor.test(
        test_data$x,
        test_data$y,
        method = "pearson"
      )
      
      spearman_test <- cor.test(
        test_data$x,
        test_data$y,
        method = "spearman",
        exact = FALSE
      )
      
      tibble(
        relationship = correlation_tests$relationship[i],
        variable_x = correlation_tests$variable_x[i],
        variable_y = correlation_tests$variable_y[i],
        pearson_r = as.numeric(pearson_test$estimate),
        pearson_p_value = pearson_test$p.value,
        spearman_rho = as.numeric(spearman_test$estimate),
        spearman_p_value = spearman_test$p.value
      )
    }
  )
  
  print(correlation_results)
  
  return(correlation_results)
}

correlation_results <- run_correlation_tests(
  la_model_features
)

# CORRELATION STRENGTH AND SIGNIFICANCE
interpret_p_value <- function(p_value) {
  
  case_when(
    p_value < 0.001 ~ "Highly significant",
    p_value < 0.01 ~ "Significant",
    p_value < 0.05 ~ "Weakly significant",
    TRUE ~ "Not significant"
  )
}

interpret_correlation_strength <- function(correlation_value) {
  
  absolute_correlation <- abs(correlation_value)
  
  case_when(
    absolute_correlation >= 0.70 ~ "Strong",
    absolute_correlation >= 0.40 ~ "Moderate",
    absolute_correlation >= 0.20 ~ "Weak",
    TRUE ~ "Very weak"
  )
}

summarise_correlation_test_interpretation <- function(correlation_results) {
  
  correlation_results_interpreted <- correlation_results %>%
    mutate(
      pearson_strength = interpret_correlation_strength(pearson_r),
      pearson_significance = interpret_p_value(pearson_p_value),
      spearman_strength = interpret_correlation_strength(spearman_rho),
      spearman_significance = interpret_p_value(spearman_p_value)
    )
  
  print(correlation_results_interpreted)
  
  return(correlation_results_interpreted)
}

correlation_results_interpreted <- summarise_correlation_test_interpretation(
  correlation_results
)

# PLOTTING CORRELATION TEST RESULTS
plot_correlation_test_results <- function(correlation_results_interpreted) {
  
  correlation_test_plot <- ggplot(
    correlation_results_interpreted,
    aes(
      x = pearson_r,
      y = reorder(relationship, pearson_r)
    )
  ) +
    geom_col() +
    geom_vline(
      xintercept = 0,
      linetype = "dashed"
    ) +
    labs(
      title = "Pearson Correlation Strengths for Selected Demographic Relationships",
      x = "Pearson Correlation Coefficient",
      y = "Relationship"
    ) +
    theme_minimal()
  
  print(correlation_test_plot)
  
  return(correlation_test_plot)
}

correlation_test_plot <- plot_correlation_test_results(
  correlation_results_interpreted
)

# MIGRATION BY SEX TEST
test_migration_by_sex <- function(population_panel_features) {
  
  sex_migration_test_data <- population_panel_features %>%
    filter(year >= 2002) %>%
    group_by(
      ladcode21,
      laname21,
      country,
      year,
      sex
    ) %>%
    summarise(
      total_net_migration = sum(total_net_migration, na.rm = TRUE),
      .groups = "drop"
    )
  
  sex_migration_pivot <- sex_migration_test_data %>%
    pivot_wider(
      names_from = sex,
      values_from = total_net_migration
    )
  
  migration_sex_ttest <- t.test(
    sex_migration_pivot$Female,
    sex_migration_pivot$Male,
    paired = TRUE
  )
  
  sex_migration_summary <- sex_migration_test_data %>%
    group_by(sex) %>%
    summarise(
      mean_net_migration = mean(total_net_migration, na.rm = TRUE),
      median_net_migration = median(total_net_migration, na.rm = TRUE),
      std_net_migration = sd(total_net_migration, na.rm = TRUE),
      count = sum(!is.na(total_net_migration)),
      .groups = "drop"
    )
  
  print(migration_sex_ttest)
  print(sex_migration_summary)
  
  return(list(
    sex_migration_test_data = sex_migration_test_data,
    sex_migration_pivot = sex_migration_pivot,
    migration_sex_ttest = migration_sex_ttest,
    sex_migration_summary = sex_migration_summary
  ))
}

migration_by_sex_output <- test_migration_by_sex(
  population_panel_features
)

sex_migration_test_data <- migration_by_sex_output$sex_migration_test_data
sex_migration_pivot <- migration_by_sex_output$sex_migration_pivot
migration_sex_ttest <- migration_by_sex_output$migration_sex_ttest
sex_migration_summary <- migration_by_sex_output$sex_migration_summary

plot_migration_by_sex_boxplot <- function(sex_migration_test_data) {
  
  sex_migration_boxplot <- ggplot(
    sex_migration_test_data,
    aes(
      x = sex,
      y = total_net_migration
    )
  ) +
    geom_boxplot() +
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Net Migration Distribution by Sex",
      x = "Sex",
      y = "Total Net Migration"
    ) +
    theme_minimal()
  
  print(sex_migration_boxplot)
  
  return(sex_migration_boxplot)
}

sex_migration_boxplot <- plot_migration_by_sex_boxplot(
  sex_migration_test_data
)

# DEATH RATE VS POPULATION GROWTH RATE
test_death_rate_growth_relationship <- function(la_model_features) {
  
  death_growth_corr_data <- la_model_features %>%
    select(
      avg_death_rate,
      growth_rate_2001_2020
    ) %>%
    drop_na()
  
  death_growth_pearson <- cor.test(
    death_growth_corr_data$avg_death_rate,
    death_growth_corr_data$growth_rate_2001_2020,
    method = "pearson"
  )
  
  death_growth_spearman <- cor.test(
    death_growth_corr_data$avg_death_rate,
    death_growth_corr_data$growth_rate_2001_2020,
    method = "spearman",
    exact = FALSE
  )
  
  death_growth_result <- tibble(
    test = "Death rate vs population growth rate",
    pearson_r = as.numeric(death_growth_pearson$estimate),
    pearson_p_value = death_growth_pearson$p.value,
    spearman_rho = as.numeric(death_growth_spearman$estimate),
    spearman_p_value = death_growth_spearman$p.value
  )
  
  print(death_growth_result)
  
  return(list(
    death_growth_corr_data = death_growth_corr_data,
    death_growth_result = death_growth_result
  ))
}

death_growth_output <- test_death_rate_growth_relationship(
  la_model_features
)

death_growth_corr_data <- death_growth_output$death_growth_corr_data
death_growth_result <- death_growth_output$death_growth_result

plot_death_rate_growth_relationship <- function(death_growth_corr_data) {
  
  death_growth_plot <- ggplot(
    death_growth_corr_data,
    aes(
      x = avg_death_rate,
      y = growth_rate_2001_2020
    )
  ) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
      title = "Death Rate and Population Growth Rate",
      x = "Average Death Rate per 1,000",
      y = "Population Growth Rate 2001–2020 (%)"
    ) +
    theme_minimal()
  
  print(death_growth_plot)
  
  return(death_growth_plot)
}

death_growth_plot <- plot_death_rate_growth_relationship(
  death_growth_corr_data
)

# WELCH T-TEST: BIRTH RATE BY COUNTRY
run_birth_rate_country_ttest <- function(la_year_features) {
  
  birth_rate_country_data <- la_year_features %>%
    filter(year >= 2002) %>%
    mutate(
      country_name = case_when(
        country == "E" ~ "England",
        country == "W" ~ "Wales",
        TRUE ~ country
      )
    ) %>%
    select(
      country_name,
      birth_rate_per_1000
    ) %>%
    drop_na()
  
  birth_rate_country_ttest <- t.test(
    birth_rate_per_1000 ~ country_name,
    data = birth_rate_country_data
  )
  
  birth_rate_country_summary <- birth_rate_country_data %>%
    group_by(country_name) %>%
    summarise(
      mean_birth_rate = mean(birth_rate_per_1000, na.rm = TRUE),
      median_birth_rate = median(birth_rate_per_1000, na.rm = TRUE),
      standard_deviation = sd(birth_rate_per_1000, na.rm = TRUE),
      count = n(),
      .groups = "drop"
    )
  
  print(birth_rate_country_ttest)
  print(birth_rate_country_summary)
  
  return(list(
    birth_rate_country_data = birth_rate_country_data,
    birth_rate_country_ttest = birth_rate_country_ttest,
    birth_rate_country_summary = birth_rate_country_summary
  ))
}

birth_rate_country_output <- run_birth_rate_country_ttest(
  la_year_features
)

birth_rate_country_data <- birth_rate_country_output$birth_rate_country_data
birth_rate_country_ttest <- birth_rate_country_output$birth_rate_country_ttest
birth_rate_country_summary <- birth_rate_country_output$birth_rate_country_summary

plot_birth_rate_country_ttest <- function(birth_rate_country_data) {
  
  birth_rate_country_plot <- ggplot(
    birth_rate_country_data,
    aes(
      x = country_name,
      y = birth_rate_per_1000
    )
  ) +
    geom_boxplot() +
    labs(
      title = "Birth Rate Distribution by Country",
      x = "Country",
      y = "Birth Rate per 1,000 Population"
    ) +
    theme_minimal()
  
  print(birth_rate_country_plot)
  
  return(birth_rate_country_plot)
}

birth_rate_country_plot <- plot_birth_rate_country_ttest(
  birth_rate_country_data
)

# WELCH T-TEST: DEATH RATE BY COUNTRY
run_death_rate_country_ttest <- function(la_year_features) {
  
  death_rate_country_data <- la_year_features %>%
    filter(year >= 2002) %>%
    mutate(
      country_name = case_when(
        country == "E" ~ "England",
        country == "W" ~ "Wales",
        TRUE ~ country
      )
    ) %>%
    select(
      country_name,
      death_rate_per_1000
    ) %>%
    drop_na()
  
  death_rate_country_ttest <- t.test(
    death_rate_per_1000 ~ country_name,
    data = death_rate_country_data
  )
  
  death_rate_country_summary <- death_rate_country_data %>%
    group_by(country_name) %>%
    summarise(
      mean_death_rate = mean(death_rate_per_1000, na.rm = TRUE),
      median_death_rate = median(death_rate_per_1000, na.rm = TRUE),
      standard_deviation = sd(death_rate_per_1000, na.rm = TRUE),
      count = n(),
      .groups = "drop"
    )
  
  print(death_rate_country_ttest)
  print(death_rate_country_summary)
  
  return(list(
    death_rate_country_data = death_rate_country_data,
    death_rate_country_ttest = death_rate_country_ttest,
    death_rate_country_summary = death_rate_country_summary
  ))
}

death_rate_country_output <- run_death_rate_country_ttest(
  la_year_features
)

death_rate_country_data <- death_rate_country_output$death_rate_country_data
death_rate_country_ttest <- death_rate_country_output$death_rate_country_ttest
death_rate_country_summary <- death_rate_country_output$death_rate_country_summary

plot_death_rate_country_ttest <- function(death_rate_country_data) {
  
  death_rate_country_plot <- ggplot(
    death_rate_country_data,
    aes(
      x = country_name,
      y = death_rate_per_1000
    )
  ) +
    geom_boxplot() +
    labs(
      title = "Death Rate Distribution by Country",
      x = "Country",
      y = "Death Rate per 1,000 Population"
    ) +
    theme_minimal()
  
  print(death_rate_country_plot)
  
  return(death_rate_country_plot)
}

death_rate_country_plot <- plot_death_rate_country_ttest(
  death_rate_country_data
)

# PAIRED T-TEST: POPULATION BY SEX
run_population_by_sex_paired_ttest <- function(population_panel_features) {
  
  sex_population_data <- population_panel_features %>%
    group_by(
      ladcode21,
      laname21,
      country,
      year,
      sex
    ) %>%
    summarise(
      total_population = sum(population, na.rm = TRUE),
      .groups = "drop"
    )
  
  sex_population_pivot <- sex_population_data %>%
    pivot_wider(
      names_from = sex,
      values_from = total_population
    ) %>%
    drop_na(Female, Male)
  
  population_by_sex_ttest <- t.test(
    sex_population_pivot$Female,
    sex_population_pivot$Male,
    paired = TRUE
  )
  
  sex_population_summary <- sex_population_data %>%
    group_by(sex) %>%
    summarise(
      mean_population = mean(total_population, na.rm = TRUE),
      median_population = median(total_population, na.rm = TRUE),
      standard_deviation = sd(total_population, na.rm = TRUE),
      count = n(),
      .groups = "drop"
    )
  
  print(population_by_sex_ttest)
  print(sex_population_summary)
  
  return(list(
    sex_population_data = sex_population_data,
    sex_population_pivot = sex_population_pivot,
    population_by_sex_ttest = population_by_sex_ttest,
    sex_population_summary = sex_population_summary
  ))
}

population_by_sex_output <- run_population_by_sex_paired_ttest(
  population_panel_features
)

sex_population_data <- population_by_sex_output$sex_population_data
sex_population_pivot <- population_by_sex_output$sex_population_pivot
population_by_sex_ttest <- population_by_sex_output$population_by_sex_ttest
sex_population_summary <- population_by_sex_output$sex_population_summary

plot_population_by_sex <- function(sex_population_data) {
  
  sex_population_plot <- ggplot(
    sex_population_data,
    aes(
      x = sex,
      y = total_population
    )
  ) +
    geom_boxplot() +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Population Distribution by Sex",
      x = "Sex",
      y = "Total Population"
    ) +
    theme_minimal()
  
  print(sex_population_plot)
  
  return(sex_population_plot)
}

sex_population_plot <- plot_population_by_sex(
  sex_population_data
)

# ANOVA: MIGRATION RATE BY GROWTH CLASS
run_migration_rate_growth_class_anova <- function(la_model_features) {
  
  migration_anova_data <- la_model_features %>%
    select(
      growth_class,
      avg_migration_rate
    ) %>%
    drop_na()
  
  migration_rate_anova_model <- aov(
    avg_migration_rate ~ growth_class,
    data = migration_anova_data
  )
  
  migration_rate_group_summary <- migration_anova_data %>%
    group_by(growth_class) %>%
    summarise(
      mean_migration_rate = mean(avg_migration_rate, na.rm = TRUE),
      median_migration_rate = median(avg_migration_rate, na.rm = TRUE),
      standard_deviation = sd(avg_migration_rate, na.rm = TRUE),
      count = n(),
      .groups = "drop"
    )
  
  print(summary(migration_rate_anova_model))
  print(migration_rate_group_summary)
  
  return(list(
    migration_anova_data = migration_anova_data,
    migration_rate_anova_model = migration_rate_anova_model,
    migration_rate_group_summary = migration_rate_group_summary
  ))
}

migration_rate_anova_output <- run_migration_rate_growth_class_anova(
  la_model_features
)

migration_anova_data <- migration_rate_anova_output$migration_anova_data
migration_rate_anova_model <- migration_rate_anova_output$migration_rate_anova_model
migration_rate_group_summary <- migration_rate_anova_output$migration_rate_group_summary

run_migration_rate_tukey_test <- function(migration_rate_anova_model) {
  
  migration_rate_tukey_test <- TukeyHSD(
    migration_rate_anova_model
  )
  
  print(migration_rate_tukey_test)
  
  return(migration_rate_tukey_test)
}

migration_rate_tukey_test <- run_migration_rate_tukey_test(
  migration_rate_anova_model
)

plot_migration_rate_by_growth_class <- function(migration_anova_data) {
  
  migration_rate_growth_class_plot <- ggplot(
    migration_anova_data,
    aes(
      x = growth_class,
      y = avg_migration_rate
    )
  ) +
    geom_boxplot() +
    labs(
      title = "Average Migration Rate by Growth Class",
      x = "Growth Class",
      y = "Average Migration Rate per 1,000 Population"
    ) +
    theme_minimal()
  
  print(migration_rate_growth_class_plot)
  
  return(migration_rate_growth_class_plot)
}

migration_rate_growth_class_plot <- plot_migration_rate_by_growth_class(
  migration_anova_data
)

# KRUSKAL-WALLIS TEST: DEPENDENCY RATIO BY GROWTH CLASS
run_dependency_ratio_growth_class_kruskal <- function(la_model_features) {
  
  dependency_ratio_kruskal_data <- la_model_features %>%
    select(
      growth_class,
      avg_dependency_ratio
    ) %>%
    drop_na()
  
  dependency_ratio_kruskal_test <- kruskal.test(
    avg_dependency_ratio ~ growth_class,
    data = dependency_ratio_kruskal_data
  )
  
  dependency_ratio_group_summary <- dependency_ratio_kruskal_data %>%
    group_by(growth_class) %>%
    summarise(
      mean_dependency_ratio = mean(avg_dependency_ratio, na.rm = TRUE),
      median_dependency_ratio = median(avg_dependency_ratio, na.rm = TRUE),
      standard_deviation = sd(avg_dependency_ratio, na.rm = TRUE),
      count = n(),
      .groups = "drop"
    )
  
  print(dependency_ratio_kruskal_test)
  print(dependency_ratio_group_summary)
  
  return(list(
    dependency_ratio_kruskal_data = dependency_ratio_kruskal_data,
    dependency_ratio_kruskal_test = dependency_ratio_kruskal_test,
    dependency_ratio_group_summary = dependency_ratio_group_summary
  ))
}

dependency_ratio_kruskal_output <- run_dependency_ratio_growth_class_kruskal(
  la_model_features
)

dependency_ratio_kruskal_data <- dependency_ratio_kruskal_output$dependency_ratio_kruskal_data
dependency_ratio_kruskal_test <- dependency_ratio_kruskal_output$dependency_ratio_kruskal_test
dependency_ratio_group_summary <- dependency_ratio_kruskal_output$dependency_ratio_group_summary

plot_dependency_ratio_by_growth_class <- function(dependency_ratio_kruskal_data) {
  
  dependency_ratio_growth_class_plot <- ggplot(
    dependency_ratio_kruskal_data,
    aes(
      x = growth_class,
      y = avg_dependency_ratio
    )
  ) +
    geom_boxplot() +
    labs(
      title = "Average Dependency Ratio by Growth Class",
      x = "Growth Class",
      y = "Average Dependency Ratio"
    ) +
    theme_minimal()
  
  print(dependency_ratio_growth_class_plot)
  
  return(dependency_ratio_growth_class_plot)
}

dependency_ratio_growth_class_plot <- plot_dependency_ratio_by_growth_class(
  dependency_ratio_kruskal_data
)

# CHI-SQUARE TEST: COUNTRY BY MAIN GROWTH DRIVER
run_country_growth_driver_chi_square <- function(la_model_features) {
  
  country_growth_driver_data <- la_model_features %>%
    mutate(
      country_name = case_when(
        country == "E" ~ "England",
        country == "W" ~ "Wales",
        TRUE ~ country
      )
    ) %>%
    select(
      country_name,
      main_growth_driver
    ) %>%
    drop_na()
  
  country_growth_driver_table <- table(
    country_growth_driver_data$country_name,
    country_growth_driver_data$main_growth_driver
  )
  
  country_growth_driver_chi_square <- chisq.test(
    country_growth_driver_table
  )
  
  country_growth_driver_result <- tibble(
    test = "Chi-square test: country and main growth driver",
    chi_square_statistic = as.numeric(country_growth_driver_chi_square$statistic),
    degrees_of_freedom = as.numeric(country_growth_driver_chi_square$parameter),
    p_value = country_growth_driver_chi_square$p.value
  )
  
  print(country_growth_driver_table)
  print(country_growth_driver_chi_square$expected)
  print(country_growth_driver_result)
  
  return(list(
    country_growth_driver_data = country_growth_driver_data,
    country_growth_driver_table = country_growth_driver_table,
    country_growth_driver_chi_square = country_growth_driver_chi_square,
    country_growth_driver_result = country_growth_driver_result
  ))
}

country_growth_driver_output <- run_country_growth_driver_chi_square(
  la_model_features
)

country_growth_driver_data <- country_growth_driver_output$country_growth_driver_data
country_growth_driver_table <- country_growth_driver_output$country_growth_driver_table
country_growth_driver_chi_square <- country_growth_driver_output$country_growth_driver_chi_square
country_growth_driver_result <- country_growth_driver_output$country_growth_driver_result

plot_country_growth_driver <- function(country_growth_driver_data) {
  
  country_growth_driver_plot <- ggplot(
    country_growth_driver_data,
    aes(
      x = country_name,
      fill = main_growth_driver
    )
  ) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = percent) +
    labs(
      title = "Main Growth Driver by Country",
      x = "Country",
      y = "Proportion of Local Authorities",
      fill = "Main Growth Driver"
    ) +
    theme_minimal()
  
  print(country_growth_driver_plot)
  
  return(country_growth_driver_plot)
}

country_growth_driver_plot <- plot_country_growth_driver(
  country_growth_driver_data
)

# REGRESSION DATASET
create_regression_dataset <- function(la_model_features) {
  
  regression_dataset <- la_model_features %>%
    select(
      ladcode21,
      laname21,
      country,
      growth_rate_2001_2020,
      avg_birth_rate,
      avg_death_rate,
      avg_migration_rate,
      avg_natural_change_rate,
      avg_children_pct,
      avg_young_pct,
      avg_working_pct,
      avg_older_pct,
      avg_dependency_ratio,
      total_net_migration,
      total_natural_change
    ) %>%
    drop_na()
  
  print(head(regression_dataset, 10))
  print(dim(regression_dataset))
  
  return(regression_dataset)
}

regression_dataset <- create_regression_dataset(
  la_model_features
)

# TRAIN-TEST SPLIT FOR REGRESSION
split_regression_dataset <- function(regression_dataset) {
  
  set.seed(42)
  
  train_index <- caret::createDataPartition(
    regression_dataset$growth_rate_2001_2020,
    p = 0.8,
    list = FALSE
  )
  
  regression_train_data <- regression_dataset[train_index, ]
  regression_test_data <- regression_dataset[-train_index, ]
  
  print(dim(regression_train_data))
  print(dim(regression_test_data))
  
  return(list(
    regression_train_data = regression_train_data,
    regression_test_data = regression_test_data
  ))
}

regression_split_output <- split_regression_dataset(
  regression_dataset
)

regression_train_data <- regression_split_output$regression_train_data
regression_test_data <- regression_split_output$regression_test_data

# TRAIN LINEAR REGRESSION MODEL
train_linear_regression_model <- function(regression_train_data) {
  
  linear_regression_model <- lm(
    growth_rate_2001_2020 ~
      avg_birth_rate +
      avg_death_rate +
      avg_migration_rate +
      avg_natural_change_rate +
      avg_children_pct +
      avg_young_pct +
      avg_working_pct +
      avg_older_pct +
      avg_dependency_ratio +
      total_net_migration +
      total_natural_change,
    data = regression_train_data
  )
  
  print(summary(linear_regression_model))
  
  return(linear_regression_model)
}

linear_regression_model <- train_linear_regression_model(
  regression_train_data
)
# REGRESSION PERFORMANCE FUNCTION
calculate_regression_metrics <- function(actual_values, predicted_values) {
  
  mae <- mean(abs(actual_values - predicted_values), na.rm = TRUE)
  
  rmse <- sqrt(
    mean((actual_values - predicted_values)^2, na.rm = TRUE)
  )
  
  r_squared <- cor(
    actual_values,
    predicted_values,
    use = "complete.obs"
  )^2
  
  regression_metrics <- tibble(
    MAE = mae,
    RMSE = rmse,
    R_squared = r_squared
  )
  
  return(regression_metrics)
}

# EVALUATE LINEAR REGRESSION MODEL
evaluate_linear_regression_model <- function(
    linear_regression_model,
    regression_train_data,
    regression_test_data
) {
  
  train_predictions <- predict(
    linear_regression_model,
    newdata = regression_train_data
  )
  
  test_predictions <- predict(
    linear_regression_model,
    newdata = regression_test_data
  )
  
  training_metrics <- calculate_regression_metrics(
    regression_train_data$growth_rate_2001_2020,
    train_predictions
  ) %>%
    mutate(dataset = "Training")
  
  testing_metrics <- calculate_regression_metrics(
    regression_test_data$growth_rate_2001_2020,
    test_predictions
  ) %>%
    mutate(dataset = "Testing")
  
  regression_performance <- bind_rows(
    training_metrics,
    testing_metrics
  ) %>%
    select(
      dataset,
      MAE,
      RMSE,
      R_squared
    )
  
  regression_test_predictions <- regression_test_data %>%
    mutate(
      predicted_growth_rate = test_predictions,
      residual = growth_rate_2001_2020 - predicted_growth_rate
    )
  
  print(regression_performance)
  print(head(regression_test_predictions, 10))
  
  return(list(
    train_predictions = train_predictions,
    test_predictions = test_predictions,
    regression_performance = regression_performance,
    regression_test_predictions = regression_test_predictions
  ))
}

linear_regression_evaluation <- evaluate_linear_regression_model(
  linear_regression_model,
  regression_train_data,
  regression_test_data
)

linear_train_predictions <- linear_regression_evaluation$train_predictions
linear_test_predictions <- linear_regression_evaluation$test_predictions
regression_performance <- linear_regression_evaluation$regression_performance
regression_test_predictions <- linear_regression_evaluation$regression_test_predictions

# LINEAR REGRESSION COEFFICIENTS
extract_linear_regression_coefficients <- function(linear_regression_model) {
  
  regression_coefficients <- broom::tidy(
    linear_regression_model
  ) %>%
    arrange(desc(abs(estimate)))
  
  print(regression_coefficients)
  
  return(regression_coefficients)
}

regression_coefficients <- extract_linear_regression_coefficients(
  linear_regression_model
)

# ACTUAL VS PREDICTED PLOT
plot_actual_vs_predicted_growth <- function(regression_test_predictions) {
  
  actual_vs_predicted_plot <- ggplot(
    regression_test_predictions,
    aes(
      x = growth_rate_2001_2020,
      y = predicted_growth_rate
    )
  ) +
    geom_point(alpha = 0.7) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed"
    ) +
    labs(
      title = "Actual vs Predicted Population Growth Rate",
      x = "Actual Growth Rate 2001–2020 (%)",
      y = "Predicted Growth Rate 2001–2020 (%)"
    ) +
    theme_minimal()
  
  print(actual_vs_predicted_plot)
  
  return(actual_vs_predicted_plot)
}

actual_vs_predicted_plot <- plot_actual_vs_predicted_growth(
  regression_test_predictions
)

# RESIDUAL PLOT
plot_regression_residuals <- function(regression_test_predictions) {
  
  residual_plot <- ggplot(
    regression_test_predictions,
    aes(
      x = predicted_growth_rate,
      y = residual
    )
  ) +
    geom_point(alpha = 0.7) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    labs(
      title = "Regression Residual Plot",
      x = "Predicted Growth Rate 2001–2020 (%)",
      y = "Residual"
    ) +
    theme_minimal()
  
  print(residual_plot)
  
  return(residual_plot)
}

regression_residual_plot <- plot_regression_residuals(
  regression_test_predictions
)

# FEATURE IMPORTANCE FROM LINEAR REGRESSION
plot_linear_regression_coefficients <- function(regression_coefficients) {
  
  coefficient_plot_data <- regression_coefficients %>%
    filter(term != "(Intercept)") %>%
    mutate(
      term = reorder(term, abs(estimate))
    )
  
  coefficient_plot <- ggplot(
    coefficient_plot_data,
    aes(
      x = estimate,
      y = term
    )
  ) +
    geom_col() +
    geom_vline(
      xintercept = 0,
      linetype = "dashed"
    ) +
    labs(
      title = "Linear Regression Coefficients",
      x = "Coefficient Estimate",
      y = "Feature"
    ) +
    theme_minimal()
  
  print(coefficient_plot)
  
  return(coefficient_plot)
}

linear_regression_coefficient_plot <- plot_linear_regression_coefficients(
  regression_coefficients
)

# ADD PREDICTED GROWTH RATE BACK TO LOCAL AUTHORITY DATASET
add_regression_predictions_to_local_authorities <- function(
    la_model_features,
    linear_regression_model
) {
  
  la_model_features_with_predictions <- la_model_features %>%
    mutate(
      predicted_growth_rate = predict(
        linear_regression_model,
        newdata = la_model_features
      ),
      prediction_residual = growth_rate_2001_2020 - predicted_growth_rate
    )
  
  print(
    la_model_features_with_predictions %>%
      select(
        ladcode21,
        laname21,
        country,
        growth_rate_2001_2020,
        predicted_growth_rate,
        prediction_residual
      ) %>%
      head(10)
  )
  
  return(la_model_features_with_predictions)
}

la_model_features_with_predictions <- add_regression_predictions_to_local_authorities(
  la_model_features,
  linear_regression_model
)

# TOP OVER-PREDICTED AND UNDER-PREDICTED LOCAL AUTHORITIES
identify_regression_prediction_errors <- function(la_model_features_with_predictions) {
  
  most_under_predicted <- la_model_features_with_predictions %>%
    arrange(desc(prediction_residual)) %>%
    select(
      laname21,
      country,
      growth_rate_2001_2020,
      predicted_growth_rate,
      prediction_residual
    ) %>%
    head(10)
  
  most_over_predicted <- la_model_features_with_predictions %>%
    arrange(prediction_residual) %>%
    select(
      laname21,
      country,
      growth_rate_2001_2020,
      predicted_growth_rate,
      prediction_residual
    ) %>%
    head(10)
  
  print(most_under_predicted)
  print(most_over_predicted)
  
  return(list(
    most_under_predicted = most_under_predicted,
    most_over_predicted = most_over_predicted
  ))
}

regression_error_output <- identify_regression_prediction_errors(
  la_model_features_with_predictions
)

most_under_predicted <- regression_error_output$most_under_predicted
most_over_predicted <- regression_error_output$most_over_predicted

# MACHINE LEARNING REGRESSION DATA
create_ml_regression_dataset <- function(la_model_features) {
  
  ml_regression_dataset <- la_model_features %>%
    select(
      growth_rate_2001_2020,
      avg_birth_rate,
      avg_death_rate,
      avg_migration_rate,
      avg_natural_change_rate,
      avg_children_pct,
      avg_young_pct,
      avg_working_pct,
      avg_older_pct,
      avg_dependency_ratio,
      total_net_migration,
      total_natural_change
    ) %>%
    drop_na()
  
  print(head(ml_regression_dataset, 10))
  print(dim(ml_regression_dataset))
  
  return(ml_regression_dataset)
}

ml_regression_dataset <- create_ml_regression_dataset(
  la_model_features
)

# TRAIN-TEST SPLIT FOR MACHINE LEARNING REGRESSION
split_ml_regression_dataset <- function(ml_regression_dataset) {
  
  set.seed(42)
  
  train_index <- caret::createDataPartition(
    ml_regression_dataset$growth_rate_2001_2020,
    p = 0.8,
    list = FALSE
  )
  
  ml_regression_train <- ml_regression_dataset[train_index, ]
  ml_regression_test <- ml_regression_dataset[-train_index, ]
  
  x_train <- ml_regression_train %>%
    select(-growth_rate_2001_2020)
  
  y_train <- ml_regression_train$growth_rate_2001_2020
  
  x_test <- ml_regression_test %>%
    select(-growth_rate_2001_2020)
  
  y_test <- ml_regression_test$growth_rate_2001_2020
  
  print(dim(x_train))
  print(dim(x_test))
  
  return(list(
    ml_regression_train = ml_regression_train,
    ml_regression_test = ml_regression_test,
    x_train = x_train,
    y_train = y_train,
    x_test = x_test,
    y_test = y_test
  ))
}

ml_regression_split <- split_ml_regression_dataset(
  ml_regression_dataset
)

ml_regression_train <- ml_regression_split$ml_regression_train
ml_regression_test <- ml_regression_split$ml_regression_test
x_train <- ml_regression_split$x_train
y_train <- ml_regression_split$y_train
x_test <- ml_regression_split$x_test
y_test <- ml_regression_split$y_test

# REGRESSION METRIC FUNCTION
calculate_ml_regression_metrics <- function(actual_values, predicted_values) {
  
  mae <- mean(abs(actual_values - predicted_values), na.rm = TRUE)
  
  rmse <- sqrt(
    mean((actual_values - predicted_values)^2, na.rm = TRUE)
  )
  
  r_squared <- cor(
    actual_values,
    predicted_values,
    use = "complete.obs"
  )^2
  
  regression_metrics <- tibble(
    MAE = mae,
    RMSE = rmse,
    R_squared = r_squared
  )
  
  return(regression_metrics)
}

# RANDOM FOREST REGRESSION MODEL
train_random_forest_regression <- function(x_train, y_train) {
  
  set.seed(42)
  
  random_forest_regression_model <- randomForest(
    x = x_train,
    y = y_train,
    ntree = 500,
    importance = TRUE
  )
  
  print(random_forest_regression_model)
  
  return(random_forest_regression_model)
}

random_forest_regression_model <- train_random_forest_regression(
  x_train,
  y_train
)
# EVALUATE RANDOM FOREST REGRESSION MODEL
evaluate_random_forest_regression <- function(
    random_forest_regression_model,
    x_train,
    y_train,
    x_test,
    y_test
) {
  
  rf_train_predictions <- predict(
    random_forest_regression_model,
    newdata = x_train
  )
  
  rf_test_predictions <- predict(
    random_forest_regression_model,
    newdata = x_test
  )
  
  rf_training_metrics <- calculate_ml_regression_metrics(
    y_train,
    rf_train_predictions
  ) %>%
    mutate(
      model = "Random Forest",
      dataset = "Training"
    )
  
  rf_testing_metrics <- calculate_ml_regression_metrics(
    y_test,
    rf_test_predictions
  ) %>%
    mutate(
      model = "Random Forest",
      dataset = "Testing"
    )
  
  rf_regression_performance <- bind_rows(
    rf_training_metrics,
    rf_testing_metrics
  ) %>%
    select(
      model,
      dataset,
      MAE,
      RMSE,
      R_squared
    )
  
  rf_test_results <- tibble(
    actual_growth_rate = y_test,
    predicted_growth_rate = rf_test_predictions,
    residual = actual_growth_rate - predicted_growth_rate
  )
  
  print(rf_regression_performance)
  print(head(rf_test_results, 10))
  
  return(list(
    rf_train_predictions = rf_train_predictions,
    rf_test_predictions = rf_test_predictions,
    rf_regression_performance = rf_regression_performance,
    rf_test_results = rf_test_results
  ))
}

rf_regression_output <- evaluate_random_forest_regression(
  random_forest_regression_model,
  x_train,
  y_train,
  x_test,
  y_test
)

rf_train_predictions <- rf_regression_output$rf_train_predictions
rf_test_predictions <- rf_regression_output$rf_test_predictions
rf_regression_performance <- rf_regression_output$rf_regression_performance
rf_test_results <- rf_regression_output$rf_test_results

# RANDOM FOREST FEATURE IMPORTANCE
extract_random_forest_importance <- function(random_forest_regression_model) {
  
  rf_importance_matrix <- importance(
    random_forest_regression_model
  )
  
  rf_feature_importance <- as.data.frame(rf_importance_matrix) %>%
    rownames_to_column("feature") %>%
    arrange(desc(IncNodePurity))
  
  print(rf_feature_importance)
  
  return(rf_feature_importance)
}

rf_feature_importance <- extract_random_forest_importance(
  random_forest_regression_model
)

plot_random_forest_importance <- function(rf_feature_importance) {
  
  rf_importance_plot <- ggplot(
    rf_feature_importance,
    aes(
      x = reorder(feature, IncNodePurity),
      y = IncNodePurity
    )
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Random Forest Feature Importance",
      x = "Feature",
      y = "Increase in Node Purity"
    ) +
    theme_minimal()
  
  print(rf_importance_plot)
  
  return(rf_importance_plot)
}

rf_importance_plot <- plot_random_forest_importance(
  rf_feature_importance
)

# XGBOOST REGRESSION MODEL
train_xgboost_regression <- function(x_train, y_train) {
  
  set.seed(42)
  
  xgboost_regression_model <- xgboost(
    x = as.matrix(x_train),
    y = y_train,
    objective = "reg:squarederror",
    nrounds = 100,
    max_depth = 3,
    learning_rate = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    verbosity = 0
  )
  
  print(xgboost_regression_model)
  
  return(xgboost_regression_model)
}

xgboost_regression_model <- train_xgboost_regression(
  x_train,
  y_train
)

# EVALUATE XGBOOST REGRESSION MODEL
evaluate_xgboost_regression <- function(
    xgboost_regression_model,
    x_train,
    y_train,
    x_test,
    y_test
) {
  
  xgb_train_predictions <- predict(
    xgboost_regression_model,
    as.matrix(x_train)
  )
  
  xgb_test_predictions <- predict(
    xgboost_regression_model,
    as.matrix(x_test)
  )
  
  xgb_training_metrics <- calculate_ml_regression_metrics(
    y_train,
    xgb_train_predictions
  ) %>%
    mutate(
      model = "XGBoost",
      dataset = "Training"
    )
  
  xgb_testing_metrics <- calculate_ml_regression_metrics(
    y_test,
    xgb_test_predictions
  ) %>%
    mutate(
      model = "XGBoost",
      dataset = "Testing"
    )
  
  xgb_regression_performance <- bind_rows(
    xgb_training_metrics,
    xgb_testing_metrics
  ) %>%
    select(
      model,
      dataset,
      MAE,
      RMSE,
      R_squared
    )
  
  xgb_test_results <- tibble(
    actual_growth_rate = y_test,
    predicted_growth_rate = xgb_test_predictions,
    residual = actual_growth_rate - predicted_growth_rate
  )
  
  print(xgb_regression_performance)
  print(head(xgb_test_results, 10))
  
  return(list(
    xgb_train_predictions = xgb_train_predictions,
    xgb_test_predictions = xgb_test_predictions,
    xgb_regression_performance = xgb_regression_performance,
    xgb_test_results = xgb_test_results
  ))
}

xgb_regression_output <- evaluate_xgboost_regression(
  xgboost_regression_model,
  x_train,
  y_train,
  x_test,
  y_test
)

xgb_train_predictions <- xgb_regression_output$xgb_train_predictions
xgb_test_predictions <- xgb_regression_output$xgb_test_predictions
xgb_regression_performance <- xgb_regression_output$xgb_regression_performance
xgb_test_results <- xgb_regression_output$xgb_test_results

# XGBOOST FEATURE IMPORTANCE
extract_xgboost_importance <- function(xgboost_regression_model) {
  
  xgb_feature_importance <- xgb.importance(
    model = xgboost_regression_model
  )
  
  print(xgb_feature_importance)
  
  return(xgb_feature_importance)
}

xgb_feature_importance <- extract_xgboost_importance(
  xgboost_regression_model
)

plot_xgboost_importance <- function(xgb_feature_importance) {
  
  xgb_importance_plot <- ggplot(
    xgb_feature_importance,
    aes(
      x = reorder(Feature, Gain),
      y = Gain
    )
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = "XGBoost Feature Importance",
      x = "Feature",
      y = "Gain"
    ) +
    theme_minimal()
  
  print(xgb_importance_plot)
  
  return(xgb_importance_plot)
}

xgb_importance_plot <- plot_xgboost_importance(
  xgb_feature_importance
)
# COMBINE LINEAR, RANDOM FOREST AND XGBOOST PERFORMANCE
combine_regression_model_performance <- function(
    regression_performance,
    rf_regression_performance,
    xgb_regression_performance
) {
  
  linear_regression_performance <- regression_performance %>%
    mutate(
      model = "Linear Regression"
    ) %>%
    select(
      model,
      dataset,
      MAE,
      RMSE,
      R_squared
    )
  
  regression_model_comparison <- bind_rows(
    linear_regression_performance,
    rf_regression_performance,
    xgb_regression_performance
  ) %>%
    arrange(
      dataset,
      RMSE
    )
  
  print(regression_model_comparison)
  
  return(regression_model_comparison)
}

regression_model_comparison <- combine_regression_model_performance(
  regression_performance,
  rf_regression_performance,
  xgb_regression_performance
)

# PLOT REGRESSION MODEL COMPARISON
plot_regression_model_comparison <- function(regression_model_comparison) {
  
  regression_model_comparison_plot <- ggplot(
    regression_model_comparison,
    aes(
      x = model,
      y = RMSE,
      fill = dataset
    )
  ) +
    geom_col(
      position = "dodge"
    ) +
    labs(
      title = "Regression Model Comparison Using RMSE",
      x = "Model",
      y = "RMSE",
      fill = "Dataset"
    ) +
    theme_minimal()
  
  print(regression_model_comparison_plot)
  
  return(regression_model_comparison_plot)
}

regression_model_comparison_plot <- plot_regression_model_comparison(
  regression_model_comparison
)

# ACTUAL VS PREDICTED PLOTS FOR RANDOM FOREST AND XGBOOST
plot_ml_actual_vs_predicted <- function(model_results, model_name) {
  
  actual_vs_predicted_plot <- ggplot(
    model_results,
    aes(
      x = actual_growth_rate,
      y = predicted_growth_rate
    )
  ) +
    geom_point(alpha = 0.7) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed"
    ) +
    labs(
      title = paste("Actual vs Predicted Growth Rate:", model_name),
      x = "Actual Growth Rate 2001–2020 (%)",
      y = "Predicted Growth Rate 2001–2020 (%)"
    ) +
    theme_minimal()
  
  print(actual_vs_predicted_plot)
  
  return(actual_vs_predicted_plot)
}

rf_actual_vs_predicted_plot <- plot_ml_actual_vs_predicted(
  rf_test_results,
  "Random Forest"
)

xgb_actual_vs_predicted_plot <- plot_ml_actual_vs_predicted(
  xgb_test_results,
  "XGBoost"
)

# RESIDUAL PLOTS FOR RANDOM FOREST AND XGBOOST
plot_ml_regression_residuals <- function(model_results, model_name) {
  
  residual_plot <- ggplot(
    model_results,
    aes(
      x = predicted_growth_rate,
      y = residual
    )
  ) +
    geom_point(alpha = 0.7) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    labs(
      title = paste("Residual Plot:", model_name),
      x = "Predicted Growth Rate 2001–2020 (%)",
      y = "Residual"
    ) +
    theme_minimal()
  
  print(residual_plot)
  
  return(residual_plot)
}

rf_residual_plot <- plot_ml_regression_residuals(
  rf_test_results,
  "Random Forest"
)

xgb_residual_plot <- plot_ml_regression_residuals(
  xgb_test_results,
  "XGBoost"
)

# CREATE CLUSTERING DATASET
create_clustering_dataset <- function(la_model_features) {
  
  clustering_variables <- c(
    "avg_birth_rate",
    "avg_death_rate",
    "avg_migration_rate",
    "avg_natural_change_rate",
    "avg_children_pct",
    "avg_working_pct",
    "avg_older_pct",
    "avg_dependency_ratio",
    "growth_rate_2001_2020"
  )
  
  clustering_dataset <- la_model_features %>%
    select(
      ladcode21,
      laname21,
      country,
      growth_class,
      main_growth_driver,
      all_of(clustering_variables)
    ) %>%
    drop_na()
  
  clustering_numeric_data <- clustering_dataset %>%
    select(all_of(clustering_variables))
  
  print(head(clustering_dataset, 10))
  print(dim(clustering_dataset))
  
  return(list(
    clustering_dataset = clustering_dataset,
    clustering_numeric_data = clustering_numeric_data,
    clustering_variables = clustering_variables
  ))
}

clustering_output <- create_clustering_dataset(
  la_model_features
)

clustering_dataset <- clustering_output$clustering_dataset
clustering_numeric_data <- clustering_output$clustering_numeric_data
clustering_variables <- clustering_output$clustering_variables

# SCALE CLUSTERING VARIABLES
scale_clustering_data <- function(clustering_numeric_data) {
  
  clustering_scaled_matrix <- scale(
    clustering_numeric_data
  )
  
  clustering_scaled_data <- as.data.frame(
    clustering_scaled_matrix
  )
  
  print(head(clustering_scaled_data, 10))
  print(summary(clustering_scaled_data))
  
  return(list(
    clustering_scaled_matrix = clustering_scaled_matrix,
    clustering_scaled_data = clustering_scaled_data
  ))
}

scaled_clustering_output <- scale_clustering_data(
  clustering_numeric_data
)

clustering_scaled_matrix <- scaled_clustering_output$clustering_scaled_matrix
clustering_scaled_data <- scaled_clustering_output$clustering_scaled_data

# ELBOW METHOD FOR CHOOSING NUMBER OF CLUSTERS
calculate_kmeans_elbow_results <- function(clustering_scaled_matrix) {
  
  set.seed(42)
  
  elbow_results <- tibble(
    k = 2:10,
    total_within_cluster_sum_of_squares = map_dbl(
      2:10,
      function(k_value) {
        kmeans(
          clustering_scaled_matrix,
          centers = k_value,
          nstart = 25
        )$tot.withinss
      }
    )
  )
  
  print(elbow_results)
  
  return(elbow_results)
}

elbow_results <- calculate_kmeans_elbow_results(
  clustering_scaled_matrix
)

plot_kmeans_elbow_method <- function(elbow_results) {
  
  elbow_plot <- ggplot(
    elbow_results,
    aes(
      x = k,
      y = total_within_cluster_sum_of_squares
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = 2:10) +
    labs(
      title = "Elbow Method for Selecting Number of Clusters",
      x = "Number of Clusters",
      y = "Total Within-Cluster Sum of Squares"
    ) +
    theme_minimal()
  
  print(elbow_plot)
  
  return(elbow_plot)
}

elbow_plot <- plot_kmeans_elbow_method(
  elbow_results
)

# SILHOUETTE METHOD FOR CHOOSING NUMBER OF CLUSTERS
calculate_kmeans_silhouette_results <- function(clustering_scaled_matrix) {
  
  set.seed(42)
  
  distance_matrix <- dist(
    clustering_scaled_matrix
  )
  
  silhouette_results <- map_dfr(
    2:10,
    function(k_value) {
      
      kmeans_model <- kmeans(
        clustering_scaled_matrix,
        centers = k_value,
        nstart = 25
      )
      
      silhouette_values <- silhouette(
        kmeans_model$cluster,
        distance_matrix
      )
      
      tibble(
        k = k_value,
        average_silhouette_score = mean(silhouette_values[, 3])
      )
    }
  )
  
  print(silhouette_results)
  
  return(silhouette_results)
}

silhouette_results <- calculate_kmeans_silhouette_results(
  clustering_scaled_matrix
)

plot_kmeans_silhouette_results <- function(silhouette_results) {
  
  silhouette_plot <- ggplot(
    silhouette_results,
    aes(
      x = k,
      y = average_silhouette_score
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = 2:10) +
    labs(
      title = "Silhouette Scores for K-Means Clustering",
      x = "Number of Clusters",
      y = "Average Silhouette Score"
    ) +
    theme_minimal()
  
  print(silhouette_plot)
  
  return(silhouette_plot)
}

silhouette_plot <- plot_kmeans_silhouette_results(
  silhouette_results
)

# K-MEANS MODEL
run_final_kmeans_clustering <- function(
    clustering_dataset,
    clustering_scaled_matrix,
    number_of_clusters = 4
) {
  
  set.seed(42)
  
  final_kmeans_model <- kmeans(
    clustering_scaled_matrix,
    centers = number_of_clusters,
    nstart = 25
  )
  
  clustered_local_authorities <- clustering_dataset %>%
    mutate(
      cluster = factor(final_kmeans_model$cluster)
    )
  
  cluster_size_summary <- clustered_local_authorities %>%
    count(
      cluster,
      name = "number_of_local_authorities"
    )
  
  print(final_kmeans_model)
  print(cluster_size_summary)
  print(head(clustered_local_authorities, 10))
  
  return(list(
    final_kmeans_model = final_kmeans_model,
    clustered_local_authorities = clustered_local_authorities,
    cluster_size_summary = cluster_size_summary
  ))
}

kmeans_clustering_output <- run_final_kmeans_clustering(
  clustering_dataset,
  clustering_scaled_matrix,
  number_of_clusters = 4
)

final_kmeans_model <- kmeans_clustering_output$final_kmeans_model
clustered_local_authorities <- kmeans_clustering_output$clustered_local_authorities
cluster_size_summary <- kmeans_clustering_output$cluster_size_summary

# CLUSTER PROFILE TABLE
create_cluster_profile <- function(clustered_local_authorities) {
  
  cluster_profile <- clustered_local_authorities %>%
    group_by(cluster) %>%
    summarise(
      number_of_local_authorities = n(),
      mean_growth_rate = mean(growth_rate_2001_2020, na.rm = TRUE),
      mean_birth_rate = mean(avg_birth_rate, na.rm = TRUE),
      mean_death_rate = mean(avg_death_rate, na.rm = TRUE),
      mean_migration_rate = mean(avg_migration_rate, na.rm = TRUE),
      mean_natural_change_rate = mean(avg_natural_change_rate, na.rm = TRUE),
      mean_children_pct = mean(avg_children_pct, na.rm = TRUE),
      mean_working_pct = mean(avg_working_pct, na.rm = TRUE),
      mean_older_pct = mean(avg_older_pct, na.rm = TRUE),
      mean_dependency_ratio = mean(avg_dependency_ratio, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_growth_rate))
  
  print(cluster_profile)
  
  return(cluster_profile)
}

cluster_profile <- create_cluster_profile(
  clustered_local_authorities
)

# CLUSTER PROFILE HEATMAP
plot_cluster_profile_heatmap <- function(cluster_profile) {
  
  cluster_profile_long <- cluster_profile %>%
    select(
      cluster,
      mean_growth_rate,
      mean_birth_rate,
      mean_death_rate,
      mean_migration_rate,
      mean_natural_change_rate,
      mean_children_pct,
      mean_working_pct,
      mean_older_pct,
      mean_dependency_ratio
    ) %>%
    pivot_longer(
      cols = -cluster,
      names_to = "indicator",
      values_to = "value"
    ) %>%
    group_by(indicator) %>%
    mutate(
      scaled_value = as.numeric(scale(value))
    ) %>%
    ungroup()
  
  cluster_profile_heatmap <- ggplot(
    cluster_profile_long,
    aes(
      x = indicator,
      y = cluster,
      fill = scaled_value
    )
  ) +
    geom_tile(colour = "white") +
    geom_text(
      aes(label = round(value, 2)),
      size = 3
    ) +
    scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "steelblue",
      midpoint = 0,
      name = "Scaled value"
    ) +
    labs(
      title = "Cluster Profile Heatmap",
      x = "Demographic Indicator",
      y = "Cluster"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      panel.grid = element_blank()
    )
  
  print(cluster_profile_heatmap)
  
  return(cluster_profile_heatmap)
}

cluster_profile_heatmap <- plot_cluster_profile_heatmap(
  cluster_profile
)

# PCA FOR CLUSTER VISUALISATION
create_cluster_pca <- function(
    clustering_scaled_matrix,
    clustered_local_authorities
) {
  
  pca_model <- prcomp(
    clustering_scaled_matrix,
    center = FALSE,
    scale. = FALSE
  )
  
  pca_data <- as.data.frame(
    pca_model$x[, 1:2]
  ) %>%
    mutate(
      cluster = clustered_local_authorities$cluster,
      laname21 = clustered_local_authorities$laname21,
      country = clustered_local_authorities$country,
      growth_class = clustered_local_authorities$growth_class,
      main_growth_driver = clustered_local_authorities$main_growth_driver
    )
  
  pca_variance <- tibble(
    component = paste0("PC", seq_along(pca_model$sdev)),
    variance_explained = (pca_model$sdev^2) / sum(pca_model$sdev^2),
    cumulative_variance = cumsum((pca_model$sdev^2) / sum(pca_model$sdev^2))
  )
  
  print(head(pca_data, 10))
  print(pca_variance)
  
  return(list(
    pca_model = pca_model,
    pca_data = pca_data,
    pca_variance = pca_variance
  ))
}

cluster_pca_output <- create_cluster_pca(
  clustering_scaled_matrix,
  clustered_local_authorities
)

pca_model <- cluster_pca_output$pca_model
pca_data <- cluster_pca_output$pca_data
pca_variance <- cluster_pca_output$pca_variance

plot_cluster_pca <- function(pca_data, pca_variance) {
  
  pc1_label <- paste0(
    "PC1 (",
    round(pca_variance$variance_explained[1] * 100, 1),
    "%)"
  )
  
  pc2_label <- paste0(
    "PC2 (",
    round(pca_variance$variance_explained[2] * 100, 1),
    "%)"
  )
  
  cluster_pca_plot <- ggplot(
    pca_data,
    aes(
      x = PC1,
      y = PC2,
      colour = cluster
    )
  ) +
    geom_point(
      alpha = 0.8,
      size = 3
    ) +
    labs(
      title = "PCA Visualisation of Local Authority Clusters",
      x = pc1_label,
      y = pc2_label,
      colour = "Cluster"
    ) +
    theme_minimal()
  
  print(cluster_pca_plot)
  
  return(cluster_pca_plot)
}

# CLUSTER COMPOSITION BY GROWTH CLASS
summarise_cluster_growth_class_composition <- function(clustered_local_authorities) {
  
  cluster_growth_class_composition <- clustered_local_authorities %>%
    count(
      cluster,
      growth_class
    ) %>%
    group_by(cluster) %>%
    mutate(
      proportion = n / sum(n)
    ) %>%
    ungroup()
  
  print(cluster_growth_class_composition)
  
  return(cluster_growth_class_composition)
}

cluster_growth_class_composition <- summarise_cluster_growth_class_composition(
  clustered_local_authorities
)

cluster_pca_plot <- plot_cluster_pca(
  pca_data,
  pca_variance
)

plot_cluster_growth_class_composition <- function(cluster_growth_class_composition) {
  
  cluster_growth_class_plot <- ggplot(
    cluster_growth_class_composition,
    aes(
      x = cluster,
      y = proportion,
      fill = growth_class
    )
  ) +
    geom_col() +
    scale_y_continuous(labels = percent) +
    labs(
      title = "Growth Class Composition Within Each Cluster",
      x = "Cluster",
      y = "Proportion",
      fill = "Growth Class"
    ) +
    theme_minimal()
  
  print(cluster_growth_class_plot)
  
  return(cluster_growth_class_plot)
}

cluster_growth_class_plot <- plot_cluster_growth_class_composition(
  cluster_growth_class_composition
)

# CLUSTER COMPOSITION BY MAIN GROWTH DRIVER
summarise_cluster_growth_driver_composition <- function(clustered_local_authorities) {
  
  cluster_growth_driver_composition <- clustered_local_authorities %>%
    count(
      cluster,
      main_growth_driver
    ) %>%
    group_by(cluster) %>%
    mutate(
      proportion = n / sum(n)
    ) %>%
    ungroup()
  
  print(cluster_growth_driver_composition)
  
  return(cluster_growth_driver_composition)
}

cluster_growth_driver_composition <- summarise_cluster_growth_driver_composition(
  clustered_local_authorities
)

plot_cluster_growth_driver_composition <- function(cluster_growth_driver_composition) {
  
  cluster_growth_driver_plot <- ggplot(
    cluster_growth_driver_composition,
    aes(
      x = cluster,
      y = proportion,
      fill = main_growth_driver
    )
  ) +
    geom_col() +
    scale_y_continuous(labels = percent) +
    labs(
      title = "Main Growth Driver Composition Within Each Cluster",
      x = "Cluster",
      y = "Proportion",
      fill = "Main Growth Driver"
    ) +
    theme_minimal()
  
  print(cluster_growth_driver_plot)
  
  return(cluster_growth_driver_plot)
}

cluster_growth_driver_plot <- plot_cluster_growth_driver_composition(
  cluster_growth_driver_composition
)

# ADD INTERPRETIVE CLUSTER LABELS
create_cluster_interpretation <- function(cluster_profile) {
  
  cluster_interpretation <- cluster_profile %>%
    mutate(
      suggested_cluster_label = case_when(
        mean_growth_rate == max(mean_growth_rate, na.rm = TRUE) &
          mean_migration_rate > median(mean_migration_rate, na.rm = TRUE) ~
          "High-growth migration-driven areas",
        
        mean_older_pct == max(mean_older_pct, na.rm = TRUE) |
          mean_dependency_ratio == max(mean_dependency_ratio, na.rm = TRUE) ~
          "Ageing high-dependency areas",
        
        mean_natural_change_rate == max(mean_natural_change_rate, na.rm = TRUE) ~
          "Natural-change-supported areas",
        
        mean_growth_rate == min(mean_growth_rate, na.rm = TRUE) ~
          "Low-growth or slower-growth areas",
        
        TRUE ~
          "Mixed demographic profile"
      )
    )
  
  print(cluster_interpretation)
  
  return(cluster_interpretation)
}

cluster_interpretation <- create_cluster_interpretation(
  cluster_profile
)

# MERGE CLUSTER LABELS BACK TO LOCAL AUTHORITY DATASET
add_cluster_labels_to_local_authorities <- function(
    clustered_local_authorities,
    cluster_interpretation
) {
  
  clustered_local_authorities_labelled <- clustered_local_authorities %>%
    left_join(
      cluster_interpretation %>%
        select(
          cluster,
          suggested_cluster_label
        ),
      by = "cluster"
    )
  
  print(
    clustered_local_authorities_labelled %>%
      select(
        ladcode21,
        laname21,
        country,
        cluster,
        suggested_cluster_label,
        growth_rate_2001_2020,
        avg_migration_rate,
        avg_older_pct,
        avg_dependency_ratio
      ) %>%
      head(15)
  )
  
  return(clustered_local_authorities_labelled)
}

clustered_local_authorities_labelled <- add_cluster_labels_to_local_authorities(
  clustered_local_authorities,
  cluster_interpretation
)

# CREATE CLASSIFICATION DATASET
create_classification_dataset <- function(la_model_features) {
  
  classification_dataset <- la_model_features %>%
    select(
      growth_class,
      avg_birth_rate,
      avg_death_rate,
      avg_migration_rate,
      avg_natural_change_rate,
      avg_children_pct,
      avg_young_pct,
      avg_working_pct,
      avg_older_pct,
      avg_dependency_ratio,
      total_net_migration,
      total_natural_change
    ) %>%
    drop_na() %>%
    mutate(
      growth_class = factor(
        growth_class,
        levels = c(
          "Low_growth",
          "Medium_growth",
          "High_growth"
        )
      )
    )
  
  print(head(classification_dataset, 10))
  print(dim(classification_dataset))
  print(table(classification_dataset$growth_class))
  
  return(classification_dataset)
}

classification_dataset <- create_classification_dataset(
  la_model_features
)

# TRAIN-TEST SPLIT FOR CLASSIFICATION
split_classification_dataset <- function(classification_dataset) {
  
  set.seed(42)
  
  train_index <- caret::createDataPartition(
    classification_dataset$growth_class,
    p = 0.8,
    list = FALSE
  )
  
  classification_train_data <- classification_dataset[train_index, ]
  classification_test_data <- classification_dataset[-train_index, ]
  
  x_classification_train <- classification_train_data %>%
    select(-growth_class)
  
  y_classification_train <- classification_train_data$growth_class
  
  x_classification_test <- classification_test_data %>%
    select(-growth_class)
  
  y_classification_test <- classification_test_data$growth_class
  
  print(dim(classification_train_data))
  print(dim(classification_test_data))
  print(table(y_classification_train))
  print(table(y_classification_test))
  
  return(list(
    classification_train_data = classification_train_data,
    classification_test_data = classification_test_data,
    x_classification_train = x_classification_train,
    y_classification_train = y_classification_train,
    x_classification_test = x_classification_test,
    y_classification_test = y_classification_test
  ))
}

classification_split <- split_classification_dataset(
  classification_dataset
)

classification_train_data <- classification_split$classification_train_data
classification_test_data <- classification_split$classification_test_data
x_classification_train <- classification_split$x_classification_train
y_classification_train <- classification_split$y_classification_train
x_classification_test <- classification_split$x_classification_test
y_classification_test <- classification_split$y_classification_test

# CLASSIFICATION METRIC FUNCTION
calculate_classification_metrics <- function(actual_classes, predicted_classes, model_name) {
  
  confusion_matrix <- caret::confusionMatrix(
    predicted_classes,
    actual_classes
  )
  
  class_metrics <- as.data.frame(
    confusion_matrix$byClass
  )
  
  accuracy <- as.numeric(confusion_matrix$overall["Accuracy"])
  
  macro_precision <- mean(
    class_metrics$Precision,
    na.rm = TRUE
  )
  
  macro_recall <- mean(
    class_metrics$Recall,
    na.rm = TRUE
  )
  
  macro_f1 <- mean(
    class_metrics$F1,
    na.rm = TRUE
  )
  
  metric_summary <- tibble(
    model = model_name,
    accuracy = accuracy,
    macro_precision = macro_precision,
    macro_recall = macro_recall,
    macro_f1 = macro_f1
  )
  
  return(list(
    confusion_matrix = confusion_matrix,
    metric_summary = metric_summary
  ))
}

# MULTINOMIAL LOGISTIC REGRESSION CLASSIFIER
train_multinomial_logistic_classifier <- function(classification_train_data) {
  
  set.seed(42)
  
  multinomial_logistic_model <- nnet::multinom(
    growth_class ~ .,
    data = classification_train_data,
    trace = FALSE
  )
  
  print(summary(multinomial_logistic_model))
  
  return(multinomial_logistic_model)
}

multinomial_logistic_model <- train_multinomial_logistic_classifier(
  classification_train_data
)

evaluate_multinomial_logistic_classifier <- function(
    multinomial_logistic_model,
    classification_test_data,
    y_classification_test
) {
  
  multinomial_predictions <- predict(
    multinomial_logistic_model,
    newdata = classification_test_data,
    type = "class"
  )
  
  multinomial_predictions <- factor(
    multinomial_predictions,
    levels = levels(y_classification_test)
  )
  
  multinomial_evaluation <- calculate_classification_metrics(
    actual_classes = y_classification_test,
    predicted_classes = multinomial_predictions,
    model_name = "Multinomial Logistic Regression"
  )
  
  print(multinomial_evaluation$confusion_matrix)
  print(multinomial_evaluation$metric_summary)
  
  return(list(
    multinomial_predictions = multinomial_predictions,
    multinomial_confusion_matrix = multinomial_evaluation$confusion_matrix,
    multinomial_metric_summary = multinomial_evaluation$metric_summary
  ))
}

multinomial_output <- evaluate_multinomial_logistic_classifier(
  multinomial_logistic_model,
  classification_test_data,
  y_classification_test
)

multinomial_predictions <- multinomial_output$multinomial_predictions
multinomial_confusion_matrix <- multinomial_output$multinomial_confusion_matrix
multinomial_metric_summary <- multinomial_output$multinomial_metric_summary

# RANDOM FOREST CLASSIFIER
train_random_forest_classifier <- function(
    x_classification_train,
    y_classification_train
) {
  
  set.seed(42)
  
  random_forest_classifier <- randomForest(
    x = x_classification_train,
    y = y_classification_train,
    ntree = 500,
    importance = TRUE
  )
  
  print(random_forest_classifier)
  
  return(random_forest_classifier)
}

random_forest_classifier <- train_random_forest_classifier(
  x_classification_train,
  y_classification_train
)

evaluate_random_forest_classifier <- function(
    random_forest_classifier,
    x_classification_test,
    y_classification_test
) {
  
  random_forest_predictions <- predict(
    random_forest_classifier,
    newdata = x_classification_test
  )
  
  random_forest_predictions <- factor(
    random_forest_predictions,
    levels = levels(y_classification_test)
  )
  
  random_forest_evaluation <- calculate_classification_metrics(
    actual_classes = y_classification_test,
    predicted_classes = random_forest_predictions,
    model_name = "Random Forest"
  )
  
  print(random_forest_evaluation$confusion_matrix)
  print(random_forest_evaluation$metric_summary)
  
  return(list(
    random_forest_predictions = random_forest_predictions,
    random_forest_confusion_matrix = random_forest_evaluation$confusion_matrix,
    random_forest_metric_summary = random_forest_evaluation$metric_summary
  ))
}

random_forest_classification_output <- evaluate_random_forest_classifier(
  random_forest_classifier,
  x_classification_test,
  y_classification_test
)

random_forest_predictions <- random_forest_classification_output$random_forest_predictions
random_forest_confusion_matrix <- random_forest_classification_output$random_forest_confusion_matrix
random_forest_metric_summary <- random_forest_classification_output$random_forest_metric_summary

# RANDOM FOREST CLASSIFICATION FEATURE IMPORTANCE
extract_random_forest_classification_importance <- function(random_forest_classifier) {
  
  random_forest_classification_importance <- as.data.frame(
    importance(random_forest_classifier)
  ) %>%
    rownames_to_column("feature") %>%
    arrange(desc(MeanDecreaseGini))
  
  print(random_forest_classification_importance)
  
  return(random_forest_classification_importance)
}

random_forest_classification_importance <- extract_random_forest_classification_importance(
  random_forest_classifier
)

plot_random_forest_classification_importance <- function(random_forest_classification_importance) {
  
  random_forest_classification_importance_plot <- ggplot(
    random_forest_classification_importance,
    aes(
      x = reorder(feature, MeanDecreaseGini),
      y = MeanDecreaseGini
    )
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Random Forest Classification Feature Importance",
      x = "Feature",
      y = "Mean Decrease Gini"
    ) +
    theme_minimal()
  
  print(random_forest_classification_importance_plot)
  
  return(random_forest_classification_importance_plot)
}

random_forest_classification_importance_plot <- plot_random_forest_classification_importance(
  random_forest_classification_importance
)

# XGBOOST CLASSIFIER
prepare_xgboost_classification_labels <- function(
    y_classification_train,
    y_classification_test
) {
  
  class_levels <- levels(y_classification_train)
  
  xgb_y_train <- as.integer(y_classification_train) - 1
  xgb_y_test <- as.integer(y_classification_test) - 1
  
  print(class_levels)
  print(table(xgb_y_train))
  print(table(xgb_y_test))
  
  return(list(
    class_levels = class_levels,
    xgb_y_train = xgb_y_train,
    xgb_y_test = xgb_y_test
  ))
}

xgb_label_output <- prepare_xgboost_classification_labels(
  y_classification_train,
  y_classification_test
)

class_levels <- xgb_label_output$class_levels
xgb_y_train <- xgb_label_output$xgb_y_train
xgb_y_test <- xgb_label_output$xgb_y_test

train_xgboost_classifier <- function(
    x_classification_train,
    xgb_y_train,
    class_levels
) {
  
  set.seed(42)
  
  xgb_train_matrix <- xgb.DMatrix(
    data = as.matrix(x_classification_train),
    label = xgb_y_train
  )
  
  xgb_parameters <- list(
    objective = "multi:softprob",
    num_class = length(class_levels),
    eval_metric = "mlogloss",
    max_depth = 3,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  xgboost_classifier <- xgb.train(
    params = xgb_parameters,
    data = xgb_train_matrix,
    nrounds = 100,
    verbose = 0
  )
  
  print(xgboost_classifier)
  
  return(xgboost_classifier)
}

xgboost_classifier <- train_xgboost_classifier(
  x_classification_train,
  xgb_y_train,
  class_levels
)

evaluate_xgboost_classifier <- function(
    xgboost_classifier,
    x_classification_test,
    y_classification_test,
    class_levels
) {
  
  xgb_test_matrix <- xgb.DMatrix(
    data = as.matrix(x_classification_test)
  )
  
  xgb_prediction_probabilities <- predict(
    xgboost_classifier,
    newdata = xgb_test_matrix
  )
  
  xgb_prediction_matrix <- matrix(
    xgb_prediction_probabilities,
    ncol = length(class_levels),
    byrow = TRUE
  )
  
  xgb_predicted_class_index <- max.col(
    xgb_prediction_matrix
  )
  
  xgboost_predictions <- factor(
    class_levels[xgb_predicted_class_index],
    levels = class_levels
  )
  
  xgboost_evaluation <- calculate_classification_metrics(
    actual_classes = y_classification_test,
    predicted_classes = xgboost_predictions,
    model_name = "XGBoost"
  )
  
  print(xgboost_evaluation$confusion_matrix)
  print(xgboost_evaluation$metric_summary)
  
  return(list(
    xgb_prediction_probabilities = xgb_prediction_probabilities,
    xgb_prediction_matrix = xgb_prediction_matrix,
    xgboost_predictions = xgboost_predictions,
    xgboost_confusion_matrix = xgboost_evaluation$confusion_matrix,
    xgboost_metric_summary = xgboost_evaluation$metric_summary
  ))
}

xgboost_classification_output <- evaluate_xgboost_classifier(
  xgboost_classifier,
  x_classification_test,
  y_classification_test,
  class_levels
)

xgboost_predictions <- xgboost_classification_output$xgboost_predictions
xgboost_confusion_matrix <- xgboost_classification_output$xgboost_confusion_matrix
xgboost_metric_summary <- xgboost_classification_output$xgboost_metric_summary

# XGBOOST CLASSIFICATION FEATURE IMPORTANCE
extract_xgboost_classification_importance <- function(xgboost_classifier) {
  
  xgboost_classification_importance <- xgb.importance(
    model = xgboost_classifier
  )
  
  print(xgboost_classification_importance)
  
  return(xgboost_classification_importance)
}

xgboost_classification_importance <- extract_xgboost_classification_importance(
  xgboost_classifier
)

plot_xgboost_classification_importance <- function(xgboost_classification_importance) {
  
  xgboost_classification_importance_plot <- ggplot(
    xgboost_classification_importance,
    aes(
      x = reorder(Feature, Gain),
      y = Gain
    )
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = "XGBoost Classification Feature Importance",
      x = "Feature",
      y = "Gain"
    ) +
    theme_minimal()
  
  print(xgboost_classification_importance_plot)
  
  return(xgboost_classification_importance_plot)
}

xgboost_classification_importance_plot <- plot_xgboost_classification_importance(
  xgboost_classification_importance
)

# COMBINE CLASSIFICATION MODEL PERFORMANCE
combine_classification_model_performance <- function(
    multinomial_metric_summary,
    random_forest_metric_summary,
    xgboost_metric_summary
) {
  
  classification_model_comparison <- bind_rows(
    multinomial_metric_summary,
    random_forest_metric_summary,
    xgboost_metric_summary
  ) %>%
    arrange(desc(macro_f1))
  
  print(classification_model_comparison)
  
  return(classification_model_comparison)
}

classification_model_comparison <- combine_classification_model_performance(
  multinomial_metric_summary,
  random_forest_metric_summary,
  xgboost_metric_summary
)

# PLOT CLASSIFICATION MODEL COMPARISON
plot_classification_model_comparison <- function(classification_model_comparison) {
  
  classification_model_comparison_long <- classification_model_comparison %>%
    pivot_longer(
      cols = c(
        accuracy,
        macro_precision,
        macro_recall,
        macro_f1
      ),
      names_to = "metric",
      values_to = "score"
    )
  
  classification_model_comparison_plot <- ggplot(
    classification_model_comparison_long,
    aes(
      x = model,
      y = score,
      fill = metric
    )
  ) +
    geom_col(
      position = "dodge"
    ) +
    scale_y_continuous(
      limits = c(0, 1)
    ) +
    labs(
      title = "Classification Model Comparison",
      x = "Model",
      y = "Score",
      fill = "Metric"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 25,
        hjust = 1
      )
    )
  
  print(classification_model_comparison_plot)
  
  return(classification_model_comparison_plot)
}

classification_model_comparison_plot <- plot_classification_model_comparison(
  classification_model_comparison
)

# CONFUSION MATRIX DATA FOR VISUALISATION
create_confusion_matrix_plot_data <- function(actual_classes, predicted_classes, model_name) {
  
  confusion_plot_data <- tibble(
    actual = actual_classes,
    predicted = predicted_classes
  ) %>%
    count(
      actual,
      predicted
    ) %>%
    mutate(
      model = model_name
    )
  
  return(confusion_plot_data)
}

multinomial_confusion_plot_data <- create_confusion_matrix_plot_data(
  y_classification_test,
  multinomial_predictions,
  "Multinomial Logistic Regression"
)

random_forest_confusion_plot_data <- create_confusion_matrix_plot_data(
  y_classification_test,
  random_forest_predictions,
  "Random Forest"
)

xgboost_confusion_plot_data <- create_confusion_matrix_plot_data(
  y_classification_test,
  xgboost_predictions,
  "XGBoost"
)

classification_confusion_plot_data <- bind_rows(
  multinomial_confusion_plot_data,
  random_forest_confusion_plot_data,
  xgboost_confusion_plot_data
)

print(classification_confusion_plot_data)

# PLOT CONFUSION MATRICES
plot_classification_confusion_matrices <- function(classification_confusion_plot_data) {
  
  classification_confusion_matrix_plot <- ggplot(
    classification_confusion_plot_data,
    aes(
      x = predicted,
      y = actual,
      fill = n
    )
  ) +
    geom_tile(
      colour = "white"
    ) +
    geom_text(
      aes(label = n),
      size = 4
    ) +
    facet_wrap(
      ~ model
    ) +
    labs(
      title = "Confusion Matrices for Classification Models",
      x = "Predicted Growth Class",
      y = "Actual Growth Class",
      fill = "Count"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 35,
        hjust = 1
      ),
      panel.grid = element_blank()
    )
  
  print(classification_confusion_matrix_plot)
  
  return(classification_confusion_matrix_plot)
}

classification_confusion_matrix_plot <- plot_classification_confusion_matrices(
  classification_confusion_plot_data
)


# FINAL HYPOTHESIS TEST INTERPRETATION TABLE
create_final_hypothesis_interpretation <- function(hypothesis_test_summary) {
  
  final_hypothesis_interpretation <- hypothesis_test_summary %>%
    mutate(
      significance = case_when(
        p_value < 0.001 ~ "Highly significant",
        p_value < 0.01 ~ "Significant",
        p_value < 0.05 ~ "Weakly significant",
        TRUE ~ "Not statistically significant"
      ),
      report_interpretation = case_when(
        test_name == "Welch t-test: birth rate by country" &
          p_value < 0.05 ~
          "Birth rates differ significantly between England and Wales.",
        
        test_name == "Welch t-test: birth rate by country" &
          p_value >= 0.05 ~
          "There is no statistically significant evidence that birth rates differ between England and Wales.",
        
        test_name == "Welch t-test: death rate by country" &
          p_value < 0.05 ~
          "Death rates differ significantly between England and Wales.",
        
        test_name == "Welch t-test: death rate by country" &
          p_value >= 0.05 ~
          "There is no statistically significant evidence that death rates differ between England and Wales.",
        
        test_name == "Paired t-test: population by sex" &
          p_value < 0.05 ~
          "Male and female population totals differ significantly across local authority-year observations.",
        
        test_name == "Paired t-test: population by sex" &
          p_value >= 0.05 ~
          "There is no statistically significant evidence of a difference between male and female population totals.",
        
        test_name == "ANOVA: migration rate by growth class" &
          p_value < 0.05 ~
          "Migration rates differ significantly across low, medium and high-growth local authorities.",
        
        test_name == "ANOVA: migration rate by growth class" &
          p_value >= 0.05 ~
          "There is no statistically significant evidence that migration rates differ across growth classes.",
        
        test_name == "Kruskal-Wallis: dependency ratio by growth class" &
          p_value < 0.05 ~
          "Dependency ratios differ significantly across growth classes.",
        
        test_name == "Kruskal-Wallis: dependency ratio by growth class" &
          p_value >= 0.05 ~
          "There is no statistically significant evidence that dependency ratios differ across growth classes.",
        
        test_name == "Chi-square: country by main growth driver" &
          p_value < 0.05 ~
          "Country is statistically associated with the main growth driver.",
        
        test_name == "Chi-square: country by main growth driver" &
          p_value >= 0.05 ~
          "There is no statistically significant association between country and main growth driver.",
        
        TRUE ~ "Interpretation not specified."
      )
    )
  
  print(final_hypothesis_interpretation)
  
  return(final_hypothesis_interpretation)
}

final_hypothesis_interpretation <- create_final_hypothesis_interpretation(
  hypothesis_test_summary
)

# FINAL REGRESSION MODEL SUMMARY
create_final_regression_summary <- function(regression_model_comparison) {
  
  final_regression_summary <- regression_model_comparison %>%
    filter(dataset == "Testing") %>%
    arrange(RMSE) %>%
    mutate(
      model_rank = row_number(),
      interpretation = case_when(
        model_rank == 1 ~ "Best testing performance based on lowest RMSE.",
        TRUE ~ "Lower predictive performance compared with the best model."
      )
    )
  
  print(final_regression_summary)
  
  return(final_regression_summary)
}

final_regression_summary <- create_final_regression_summary(
  regression_model_comparison
)

# FINAL CLASSIFICATION MODEL SUMMARY
create_final_classification_summary <- function(classification_model_comparison) {
  
  final_classification_summary <- classification_model_comparison %>%
    arrange(desc(macro_f1)) %>%
    mutate(
      model_rank = row_number(),
      interpretation = case_when(
        model_rank == 1 ~ "Best classification model based on macro F1-score.",
        TRUE ~ "Lower classification performance compared with the best model."
      )
    )
  
  print(final_classification_summary)
  
  return(final_classification_summary)
}

final_classification_summary <- create_final_classification_summary(
  classification_model_comparison
)

