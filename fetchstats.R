# Author - eli, faro.gov
# Contributors - marketsupreme

options(repos = c(CRAN = "https://cran.rstudio.com"))
required_packages <- trimws(readLines("./dependencies/packages.txt"))

# Check each package and install if it's not already installed
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load all the required packages
library(nflfastR)
library(tidyr)
library(tidyverse)
library(DT)
library(ggplot2)
library(gridExtra)
library(nflplotR)
library(plotly)

# Load play-by-play data for the current season
pbp <- load_pbp(2024)

# Calculate game results
game_results <- pbp %>%
  group_by(game_id) %>%
  summarize(
    home_team = first(home_team),
    away_team = first(away_team),
    home_score = max(total_home_score, na.rm = TRUE),
    away_score = max(total_away_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    winner = case_when(
      home_score > away_score ~ home_team,
      away_score > home_score ~ away_team,
      TRUE ~ NA_character_
    )
  )

# Add game_result to pbp dataset
pbp <- pbp %>%
  left_join(game_results %>% select(game_id, winner), by = "game_id") %>%
  mutate(game_result = ifelse(posteam == winner, 1, 0))

# Calculate weekly team offensive EPA stats
weekly_epa_stats <- pbp %>%
  filter(!is.na(posteam)) %>%
  group_by(week, posteam) %>%
  summarize(
    points_per_play = sum(case_when(
      touchdown == 1 ~ 6,
      field_goal_result == "made" ~ 3,
      extra_point_result == "good" ~ 1,
      two_point_attempt == 1 & two_point_conv_result == "success" ~ 2,
      safety == 1 ~ 2,
      TRUE ~ 0
    ), na.rm = TRUE) / n(),
    epa_pass_per_play = mean(epa[pass == 1], na.rm = TRUE),
    epa_run_per_play = mean(epa[rush == 1], na.rm = TRUE),
    success_rate = mean(success[!is.na(success)], na.rm = TRUE),
    yards_per_play = sum(yards_gained, na.rm = TRUE) / n(),
    wins = sum(game_result, na.rm = TRUE),
    losses = sum(1 - game_result, na.rm = TRUE),
    win_percentage = wins / (wins + losses),
    .groups = 'drop'
  )

# Calculate weekly team defensive EPA stats
weekly_def_epa_stats <- pbp %>%
  filter(!is.na(defteam)) %>%
  group_by(week, defteam) %>%
  summarize(
    points_against_per_play = sum(case_when(
      touchdown == 1 ~ 6,
      field_goal_result == "made" ~ 3,
      extra_point_result == "good" ~ 1,
      two_point_attempt == 1 & two_point_conv_result == "success" ~ 2,
      safety == 1 ~ 2,
      TRUE ~ 0
    ), na.rm = TRUE) / n(),
    epa_pass_against_per_play = mean(epa[pass == 1], na.rm = TRUE),
    epa_run_against_per_play = mean(epa[rush == 1], na.rm = TRUE),
    success_rate_against = mean(success[!is.na(success)], na.rm = TRUE),
    yards_against_per_play = sum(yards_gained, na.rm = TRUE) / n(),
    .groups = 'drop'
  )

# Calculate means and directional variances for defensive stats
def_team_stats <- weekly_def_epa_stats %>%
  group_by(defteam) %>%
  summarize(
    avg_points_against_per_play = mean(points_against_per_play, na.rm = TRUE),
    points_against_per_play_variance = 
      calculate_directional_variance(points_against_per_play, 
      mean(points_against_per_play, na.rm = TRUE)),
    avg_epa_pass_against = mean(epa_pass_against_per_play, na.rm = TRUE),
    epa_pass_against_variance = calculate_directional_variance(epa_pass_against_per_play, mean(epa_pass_against_per_play, na.rm = TRUE)),
    avg_epa_run_against = mean(epa_run_against_per_play, na.rm = TRUE),
    epa_run_against_variance = calculate_directional_variance(epa_run_against_per_play, mean(epa_run_against_per_play, na.rm = TRUE)),
    avg_success_rate_against = mean(success_rate_against, na.rm = TRUE),
    success_rate_against_variance = calculate_directional_variance(success_rate_against, mean(success_rate_against, na.rm = TRUE)),
    avg_yards_against_per_play = mean(yards_against_per_play, na.rm = TRUE),
    yards_against_per_play_variance = calculate_directional_variance(yards_against_per_play, mean(yards_against_per_play, na.rm = TRUE)),
    .groups = 'drop'
  ) %>%
  rename(team = defteam)

# Calculate combined variance score
combined_variance_stats <- team_stats %>%
  left_join(def_team_stats, by = "team") %>%
  mutate(
    combined_variance_score = (
      # Offensive variances (lower is better)
      normalize_variance(points_per_play_variance, FALSE) +
        normalize_variance(epa_pass_variance, FALSE) +
        normalize_variance(epa_run_variance, FALSE) +
        normalize_variance(success_rate_variance, FALSE) +
        normalize_variance(yards_per_play_variance, FALSE) +
        # Defensive variances (higher is better)
        normalize_variance(points_against_per_play_variance, TRUE) +
        normalize_variance(epa_pass_against_variance, TRUE) +
        normalize_variance(epa_run_against_variance, TRUE) +
        normalize_variance(success_rate_against_variance, TRUE) +
        normalize_variance(yards_against_per_play_variance, TRUE)
    ) / 10  # Divide by number of components to get average
  ) %>%
  select(team, combined_variance_score)

# Calculate means and directional variances for offensive stats
team_stats <- weekly_epa_stats %>%
  group_by(posteam) %>%
  summarize(
    avg_points_per_play = mean(points_per_play, na.rm = TRUE),
    points_per_play_variance = calculate_directional_variance(points_per_play, mean(points_per_play, na.rm = TRUE)),
    avg_epa_pass = mean(epa_pass_per_play, na.rm = TRUE),
    epa_pass_variance = calculate_directional_variance(epa_pass_per_play, mean(epa_pass_per_play, na.rm = TRUE)),
    avg_epa_run = mean(epa_run_per_play, na.rm = TRUE),
    epa_run_variance = calculate_directional_variance(epa_run_per_play, mean(epa_run_per_play, na.rm = TRUE)),
    avg_success_rate = mean(success_rate, na.rm = TRUE),
    success_rate_variance = calculate_directional_variance(success_rate, mean(success_rate, na.rm = TRUE)),
    avg_yards_per_play = mean(yards_per_play, na.rm = TRUE),
    yards_per_play_variance = calculate_directional_variance(yards_per_play, mean(yards_per_play, na.rm = TRUE)),
    win_percentage = mean(win_percentage, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(team = posteam)

# Calculate means and directional variances for defensive stats
def_team_stats <- weekly_def_epa_stats %>%
  group_by(defteam) %>%
  summarize(
    avg_points_against_per_play = mean(points_against_per_play, na.rm = TRUE),
    points_against_per_play_variance = calculate_directional_variance(points_against_per_play, mean(points_against_per_play, na.rm = TRUE)),
    avg_epa_pass_against = mean(epa_pass_against_per_play, na.rm = TRUE),
    epa_pass_against_variance = calculate_directional_variance(epa_pass_against_per_play, mean(epa_pass_against_per_play, na.rm = TRUE)),
    avg_epa_run_against = mean(epa_run_against_per_play, na.rm = TRUE),
    epa_run_against_variance = calculate_directional_variance(epa_run_against_per_play, mean(epa_run_against_per_play, na.rm = TRUE)),
    avg_success_rate_against = mean(success_rate_against, na.rm = TRUE),
    success_rate_against_variance = calculate_directional_variance(success_rate_against, mean(success_rate_against, na.rm = TRUE)),
    avg_yards_against_per_play = mean(yards_against_per_play, na.rm = TRUE),
    yards_against_per_play_variance = calculate_directional_variance(yards_against_per_play, mean(yards_against_per_play, na.rm = TRUE)),
    .groups = 'drop'
  ) %>%
  rename(team = defteam)

final_stats <- team_stats %>%
  left_join(def_team_stats, by = "team") %>%
  left_join(combined_variance_stats, by = "team") %>%
  left_join(
    teams_colors_logos %>%
      select(team_abbr, team_name) %>%
      rename(team = team_abbr),
    by = "team"
  ) %>%
  filter(!is.na(team_name), team_name != "") %>%
  mutate(
    adj_avg_points_per_play = avg_points_per_play + points_per_play_variance,
    adj_avg_epa_pass = avg_epa_pass + epa_pass_variance,
    adj_avg_epa_run = avg_epa_run + epa_run_variance, 
    adj_avg_success_rate = avg_success_rate + success_rate_variance,
    adj_avg_yards_per_play = avg_yards_per_play + yards_per_play_variance,
    adj_avg_points_against_per_play = avg_points_against_per_play - points_against_per_play_variance,
    adj_avg_epa_pass_against = avg_epa_pass_against - epa_pass_against_variance,
    adj_avg_epa_run_against = avg_epa_run_against - epa_run_against_variance,
    adj_avg_success_rate_against = avg_success_rate_against - success_rate_against_variance,
    adj_avg_yards_against_per_play = avg_yards_against_per_play - yards_against_per_play_variance
  )

create_nfl_scatterplot <- function(data, x_col_num, y_col_num, add_trendline = FALSE) {
  col_names <- colnames(data)
  x_col <- col_names[x_col_num]
  y_col <- col_names[y_col_num]
  
  better_lower_stats <- c(
    "avg_points_against_per_play",
    "avg_epa_pass_against",
    "avg_epa_run_against",
    "avg_success_rate_against",
    "avg_yards_against_per_play",
    "points_per_play_variance",
    "epa_pass_variance",
    "epa_run_variance",
    "success_rate_variance",
    "yards_per_play_variance"
  )
  
  better_higher_stats <- c(
    "avg_points_per_play",
    "avg_epa_pass",
    "avg_epa_run",
    "avg_success_rate",
    "avg_yards_per_play",
    "points_against_per_play_variance",
    "epa_pass_against_variance",
    "epa_run_against_variance",
    "success_rate_against_variance",
    "yards_against_per_play_variance",
    "combined_variance_score",
    "win_percentage"
  )
  
  # Determine if axes should be inverted
  x_invert <- x_col %in% better_lower_stats
  y_invert <- y_col %in% better_lower_stats
  
  # Remove NA values before creating plot data
  plot_data <- data.frame(
    x = as.numeric(data[[x_col]]),
    y = as.numeric(data[[y_col]]),
    team_abbr = data$team,
    team_name = data$team_name,
    stringsAsFactors = FALSE
  ) %>%
    na.omit()  # Remove rows with NA values
  
  # Calculate correlation
  cor_val <- cor(plot_data$x, plot_data$y, use = "complete.obs")
  
  # Calculate the plot limits and center point with padding
  x_padding <- diff(range(plot_data$x, na.rm = TRUE)) * 0.2
  y_padding <- diff(range(plot_data$y, na.rm = TRUE)) * 0.2
  
  x_limits <- range(plot_data$x, na.rm = TRUE) + c(-x_padding, x_padding)
  y_limits <- range(plot_data$y, na.rm = TRUE) + c(-y_padding, y_padding)
  
  x_center <- mean(range(plot_data$x, na.rm = TRUE))
  y_center <- mean(range(plot_data$y, na.rm = TRUE))
  
  # Create base plot
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.margin = margin(5, 5, 5, 5),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      aspect.ratio = 0.7
    )
  
  if(add_trendline) {
    plot <- plot + 
      geom_smooth(method = "lm", se = FALSE, color = "blue", 
                  alpha = 0.3, linetype = "dashed")
  }
  
  # Add team logos and reference lines
  plot <- plot +
    nflplotR::geom_nfl_logos(aes(team_abbr = team_abbr), width = 0.07, alpha = 0.7) +
    geom_vline(xintercept = x_center, 
               color = "red", linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = y_center, 
               color = "red", linetype = "dashed", alpha = 0.5)
  
  # Set axis scales with proper inversion and limits
  if(x_invert) {
    plot <- plot + 
      scale_x_reverse(limits = rev(x_limits))
  } else {
    plot <- plot + 
      scale_x_continuous(limits = x_limits)
  }
  
  if(y_invert) {
    plot <- plot + 
      scale_y_reverse(limits = rev(y_limits))
  } else {
    plot <- plot + 
      scale_y_continuous(limits = y_limits)
  }
  
  # Add labels and title
  plot <- plot +
    labs(
      x = gsub("_", " ", toupper(x_col)),
      y = gsub("_", " ", toupper(y_col)),
      title = paste(gsub("_", " ", toupper(x_col)), "vs", gsub("_", " ", toupper(y_col))),
      subtitle = sprintf("Correlation: %.3f", cor_val)
    )
  
  print(plot)
}

# Update the graph function
graph <- function(x_col_num, y_col_num, add_trendline = FALSE) {
  create_nfl_scatterplot(result_table, x_col_num, y_col_num, add_trendline)
}

heat <- function(...) {
  # Get the column numbers from the arguments
  stat_nums <- c(...)
  
  # Get the selected columns
  selected_cols <- colnames(result_table)[stat_nums]
  
  # Create heatmap data
  heatmap_data <- result_table %>%
    select(team, all_of(selected_cols)) %>%
    pivot_longer(cols = -team, names_to = "stat", values_to = "value") %>%
    group_by(stat) %>%
    mutate(
      # Calculate z-scores for each stat independently
      scaled_value = (value - mean(value, na.rm = TRUE)) / sd(value, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Create the heatmap
  ggplot(heatmap_data, aes(x = stat, y = team, fill = scaled_value)) +
    geom_tile(width = 1) +  # Make tiles full width
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      name = "Standard Deviations\nfrom Mean"
    ) +
    theme_minimal() +
    theme(
      # Text colors
      axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
      axis.text.y = element_text(color = "white"),
      title = element_text(color = "white"),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      
      # Background colors
      plot.background = element_rect(fill = "black", color = "black"),
      panel.background = element_rect(fill = "black", color = "black"),
      legend.background = element_rect(fill = "black"),
      
      # Remove grid and spacing
      panel.grid = element_blank(),
      panel.spacing = unit(0, "lines"),
      
      # Center title
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(title = paste("Comparison of Multiple Statistics"))
}

# Calculate adjusted stats
final_stats <- final_stats %>%
  mutate(
    adj_avg_points_per_play = avg_points_per_play + points_per_play_variance,
    adj_avg_epa_pass = avg_epa_pass + epa_pass_variance,
    adj_avg_epa_run = avg_epa_run + epa_run_variance,
    adj_avg_success_rate = avg_success_rate + success_rate_variance,
    adj_avg_yards_per_play = avg_yards_per_play + yards_per_play_variance,
    adj_avg_points_against_per_play = avg_points_against_per_play - points_against_per_play_variance,
    adj_avg_epa_pass_against = avg_epa_pass_against - epa_pass_against_variance,
    adj_avg_epa_run_against = avg_epa_run_against - epa_run_against_variance,
    adj_avg_success_rate_against = avg_success_rate_against - success_rate_against_variance,
    adj_avg_yards_against_per_play = avg_yards_against_per_play - yards_against_per_play_variance
  )

# Update the result_table to include the new adjusted stats
result_table <- final_stats %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  select(team, win_percentage, 
         avg_points_per_play, points_per_play_variance, adj_avg_points_per_play,
         avg_epa_pass, epa_pass_variance, adj_avg_epa_pass,
         avg_epa_run, epa_run_variance, adj_avg_epa_run,
         avg_success_rate, success_rate_variance, adj_avg_success_rate,
         avg_yards_per_play, yards_per_play_variance, adj_avg_yards_per_play,
         avg_points_against_per_play, points_against_per_play_variance, adj_avg_points_against_per_play,
         avg_epa_pass_against, epa_pass_against_variance, adj_avg_epa_pass_against,
         avg_epa_run_against, epa_run_against_variance, adj_avg_epa_run_against,
         avg_success_rate_against, success_rate_against_variance, adj_avg_success_rate_against,
         avg_yards_against_per_play, yards_against_per_play_variance, adj_avg_yards_against_per_play,
         combined_variance_score,
         everything())

# Update the datatable function to include the new columns
datatable(result_table,
          colnames = c("Team", "Win %",
                       "Avg Points per Play", "Points per Play Variance", "Adj Avg Points per Play",
                       "Avg EPA Pass", "EPA Pass Variance", "Adj Avg EPA Pass",
                       "Avg EPA Run", "EPA Run Variance", "Adj Avg EPA Run",
                       "Avg Success Rate", "Success Rate Variance", "Adj Avg Success Rate",
                       "Avg Yards per Play", "Yards per Play Variance", "Adj Avg Yards per Play",
                       "Avg Points Against per Play", "Points Against per Play Variance", "Adj Avg Points Against per Play",
                       "Avg EPA Pass Against", "EPA Pass Against Variance", "Adj Avg EPA Pass Against",
                       "Avg EPA Run Against", "EPA Run Against Variance", "Adj Avg EPA Run Against",
                       "Avg Success Rate Against", "Success Rate Against Variance", "Adj Avg Success Rate Against",
                       "Avg Yards Against per Play", "Yards Against per Play Variance", "Adj Avg Yards Against per Play",
                       "Combined Variance Score",
                       "Team Name"),
          options = list(
            pageLength = 32,
            order = list(list(2, 'desc')),  # Sort by Win % (now the 2nd column) in descending order
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = list('copy')
          ),
          extensions = 'Buttons',
          rownames = FALSE) %>%
  formatStyle(0, target = "row", lineHeight = '1.2')


# Update the graph function to use result_table instead of final_stats
graph <- function(x_col_num, y_col_num, add_trendline = FALSE) {
  create_nfl_scatterplot(result_table, x_col_num, y_col_num, add_trendline)
}

# Display column names with indices, matching the order in result_table
cat("\nAvailable Statistics for Plotting:\n")
for(i in seq_along(colnames(result_table))) {
  cat(sprintf("%2d: %s\n", i, colnames(result_table)[i]))
}
cat("\nTo create a plot, use: graph(x, y) where x and y are numbers from the list above\n")
cat("To add a trend line, use: graph(x, y, add_trendline = TRUE)\n")
