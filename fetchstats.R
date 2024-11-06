# Load required packages
library(tidyverse)
library(nflreadr)
library(DT)
library(nflplotR)

# Load play-by-play data for current season
pbp <- load_pbp(2024)

# Load teams colors and logos data
teams_colors_logos <- nflreadr::load_teams() 

# Calculate team records and win percentage
team_records <- pbp %>%
  filter(!is.na(result)) %>%
  group_by(game_id) %>%
  slice(1) %>%
  mutate(
    winner = case_when(
      result > 0 ~ home_team,
      result < 0 ~ away_team,
      result == 0 ~ NA_character_
    ),
    loser = case_when(
      result < 0 ~ home_team,
      result > 0 ~ away_team,
      result == 0 ~ NA_character_
    ),
    tie = result == 0
  ) %>%
  ungroup()

team_win_pct <- bind_rows(
  team_records %>%
    count(winner) %>%
    rename(team = winner, wins = n),
  team_records %>%
    count(loser) %>%
    rename(team = loser, losses = n),
  team_records %>%
    filter(tie) %>%
    count(home_team) %>%
    rename(team = home_team, ties = n)
) %>%
  group_by(team) %>%
  summarize(
    wins = sum(wins, na.rm = TRUE),
    losses = sum(losses, na.rm = TRUE),
    ties = sum(ties, na.rm = TRUE)
  ) %>%
  mutate(
    games_played = wins + losses + ties,
    win_pct = (wins + 0.5 * ties) / games_played
  )

# Create team stats with comprehensive metrics
team_stats <- pbp %>%
  filter(!is.na(epa), !is.na(posteam)) %>%
  group_by(posteam, game_id) %>%
  summarize(
    # Points and EPA
    points = sum(posteam_score_post - posteam_score, na.rm = TRUE),
    total_epa = sum(epa, na.rm = TRUE),
    pass_epa = sum(epa * pass, na.rm = TRUE),
    rush_epa = sum(epa * rush, na.rm = TRUE),
    # Yardage
    total_yards = sum(yards_gained, na.rm = TRUE),
    pass_yards = sum(yards_gained * pass, na.rm = TRUE),
    rush_yards = sum(yards_gained * rush, na.rm = TRUE),
    # Play Counts
    plays = n(),
    pass_plays = sum(pass, na.rm = TRUE),
    rush_plays = sum(rush, na.rm = TRUE),
    first_downs = sum(first_down, na.rm = TRUE),
    # Success and Explosiveness
    success_plays = sum(success, na.rm = TRUE),
    explosive_plays = sum(yards_gained >= 20, na.rm = TRUE),
    # Situational
    third_downs = sum(down == 3, na.rm = TRUE),
    third_conv = sum(third_down_converted, na.rm = TRUE),
    fourth_downs = sum(down == 4, na.rm = TRUE),
    fourth_conv = sum(fourth_down_converted, na.rm = TRUE),
    # Passing Specific
    completions = sum(complete_pass, na.rm = TRUE),
    pass_tds = sum(touchdown * pass, na.rm = TRUE),
    interceptions = sum(interception, na.rm = TRUE),
    sacks = sum(sack, na.rm = TRUE),
    qb_hits = sum(qb_hit, na.rm = TRUE),
    # Rushing Specific
    rush_tds = sum(touchdown * rush, na.rm = TRUE),
    stuffed_runs = sum(yards_gained <= 0 & rush == 1, na.rm = TRUE),
    # Red Zone
    rz_plays = sum(yardline_100 <= 20, na.rm = TRUE),
    rz_tds = sum(touchdown * (yardline_100 <= 20), na.rm = TRUE),
    # Turnovers
    fumbles_lost = sum(fumble_lost, na.rm = TRUE),
    total_turnovers = sum(interception + fumble_lost, na.rm = TRUE)
  ) %>%
  group_by(posteam) %>%
  summarize(
    # Win Percentage
    "Win Percentage" = mean(team_win_pct$win_pct[match(posteam, team_win_pct$team)]),
    # Points Per Play/Game
    "Points Per Play" = mean(points/plays),
    "Points Per Game" = mean(points),
    # EPA Stats
    "EPA Per Play" = mean(total_epa/plays),
    "Pass EPA Per Play" = mean(pass_epa/pass_plays),
    "Rush EPA Per Play" = mean(rush_epa/rush_plays),
    "EPA Per Game" = mean(total_epa),
    "Pass EPA Per Game" = mean(pass_epa),
    "Rush EPA Per Game" = mean(rush_epa),
    # Yardage Stats
    "Yards Per Play" = mean(total_yards/plays),
    "Pass Yards Per Game" = mean(pass_yards),
    "Rush Yards Per Game" = mean(rush_yards),
    "First Downs Per Game" = mean(first_downs),
    # Success Rates
    "Success Rate" = mean(success_plays/plays),
    "Explosive Play Rate" = mean(explosive_plays/plays),
    # Passing Stats
    "Completion Rate" = mean(completions/pass_plays),
    "Pass TD Rate" = mean(pass_tds/pass_plays),
    "INT Rate" = mean(interceptions/pass_plays),
    "Sack Rate" = mean(sacks/pass_plays),
    # Rushing Stats
    "Rush TD Rate" = mean(rush_tds/rush_plays),
    "Stuffed Run Rate" = mean(stuffed_runs/rush_plays),
    # Situational
    "Third Down Rate" = mean(third_conv/third_downs),
    "Fourth Down Rate" = mean(fourth_conv/fourth_downs),
    # Red Zone
    "Red Zone TD Rate" = mean(rz_tds/rz_plays),
    # Turnover Stats
    "Turnovers Per Game" = mean(total_turnovers)
  )

# Create defensive stats
def_stats <- pbp %>%
  filter(!is.na(epa)) %>%
  group_by(defteam, game_id) %>%
  summarize(
    points_allowed = sum(defteam_score_post - defteam_score, na.rm = TRUE),
    yards_allowed = sum(yards_gained, na.rm = TRUE),
    pass_yards_allowed = sum(yards_gained * pass, na.rm = TRUE),
    rush_yards_allowed = sum(yards_gained * rush, na.rm = TRUE),
    first_downs_allowed = sum(first_down, na.rm = TRUE),
    pass_attempts_against = sum(pass, na.rm = TRUE),
    completions_allowed = sum(complete_pass, na.rm = TRUE),
    rush_attempts_against = sum(rush, na.rm = TRUE),
    turnovers_forced = sum(interception + fumble_lost, na.rm = TRUE),
    total_epa_allowed = sum(epa, na.rm = TRUE),
    pass_epa_allowed = sum(epa * pass, na.rm = TRUE),
    rush_epa_allowed = sum(epa * rush, na.rm = TRUE),
    success_plays_allowed = sum(success, na.rm = TRUE),
    explosive_plays_allowed = sum(yards_gained >= 20, na.rm = TRUE),
    plays = n()
  ) %>%
  group_by(defteam) %>%
  summarize(
    "Points Allowed Per Game" = mean(points_allowed),
    "Yards Allowed Per Game" = mean(yards_allowed),
    "Pass Yards Allowed Per Game" = mean(pass_yards_allowed),
    "Rush Yards Allowed Per Game" = mean(rush_yards_allowed),
    "First Downs Allowed Per Game" = mean(first_downs_allowed),
    "Turnovers Forced Per Game" = mean(turnovers_forced),
    "EPA Allowed Per Play" = mean(total_epa_allowed/plays),
    "EPA Allowed Per Pass" = mean(pass_epa_allowed/pass_attempts_against),
    "EPA Allowed Per Rush" = mean(rush_epa_allowed/rush_attempts_against),
    "Success Rate Allowed" = mean(success_plays_allowed/plays),
    "Explosive Play Rate Allowed" = mean(explosive_plays_allowed/plays),
    "Yards Per Play Allowed" = mean(yards_allowed/plays),
    "Yards Per Pass Allowed" = mean(pass_yards_allowed/pass_attempts_against),
    "Yards Per Rush Allowed" = mean(rush_yards_allowed/rush_attempts_against),
    "Completion Rate Allowed" = mean(completions_allowed/pass_attempts_against)
  )

# Combine offensive and defensive stats
team_full_stats <- team_stats %>%
  left_join(def_stats, by = c("posteam" = "defteam"))

# Calculate league averages
league_averages <- colMeans(team_full_stats[, -1], na.rm = TRUE)

# Calculate percentage differences
pct_diff <- team_full_stats[, -1] %>%
  map_df(~ (. - mean(., na.rm = TRUE)) / mean(., na.rm = TRUE) * 100)

# Combine original stats with percentage differences
combined_stats <- team_full_stats %>%
  mutate(across(-posteam, ~ paste0(round(., 3), " (", sprintf("%+.1f%%", pct_diff[[cur_column()]]), ")")))

# Create the modified datatable
datatable(combined_stats, 
          extensions = 'FixedColumns',
          options = list(
            pageLength = 32,
            scrollX = TRUE,
            fixedColumns = list(left = 1),
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel')
          ),
          rownames = FALSE,
          caption = "2024 NFL Team Stats - Per Game and Per Play (with % difference from league average)",
          filter = "top",
          class = "compact stripe hover"
) %>%
  formatStyle(
    names(combined_stats)[-1],
    background = styleColorBar(pct_diff, 'lightblue'),
    backgroundSize = '98% 88%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  )

# Define stats where lower values are better
better_lower_stats <- c(
  "Points Allowed Per Game", "Yards Allowed Per Game", "Pass Yards Allowed Per Game",
  "Rush Yards Allowed Per Game", "First Downs Allowed Per Game", "Turnovers Per Game",
  "EPA Allowed Per Play", "EPA Allowed Per Pass", "EPA Allowed Per Rush",
  "Success Rate Allowed", "Explosive Play Rate Allowed", "Yards Per Play Allowed",
  "Yards Per Pass Allowed", "Yards Per Rush Allowed", "Completion Rate Allowed",
  "INT Rate", "Sack Rate", "Stuffed Run Rate"
)

# Create visualization function
create_nfl_stat_plot <- function(data, x_col, y_col, add_trendline = FALSE) {
  x_invert <- x_col %in% better_lower_stats
  y_invert <- y_col %in% better_lower_stats
  
  # Join with team logos data
  plot_data <- data %>%
    left_join(
      teams_colors_logos %>%
        select(team_abbr, team_name) %>%
        rename(Team = team_abbr),
      by = c("posteam" = "Team")
    ) %>%
    select(posteam, team_name, all_of(c(x_col, y_col))) %>%
    na.omit()
  
  # Calculate correlation
  cor_val <- cor(plot_data[[x_col]], plot_data[[y_col]], use = "complete.obs")
  
  # Calculate plot limits and padding
  x_padding <- diff(range(plot_data[[x_col]], na.rm = TRUE)) * 0.2
  y_padding <- diff(range(plot_data[[y_col]], na.rm = TRUE)) * 0.2
  
  x_limits <- range(plot_data[[x_col]], na.rm = TRUE) + c(-x_padding, x_padding)
  y_limits <- range(plot_data[[y_col]], na.rm = TRUE) + c(-y_padding, y_padding)
  
  x_center <- mean(range(plot_data[[x_col]], na.rm = TRUE))
  y_center <- mean(range(plot_data[[y_col]], na.rm = TRUE))
  
  # Create base plot
  plot <- ggplot(plot_data, aes(x = .data[[x_col]], y = .data[[y_col]])) +
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
    geom_nfl_logos(aes(team_abbr = posteam), width = 0.069, alpha = 0.9) +
    geom_vline(xintercept = x_center, color = "red", linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = y_center, color = "red", linetype = "dashed", alpha = 0.5)
  
  # Set axis scales with proper inversion
  if(x_invert) {
    plot <- plot + scale_x_reverse(limits = rev(x_limits))
  } else {
    plot <- plot + scale_x_continuous(limits = x_limits)
  }
  
  if(y_invert) {
    plot <- plot + scale_y_reverse(limits = rev(y_limits))
  } else {
    plot <- plot + scale_y_continuous(limits = y_limits)
  }
  
  # Add labels
  plot <- plot +
    labs(
      x = x_col,
      y = y_col,
      title = paste(x_col, "vs", y_col),
      subtitle = sprintf("Correlation: %.3f", cor_val)
    )
  
  print(plot)
}

write_csv(team_full_stats, "./statstable.csv")


# Function to display available columns
display_columns <- function() {
  cat("\nAvailable Statistics for Plotting:\n")
  for(i in seq_along(colnames(team_full_stats))) {
    cat(sprintf("%2d: %s\n", i, colnames(team_full_stats)[i]))
  }
  cat("\nTo create a plot, use: graph(x, y) where x and y are numbers from the list above\n")
  cat("To add a trend line, use: graph(x, y, add_trendline = TRUE)\n")
}

# Update the graph function
graph <- function(x_col_num, y_col_num, add_trendline = FALSE) {
  col_names <- colnames(team_full_stats)
  x_col <- col_names[x_col_num]
  y_col <- col_names[y_col_num]
  create_nfl_stat_plot(team_full_stats, x_col, y_col, add_trendline)
}

correl <- function(...) {
  # Get column numbers from arguments
  col_nums <- c(...)
  
  # Get column names
  col_names <- colnames(team_full_stats)[col_nums]
  
  # Create correlation matrix
  cor_matrix <- cor(team_full_stats[, col_nums], use = "complete.obs")
  
  # Convert to a data frame for better display
  cor_df <- as.data.frame(cor_matrix)
  colnames(cor_df) <- col_names
  rownames(cor_df) <- col_names
  
  # Calculate the breaks for the correlation values
  cor_values <- as.vector(cor_matrix[lower.tri(cor_matrix) | upper.tri(cor_matrix)])
  cor_breaks <- quantile(cor_values, probs = c(0.2, 0.4, 0.6, 0.8))
  
  # Create interactive datatable with dynamic formatting
  datatable(cor_df, 
            options = list(
              pageLength = 50,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel')
            ),
            caption = "Correlation Matrix of Selected NFL Statistics"
  ) %>%
    formatRound(columns = 1:ncol(cor_df), digits = 3) %>%
    formatStyle(
      columns = names(cor_df),
      backgroundColor = styleInterval(
        cor_breaks,
        c("#FF0000", "#FFAAAA", "#FFEEEE", "white", "#00FF00")
      )
    )
}

# Display the available columns
display_columns()

