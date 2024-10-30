import subprocess
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.offsetbox import OffsetImage, AnnotationBbox
import numpy as np
import argparse #remove this - debugging only

#run R script
def fetch_stats():
    subprocess.run(["Rscript", "./fetchstats.R"], capture_output=False, text=True)

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.offsetbox import OffsetImage, AnnotationBbox
import numpy as np

# Function to add team logo images to the plot
def add_team_logos(ax, plot_data, logo_dict):
    for _, row in plot_data.iterrows():
        team_abbr = row['team_abbr']
        x, y = row['x'], row['y']
        if team_abbr in logo_dict:
            logo_img = logo_dict[team_abbr]
            imagebox = OffsetImage(logo_img, zoom=0.07, alpha=0.7)
            ab = AnnotationBbox(imagebox, (x, y), frameon=False)
            ax.add_artist(ab)

# Main function to create NFL scatter plot
def create_nfl_scatterplot(data, x_col_num, y_col_num, add_trendline=False):
    col_names = data.columns
    x_col = col_names[x_col_num]
    y_col = col_names[y_col_num]
    
    # Lists of stats to determine axis inversion
    better_lower_stats = [
        "avg_points_against_per_play", "avg_epa_pass_against", "avg_epa_run_against",
        "avg_success_rate_against", "avg_yards_against_per_play", "points_per_play_variance",
        "epa_pass_variance", "epa_run_variance", "success_rate_variance", "yards_per_play_variance"
    ]
    
    # Check if axes should be inverted
    x_invert = x_col in better_lower_stats
    y_invert = y_col in better_lower_stats
    
    # Prepare data for plotting, removing rows with NA values
    plot_data = data[[x_col, y_col, 'team', 'team_name']].dropna().copy()
    plot_data['x'] = plot_data[x_col].astype(float)
    plot_data['y'] = plot_data[y_col].astype(float)
    plot_data['team_abbr'] = data['team']  # Abbreviations for logos

    # Calculate correlation
    cor_val = plot_data['x'].corr(plot_data['y'])

    # Calculate plot limits with padding
    x_padding = (plot_data['x'].max() - plot_data['x'].min()) * 0.2
    y_padding = (plot_data['y'].max() - plot_data['y'].min()) * 0.2
    x_limits = [plot_data['x'].min() - x_padding, plot_data['x'].max() + x_padding]
    y_limits = [plot_data['y'].min() - y_padding, plot_data['y'].max() + y_padding]
    x_center = plot_data['x'].mean()
    y_center = plot_data['y'].mean()

    # Set up plot
    fig, ax = plt.subplots(figsize=(10, 7))
    sns.set_theme(style="whitegrid")

    # Scatter plot for team locations
    sns.scatterplot(x='x', y='y', data=plot_data, ax=ax, color='white', edgecolor='black', s=100)

    # Add trendline if specified
    if add_trendline:
        sns.regplot(x='x', y='y', data=plot_data, scatter=False, color="blue", 
                    line_kws={'linestyle': 'dashed'}, ax=ax)

    # Add reference lines
    ax.axvline(x=x_center, color='red', linestyle='--', alpha=0.5)
    ax.axhline(y=y_center, color='red', linestyle='--', alpha=0.5)

    # Add team logos
    # Load your logos as a dictionary where keys are team abbreviations
    logo_dict = {
        # Example: 'team_abbr': plt.imread("path_to_team_logo.png")
    }
    add_team_logos(ax, plot_data, logo_dict)

    # Set axis limits and inversion
    if x_invert:
        ax.set_xlim(x_limits[::-1])
    else:
        ax.set_xlim(x_limits)
        
    if y_invert:
        ax.set_ylim(y_limits[::-1])
    else:
        ax.set_ylim(y_limits)

    # Add labels and title
    ax.set_xlabel(x_col.replace("_", " ").title())
    ax.set_ylabel(y_col.replace("_", " ").title())
    ax.set_title(f"{x_col.replace('_', ' ').title()} vs {y_col.replace('_', ' ').title()}")
    plt.suptitle(f"Correlation: {cor_val:.3f}", y=0.95)
    
    plt.show()


# Wrapper function to create the scatter plot
def graph(x_col_num, y_col_num, add_trendline=False):
    result_table = pd.read_csv('./statstable.csv')
    create_nfl_scatterplot(result_table, x_col_num, y_col_num, add_trendline)

if __name__ == "__main__":
    # Set up argument parser
    parser = argparse.ArgumentParser(description="Generate an NFL scatterplot.")
    parser.add_argument("x_col_num", type=int, help="Column number for x-axis")
    parser.add_argument("y_col_num", type=int, help="Column number for y-axis")
    parser.add_argument("--add_trendline", action="store_true", help="Add a trendline to the plot")
    
    # Parse arguments
    args = parser.parse_args()

    # Call the graph function with arguments
    graph(args.x_col_num, args.y_col_num, args.add_trendline)