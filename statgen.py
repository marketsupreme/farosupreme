import subprocess
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import argparse #remove this - debugging only

#run R script
def fetch_stats():
    subprocess.run(["Rscript", "./fetchstats.R"], capture_output=False, text=True)



def create_nfl_scatterplot(data, x_col_num, y_col_num, add_trendline=False):
    # Get column names
    col_names = data.columns
    x_col = col_names[x_col_num]
    y_col = col_names[y_col_num]

    # Define which statistics are better when lower or higher
    better_lower_stats = [
        "avg_points_against_per_play", "avg_epa_pass_against", "avg_epa_run_against", 
        "avg_success_rate_against", "avg_yards_against_per_play", "points_per_play_variance", 
        "epa_pass_variance", "epa_run_variance", "success_rate_variance", "yards_per_play_variance"
    ]
    
    better_higher_stats = [
        "avg_points_per_play", "avg_epa_pass", "avg_epa_run", "avg_success_rate", 
        "avg_yards_per_play", "points_against_per_play_variance", "epa_pass_against_variance", 
        "epa_run_against_variance", "success_rate_against_variance", "yards_against_per_play_variance", 
        "combined_variance_score", "win_percentage"
    ]
    
    # Create scatter plot
    plt.figure(figsize=(10, 6))
    sns.scatterplot(data=data, x=x_col, y=y_col)
    
    # Add trendline if required
    if add_trendline:
        sns.regplot(data=data, x=x_col, y=y_col, scatter=False, color="blue", line_kws={"linestyle": "dashed"})

    plt.xlabel(x_col.replace('_', ' ').title())
    plt.ylabel(y_col.replace('_', ' ').title())
    plt.title(f'{x_col.replace("_", " ").title()} vs. {y_col.replace("_", " ").title()}')
    plt.show()

# Creating the table display with pandas
def display_result_table(result_table):
    # Adjust the display to simulate column renaming
    col_rename_dict = {
        # Complete list of column renamings as needed...
        "team": "Team",
        "win_percentage": "Win %",
        "avg_points_per_play": "Avg Points per Play",
        "points_per_play_variance": "Points per Play Variance",
        "combined_variance_score": "Combined Variance Score",
        # Continue mapping for all columns...
    }
    renamed_table = result_table.rename(columns=col_rename_dict)
    
    # Sort by "Win %" in descending order
    sorted_table = renamed_table.sort_values(by="Win %", ascending=False)
    
    # Display the table using the pandas dataframe style
    return sorted_table.style.set_properties(**{'line-height': '1.2'})

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