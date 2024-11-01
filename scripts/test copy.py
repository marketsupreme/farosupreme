import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from PIL import Image

# Main function to create NFL scatter plot using Plotly
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
    
    # Calculate correlation
    correlation = plot_data['x'].corr(plot_data['y'])
        
    # Generate image URLs for each team
    plot_data['image_path'] = '../NFLstats/static/images/logos/' + plot_data['team'] + '.tif'
    
    # Calculate ranges for centering, with padding for logos
    padding = 0.1  # Adjust padding as needed
    x_center = plot_data['x'].mean()
    y_center = plot_data['y'].mean()
    x_range = max(abs(plot_data['x'].max() - x_center), abs(plot_data['x'].min() - x_center)) * (1 + padding)
    y_range = max(abs(plot_data['y'].max() - y_center), abs(plot_data['y'].min() - y_center)) * (1 + padding)
    
    # Create the scatter plot using Plotly Express
    fig = px.scatter(
        plot_data,
        x='x',
        y='y',
        hover_name='team_name',
        title=f"{x_col.replace('_', ' ').title()} vs {y_col.replace('_', ' ').title()}<br>Correlation: {correlation:.2f}",
        labels={'x': x_col.replace("_", " ").title(), 'y': y_col.replace("_", " ").title()},
        trendline="ols" if add_trendline else None  # Add trendline if specified
    )

    # Add images as custom markers
    for i, row in plot_data.iterrows():
        img = Image.open(row['image_path'])
        fig.add_layout_image(
            dict(
                source=img,
                xref='x',
                yref='y',
                x=row['x'],
                y=row['y'],
                sizex=0.08,  # Adjust size as necessary
                sizey=0.08,  # Adjust size as necessary
                xanchor="center",
                yanchor="middle",
                opacity=0.8,
                layer="above"
            )
        )

    # Add reference lines that extend to the edges
    fig.add_shape(
        type="line",
        x0=x_center, y0=y_center - y_range, x1=x_center, y1=y_center + y_range,
        line=dict(color="red", width=2, dash="dash")
    )
    fig.add_shape(
        type="line",
        x0=x_center - x_range, y0=y_center, x1=x_center + x_range, y1=y_center,
        line=dict(color="red", width=2, dash="dash")
    )

    # Update layout for inversion and centering with padding
    fig.update_xaxes(range=[x_center - x_range, x_center + x_range] if x_invert else [x_center - x_range, x_center + x_range])
    fig.update_yaxes(range=[y_center - y_range, y_center + y_range] if y_invert else [y_center - y_range, y_center + y_range])
    
    # Show the figure
    fig.show()

# Load data and run the function
result_table = pd.read_csv("./statstable.csv")
create_nfl_scatterplot(result_table, 1, 2, False)
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from PIL import Image

# Main function to create NFL scatter plot using Plotly
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
    
    # Calculate correlation
    correlation = plot_data['x'].corr(plot_data['y'])
        
    # Generate image URLs for each team
    plot_data['image_path'] = '../NFLstats/static/images/logos/' + plot_data['team'] + '.tif'
    
    # Calculate ranges for centering, with padding for logos
    padding = 0.1  # Adjust padding as needed
    x_center = plot_data['x'].mean()
    y_center = plot_data['y'].mean()
    x_range = max(abs(plot_data['x'].max() - x_center), abs(plot_data['x'].min() - x_center)) * (1 + padding)
    y_range = max(abs(plot_data['y'].max() - y_center), abs(plot_data['y'].min() - y_center)) * (1 + padding)
    
    # Create the scatter plot using Plotly Express
    fig = px.scatter(
        plot_data,
        x='x',
        y='y',
        hover_name='team_name',
        title=f"{x_col.replace('_', ' ').title()} vs {y_col.replace('_', ' ').title()}<br>Correlation: {correlation:.2f}",
        labels={'x': x_col.replace("_", " ").title(), 'y': y_col.replace("_", " ").title()},
        trendline="ols" if add_trendline else None  # Add trendline if specified
    )

    # Add images as custom markers
    for i, row in plot_data.iterrows():
        img = Image.open(row['image_path'])
        fig.add_layout_image(
            dict(
                source=img,
                xref='x',
                yref='y',
                x=row['x'],
                y=row['y'],
                sizex=0.08,  # Adjust size as necessary
                sizey=0.08,  # Adjust size as necessary
                xanchor="center",
                yanchor="middle",
                opacity=0.8,
                layer="above"
            )
        )

    # Add reference lines that extend to the edges
    fig.add_shape(
        type="line",
        x0=x_center, y0=y_center - y_range, x1=x_center, y1=y_center + y_range,
        line=dict(color="red", width=2, dash="dash")
    )
    fig.add_shape(
        type="line",
        x0=x_center - x_range, y0=y_center, x1=x_center + x_range, y1=y_center,
        line=dict(color="red", width=2, dash="dash")
    )

    # Update layout for inversion and centering with padding
    fig.update_xaxes(range=[x_center - x_range, x_center + x_range] if x_invert else [x_center - x_range, x_center + x_range])
    fig.update_yaxes(range=[y_center - y_range, y_center + y_range] if y_invert else [y_center - y_range, y_center + y_range])
    
    # Show the figure
    fig.show()

# Load data and run the function
result_table = pd.read_csv("./statstable.csv")
create_nfl_scatterplot(result_table, 1, 2, False)
