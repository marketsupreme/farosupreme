import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from PIL import Image
import plotly.io as pio
import json
import base64
from io import BytesIO

def create_nfl_scatterplot(data, x_col_num, y_col_num, add_trendline=False):
    col_names = data.columns
    x_col = col_names[x_col_num]
    y_col = col_names[y_col_num]
    
    better_lower_stats = [
        "avg_points_against_per_play", "avg_epa_pass_against", "avg_epa_run_against",
        "avg_success_rate_against", "avg_yards_against_per_play", "points_per_play_variance",
        "epa_pass_variance", "epa_run_variance", "success_rate_variance", "yards_per_play_variance"
    ]
    
    x_invert = x_col in better_lower_stats
    y_invert = y_col in better_lower_stats
    
    plot_data = data[[x_col, y_col, 'team', 'team_name']].dropna().copy()
    plot_data['x'] = plot_data[x_col].astype(float)
    plot_data['y'] = plot_data[y_col].astype(float)
        
    correlation = plot_data['x'].corr(plot_data['y'])
    
    plot_data['image_path'] = '../NFLstats/static/images/logos/' + plot_data['team'] + '.tif'
    
    padding = 0.1
    x_center = plot_data['x'].mean()
    y_center = plot_data['y'].mean()
    x_range = max(abs(plot_data['x'].max() - x_center), abs(plot_data['x'].min() - x_center)) * (1 + padding)
    y_range = max(abs(plot_data['y'].max() - y_center), abs(plot_data['y'].min() - y_center)) * (1 + padding)
    
    fig = go.Figure()

    # Add scatter traces and images for each team
    image_data = []
    for i, row in plot_data.iterrows():
        fig.add_trace(go.Scatter(
            x=[row['x']],
            y=[row['y']],
            mode='markers',
            marker=dict(opacity=0),
            name=row['team_name'],
            hoverinfo='text',
            hovertext=f"{row['team_name']}<br>{x_col}: {row['x']:.2f}<br>{y_col}: {row['y']:.2f}",
            showlegend=True
        ))
        
        # Convert image to base64
        img = Image.open(row['image_path'])
        buffered = BytesIO()
        img.save(buffered, format="PNG")
        img_str = base64.b64encode(buffered.getvalue()).decode()
        
        image_data.append({
            'source': f'data:image/png;base64,{img_str}',
            'x': row['x'],
            'y': row['y'],
            'sizex': 0.08,
            'sizey': 0.08,
            'xref': 'x',
            'yref': 'y',
            'opacity': 0.8,
            'layer': 'above',
            'sizing': 'contain',
            'visible': True
        })

    # Add reference lines
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

    # Update layout
    fig.update_layout(
        title=f"{x_col.replace('_', ' ').title()} vs {y_col.replace('_', ' ').title()}<br>Correlation: {correlation:.2f}",
        xaxis_title=x_col.replace("_", " ").title(),
        yaxis_title=y_col.replace("_", " ").title(),
        width=800,
        height=800,
        xaxis=dict(range=[x_center - x_range, x_center + x_range]),
        yaxis=dict(range=[y_center - y_range, y_center + y_range]),
        images=image_data
    )

    if add_trendline:
        fig.add_trace(px.scatter(plot_data, x='x', y='y', trendline="ols").data[1])

    # Add custom JavaScript for legend interactivity
    custom_js = """
    function(gd) {
        gd.on('plotly_legendclick', function(data) {
            var traceIndex = data.curveNumber;
            var images = gd.layout.images;
            
            // Toggle image visibility
            images[traceIndex].visible = !images[traceIndex].visible;
            
            Plotly.relayout(gd, {images: images});
            
            // Prevent default legend click behavior
            return false;
        });
    }
    """

    config = {
        'responsive': True,
        'displayModeBar': True,
        'displaylogo': False,
    }

    fig.show()
    plot_html = pio.to_html(fig, full_html=False, config=config, post_script=custom_js)
    return plot_html

result_table = pd.read_csv("./statstable.csv")
plot_html = create_nfl_scatterplot(result_table, 1, 2, False)
