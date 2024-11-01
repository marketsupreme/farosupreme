from flask import Flask, render_template, request, redirect, url_for
from waitress import serve
from flask_sqlalchemy import SQLAlchemy
from graph import create_nfl_scatterplot
import pandas as pd
import subprocess
import os

app = Flask(__name__)
# app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///C:/Users/Michael/Desktop/Coding/CircuitForge/database/orders.db'
# app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
# db = SQLAlchemy(app)

app.app_context().push()

def setResultTable():
    if not os.path.isfile("./statstable.csv"):
        subprocess.run(["Rscript", "./fetchstats.R"], capture_output=False, text=True)
        return pd.read_csv("./statstable.csv")
    else:
        return pd.read_csv("./statstable.csv")
        
def graph_function(result_table, x_col_num, y_col_num, add_trendline=False):
    plot = create_nfl_scatterplot(result_table, x_col_num, y_col_num)
    return plot

@app.route('/')
@app.route('/index')
def index():
    setResultTable()
    return render_template('index.html')

@app.route('/metrics')
def metrics():
    result_table = setResultTable()
    column_names = result_table.columns.tolist()
    filtered_columns = [col for i, col in enumerate(column_names) if i not in (0, 33)]  # Exclude index 0 and 32
    columns_with_indices = list(enumerate(filtered_columns, start=1))
    return render_template('metrics.html', columns = columns_with_indices)

@app.route('/graph', methods=['GET'])
def graph():
    result_table = setResultTable()
    column_names = result_table.columns.tolist()
    filtered_columns = [col for i, col in enumerate(column_names) if i not in (0, 33)]  # Exclude index 0 and 32
    x_axis = request.args.get('x_axis')
    y_axis = request.args.get('y_axis')
    columns_with_indices = list(enumerate(filtered_columns, start=1))
    plot = graph_function(result_table, int(x_axis), int(y_axis))
    return render_template('graph.html', plot=plot, columns=columns_with_indices)

@app.route('/run-script', methods=['POST'])
def run_script_route():
    subprocess.run(["Rscript", "./fetchstats.R"], capture_output=False, text=True)
    return render_template('refresh.html')

if __name__ == "__main__":
    app.run(debug=True)
    serve(app, host="0.0.0.0", port=8000)