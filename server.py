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

# Wrapper function to create the scatter plot
def graph_function(x_col_num, y_col_num, add_trendline=False):
    if not os.path.isfile("./statstable.csv"):
        subprocess.run(["Rscript", "./fetchstats.R"], capture_output=False, text=True)
        result_table = pd.read_csv("./statstable.csv")
        create_nfl_scatterplot(result_table, x_col_num, y_col_num, add_trendline)
    else:
        result_table = pd.read_csv("./statstable.csv")
        create_nfl_scatterplot(result_table, x_col_num, y_col_num, add_trendline)

@app.route('/')
@app.route('/index')
def index():
    return render_template('index.html')

@app.route('/metrics')
def metrics():
    # Process the selections as needed
    return render_template('metrics.html')

@app.route('/graph', methods=['GET'])
def graph():
    x_axis = request.args.get('x_axis')
    y_axis = request.args.get('y_axis')
    graph_function(x_axis, y_axis)
    # Process the selections as needed
    return f"X-Axis: {x_axis}, Y-Axis: {y_axis}"

@app.route('/run-script', methods=['POST'])
def run_script_route():
    subprocess.run(["Rscript", "./fetchstats.R"], capture_output=False, text=True)
    return render_template('refresh.html')

if __name__ == "__main__":
    app.run(debug=True)
    serve(app, host="0.0.0.0", port=8000)