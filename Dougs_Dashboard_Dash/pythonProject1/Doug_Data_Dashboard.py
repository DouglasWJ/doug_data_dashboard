from dash import Dash, html, dcc, callback, Output, Input
import pandas as pd
import plotly.express as px
import psycopg
import os
from dotenv import load_dotenv

load_dotenv()

conn = psycopg.connect(host='192.168.1.126',
                       port=5433,
                       user='doug',
                       password=os.environ.get('PG_PASSWORD'),
                       dbname='spatial_db')

sql = "select traveltype,emi_year as year,sum(length2d_km) as length from dougtracks.dougtracks_lines_mv GROUP BY traveltype,emi_year ORDER BY emi_year ASC;"

df = pd.read_sql_query(sql, conn)

app = Dash()

app.layout = [
    html.H1(children='App Title',style={'textAlign':'center'}),
    dcc.Dropdown(df.traveltype.unique(),'Walks',id='dropdown-selection'),
    dcc.Graph(id='graph-content')
]

@callback(
    Output('graph-content', 'figure'),
    Input('dropdown-selection', 'value')
)
def update_graph(value):
    dff = df[df.traveltype==value]
    return px.line(dff, x='year', y='length')

if __name__ == '__main__':
    app.run(debug=True)