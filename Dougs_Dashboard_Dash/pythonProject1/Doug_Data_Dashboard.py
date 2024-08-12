from dash import Dash, html, dcc, callback, Output, Input
import dash_bootstrap_components as dbc
import pandas as pd
import geopandas as gpd
import plotly.express as px
import psycopg
import os
import json
import dash_leaflet as dl
import dash_leaflet.express as dlx
from dash_extensions.javascript import assign
from dotenv import load_dotenv

load_dotenv()

conn = psycopg.connect(host='192.168.1.126',
                       port=5433,
                       user='doug',
                       password=os.environ.get('PG_PASSWORD'),
                       dbname='spatial_db')

sql = "select traveltype,emi_year as year,sum(length2d_km) as length from dougtracks.dougtracks_lines_mv GROUP BY traveltype,emi_year ORDER BY emi_year ASC;"
df = pd.read_sql_query(sql, conn)

sql = "select track_id,traveltype,colourv,superclass_colourv as fillColor,emi_year,time_taken,length2d_km,kg_all_co2e,geom from dougtracks.dougtracks_lines_emi_mv_simple3;"

lines_df = gpd.read_postgis(con=conn, sql=sql, geom_col="geom")

lines_df_geojson = json.loads(lines_df.to_json())

lines_df_geobuf = dlx.geojson_to_geobuf(lines_df_geojson)

#style_handle = assign("""function(feature, context){
#    const {superclass_colourv} = context.hideout;  // get props from hideout
#    const value = feature.properties[superclass_colourv];  // get value the determines the color
#    style.fillColor = value;  // set the fill color according to the class

#    return style;
#}""")

#style_handle = assign("""function(feature,context){
#    console.log("hello")
#    style.fillColor = feature.properties.superclass_colourv;  // set color based on color prop
#    return style;  // render a simple circle marker
#}""")

point_to_layer = assign("""function(feature, context){
    const {superclass_colourv, circleOptions, colorProp} = context.hideout;
    style.fillColor = feature.properties[superclass_colourv];  // set color based on color prop
    return style;  // render a simple circle marker
}""")

app = Dash(external_stylesheets=[dbc.themes.BOOTSTRAP])
SIDEBAR_STYLE = {
    "position": "fixed",
    "top": 0,
    "left": 0,
    "bottom": 0,
    "width": "16rem",
    "padding": "2rem 1rem",
    "backgroundColor": "#f8f9fa"
}

# the styles for the main content position it to the right of the sidebar and
# add some padding.
CONTENT_STYLE = {
    "marginLeft": "18rem",
    "marginRight": "2rem",
    "padding": "2rem 1rem"
}

sidebar = html.Div(
    children=[
        html.H2(children=["Sidebar"], className="display-4"),
        html.Hr(),
        html.P(children=["A simple sidebar layout with navigation links"], className="lead"),
        dbc.Nav(
            children=[
                dbc.NavLink(children=["Home"], href="/", active="exact"),
                dbc.NavLink(children=["Page 1"], href="/page-1", active="exact"),
                dbc.NavLink(children=["Page 2"], href="/page-2", active="exact")
            ],
            vertical=True,
            pills=True
        )
    ],
    id="SidebarDiv",
    style=SIDEBAR_STYLE
)

content = html.Div(
    children=[
        html.H1(children=['App Title'], style={'textAlign': 'center'}),
        dcc.Dropdown(options=df.traveltype.unique(), value='Walk', id='dropdown-selection'),
        dl.Map([
                dl.TileLayer(),
                dl.GeoJSON(data=lines_df_geobuf, format="geobuf")
                #dl.GeoJSON(data=lines_df_geojson, hideout=dict(style=style_handle))
            ], center=[56, 10], zoom=6, style={'height': '50vh'}, id="travelMap"),
        dcc.Graph(id='travelGraph')
    ], id="page-content", style=CONTENT_STYLE)

app.layout = html.Div(children=[dcc.Location(id="url"), sidebar, content])


@callback(
    Output('travelGraph', 'figure'),
    Input('dropdown-selection', 'value')
)
def update_graph(value):
    dff = df[df.traveltype == value]
    return px.line(dff, x='year', y='length')


if __name__ == '__main__':
    app.run(debug=True)
