from dash import Dash, html, dash_table, dcc
import pandas as pd
import plotly.express as px
import psycopg

with psycopg.connect("dbname=")