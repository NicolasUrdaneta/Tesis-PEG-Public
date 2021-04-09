
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from pysentimiento import SentimentAnalyzer
import dask.dataframe as dd
from multiprocessing import Pool, cpu_count
from datetime import datetime
import statistics
from tqdm import tqdm

# Directories
dir = "C:/Users/n.urdaneta/Dropbox/ANDES/Tesis PEG"
modified = dir+"/Data/ANDES/Processed"

# Open datasets to append
df_comments_collapsed1 = pd.read_csv(modified+"/Comments sentiments collapsed1.csv", encoding="unicode_escape")
df_comments_collapsed2 = pd.read_csv(modified+"/Comments sentiments collapsed2.csv", encoding="unicode_escape")
df_comments_collapsed_long = pd.read_csv(modified+"/Comments collapsed long sentiments.csv", encoding="unicode_escape")

df_comments_collapsed=df_comments_collapsed1.append(df_comments_collapsed2, sort=False).append(df_comments_collapsed_long, sort=False)

# Save
df_comments_collapsed.to_csv(modified+'/Comments collapsed sentiments.csv')
