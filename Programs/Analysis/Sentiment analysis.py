
# pip install pysentimiento
# pip install torch===1.4.0 torchvision===0.5.0 -f https://download.pytorch.org/whl/torch_stable.html

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

## Sentiment analysis function
analyzer = SentimentAnalyzer()

## Open data
df_comments = pd.read_csv(modified+"/Comments.csv", encoding="unicode_escape")
df_comments_collapsed = pd.read_csv(modified+"/Comments collapsed.csv", encoding="unicode_escape")
df_comments.head(10)
df_comments_collapsed.head(10)

df_comments.shape
df_comments_collapsed.shape
print(df_comments.iloc[316628,8])

df_comments.dropna(subset=["comment_original"],inplace=True)
df_comments_collapsed.dropna(subset=["comment_original"],inplace=True)
df_comments.shape

## Split data for cases with comments too long to analyse
df_comments["Length"]=df_comments["comment_original"].str.split().str.len()
df_comments_collapsed["Length"]=df_comments_collapsed["comment_original"].str.split().str.len()

df_comments.loc[df_comments["Length"]>450]
df_comments_collapsed.loc[df_comments_collapsed["Length"]>450]


df_comments_long = df_comments.loc[df_comments["Length"]>450]
df_comments = df_comments.loc[df_comments["Length"]<=450]
df_comments_collapsed_long = df_comments_collapsed.loc[df_comments_collapsed["Length"]>450]
df_comments_collapsed = df_comments_collapsed.loc[df_comments_collapsed["Length"]<=450]

df_comments.shape
df_comments_collapsed.shape

## Split data into smaller samples for tests

df_comments_1 = df_comments.loc[range(0,500),:]

##
analyzer.predict(df_comments_1.iloc[0,8])
analyzer.predict_probas(df_comments.iloc[0,8])

## Sentiment analysis on full dataset -> used for descriptive statistics on the validity of this sentiment analysis

start=datetime.now()
print(start)
df_comments_1["PREDICTIONS"]=df_comments_1.apply(lambda row: analyzer.predict_probas(row["comment_original"]), axis=1)


df_comments_1["POSITIVE"]=df_comments_1.apply(lambda row: row["PREDICTIONS"]["POS"], axis=1)
df_comments_1["NEGATIVE"]=df_comments_1.apply(lambda row: row["PREDICTIONS"]["NEG"], axis=1)
df_comments_1["NEUTRAL"]=df_comments_1.apply(lambda row: row["PREDICTIONS"]["NEU"], axis=1)
print(statistics.mean(df_comments_1["POSITIVE"]))
print(statistics.mean(df_comments_1["NEGATIVE"]))

print(datetime.now()-start)


## Sentiment analysis on collapsed dataset -> used for regressions

    # Row 319080 is too long as well, extract first
df_comments_collapsed=df_comments_collapsed.drop([319079], axis=0)

start=datetime.now()
print(start)

df_comments_collapsed["PREDICTIONS"]=df_comments_collapsed.apply(lambda row: analyzer.predict_probas(row["comment_original"]), axis=1)
print(datetime.now()-start)

df_comments_collapsed["POSITIVE"]=df_comments_collapsed.apply(lambda row: row["PREDICTIONS"]["POS"], axis=1)
df_comments_collapsed["NEGATIVE"]=df_comments_collapsed.apply(lambda row: row["PREDICTIONS"]["NEG"], axis=1)
df_comments_collapsed["NEUTRAL"]=df_comments_collapsed.apply(lambda row: row["PREDICTIONS"]["NEU"], axis=1)
print(statistics.mean(df_comments_1["POSITIVE"]))
print(statistics.mean(df_comments_1["NEGATIVE"]))

print(datetime.now()-start)

## Save
df_comments.to_csv(modified+'Comments sentiments.csv')
df_comments_collapsed.to_csv(modified+'Comments collapsed sentiments.csv')


################### Sentiment analysis for long comments #######################

## Add additional row that had been removed later on
df_comments_collapsed2=df_comments_collapsed.loc[319079,:]
print(df_comments_collapsed_long.shape)
df_comments_collapsed_long2=df_comments_collapsed_long
df_comments_collapsed_long2=df_comments_collapsed_long2.append(df_comments_collapsed2)
print(df_comments_collapsed_long2.shape)

df_comments_collapsed_long2

df_comments_collapsed_long2["NEGATIVE"]=df_comments_collapsed_long2.apply(lambda row: analyzer.predict_probas(row["comment_original"])["NEG"], axis=1)
print(statistics.mean(df_comments_collapsed["NEGATIVE"]))

import re
import textwrap
a=textwrap.wrap(df_comments_collapsed_long2.iloc[0,7], 700)
len(a)

df_comments_collapsed_long2["len_str"]=df_comments_collapsed_long2.apply(lambda row: len(textwrap.wrap(row["comment_original"],  700)), axis=1)

df_comments_collapsed_long2["comment_original1"]=df_comments_collapsed_long2.apply(lambda row: textwrap.wrap(row["comment_original"],  700)[0], axis=1)
df_comments_collapsed_long2["comment_original2"]=df_comments_collapsed_long2.apply(lambda row: textwrap.wrap(row["comment_original"],  700)[1], axis=1)
df_comments_collapsed_long2["comment_original3"]=df_comments_collapsed_long2.apply(lambda row: textwrap.wrap(row["comment_original"],  700)[2], axis=1)
df_comments_collapsed_long2["comment_original4"]=df_comments_collapsed_long2.apply(lambda row: textwrap.wrap(row["comment_original"],  700)[3] if (row["len_str"])>=3 else None, axis=1)

df_comments_collapsed_long2_5 = df_comments_collapsed_long2.loc[df_comments_collapsed_long2["len_str"]==5]
df_comments_collapsed_long2_6 = df_comments_collapsed_long2.loc[df_comments_collapsed_long2["len_str"]==6]
df_comments_collapsed_long2_7 = df_comments_collapsed_long2.loc[df_comments_collapsed_long2["len_str"]==7]
df_comments_collapsed_long2_8 = df_comments_collapsed_long2.loc[df_comments_collapsed_long2["len_str"]==8]

df_comments_collapsed_long2_5["comment_original5"]=df_comments_collapsed_long2_5.apply(lambda row: textwrap.wrap(row["comment_original"],  700)[4], axis=1)
df_comments_collapsed_long2_6["comment_original5"]=df_comments_collapsed_long2_6.apply(lambda row: textwrap.wrap(row["comment_original"],  700)[4], axis=1)
df_comments_collapsed_long2_7["comment_original5"]=df_comments_collapsed_long2_7.apply(lambda row: textwrap.wrap(row["comment_original"],  700)[4], axis=1)
df_comments_collapsed_long2_8["comment_original5"]=df_comments_collapsed_long2_8.apply(lambda row: textwrap.wrap(row["comment_original"],  700)[4], axis=1)

df_comments_collapsed_long2_6["comment_original6"]=df_comments_collapsed_long2_6.apply(lambda row: textwrap.wrap(row["comment_original"],  700)[5], axis=1)
df_comments_collapsed_long2_7["comment_original6"]=df_comments_collapsed_long2_7.apply(lambda row: textwrap.wrap(row["comment_original"],  700)[5], axis=1)
df_comments_collapsed_long2_8["comment_original6"]=df_comments_collapsed_long2_8.apply(lambda row: textwrap.wrap(row["comment_original"],  700)[5], axis=1)

df_comments_collapsed_long2_7["comment_original7"]=df_comments_collapsed_long2_7.apply(lambda row: textwrap.wrap(row["comment_original"],  700)[6], axis=1)
df_comments_collapsed_long2_8["comment_original7"]=df_comments_collapsed_long2_8.apply(lambda row: textwrap.wrap(row["comment_original"],  700)[6], axis=1)

df_comments_collapsed_long2_8["comment_original8"]=df_comments_collapsed_long2_8.apply(lambda row: textwrap.wrap(row["comment_original"],  700)[7], axis=1)

df_comments_collapsed_long2["NEGATIVE"]=df_comments_collapsed_long2.apply(lambda row: (analyzer.predict_probas(row["comment_original1"])["NEG"]+analyzer.predict_probas(row["comment_original2"])["NEG"]+analyzer.predict_probas(row["comment_original3"])["NEG"]+analyzer.predict_probas(row["comment_original4"])["NEG"])/4, axis=1)
df_comments_collapsed_long2["POSITIVE"]=df_comments_collapsed_long2.apply(lambda row: (analyzer.predict_probas(row["comment_original1"])["POS"]+analyzer.predict_probas(row["comment_original2"])["POS"]+analyzer.predict_probas(row["comment_original3"])["POS"]+analyzer.predict_probas(row["comment_original4"])["POS"])/4, axis=1)

df_comments_collapsed_long2_5["NEGATIVE"]=df_comments_collapsed_long2_5.apply(lambda row: (analyzer.predict_probas(row["comment_original1"])["NEG"]+analyzer.predict_probas(row["comment_original2"])["NEG"]+analyzer.predict_probas(row["comment_original3"])["NEG"]+analyzer.predict_probas(row["comment_original4"])["NEG"]+analyzer.predict_probas(row["comment_original5"])["NEG"])/5, axis=1)
df_comments_collapsed_long2_5["POSITIVE"]=df_comments_collapsed_long2_5.apply(lambda row: (analyzer.predict_probas(row["comment_original1"])["POS"]+analyzer.predict_probas(row["comment_original2"])["POS"]+analyzer.predict_probas(row["comment_original3"])["POS"]+analyzer.predict_probas(row["comment_original4"])["POS"]+analyzer.predict_probas(row["comment_original5"])["POS"])/5, axis=1)

df_comments_collapsed_long2_6["NEGATIVE"]=df_comments_collapsed_long2_6.apply(lambda row: (analyzer.predict_probas(row["comment_original1"])["NEG"]+analyzer.predict_probas(row["comment_original2"])["NEG"]+analyzer.predict_probas(row["comment_original3"])["NEG"]+analyzer.predict_probas(row["comment_original4"])["NEG"]+analyzer.predict_probas(row["comment_original5"])["NEG"]+analyzer.predict_probas(row["comment_original6"])["NEG"])/6, axis=1)
df_comments_collapsed_long2_6["POSITIVE"]=df_comments_collapsed_long2_6.apply(lambda row: (analyzer.predict_probas(row["comment_original1"])["POS"]+analyzer.predict_probas(row["comment_original2"])["POS"]+analyzer.predict_probas(row["comment_original3"])["POS"]+analyzer.predict_probas(row["comment_original4"])["POS"]+analyzer.predict_probas(row["comment_original5"])["POS"]+analyzer.predict_probas(row["comment_original6"])["POS"])/6, axis=1)

df_comments_collapsed_long2_7["NEGATIVE"]=df_comments_collapsed_long2_7.apply(lambda row: (analyzer.predict_probas(row["comment_original1"])["NEG"]+analyzer.predict_probas(row["comment_original2"])["NEG"]+analyzer.predict_probas(row["comment_original3"])["NEG"]+analyzer.predict_probas(row["comment_original4"])["NEG"]+analyzer.predict_probas(row["comment_original5"])["NEG"]+analyzer.predict_probas(row["comment_original6"])["NEG"]+analyzer.predict_probas(row["comment_original7"])["NEG"])/7, axis=1)
df_comments_collapsed_long2_7["POSITIVE"]=df_comments_collapsed_long2_7.apply(lambda row: (analyzer.predict_probas(row["comment_original1"])["POS"]+analyzer.predict_probas(row["comment_original2"])["POS"]+analyzer.predict_probas(row["comment_original3"])["POS"]+analyzer.predict_probas(row["comment_original4"])["POS"]+analyzer.predict_probas(row["comment_original5"])["POS"]+analyzer.predict_probas(row["comment_original6"])["POS"]+analyzer.predict_probas(row["comment_original7"])["POS"])/7, axis=1)

df_comments_collapsed_long2_8["NEGATIVE"]=df_comments_collapsed_long2_8.apply(lambda row: (analyzer.predict_probas(row["comment_original1"])["NEG"]+analyzer.predict_probas(row["comment_original2"])["NEG"]+analyzer.predict_probas(row["comment_original3"])["NEG"]+analyzer.predict_probas(row["comment_original4"])["NEG"]+analyzer.predict_probas(row["comment_original5"])["NEG"]+analyzer.predict_probas(row["comment_original6"])["NEG"]+analyzer.predict_probas(row["comment_original7"])["NEG"]+analyzer.predict_probas(row["comment_original8"])["NEG"])/8, axis=1)
df_comments_collapsed_long2_8["POSITIVE"]=df_comments_collapsed_long2_8.apply(lambda row: (analyzer.predict_probas(row["comment_original1"])["POS"]+analyzer.predict_probas(row["comment_original2"])["POS"]+analyzer.predict_probas(row["comment_original3"])["POS"]+analyzer.predict_probas(row["comment_original4"])["POS"]+analyzer.predict_probas(row["comment_original5"])["POS"]+analyzer.predict_probas(row["comment_original6"])["POS"]+analyzer.predict_probas(row["comment_original7"])["POS"]+analyzer.predict_probas(row["comment_original8"])["POS"])/8, axis=1)

df_comments_collapsed_long2x=df_comments_collapsed_long2.loc[df_comments_collapsed_long2["len_str"]<5]
df_comments_collapsed_long2_full=df_comments_collapsed_long2x.append(df_comments_collapsed_long2_5, sort=False).append(df_comments_collapsed_long2_6, sort=False).append(df_comments_collapsed_long2_7, sort=False).append(df_comments_collapsed_long2_8, sort=False)
df_comments_collapsed_long2_full.shape

df_comments_collapsed_long2_full=df_comments_collapsed_long2_full.drop(["comment_original1","comment_original2","comment_original3","comment_original4","comment_original5","comment_original6","comment_original7","comment_original8","len_str"], axis=1)
df_comments_collapsed_long2_full.shape
df_comments_collapsed_long2_full["NEUTRAL"]=1-(df_comments_collapsed_long2_full.POSITIVE+df_comments_collapsed_long2_full.NEGATIVE)


df_comments_collapsed_long2_full.to_csv(modified+'/Comments collapsed long sentiments.csv')
