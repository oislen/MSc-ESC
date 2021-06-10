# -*- coding: utf-8 -*-
"""
Created on Thu Dec 17 19:22:53 2020

@author: oislen
"""

# load in required libraries
import cons 
import pandas as pd
import seaborn as sns

# set column names
col_names = ['year', 'round', 'edition', 'voting', 'from_country', 'to_country', 'points', 'duplicated']

# data can be manually downloaded via kaggle or data.world (requires sign-in)
# https://www.kaggle.com/datagraver/eurovision-song-contest-scores-19752019
# https://data.world/datagraver/eurovision-song-contest-scores-1975-2019
# load in excel data
data = pd.read_excel(cons.raw_xlsx_fpath,
                     sheet_name = 'Data',
                     header = 0,
                     names = col_names,
                     engine = 'openpyxl'
                     )

# output data as feather file
data.to_feather(cons.raw_feather_fpath)
data.isnull().sum()

# investogate duplicated column
sub_cols = ['year', 'round', 'voting', 'from_country','to_country', 'points']
data.loc[:,sub_cols].duplicated().any()
dup_data = data[data['duplicated'] == 'x']
dup_data.astype(str).apply(lambda x: '_'.join(x), axis = 1).nunique() / dup_data.shape[0] * 100
# investigate unique values across all clumns bar duplicate
data.iloc[:,:-1].astype(str).apply(lambda x: '_'.join(x), axis = 1).nunique() / data.shape[0] * 100

# competitors each year
competitors = data.groupby(by = ['year']).agg({'from_country':'nunique'}).rename(columns = {'from_country':'n_countries'}).reset_index()
sns.lineplot(y = 'n_countries', x = 'year', data = competitors)

# rounds each year
n_rounds = data.groupby(by = ['year']).agg({'round':'nunique'}).rename(columns = {'round':'n_round'}).reset_index()
sns.lineplot(y = 'n_round', x = 'year', data = n_rounds)
sns.swarmplot(y = 'round', x = 'year', data = data)


# how many voting methods by year
n_voting = data.groupby(by = ['year']).agg({'voting':'nunique'}).rename(columns = {'voting':'n_voting'}).reset_index()
sns.lineplot(y = 'n_voting', x = 'year', data = n_voting)
