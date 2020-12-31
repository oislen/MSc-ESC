# -*- coding: utf-8 -*-
"""
Created on Thu Dec 17 19:25:49 2020

@author: oislen
"""

# import required libraries
from os import path
import pandas as pd

# set pandas behaviour
pd.set_option('display.max_rows', 20, 'display.max_columns', 10)

# set root directory
root_dir = 'C:\\Users\\User\\Documents\\GitHub\\ESC'

# set sub directories
scripts_sub_dir = path.join(root_dir, 'scripts')
config_sub_dir = path.join(root_dir, 'config')
data_sub_dir = path.join(root_dir, 'data')

# set data files
raw_xlsx_fpath = path.join(data_sub_dir, 'eurovision_song_contest_1975_2019v5.xlsx')
raw_feather_fpath = path.join(data_sub_dir, 'raw.feather')