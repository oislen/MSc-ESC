# -*- coding: utf-8 -*-
"""
Created on Fri Dec 18 15:10:43 2020

@author: oislen
"""


import cons 
import pandas as pd


data = pd.read_feather(cons.raw_feather_fpath)

data.head()

