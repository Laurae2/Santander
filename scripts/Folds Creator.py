# -*- coding: utf-8 -*-
"""
Created on Fri Apr 29 22:18:50 2016

@author: Laurae
"""

from sklearn import cross_validation
import os
import pandas as pd
import numpy as np

#%%

os.chdir("C:/Users/Laurae/Documents/Data Science/Santander/")
df_train = pd.read_csv('train.csv')
target = df_train["TARGET"]

#%%

folds1 = list(cross_validation.StratifiedKFold(target, 5 ,shuffle=True,random_state=11111))
folds2 = list(cross_validation.StratifiedKFold(target, 5 ,shuffle=True,random_state=2222))
folds3 = list(cross_validation.StratifiedKFold(target, 5 ,shuffle=True,random_state=5555))
folds4 = list(cross_validation.StratifiedKFold(target, 5 ,shuffle=True,random_state=7777))

#%%

np.savetxt('Fold1.1.csv', folds1[0][0], delimiter = ',', header = 'Rep1.Fold1')
np.savetxt('Fold1.2.csv', folds1[1][0], delimiter = ',', header = 'Rep1.Fold2')
np.savetxt('Fold1.3.csv', folds1[2][0], delimiter = ',', header = 'Rep1.Fold3')
np.savetxt('Fold1.4.csv', folds1[3][0], delimiter = ',', header = 'Rep1.Fold4')
np.savetxt('Fold1.5.csv', folds1[4][0], delimiter = ',', header = 'Rep1.Fold5')
np.savetxt('Fold2.1.csv', folds2[0][0], delimiter = ',', header = 'Rep2.Fold1')
np.savetxt('Fold2.2.csv', folds2[1][0], delimiter = ',', header = 'Rep2.Fold2')
np.savetxt('Fold2.3.csv', folds2[2][0], delimiter = ',', header = 'Rep2.Fold3')
np.savetxt('Fold2.4.csv', folds2[3][0], delimiter = ',', header = 'Rep2.Fold4')
np.savetxt('Fold2.5.csv', folds2[4][0], delimiter = ',', header = 'Rep2.Fold5')
np.savetxt('Fold3.1.csv', folds3[0][0], delimiter = ',', header = 'Rep3.Fold1')
np.savetxt('Fold3.2.csv', folds3[1][0], delimiter = ',', header = 'Rep3.Fold2')
np.savetxt('Fold3.3.csv', folds3[2][0], delimiter = ',', header = 'Rep3.Fold3')
np.savetxt('Fold3.4.csv', folds3[3][0], delimiter = ',', header = 'Rep3.Fold4')
np.savetxt('Fold3.5.csv', folds3[4][0], delimiter = ',', header = 'Rep3.Fold5')
np.savetxt('Fold4.1.csv', folds4[0][0], delimiter = ',', header = 'Rep4.Fold1')
np.savetxt('Fold4.2.csv', folds4[1][0], delimiter = ',', header = 'Rep4.Fold2')
np.savetxt('Fold4.3.csv', folds4[2][0], delimiter = ',', header = 'Rep4.Fold3')
np.savetxt('Fold4.4.csv', folds4[3][0], delimiter = ',', header = 'Rep4.Fold4')
np.savetxt('Fold4.5.csv', folds4[4][0], delimiter = ',', header = 'Rep4.Fold5')

