# -*- coding: utf-8 -*-
"""
Created on Mon Apr 18 22:27:53 2016

@author: Laurae
"""

import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

#from subprocess import check_output
#print(check_output(["ls", "../input"]).decode("utf8"))

# Any results you write to the current directory are saved as output.

from sklearn import cross_validation, ensemble, linear_model, neighbors, naive_bayes, discriminant_analysis
import random
import os

# for blending

import time

#%%

#Some parameters to play with
os.chdir("C:/Users/Laurae/Documents/Data Science/Santander/")

#%%

# SET 1

setused = 1
nET = 500
nRF = 300
rnd = 11111
random.seed(rnd)
train = pd.read_csv('train_set1.csv')
test = pd.read_csv('test_set1.csv')
target = pd.read_csv('train_target.csv', header=None).values.ravel()

#%%

# Level 2 Score: 

clf = ensemble.ExtraTreesClassifier(n_estimators=nET, max_features=10, max_depth=8, criterion='entropy', n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "ETC", setused=setused, tag = '10_8')

#%%

# Level 2 Score: 

clf = ensemble.ExtraTreesClassifier(n_estimators=nET, max_features=20, max_depth=15, criterion='entropy', n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "ETC", setused=setused, tag = '20_15')

#%%

# Level 2 Score: 

clf = ensemble.ExtraTreesClassifier(n_estimators=nET, max_features=30, max_depth=23, criterion='entropy', n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "ETC", setused=setused, tag = '30_23')

#%%

# Level 2 Score: 

clf = ensemble.ExtraTreesClassifier(n_estimators=nET, max_features=40, max_depth=30, criterion='entropy', n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "ETC", setused=setused, tag = '40_30')

#%%

# Level 2 Score: 

clf = ensemble.ExtraTreesClassifier(n_estimators=nET, max_features=50, max_depth=37, criterion='entropy', n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "ETC", setused=setused, tag = '50_37')

#%%

# Level 2 Score: 

clf = ensemble.ExtraTreesClassifier(n_estimators=nET, max_features=60, max_depth=45, criterion='entropy', n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "ETC", setused=setused, tag = '60_45')

#%%

# Level 2 Score: 

clf = ensemble.ExtraTreesRegressor(n_estimators=nET, max_features=10, max_depth=8, n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "ETR", setused=setused, tag = '10_8')

#%%

# Level 2 Score: 

clf = ensemble.ExtraTreesRegressor(n_estimators=nET, max_features=20, max_depth=15, n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "ETR", setused=setused, tag = '20_15')

#%%

# Level 2 Score: 

clf = ensemble.ExtraTreesRegressor(n_estimators=nET, max_features=30, max_depth=23, n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "ETR", setused=setused, tag = '30_23')

#%%

# Level 2 Score: 

clf = ensemble.RandomForestClassifier(n_estimators=nET, max_features=10, max_depth=8, criterion='entropy', n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "RFC", setused=setused, tag = '10_8')

#%%

# Level 2 Score: 

clf = ensemble.RandomForestClassifier(n_estimators=nET, max_features=20, max_depth=15, criterion='entropy', n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "RFC", setused=setused, tag = '20_15')

#%%

# Level 2 Score: 

clf = ensemble.RandomForestClassifier(n_estimators=nET, max_features=30, max_depth=23, criterion='entropy', n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "RFC", setused=setused, tag = '30_23')

#%%

# Level 2 Score: 

clf = ensemble.RandomForestClassifier(n_estimators=nET, max_features=40, max_depth=30, criterion='entropy', n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "RFC", setused=setused, tag = '40_30')

#%%

# Level 2 Score: 

clf = ensemble.RandomForestClassifier(n_estimators=nET, max_features=50, max_depth=37, criterion='entropy', n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "RFC", setused=setused, tag = '50_37')

#%%

# Level 2 Score: 

clf = ensemble.RandomForestClassifier(n_estimators=nET, max_features=60, max_depth=45, criterion='entropy', n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "RFC", setused=setused, tag = '60_45')

#%%

# Level 2 Score: 

clf = ensemble.RandomForestRegressor(n_estimators=nET, max_features=10, max_depth=8, n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "RFR", setused=setused, tag = '10_8')

#%%

# Level 2 Score: 

clf = ensemble.RandomForestRegressor(n_estimators=nET, max_features=20, max_depth=15, n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "RFR", setused=setused, tag = '20_15')

#%%

# Level 2 Score: 

clf = ensemble.RandomForestRegressor(n_estimators=nET, max_features=30, max_depth=23, n_jobs=-1, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "RFR", setused=setused, tag = '30_23')

#%%

# Level 2 Score: 

clf = ensemble.AdaBoostClassifier(random_state=rnd, learning_rate=0.4, loss='linear')     

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "AdaClass", setused=setused)

#%%

# Level 2 Score: 

clf = ensemble.AdaBoostRegressor(random_state=rnd, learning_rate=0.4, loss='linear')     

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="regressor", filename = "AdaReg", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.LinearRegression()

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="regressor", filename = "LinReg", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.LogisticRegression(solver='sag', random_state=rnd, verbose=0, n_jobs=-1)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "LogReg", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.RidgeCV(cv = 5)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="regressor", filename = "RidgeCV", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.PassiveAggressiveClassifier(n_iter=100, random_state=rnd, verbose=0, n_jobs=-1)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "PasAggC", setused=setused, tag = "1")

#%%

# Level 2 Score: 

clf = linear_model.PassiveAggressiveClassifier(n_iter=100, loss='squared_hinge', random_state=rnd, verbose=0, n_jobs=-1)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "PasAggC", setused=setused, tag = "2")

#%%

# Level 2 Score: 

clf = linear_model.PassiveAggressiveRegressor(n_iter=100, random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="regressor", filename = "PasAggR", setused=setused, tag = "1")

#%%

# Level 2 Score: 

clf = linear_model.PassiveAggressiveRegressor(n_iter=100, loss='squared_epsilon_insensitive', random_state=rnd, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="regressor", filename = "PasAggR", setused=setused, tag = "2")

#%%

# Level 2 Score: 

clf = discriminant_analysis.LinearDiscriminantAnalysis()

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "LDA", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.LarsCV(cv=5, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="regressor", filename = "LeastAngle", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.ElasticNetCV(cv=5, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5,seed=rnd, category="regressor", filename = "ElasticNet", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.BayesianRidge()

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="regressor", filename = "BayesianRidge", setused=setused)

#%%

# Level 2 Score: 

clf = neighbors.NearestCentroid()

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="regressor", filename = "NearCentroid", setused=setused)

#%%

# Level 2 Score: 

clf = naive_bayes.GaussianNB()

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "GaussianNB", setused=setused)

#%%

# Level 2 Score, k=   2: 
# Level 2 Score, k=   4: 
# Level 2 Score, k=   8: 
# Level 2 Score, k=  16: 
# Level 2 Score, k=  32: 
# Level 2 Score, k=  64: 
# Level 2 Score, k= 128: 
# Level 2 Score, k= 256: 
# Level 2 Score, k= 512:
# Level 2 Score, k=1024:  

clf = neighbors.KNeighborsClassifier(n_neighbors=2, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "knn", setused=setused, tag = "2")
clf = neighbors.KNeighborsClassifier(n_neighbors=4, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "knn", setused=setused, tag = "4")
clf = neighbors.KNeighborsClassifier(n_neighbors=8, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "knn", setused=setused, tag = "8")
clf = neighbors.KNeighborsClassifier(n_neighbors=16, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "knn", setused=setused, tag = "16")
clf = neighbors.KNeighborsClassifier(n_neighbors=32, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "knn", setused=setused, tag = "32")
clf = neighbors.KNeighborsClassifier(n_neighbors=64, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "knn", setused=setused, tag = "64")
clf = neighbors.KNeighborsClassifier(n_neighbors=128, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "knn", setused=setused, tag = "128")
clf = neighbors.KNeighborsClassifier(n_neighbors=256, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "knn", setused=setused, tag = "256")
clf = neighbors.KNeighborsClassifier(n_neighbors=512, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "knn", setused=setused, tag = "512")
clf = neighbors.KNeighborsClassifier(n_neighbors=1024, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=rnd, category="classifier", filename = "knn", setused=setused, tag = "1024")
