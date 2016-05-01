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
rnd=11111
random.seed(rnd)
#n_ft=20 #Number of features to add
#max_elts=3 #Maximum size of a group of linear features

#%%

# SET 1

setused=1
nET = 500
nRF = 300
train = pd.read_csv('train_set1.csv')
test = pd.read_csv('test_set1.csv')
target = pd.read_csv('train_target.csv', header=None).values.ravel()

#%%

# Level 2 Score: 

clf = ensemble.ExtraTreesClassifier(n_estimators=nET, max_features=16, criterion='entropy', max_depth=12, n_jobs=-1, random_state=11111, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "ETC", setused=setused)

#%%

# Level 2 Score: 

clf = ensemble.ExtraTreesRegressor(n_estimators=nET, max_depth=40, max_features=30, random_state=11111, verbose=1000, n_jobs=-1)     

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="regressor", filename = "ETR", setused=setused)

#%%

# Level 2 Score: 

clf = ensemble.RandomForestClassifier(n_estimators=nRF, max_features=16, criterion='entropy', max_depth=12, n_jobs=-1, random_state=11111, verbose=0)
                        
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "RFC", setused=setused)

#%%

# Level 2 Score: 

clf = ensemble.RandomForestRegressor(n_estimators=nRF, max_depth=26, max_features=26, random_state=11111, verbose=1000, n_jobs=-1)     

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="regressor", filename = "RFR", setused=setused)

#%%

# Level 2 Score: 

clf = ensemble.AdaBoostClassifier(random_state=11111, learning_rate=0.4, loss='linear')     

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "AdaClass", setused=setused)

#%%

# Level 2 Score: 

clf = ensemble.AdaBoostRegressor(random_state=11111, learning_rate=0.4, loss='linear')     

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="regressor", filename = "AdaReg", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.LinearRegression()

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, ategory="regressor", filename = "LinReg", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.RidgeCV(cv = 5)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="regressor", filename = "RidgeCV", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.PassiveAggressiveClassifier(n_iter=100, random_state=11111, verbose=0, n_jobs=-1)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "PasAggC1", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.PassiveAggressiveClassifier(n_iter=100, loss='squared_hinge', random_state=11111, verbose=0, n_jobs=-1)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "PasAggC2", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.PassiveAggressiveRegressor(n_iter=100, random_state=11111, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="regressor", filename = "PasAggR1", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.PassiveAggressiveRegressor(n_iter=100, loss='squared_epsilon_insensitive', random_state=11111, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="regressor", filename = "PasAggR2", setused=setused)

#%%

# Level 2 Score: 

clf = discriminant_analysis.LinearDiscriminantAnalysis()

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "LDA", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.LarsCV(cv=5, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="regressor", filename = "LeastAngle", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.ElasticNetCV(cv=5, verbose=0)

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5,seed=11111, category="regressor", filename = "ElasticNet", setused=setused)

#%%

# Level 2 Score: 

clf = linear_model.BayesianRidge()

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="regressor", filename = "BayesianRidge", setused=setused)

#%%

# Level 2 Score: 

clf = neighbors.NearestCentroid()

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="regressor", filename = "NearCentroid", setused=setused)

#%%

# Level 2 Score: 

clf = naive_bayes.GaussianNB()

model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "GaussianNB", setused=setused)

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
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "knn2", setused=setused)
clf = neighbors.KNeighborsClassifier(n_neighbors=4, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "knn4", setused=setused)
clf = neighbors.KNeighborsClassifier(n_neighbors=8, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "knn8", setused=setused)
clf = neighbors.KNeighborsClassifier(n_neighbors=16, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "knn16", setused=setused)
clf = neighbors.KNeighborsClassifier(n_neighbors=32, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "knn32", setused=setused)
clf = neighbors.KNeighborsClassifier(n_neighbors=64, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "knn64", setused=setused)
clf = neighbors.KNeighborsClassifier(n_neighbors=128, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "knn128", setused=setused)
clf = neighbors.KNeighborsClassifier(n_neighbors=256, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "knn256", setused=setused)
clf = neighbors.KNeighborsClassifier(n_neighbors=512, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "knn512", setused=setused)
clf = neighbors.KNeighborsClassifier(n_neighbors=1024, n_jobs=-1)
model_sum = blend_proba(clf=clf, X_train=train, y=target, X_test=test, nfolds=5, seed=11111, category="classifier", filename = "knn1024", setused=setused)
