# -*- coding: utf-8 -*-
"""
Created on Sun Apr 17 12:08:38 2016

@author: Laurae
"""

from sklearn import cross_validation
from sklearn.metrics import mean_squared_error, accuracy_score #adapt this
import numpy as np
import pandas as pd
import random
#import md5
import json
import gc
import time

#%%

def fmean_squared_error(ground_truth, predictions):
    fmean_squared_error_ = mean_squared_error(ground_truth, predictions)**0.5 # adapt to metric
    return fmean_squared_error_

#%%

def blend_proba(clf, X_train, y, X_test, nfolds=5, seed=11111, category="classifier"):
  print("\nBlending with classifier:\n\t%s"%(clf))
  start_time = time.time()
  folds = list(cross_validation.StratifiedKFold(y, nfolds,shuffle=True,random_state=seed))
  print(X_train.shape, " ||| Time: ", round(((time.time() - start_time)/60),2))
  dataset_blend_train = np.zeros(X_train.shape[0])
  test_temp = np.zeros(X_test.shape[0])

  #iterate through train set and train - predict folds
  loss = 0
  for i, (train_index, test_index) in enumerate( folds ):
    print("\nTraining fold %s/%s"%(i+1,nfolds), " ||| Time: ", round(((time.time() - start_time)/60),2))
    fold_X_train = X_train.iloc[train_index]
    fold_y_train = y[train_index]
    fold_X_test = X_train.iloc[test_index]
    fold_y_test = y[test_index]
    clf.fit(fold_X_train, fold_y_train)
    
    print("Predicting testing fold", " ||| Time: ", round(((time.time() - start_time)/60),2))
    
    if category == "classifier":
        fold_preds = clf.predict_proba(fold_X_test)[:,1]
    else:
        fold_preds = clf.predict(fold_X_test)
    
    for i in range(len(fold_preds)):
        if fold_preds[i]<1.0: #adapt to metric
            fold_preds[i] = 1.0 #adapt to metric
        if fold_preds[i]>3.0: #adapt to metric
            fold_preds[i] = 3.0 #adapt to metric
    
    print("Logistic loss: %s"%fmean_squared_error(fold_y_test,fold_preds)) #adapt to metric
    
    dataset_blend_train[test_index] = fold_preds
      loss += fmean_squared_error(fold_y_test,fold_preds)

    print("Predicting test set", " ||| Time: ", round(((time.time() - start_time)/60),2))
    
    if category == "classifier":
        dataset_blend_test = clf.predict_proba(X_test)[:,1]
    else:
        dataset_blend_test = clf.predict(X_test)
    
    for i in range(len(dataset_blend_test)):
       if dataset_blend_test[i]<1.0: to metric
          dataset_blend_test[i] = 1.0 to metric
       if dataset_blend_test[i]>3.0: to metric
          dataset_blend_test[i] = 3.0 to metric
    test_temp = test_temp+dataset_blend_test
    gc.collect()
    
  avg_loss = fmean_squared_error(y, dataset_blend_train)
  print("\nAverage:\t%s\n"%avg_loss)
  
  dataset_blend_test = (test_temp / nfolds)
  
  return dataset_blend_train, dataset_blend_test
