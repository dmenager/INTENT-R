# utils.py
import os.path
import numpy as np
import pandas as ps
import random as rd
import math
import pickle as pkl

def combine(dir_name = "../0adtestdata"):
    fileLengthes = []

    #collect data into one object
    for i,  filename in enumerate(os.listdir(dir_name)):
        x = ps.read_csv(os.path.join(dir_name, filename), delim_whitespace=True)
        
        if i == 0:
            X = x
        else:
            X = ps.concat([X, x],  ignore_index = True)
        
        fileLengthes.append(len(x.values))
        
    return X,  fileLengthes
    
def save(object,  fileName):
    f = open(fileName,  'w')
    s = pkl.dumps(object)
    f.write(s)
    f.close()
    
def load(fileName):
    f = open(fileName,  'r')
    s = f.read()
    f.close()
    object = pkl.loads(s)
    return object
    
# X - pandas DataFrame
# Y - numpy array
def randomize(X,  Y):
    X['Y'] = Y
    indexes = list(X.index)
    rd.shuffle(indexes)
    X = X.reindex(indexes)
    
    Y = X.Y
    X = X.drop('Y',  1)
    return X,  Y

def shiftLabels(X,  Y, shift,  fileLengthes):
    for (i, l) in enumerate(fileLengthes):
        # Extract states and labels with appropriate shift
        x = X[:l - shift]
        y = Y[shift:l]
        
        # Remove file from X and Y
        X = X[l:]
        Y = Y[l:]
        
        if i == 0:
            shiftX = x
            shiftY = y
        else:
            shiftX = ps.concat([shiftX,  x],  ignore_index=True)
            shiftY = np.concatenate([shiftY, y])
    
    return shiftX, shiftY

# array - anything array-like
# percentTrain - float value between 0 and 1
def divide (array, percentTrain):
    dividingIndex = math.trunc(percentTrain * len(array))

    train = array[dividingIndex:]
    test = array[:dividingIndex]
    
    return train,  test
