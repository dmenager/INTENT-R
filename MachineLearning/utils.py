# utils.py
import os#.path
import numpy as np
import pandas as ps
import random as rd
import pickle as pkl

def combine(dir_name = "../0adtestdata"):
    #collect data into one object
    for i,  filename in enumerate(os.listdir(dir_name)):
        x = ps.read_csv(os.path.join(dir_name, filename), delim_whitespace=True)

        # Cut beginning states
        x = x[4:]
        
        x["player"] = int(filename[0])
        x["file"] = int(filename[1:-4])
        
        if i == 0:
            X = x
        else:
            X = ps.concat([X, x],  ignore_index = True)
        
    X.to_csv("all_data.csv")

def getData():
    return ps.read_csv("all_data.csv")

def calcFeatures(X):
    X["place"] = -1
    files = separate(X,  X.file.values)
    
    for i in range(0,  len(files)):
        players = separate(files[i],  files[i].player.values)
        
        maxTimes = []
        for p in players:
            maxTimes.append(max(p.time.values))
            
        max1 = maxTimes.index(max(maxTimes))
        maxTimes[max1] = -1
        game1 = players[max1]
        
        max2 = maxTimes.index(max(maxTimes))
        maxTimes[max2] = -1
        game2 = players[max2]
        
        lastState1 = game1.iloc[-1]
        lastState2 = game2.iloc[-1]        
        if(lastState1.CurrentFood > lastState2.CurrentFood):
            players[max1].place = 0
            players[max2].place = 1
        else:
            players[max2].place = 0
            players[max1].place = 1
            
        max3 = maxTimes.index(max(maxTimes))
        maxTimes[max3] = -1
        players[max3].place = 2
        
        max4 = maxTimes.index(max(maxTimes))
        players[max4].place = 3
        
        files[i] = ps.concat(players,  ignore_index = True)
        
    X = ps.concat(files,  ignore_index = True)
    
    return X

def separate(X,  Y):
    sets = []
    for i in np.unique(Y):
        sets.append(X[Y == i])
        
    return sets

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
    
def randomize(X,  Y):
    X['Y'] = Y
    indexes = list(X.index)
    rd.shuffle(indexes)
    X = X.reindex(indexes)
    
    Y = X.Y
    X = X.drop('Y',  1)
    return X,  Y

def shiftLabels(X,  Y, shift):
    shiftX = ps.DataFrame(columns = X.columns)
    shiftY = np.array([],  dtype = int)
    prev = 0
    for next in range(1, len(X)):
        if (next == len(X) - 1 # Reached the end of X
        or (X.player.iloc[next - 1] != X.player.iloc[next] # Reached a different player
        or X.file.iloc[next - 1] != X.file.iloc[next])): # Reached a different file   
            # Extract states and labels with appropriate shift
            x = X.iloc[prev:next - shift]
            y = Y[prev + shift:next]
            prev = next

            shiftX = ps.concat([shiftX,  x],  ignore_index=True)
            shiftY = np.concatenate([shiftY, y])
    
    return shiftX, shiftY
