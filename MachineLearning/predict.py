from sklearn import svm
import pandas as ps
import numpy as np
import pickle as pkl
#from sklearn.externals import joblib
import sys

#clf = joblib.load('SVM.pkl')
f = open('SVM.spkl', 'r')
clf = pkl.loads(f.read())

filename = "../clientData/" + sys.argv[1] + ".txt"
H = ['time','food','dFood','wood','dWood','stone','dStone','metal','dMetal','inf','dInf','wrkr\
','dWrkr','fmales','dFmales','cvlry','dCvlry','chmp','dChmp','hero','dHero','ships','dShips','\
house','dHouse','econ','dEcon','outpst','dOutpst','mltry','dMltry','fortr','dFortr','civCnt','\
dCivCnt','wndr','dWndr','enK','dEnK','enBldD','dEnBldD','unitsL','dUnitsL','bldL','dBldL','Lab\
el']
x = ps.read_csv(filename, delim_whitespace=True, names=H)

# Drop hand label
x = x.drop("Label", 1)

# Combination feature(s)
x["army"] = x['inf'] + x['cvlry'] + (5 * x['chmp']) + (10 * x['hero'])

# Reorder alphabetically
x = x[['army','bldL','chmp','civCnt','cvlry','dBldL','dChmp','dCivCnt','dCvlry','dEcon','dEnBl\
dD','dEnK','dFmales','dFood','dFortr','dHero','dHouse','dInf','dMetal','dMltry','dOutpst','dSh\
ips','dStone','dUnitsL','dWndr','dWood','dWrkr','econ','enBldD','enK','fmales','food','fortr',\
'hero','house','inf','metal','mltry','outpst','ships','stone','time','unitsL','wndr','wood','w\
rkr']]

pred = clf.predict(x.values)
print("Machine learning prediction is " + pred[0])
