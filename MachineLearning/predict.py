# predict.py
import sys
import pandas as ps
from StringIO import StringIO
from sklearn.externals import joblib as jl

fileName = 'predictSVM.pkl'

str = sys.argv[1]

headers = ps.read_csv('sample.csv',  delim_whitespace = True)
x = ps.read_csv(StringIO(str),  names = headers.columns,  
header = None,  delim_whitespace = True)

# Remove feature(s)
x = x.drop("Label", 1)

# Create feature(s)
x["army"] = x['inf'] + x['cvlry'] + (5 * x['chmp']) + (10 * x['hero'])

# Reorder alphabetically
x = x.reindex_axis(sorted(x.columns),  axis = 1)

Labels = ["defend",  "build",  "attack"]
clf = jl.load(fileName)    
pred = clf.predict(x.values) 

print "Machine learning prediction is " + Labels[pred[0]]
