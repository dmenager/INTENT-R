# predict.py
import sys
from os import path
import pandas as ps
from StringIO import StringIO
from utils import load

svm_file = path.expanduser('~/INTENT-R/MachineLearning/SVM.spkl')
header_file = path.expanduser('~/INTENT-R/MachineLearning/sample.txt')

str = sys.argv[1]

headers = ps.read_csv(header_file,  delim_whitespace = True)
x = ps.read_csv(StringIO(str),  names = headers.columns,  
header = None,  delim_whitespace = True)

F = ["SupportGained", "SupportLost",  "SupportKilled", "StructuresGained", "StructuresLost",  
"StructuresDestroyed", "SiegeGained", "SiegeLost", "SiegeKilled",  "time"]

x = x[F]

Labels = ["first",  "second",  "third",  "fourth"]
clf = load(svm_file)
pred = clf.predict(x.values) 

print "Machine learning prediction: You will get " + Labels[pred[0]] + " place."
