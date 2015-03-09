from sklearn import svm
import pandas as ps
import numpy as np
from sklearn.externals import joblib
import sys

clf = svm.SVC()
clf = joblib.load('SVM.pkl')

filename = sys.argv[1] + ".txt"
x = ps.read_csv(filename, delim_whitespace=True)

pred = clf.predict(x.values)
print("Machine learning prediction is " + pred[0])
