# MachineLearning.py
from utils import combine,  save,  load,  shiftLabels,  divide,  randomize
from machine_learning import cluster,  hist,  pca,  train,  accuracy

print "Combining data files..."
X,  fileLengthes = combine()
save(X,  'data.spkl')
save(fileLengthes,  'fileLengthes.spkl')
print "Done."

#X = load('data.spkl')
#fileLengthes = load('fileLengthes.spkl')

handLabels = X["Label"]
X = X.drop("Label" , 1)
X['army'] = X['inf'] +  (X['cvlry'] * 2) +  (X['chmp'] * 5) + (X['hero'] * 10)
F = ['fmales',  'army' ,  'house',  'econ']

print "Clustering..."
Y = cluster(X,  F,  k = 3)
save(Y,  'clusters.spkl')
print "Done."

#Y = load('clusters.spkl')

## Visualizing
#hist(X,  Y,  F)
#pca(X,  Y,  F)

print "Shifting and randomizing..."
shift = 10
X,  Y = shiftLabels (X,  Y,  shift,  fileLengthes)
X,  Y = randomize (X,  Y)
save(X,  'X.spkl')
save(Y,  'Y.spkl')
print 'Done.'

#X = load('X.spkl')
#Y = load('Y.spkl')

trainX,  testX = divide(X,  0)
trainY,  testY = divide(Y,  0)

print "Training classifier..."
clf = train(trainX,  trainY)
save(clf,  'predictSVM.spkl')
print "Done."

#clf = load('predictSVM.spkl')

#print "Calculating accuracy..."
#acc = accuracy(clf,  testX,  testY) # = 63%
#print "Accuracy: ",  acc
