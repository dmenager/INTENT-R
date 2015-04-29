# MachineLearning.py
import utils
import machine_learning

print "Combining data files..."
utils.combine()
print "Done."

X = utils.getData()

print "Calculating post-game features..."
X = utils.calcFeatures(X)
print "Done."

Y = X.place.values

# Restrict X to selected features
F = ["SupportGained", "SupportLost",  "SupportKilled",  
"StructuresGained", "StructuresLost",  "StructuresDestroyed", "SiegeGained", "SiegeLost", "SiegeKilled",  "time"]

print "Shifting and randomizing..."
shift = 10
X,  Y = utils.shiftLabels (X,  Y,  shift)
X,  Y = utils.randomize (X,  Y)
print 'Done.'

fractionTest = 0.3
trainX,  testX = utils.divide(X,  fractionTest)
trainY,  testY = utils.divide(Y,  fractionTest)

print "Calculating accuracy..."
clf_test_data = machine_learning.train(trainX,  trainY,  F)
timeBound = 60
acc = machine_learning.accuracy(clf_test_data,  testX,  testY, F,  timeBound)
print "Accuracy for first " + str(timeBound) + " mintues: ",  acc

print "Training classifier..."
clf_all_data = machine_learning.train(X,  Y,  F)
utils.save(clf_test_data,  'placeSVM.spkl')
print "Done."
