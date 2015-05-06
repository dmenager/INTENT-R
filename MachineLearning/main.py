import utils
import machine_learning
import numpy as np

# Useable labels
All = ["time", "CurrentFood", "FoodUsed", "FoodSold", "FoodBought", "FoodGathered",
"CurrentWood",  "WoodUsed", "WoodSold", "WoodBought", "WoodGathered", 
"CurrentMetal", "MetalUsed", "MetalSold", "MetalBought", "MetalGathered", 
"CurrentStone", "StoneUsed", "StoneSold", "StoneBought", "StoneGathered", 
"InfantryGained", "InfantryLost", "InfantryKilled", 
"CavalryGained", "CavalryLost", "CavalryKilled", 
"SupportGained", "SupportLost", "SupportKilled", 
"SiegeGained", "SiegeLost", "SiegeKilled", 
"StructuresGained", "StructuresLost", "StructuresDestroyed", 
"dCurrentFood", "dFoodUsed", "dFoodSold", "dFoodBought", "dFoodGathered",
"dCurrentWood", "dWoodUsed", "dWoodSold", "dWoodBought", "dWoodGathered", 
"dCurrentMetal", "dMetalUsed", "dMetalSold", "dMetalBought", "dMetalGathered", 
"dCurrentStone", "dStoneUsed", "dStoneSold", "dStoneBought", "dStoneGathered", 
"dInfantryGained", "dInfantryLost", "dInfantryKilled", 
"dCavalryGained", "dCavalryLost", "dCavalryKilled", 
"dSupportGained", "dSupportLost", "dSupportKilled", 
"dSiegeGained", "dSiegeLost", "dSiegeKilled", 
"dStructuresGained", "dStructuresLost", "dStructuresDestroyed", 
"MilitaryMovementsOccurred",  "dMilitaryMovementsOccurred",
"SupportMovementsOccurred", "dSupportMovementsOccurred",
"DistanceEnemyBase", "dDistanceEnemyBase"]

# Delta labels
dAll = ["dCurrentFood", "dFoodUsed", "dFoodSold", "dFoodBought", "dFoodGathered",
"dCurrentWood", "dWoodUsed", "dWoodSold", "dWoodBought", "dWoodGathered", 
"dCurrentMetal", "dMetalUsed", "dMetalSold", "dMetalBought", "dMetalGathered", 
"dCurrentStone", "dStoneUsed", "dStoneSold", "dStoneBought", "dStoneGathered", 
"dInfantryGained", "dInfantryLost", "dInfantryKilled", 
"dCavalryGained", "dCavalryLost", "dCavalryKilled", 
"dSupportGained", "dSupportLost", "dSupportKilled", 
"dSiegeGained", "dSiegeLost", "dSiegeKilled", 
"dStructuresGained", "dStructuresLost", "dStructuresDestroyed", 
"MilitaryMovementsOccurred",  "SupportMovementsOccurred", "dDistanceEnemyBase"]

print "Gathering and initializing data..."
utils.combine()
X = utils.getData()
X[All] = machine_learning.norm(X[All])
print "Done."

print "Hierarchial clustering..."
hierarchy = machine_learning.recursiveCluster(X[dAll], size = 500)
Y = machine_learning.flatten(hierarchy, min = 40)
X = X[Y >= 0] # Eliminating outliers
Y = Y[Y >= 0]
y_values = np.unique(Y)
for i in range(0, len(y_values)):
    Y[Y==y_values[i]] = i
print "Done."

print "Visualizing..."
machine_learning.pca(X[dAll], Y)
machine_learning.hist(X[["time"]], Y)
print "Done."

print "Shifting and randomizing..."
shift = 10
X,  Y = utils.shiftLabels (X,  Y,  shift)
X,  Y = utils.randomize (X,  Y)
print 'Done.'

print "Choosing best parameters for classifier..."
clf, clf_acc, test_acc = machine_learning.best_classifier(X[All], Y)
print "Done."
print "Classifier accuracy: ", clf_acc
print "Test data accuracy: ", test_acc
print "Classifier model: ", clf

utils.save(clf,  'SVM.spkl')
print "Classifier saved."
