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

x = x[All]

clf = load(svm_file)
pred = clf.predict(x.values) 

print "Machine learning prediction: ", pred[0]
