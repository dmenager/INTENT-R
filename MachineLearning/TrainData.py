import pandas as ps
import numpy as np
import random as rd
import math
import os.path
from sklearn import cluster
from sklearn import svm
from sklearn.externals import joblib as jl
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt

file_counter = 0
dir_name = "/home/kaleb/Documents/0adtestdata"

#length of each file
length = []
#first template file dir
file_dir = "sample.csv"
#excess offset top
m = 20
#excess offset bottom
n = 0
#shift offset
offset = 10
#training data
X = ps.read_csv(file_dir, delim_whitespace=True)
Z = X
Z = Z.drop("Label", 1)
#hand labels

#collect data into one object
for filename in os.listdir(dir_name):
	x = ps.read_csv(os.path.join(dir_name, filename), delim_whitespace=True)
	X = ps.concat([X, x])
	length.append(len(x.values))
	file_counter = file_counter + 1

#X = ps.read_csv("data.csv", delim_whitespace=True)

# Combination feature(s)
X['army'] = X['inf'] +  X['cvlry'] +  (X['chmp'] * 5) + (X['hero'] * 10)

#Clustering
# X - pandas DataFrame to be clustered
# F - array of strings corresponding to desired features
# k - number of clusters to use in clustering model (when appropriate)
#
# Returns - an array of cluster labels (Y)
def train(X,  F, k=4):    
    #
    # Uncomment methods below to call appropriate clustering algorithm
    #
    centers = np.array([[  15.94247788,   35.3818145,     7.61274742,    4.12852125],
 [  34.19272883,  150.94362703,   18.04431138,   10.02369547],
 [  30.30757769,  380.48999574,   21.33205619,   12.47594721]])
    # http://scikit-learn.org/stable/modules/generated/sklearn.cluster.KMeans.html#sklearn.cluster.KMeans
    clusterModel = cluster.KMeans(n_clusters = k, n_jobs = 1, init = centers)
    
    #
    #   Use F to determine the features to be used in clustering.
    #
    if F != []:
        X = X[F]
    clusterModel.fit(X.values)

    print clusterModel.cluster_centers_
    
    return clusterModel.predict(X.values)

# Colors for plotting
colors = ['b',  'r',  'g',  'c', 'm',  'y',  'k',  'w']

# Generates histograms for given features
#
# X - Pandas data set
# Y - labels from clustering
# F - the features to generated histograms for (an array of strings)
def hist(X,  Y,  F):
    i_figure = 1
    
    # Generate a histogram for the given feature
    for f in F:
        # Discover number of clusters from label vector Y
        k = np.unique(Y).size
        
        # Array containing just that feature
        feat_array = X[f]
        
        clusters = np.ndarray(shape = (k,  0)).tolist()
        
        # Divide up (based on clustering label) into subsets 
        for i in range(0,  k):
            clusters[i] = feat_array[Y == i ].values.tolist()
        
        # Create seperate figures
        plt.figure(i_figure)
        i_figure += 1
    
        # Automatically sets up histogram with bar-stacking style
        plt.hist(clusters,  histtype = 'barstacked',  color = colors[0:k])

        plt.xlabel(f)
        plt.ylabel('Occurences of ranges')
        plt.title('Histogram of %s' % f)
    
    # Show all the figures at once
    plt.show()

# Generates a 2D plot of the given features using PCA
# 
# X - Pandas data set
# Y - labels from clustering
# F - the features include in PCA plot (an array of strings)
def pca(X,  Y,  F):
    # Subset of X that has been selected
    X_selected = X[F]
    
    # Reduce X to 2 dimensions using principle component analysis
    X_2D = PCA(n_components=2).fit_transform(X_selected.values)

    # Convert Numpy back to Pandas
    X_2D = ps.DataFrame(X_2D)
    
    # Discover number of clusters from label vector Y
    k = np.unique(Y).size

    # Plot each set in a different color
    for i in range(0,  k):
        # Pandas data set of cluster k
        s = X_2D[Y == i]
        
        # Plot the x and y coordinates of s using the appropriate color
        plt.plot(s[0],  s[1],  "o" + colors[i],  markersize=4)

    plt.title("PCA plot of %s" % F)
    plt.show()

# Useful features groups
all = ['food', 'dFood', 'wood', 'dWood','stone', 'dStone', 'metal', 'dMetal', 'inf', 'dInf', 'wrkr', 'dWrkr', 'fmales', 'dFmales', 'cvlry', 'dCvlry', 'chmp', 'dChmp', 'hero', 'dHero', 'ships', 'dShips', 'house', 'dHouse', 'econ', 'dEcon', 'outpst', 'dOutpst', 'mltry', 'dMltry', 'fortr', 'dFortr', 'civCnt', 'dCivCnt', 'wndr', 'dWndr', 'enK', 'dEnK', 'enBldD', 'dEnBldD', 'unitsL', 'dUnitsL', 'bldL', 'dBldL']
values = ['food', 'wood', 'stone',  'metal', 'inf', 'wrkr', 'fmales', 'cvlry', 'chmp', 'hero', 'ships', 'house','econ', 'outpst', 'mltry', 'fortr', 'civCnt', 'wndr', 'enK', 'enBldD', 'unitsL', 'bldL']
dValues = ['dFood', 'dWood', 'dStone', 'dMetal', 'dInf', 'dWrkr', 'dFmales', 'dCvlry', 'dChmp', 'dHero', 'dShips', 'dHouse', 'dEcon', 'dOutpst', 'dMltry', 'dFortr','dCivCnt', 'dWndr', 'dEnK', 'dEnBldD','dUnitsL', 'dBldL']
military = ['inf', 'cvlry', 'chmp', 'hero', 'ships', 'mltry']
dMilitary = ['dInf', 'dCvlry', 'dChmp', 'dHero', 'dShips',  'dMltry']
resources = ['food', 'wood', 'stone',  'metal']
dResources = ['dFood', 'dWood', 'dStone', 'dMetal']

F = ['fmales',  'army' ,  'house',  'econ']

# Fit clustering model on train_X; predict on test_X; return Y, an array of cluster labels
Y = train(X,  F,  k = 3)
#pca(X,  Y,  F)
#F += ['time',  'Label']
#hist(X,  Y,  F)


#1 = Attack
#2 = Defend
#0 = build

handLabels = ['build', 'attack', 'defend']
hand = []
for y in Y:
    hand.append(handLabels[y])

Y = np.array(hand)

X["Y"] = Y

Y = np.array([])

ind = np.array(list(xrange(len(X))))
X["index"] = ind
X = X.set_index("index")

currIndex = 0
for l in length:
    z = X.ix[currIndex:currIndex + l - 1]
    y = z["Y"]
    z = z.drop("Y", 1)
    z = z.ix[currIndex:(currIndex + len(z)) - offset - 1]
    y = y.ix[(currIndex + offset):currIndex + len(y)]
    z["Y"] = y.values
    Z = ps.concat([Z, z])
    currIndex += l

Z = Z.drop("Label" , 1)
row = Z.index
rows = list(row)
rd.shuffle(rows)
Z = Z.reindex(rows)

div = math.trunc(.3 * len(Z))

train = Z.ix[div:]
trainY = train["Y"]
train = train.drop("Y", 1)

test = Z.ix[:div]
compY = test["Y"]
test = test.drop("Y", 1)


clf = svm.SVC()
#clf.fit(train.values, trainY.values)
clf = jl.load('SVM.pkl')

Y = clf.predict(test.values)
check = 0
print len(Y), len(test)
for i in range (0, len(Y)):
    if Y[i] == compY.values[i]:
       check += 1
    print check, i

print ((check / len(Y)) * 100)

print str("Finished svm")
#jl.dump(clf, 'SVM.pkl')
