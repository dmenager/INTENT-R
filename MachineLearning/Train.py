import pandas as ps
import numpy as np
import math
from sklearn import cluster
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt

X = ps.read_csv("data.csv", delim_whitespace=True)

X = X.drop("index", 1)

count = len(X.index)

div = math.trunc(count * .3)

train_X = X[div:]
test_X = X[:div]
train_Z = train_X["Label"]
test_Z = test_X["Label"]
train_X = train_X.drop("Label", 1)
test_X = test_X.drop("Label", 1)

#Clustering
# X - pandas DataFrame to be clustered
# F - array of strings corresponding to desired features
# k - number of clusters to use in clustering model (when appropriate)
#
# Returns - an array of cluster labels (Y)
def train(train_X, test_X,  F, k=4):    
    #
    # Uncomment methods below to call appropriate clustering algorithm
    #
    
    # http://scikit-learn.org/stable/modules/generated/sklearn.cluster.KMeans.html#sklearn.cluster.KMeans
    clusterModel = cluster.KMeans(n_clusters = k)
    
    # http://scikit-learn.org/stable/modules/generated/sklearn.cluster.AffinityPropagation.html#sklearn.cluster.AffinityPropagation
    #clusterModel = cluster.AffinityPropagation()
    
    # http://scikit-learn.org/stable/modules/generated/sklearn.cluster.MeanShift.html#sklearn.cluster.MeanShift
    #clusterModel = cluster.MeanShift()
    
    # http://scikit-learn.org/stable/modules/generated/sklearn.cluster.AgglomerativeClustering.html#sklearn.cluster.AgglomerativeClustering
    #clusterModel = cluster.AgglomerativeClustering(n_clusters = k)
    
    # http://scikit-learn.org/stable/modules/generated/sklearn.cluster.SpectralClustering.html#sklearn.cluster.SpectralClustering
    #clusterModel = cluster.SpectralClustering(n_clusters = k)
    
    # http://scikit-learn.org/stable/modules/generated/sklearn.cluster.FeatureAgglomeration.html#sklearn.cluster.FeatureAgglomeration	
    #clusterModel = cluster.FeatureAgglomeration(n_clusters = k)
    
    # http://scikit-learn.org/stable/modules/generated/sklearn.cluster.DBSCAN.html#sklearn.cluster.DBSCAN
    #clusterModel = cluster.DBSCAN()
    
    #
    #   Use F to determine the features to be used in clustering.
    #
    train_X = train_X[F]
    test_X = test_X[F]
    clusterModel.fit(train_X.values)
    return clusterModel.predict(test_X.values)

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
all = ['food', 'dFood', 'wood', 'dWood', 'stone', 'dStone', 'metal', 'dMetal', 'inf', 'dInf', 'wrkr', 'dWrkr', 'fmales', 'dFmales', 'cvlry', 'dCvlry', 'chmp', 'dChmp', 'hero', 'dHero', 'ships', 'dShips', 'house', 'dHouse', 'econ', 'dEcon', 'outpst', 'dOutpst', 'mltry', 'dMltry', 'fortr', 'dFortr', 'civCnt', 'dCivCnt', 'wndr', 'dWndr', 'enK', 'dEnK', 'enBldD', 'dEnBldD', 'unitsL', 'dUnitsL', 'bldL', 'dBldL']
values = ['food', 'wood', 'stone',  'metal', 'inf', 'wrkr', 'fmales', 'cvlry', 'chmp', 'hero', 'ships', 'house','econ', 'outpst', 'mltry', 'fortr', 'civCnt', 'wndr', 'enK', 'enBldD', 'unitsL', 'bldL']
dValues = ['dFood', 'dWood', 'dStone', 'dMetal', 'dInf', 'dWrkr', 'dFmales', 'dCvlry', 'dChmp', 'dHero', 'dShips', 'dHouse', 'dEcon', 'dOutpst', 'dMltry', 'dFortr','dCivCnt', 'dWndr', 'dEnK', 'dEnBldD','dUnitsL', 'dBldL']
military = ['inf', 'cvlry', 'chmp', 'hero', 'ships', 'mltry']
dMilitary = ['dInf', 'dCvlry', 'dChmp', 'dHero', 'dShips',  'dMltry']
resources = ['food', 'wood', 'stone',  'metal']
dResources = ['dFood', 'dWood', 'dStone', 'dMetal']

# Fit clustering model on train_X; predict on test_X; return Y, an array of cluster labels
Y = train(train_X,  test_X,  all)

X = test_X

# PCA of everything
pca(X,  Y,  all)

# Histogram of the features that appear to have the most seperation
hist(X,  Y,  ['dFood', 'food', 'wood', 'stone', 'metal', 'inf', 'wrkr', 'fmales', 'cvlry'])
