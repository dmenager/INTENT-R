#folder name clientData /home/ec2-user/clientData
import numpy as np
import pandas as ps
from sklearn import cluster as cstr
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt

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
        
        clusters = np.ndarray(shape = (k,  0))
        
        # Divide up (based on clustering label) into subsets 
        for i in range(0,  k):
            clusters.append(feat_array[Y == i])    
        
        plt.figure(i_figure)
        i_figure += 1
    
        # Automatically sets up histogram with bar-stacking style
        plt.hist(clusters,  histtype = 'barstacked')

        plt.xlabel(f)
        plt.ylabel('Occurences of ranges')
        plt.title('Histogram of %s (blue = 0, red = 1, green = 2, cyan = 3)' % f)
    
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

    # Colors for plotting
    colors = ['b',  'r',  'g',  'c', 'm',  'y',  'k',  'w']

    # Plot each set in a different color
    for i in range(0,  k):
        # Pandas data set of cluster k
        s = X_2D[Y == i]
        
        # Plot the x and y coordinates of s using the appropriate color
        plt.plot(s[0],  s[1],  "o" + colors[i],  markersize=4)

    plt.title("PCA plot (blue = 0, red = 1, green = 2, cyan = 3)")
    plt.show()

def open(file):
    return ps.read_csv(file)

#Clustering
# X - pandas DataFrame to be cstred
# F - array of strings corresponding to desired features
# k - number of clusters to use in clustering model (when appropriate)
#
# Returns - an array of cluster labels (Y)
def cluster(X,  F, k=4):    
    #
    # Uncomment methods below to call appropriate clustering algorithm
    #
    
    # http://scikit-learn.org/stable/modules/generated/sklearn.cluster.KMeans.html#sklearn.cluster.KMeans
    clusterModel = cstr.KMeans(n_clusters = k)
    
    # http://scikit-learn.org/stable/modules/generated/sklearn.cluster.AffinityPropagation.html#sklearn.cluster.AffinityPropagation
    #clusterModel = cstr.AffinityPropagation()
    
    # http://scikit-learn.org/stable/modules/generated/sklearn.cluster.MeanShift.html#sklearn.cluster.MeanShift
    #clusterModel = cstr.MeanShift()
    
    # http://scikit-learn.org/stable/modules/generated/sklearn.cluster.AgglomerativeClustering.html#sklearn.cluster.AgglomerativeClustering
    #clusterModel = cstr.AgglomerativeClustering(n_clusters = k)
    
    # http://scikit-learn.org/stable/modules/generated/sklearn.cluster.SpectralClustering.html#sklearn.cluster.SpectralClustering
    #clusterModel = cstr.SpectralClustering(n_clusters = k)
    
    # http://scikit-learn.org/stable/modules/generated/sklearn.cluster.FeatureAgglomeration.html#sklearn.cluster.FeatureAgglomeration	
    #clusterModel = cstr.FeatureAgglomeration(n_clusters = k)
    
    # http://scikit-learn.org/stable/modules/generated/sklearn.cluster.DBSCAN.html#sklearn.cluster.DBSCAN
    #clusterModel = cluster.DBSCAN()
    
    #
    #   Use F to determine the features to be used in clustering.
    #
    X = X[F]
        
    return clusterModel.fit_predict(X.values)

all = ['time', 'food', 'dFood', 'wood', 'dWood', 'stone', 'dStone', 'metal', 'dMetal', 'inf', 'dInf', 'wrkr', 'dWrkr', 'fmales', 'dFmales', 'cvlry', 'dCvlry', 'chmp', 'dChmp', 'hero', 'dHero', 'ships', 'dShips', 'house', 'dHouse', 'econ', 'dEcon', 'outpst', 'dOutpst', 'mltry', 'dMltry', 'fortr', 'dFortr', 'civCnt', 'dCivCnt', 'wndr', 'dWndr', 'enK', 'dEnK', 'enBldD', 'dEnBldD', 'unitsL', 'dUnitsL', 'bldL', 'dBldL', 'Label']
delta = ['time', 'dFood', 'dWood', 'dStone', 'dMetal', 'dInf', 'dWrkr', 'dFmales', 'dCvlry', 'dChmp', 'dHero', 'dShips', 'dHouse', 'dEcon', 'dOutpst', 'dMltry', 'dFortr','dCivCnt', 'dWndr', 'dEnK', 'dEnBldD','dUnitsL', 'dBldL', 'Label']
nonDelta = ['time', 'food', 'wood', 'stone',  'metal', 'inf', 'wrkr', 'fmales', 'cvlry', 'chmp', 'hero', 'ships', 'house','econ', 'outpst', 'mltry', 'fortr', 'civCnt', 'wndr', 'enK', 'enBldD', 'unitsL', 'bldL', 'Label']
