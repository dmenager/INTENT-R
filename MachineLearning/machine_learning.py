# machine_learning.py
import numpy as np
import pandas as ps
import sklearn.cluster
import sklearn.svm
from sklearn.decomposition import PCA
from matplotlib import pyplot as plt

# Cluster X based on features in F
#
# X - pandas DataFrame to be clustered
# F - array of strings corresponding to desired features
# k - number of clusters to use in clustering model (when appropriate)
#
# Returns - an array of cluster labels (Y)
def cluster(X,  F, k=4):    
    #  Restrict X to selected features
    X_F = X[F]

    clusterModel = sklearn.cluster.KMeans(n_clusters = k)
    Y = clusterModel.fit_predict(X_F.values)
    
    return Y

# Colors for plotting
colors = ['b',  'r',  'g',  'c', 'm',  'y',  'k',  'w']

# Generates histograms for given features
#
# X - Pandas data set
# Y - labels from clustering
# F - the features to generated histograms for (an array of strings)
def hist(X,  Y,  F,  bins = 10):
    i_figure = 1
    
    # Generate a histogram for the given feature
    for f in F:
        # Discover number of clusters from label vector Y
        k = np.unique(Y).size
        
        # Array containing just that feature
        X_F = X[f]
        
        clusters = np.ndarray(shape = (k,  0)).tolist()
        
        # Divide up (based on clustering label) into subsets 
        for i in range(0,  k):
            clusters[i] = X_F[Y == i ].values.tolist()
        
        # Create seperate figures
        plt.figure(i_figure)
        i_figure += 1
    
        # Automatically sets up histogram with bar-stacking style
        plt.hist(clusters,  histtype = 'barstacked',  color = colors[0:k],  bins = bins)

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
    X_F = X[F]
    
    # Reduce X to 2 dimensions using principle component analysis
    X_2D = PCA(n_components=2).fit_transform(X_F.values)

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


# Train classification model
#
# X - training data in pandas DataFrame
# Y - cluster labels in numpy array
def train(X,  Y,  F):
    X_F = X[F]
    
    clf = sklearn.svm.SVC()
    clf.fit(X_F.values, Y.values)
    return clf
    
# Test the accuracy of the classifier
#
# clf - classification model
# testX - test data in pandas DataFrame
# testY - human-readable labels in numpy array
def accuracy(clf,  testX,  testY, F,   timeBound = 300):
    X_F = testX[F]
    compY = clf.predict(X_F.values)
        
    correct = 0
    for i in range(0,  len(testY)):
        if (testY.iloc[i] == compY[i]) and (testX.iloc[i].time <= timeBound):
            correct += 1
    
    percentCorrect = (float(correct) / len(testY)) * 100
    return percentCorrect
