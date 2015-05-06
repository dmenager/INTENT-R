# machine_learning.py
import utils
import numpy as np
import pandas as ps
from sklearn.preprocessing import normalize
from sklearn.cluster import KMeans
from sklearn.svm import SVC
from sklearn.decomposition import PCA
from sklearn.feature_selection import RFE
from matplotlib import pyplot as plt
import math

def norm(X):
    X = X.astype(float)
    columns = X.columns
    normalize(X)
    return ps.DataFrame(X, columns = columns)
    
def recursiveCluster(X, size):
    Y = KMeans(n_clusters = 2, random_state = 42).fit_predict(X.values)
    clusters = utils.separate(X, Y)
    hierarchy = [Y]
    for c in clusters:     
        if len(c) > size * 2:
            hierarchy.append(recursiveCluster(c, size))
        else:
            hierarchy.append(len(c))
    return hierarchy

def flatten(H, min):
    Y = H[0]
    for (i, h) in enumerate(H[1:]):  
        if type(h) == list:
            Y[Y == i] = (flatten(h, min) * 2) + i
        elif h < min:
            Y[Y == i] = -1
    return Y

# Colors for plotting
colors = ['b', 'r', 'g', 'c', 'm', 'y', 'b', '#888888', '#ff0088', '#ff8800', '#88ff00', '#8800ff', '#0088ff', '#00ff88']
markers = ['o', 's', '^', 'D' , '*', 'H', 'p', 'v', '<', '>',  'h', 'x', '+', '.']

# Generates histograms for given features
#
# X - Pandas data set
# Y - labels from clustering
# F - the features to generated histograms for (an array of strings)
def hist(X,  Y,  bins = 20):
    i_figure = 1
    clusters = utils.separate(X,  Y)
    
    # Generate a histogram for the given feature
    for f in list(X.columns):
        # Discover number of clusters from label vector Y
        k = np.unique(Y).size
        
        # Feature divided by clusters
        clusters_f = []
        for i in range(0,  k):            
            clusters_f.append(clusters[i][f])
        
        # Create seperate figures
        plt.figure(i_figure)
        i_figure += 1
    
        # Automatically sets up histogram with bar-stacking style
        plt.hist(clusters_f,  histtype = 'barstacked',  color = colors[0:k],  bins = bins)

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
def pca(X,  Y):
    # Reduce X to 2 dimensions using principle component analysis
    X_2D = PCA(n_components=2).fit_transform(X.values)

    # Convert Numpy back to Pandas
    X_2D = ps.DataFrame(X_2D)
    
    # Discover number of clusters from label vector Y
    k = np.unique(Y).size

    # Plot each set in a different color
    for i in range(0,  k):
        # Pandas data set of cluster k
        s = X_2D[Y == i]
        
        # Plot the x and y coordinates of s using the appropriate color
        plt.plot(s[0],  s[1],  color = colors[i], marker = markers[i], linestyle = 'None', markersize=4)

    plt.title("PCA plot of %s" % X.columns)
    plt.show()

## Classification

def divide(X,  Y,  fractionTest=0.4):
    dividingIndex = math.trunc(fractionTest * len(X))
    trainX = X[dividingIndex:]
    testX = X[:dividingIndex]
    trainY = Y[dividingIndex:]
    testY = Y[:dividingIndex]
    return trainX,  testX,  trainY,  testY

def accuracies(clf, X, Y):
    trainX,  testX,  trainY,  testY = divide(X,  Y)
    clf.fit(trainX, trainY)
    classifier_accuracy = clf.score(trainX,  trainY)
    test_data_accuracy = clf.score(testX,  testY)
    return classifier_accuracy, test_data_accuracy
    
def validate_acc(clf, X,  Y):
    clf_acc, test_acc = accuracies(clf, X, Y)
    THRESHOLD_OF_OVERFITTING = 0.75
    if (test_acc/ clf_acc) < THRESHOLD_OF_OVERFITTING:
        return -1 # Model is overfit
    else:
        return test_acc

def featureSelection(X, Y):
    clf = SVC(kernel = "linear")
    selector = RFE(clf, step = 1, verbose = 1)
    selector.fit(X, Y)
    return X.columns[selector.support_]

def best_classifier(X, Y):
    best_params = []
    best_acc = -1
    
    #Linear
    clf = SVC(kernel = "linear")    
    acc = validate_acc(clf, X, Y)
    best_params  = clf.get_params()
    best_acc = acc
    
    # Polynomial
    clf.set_params(kernel = "poly")
    degrees = [2, 3, 4]
    for d in degrees:
        clf.set_params(degree = d)
        acc = validate_acc(clf, X, Y)
        print "poly: ", " degree=", d, " acc=", acc
        if (acc > best_acc):
            best_params  = clf.get_params()
            best_acc = acc
    
    # Radial basis function
    clf.set_params(kernel = "rbf")
    c_values = [0.001, 0.01,  0.1,  1,  10,  100, 1000]
    for c in c_values:
        clf.set_params(C = c)
        gammas = [0.001,  0.01,  0.1,  1,  10,  100,  1000]
        for g in gammas:            
            clf.set_params(gamma = g)
            acc = validate_acc(clf, X, Y)
            print "rbf: ", " C= ", c, " gamma=", g, " acc=", acc
            if (acc > best_acc):
                best_params  = clf.get_params()
                best_acc = acc
    
    # Sigmoid
    clf.set_params(kernel = "sigmoid")
    c_values = [0.001, 0.01,  0.1,  1,  10,  100, 1000]
    for c in c_values:
        clf.set_params(C = c)
        acc = validate_acc(clf, X, Y)
        print "sigmoid: ", " C=", c, " acc=", acc
        if (acc > best_acc):
            best_params  = clf.get_params()
            best_acc = acc
            
    # Train the best
    clf.set_params(kernel = best_params["kernel"], C = best_params["C"], gamma = best_params["gamma"], degree = best_params["degree"])
    clf_acc, test_acc = accuracies(clf, X, Y)
    clf.fit(X, Y)
    return clf, clf_acc, test_acc
