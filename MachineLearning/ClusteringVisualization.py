import numpy as np
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt

# X - data set being clustered
# Y - labels from clustering
# k - number of clusters
def visualize(X,  Y,  k):
    # Generate a histogram for the given feature
    def hist_feature(i_feature):
        # list of k empty lists
        sets = np.ndarray(shape  = (k,  0)).tolist()
        
        # Array containing just that feature
        feat_array = X[:, i_feature]
        
        # Divide up (based on clustering label) into subsets 
        for i in range(0,  Y.size):
            sets[Y[i]].append(feat_array[i])        
            
        # Automatically sets up histogram with bar-stacking style
        plt.hist(sets,  histtype = 'barstacked')

        plt.xlabel('Value')
        plt.ylabel('Occurences')
        plt.title('Histogram of Feature %d' % i_feature)
        plt.show()
    
    # Generate a histogram for each feature
    for i in range(0,  X[0].size):
        hist_feature(i)
    
    # Reduce X to 2 dimensions using principle component analysis
    X_2D = PCA(n_components=2).fit_transform(X)

    # list of k empty lists
    sets = np.ndarray(shape  = (k,  0)).tolist()

    # Divide up into subsets based on clustering label
    for i in range(0,  Y.size):
        sets[Y[i]].append(X_2D[i])

    # Colors for plotting
    colors = ['b',  'r',  'g',  'y', 'm',  'c',  'k',  'w']

    # Plot each set in a different color
    for i in range(0,  k):
        # Convert set to a numpy array
        s = np.asarray(sets[i])
        
        # Plot the x and y coordinates of s using the appropriate color
        plt.plot(s[:, 0],  s[:, 1],  "o" + colors[i],  markersize=4)

    plt.title("Scatter plot of clusters reduced to 2D by PCA")
    plt.show()
   
# Test run with boston data set
if __name__ == '__main__':
    # Temporary dummy data
    from sklearn.datasets import load_boston
    X = load_boston().data

    k = 4

    # Build clustering model
    kmeans = KMeans(n_clusters= k)

    # Label data points
    Y = kmeans.fit_predict(X)

    visualize(X,  Y,  k)
