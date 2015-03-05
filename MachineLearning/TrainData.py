#folder name clientData /home/ec2-user/clientData
import pandas as ps
import numpy as np
import os.path

file_counter = 0
dir_name = "/home/ec2-user/clientData"

#length of each file
length = []
#first template file dir
file_dir =
#excess offset top
m = 5
#excess offset bottom
n = 5 
#shift offset
offset = 5
#training data
X = ps.read_csv(file_dir)
#hand labels
Z = X["Label"]
X = X.drop("Label", 1)

#collect data into one object
for filename in os.listdir(dir_name):
	x = ps.read_csv(os.path.join(dir_name, filename))
	x = x.ix[m:n]
	z = x["Label"]
	x = x.drop("Label", 1)
	X = concat(X, x)
	Z = concat(Z, z)
	length.append(len(x.values))
	file_counter++
	
#Clustering
# X - pandas DataFrame to be clustered
# F - array of strings corresponding to desired features
# k - number of clusters to use in clustering model (when appropriate)
#
# Returns - an array of cluster labels (Y)
def cluster(X,  F='all', k=4):
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
    
    # Everything: 'time', 'food', 'dFood', 'wood', 'dWood', 'stone', 'dStone', 'metal', 'dMetal', 'inf', 'dInf', 'wrkr', 'dWrkr', 'fmales', 'dFmales', 'cvlry', 'dCvlry', 'chmp', 'dChmp', 'hero', 'dHero', 'ships', 'dShips', 'house', 'dHouse', 'econ', 'dEcon', 'outpst', 'dOutpst', 'mltry', 'dMltry', 'fortr', 'dFortr', 'civCnt', 'dCivCnt', 'wndr', 'dWndr', 'enK', 'dEnK', 'enBldD', 'dEnBldD', 'unitsL', 'dUnitsL', 'bldL', 'dBldL', 'Label'
    if F == 'delta':
        X = X[['time', 'dFood', 'dWood', 'dStone', 'dMetal', 'dInf', 'dWrkr', 'dFmales', 'dCvlry', 'dChmp', 'dHero', 'dShips', 'dHouse', 'dEcon', 'dOutpst', 'dMltry', 'dFortr','dCivCnt', 'dWndr', 'dEnK', 'dEnBldD','dUnitsL', 'dBldL', 'Label']]
    else if F == 'nonDelta':
        X = X[['time', 'food', 'wood', 'stone',  'metal', 'inf', 'wrkr', 'fmales', 'cvlry', 'chmp', 'hero', 'ships', 'house','econ', 'outpst', 'mltry', 'fortr', 'civCnt', 'wndr', 'enK', 'enBldD', 'unitsL', 'bldL', 'Label']]
    else if type(F) != type([]):
        X = X
    else:
        X = X[F]
        
    return clusterModel.fit_predict(X.values)


#use X.values to send data in a numpy array
