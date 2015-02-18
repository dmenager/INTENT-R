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

#use X.values to send data in a numpy array