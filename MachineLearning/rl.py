# current.py
import sys
import pandas as ps
from StringIO import StringIO
from sklearn.externals import joblib as jl

def getRaw(str):
	fileName = 'currentSVM.pkl'

	headers = ps.read_csv('sample.csv',  delim_whitespace = True)
	x = ps.read_csv(StringIO(str),  names = headers.columns,  
	header = None,  delim_whitespace = True)

	# Remove feature(s)
	x = x.drop("Label", 1)

	# Create feature(s)
	x["army"] = x['inf'] + x['cvlry'] + (5 * x['chmp']) + (10 * x['hero'])

	# Reorder alphabetically
	x = x.reindex_axis(sorted(x.columns),  axis = 1)
	
	return x

def getState(state):
	Labels = ["defend",  "build",  "attack"]
	clf = jl.load(fileName)    
	pred = clf.predict(state.values)
	
	return Labels[pred]

def getAction(state):
	action = ""
	countmax = 3
	count = 0;
	
	state["marmy"] = state['minf'] + state['mcvlry'] + state['mchmp'] + state['mhero']
	state["garmy"] = state['ginf'] + state['gcvlry'] + (5 * state['gchmp']) + (10 * state['ghero'])
	state["harmy"] = state['hinf'] + state['hcvlry'] + state['hchmp'] + state['hhero']
	
	while(count < countmax) :
		if(state["alarm"] != np.array([0])):
			action += "Raise-Alert"
			state["alarm"] = np.array([0])
		else if(state["marmy"] != np.array([0])):
			action += "Move-Army"
			state["marmy"] = np.array([0])
		else if(state["garmy"] != np.array([0])):
			action += "Create-Army"
			state["garmy"] = np.array([0])
		else if(state["bstruct"] != np.array([0])):
			action += "Create-Structure"
			state["bstruct"] = np.array([0])
		else if(state["hstruct"] != np.array([0])):
			action += "Repair-Structure"
			state["hstruct"] = np.array([0])
		else if(state["harmy"] != np.array([0])):
			action += "Heal-Army"
			state["harmy"] = np.array([0])
		else if(state["hsupp"] != np.array([0])):
			action += "Heal-Support"
			state["hsupp"] = np.array([0])
		else if(state["gsupp"] != np.array([0])):
			action += "Create-Support"
			state["gsupp"] = np.array([0])
		else if(state["msupp"] != np.array([0])):
			action += "Move-Support"
			state["msupp"] = np.array([0])
		else if(state["trade"] != np.array([0])):
			action += "Trade-Resources"
			state["trade"] = np.array([0])
		
		if(action != ""):
			action += ","
		count++
		
	return action

str = sys.argv[1]
X = getRaw(str)
state = getState(X)
action = getAction(X)


