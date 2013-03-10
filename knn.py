from sklearn import neighbors
import numpy as np

trainingInput = open("data/train.csv")
trainingInput.readline() # skip the header

trainingData = np.loadtxt(trainingInput, delimiter=',')

labels       = trainingData[:,0]
trainingData = trainingData[:,1:]

clf = neighbors.KNeighborsClassifier(n_neighbors=10, weights='uniform')
clf.fit(trainingData, labels)

testingInput = open("data/test.csv")
testingInput.readline() # skip the header

testingData = np.loadtxt(testingInput, delimiter=',')

output = clf.predict(testingData)
np.savetxt("data/output.csv", output, fmt="%0.0f")
