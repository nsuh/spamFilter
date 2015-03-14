#Nathan Suh
#Spam classification
#k Nearest Neighbor classification implementation

load("~/Projects/spamFilter/cleanTrainingVariableData.rda")
load("~/Projects/spamFilter/testData.rda")

#I chose to just take some form of weighted euclidean distance for the variables: percentCapitals + subjectExclamationCount + percentForwards + subjectSpamWords + numRecipients
#j == test data element
#i == training data element
#for each test element, we need to iterate over the entire training data set! 
#assuming each sort takes nlogn time, n being i, or the whole training set, there are j sorts
#O(j*n + j*nlogn) is a fair guesstimate of the time complexity.  This was taking too long on my computer, so I ran it on the cloud!  I ran it on sense.io.  I want to run it through spark soon, when I learn how to use it

kNN = function(k, trainVariables, testVariables) {
  type1ErrorCounts = 0
  type2ErrorCounts = 0
  for (j in 1:length(testVariables[,1])) { #for each test data
    print(j)
    if (is.na(testVariables$isSpam[j]) == TRUE) {
      print('fail')
      print()
      next
    }
    distVector = c()
    for (i in 1:length(trainVariables[,1])) {  #iterating over all of the training data.  I'm weighting the distances somewhat arbitrarily
      diff1 = ((testVariables$percentCapitals[j]-trainVariables$percentCapitals[i])^2) #max 10,000 
      diff2 = ((testVariables$subjectExclamationCount[j]-trainVariables$subjectExclamationCount[i])^2)*100 #max 10,000 except for outlier.
      diff3 = ((testVariables$percentForwards[j]-trainVariables$percentForwards[i])^2) #max 10,000
      diff4 = ((testVariables$subjectSpamWords[j]-trainVariables$subjectSpamWords[i])^2)*5000 #bool, max 5,000
      diff5 = ((testVariables$numRecipients[j]-trainVariables$numRecipients[i])^2) #max ~10,000
      euDistance = (diff1 + diff2 + diff3 + diff4 + diff5)^0.5
      distVector[i] = euDistance
    }
    
    nearestNeighbors = order(distVector)
    kNearestNeighbors = nearestNeighbors[1:k]
    spamCount = 0  #number of spams in k nearest neighbors
    for (i in 1:k) { #classify the test data point
      if(trainVariables$isSpam[kNearestNeighbors[k]] == TRUE) { #neighbor is spam
        spamCount = spamCount + 1
      }
    }
    if ((spamCount/k) > 0.5) { #if more than half of the nearest neighbors are spam, classify as spam. else, ham
      spamPrediction = TRUE
    } else {
      spamPrediction = FALSE
    }
    if ((spamPrediction == TRUE) & (testVariables$isSpam[j] == FALSE)) {  #false positive, we classified a HAM as SPAM!  this is bad.
      type1ErrorCounts = type1ErrorCounts + 1
    } else if ((spamPrediction == FALSE) & (testVariables$isSpam[j] == TRUE)) {  #false negative, classified a SPAM as HAM, not so bad
      type2ErrorCounts = type2ErrorCounts + 1
    }
  }
  
  totalError = type1ErrorCounts + type2ErrorCounts
  knnAccuracy = (((totalError-2000)^2)^0.5)/2000
  print(knnAccuracy)
}

#for the test data, I got:
#k = 1, accuracy 0.8755
  #96 type 1 errors, 153 type 2 errors
#k = 3, accuracy 0.8205
  #154 type 1 errors, 205 type2 errors
#k = 5, accuracy 0.8085
  #171 type 1 errors, and 212 type 2 errors.
#k = 7, accuracy 0.815
#this is a somewhat high error rate?  maybe I took away too many variables, or used too weird of a euclidean distance scale.

#kNN(3,trainVariables, testVariables[1:3,])

###########
#10 fold cross validation
#my implementation is very suboptimal because it doesn't memoize distances.  Good thing I ran it on the cloud.. It's taking quite a long time (>1 hr I estimate...)
###########
set.seed(6)
trainVariablesRandomized = sample(trainVariables)

for (i in 2:10) {
  print(i)
  if (i == 1){
    kNN(3, trainVariablesRandomized[654:6540,], trainVariablesRandomized[1:654,])
  } else if (i<10) {
    #trainData = rbind(trainVariablesRandomized[1:(i-1)*654,], trainVariablesRandomized[i*654:6540,])
    kNN(3, rbind(trainVariablesRandomized[1:(i-1)*654,], trainVariablesRandomized[i*654:6540,]), trainVariablesRandomized[(i-1)*654:(i*654),])
  } else {
    kNN(3, trainVariablesRandomized[1:6540-654,], trainVariablesRandomized[6540-654:6540,])
  }
}


#0.962
#0.8985
#0.806




