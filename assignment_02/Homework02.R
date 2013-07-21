### Homework02.R by dexlau@gmail.com (Dexter Lau) 20130720 ###

library(class)			# load the class library

CalculateNthFoldIndices <- function(indices, totalFolds, nthFold) {
  # Computes the indices of the nthFold
  #
  # Args:
  #   indices: the vector of indices, where the nth fold of indices come from
  #   totalFolds: the total number of folds 
  #   nthFold: the fold of the desired indices, starting from 1
  #
  # Returns:
  #   The indices of the nthFold of indices
  nthFoldIndices <- NULL
  for(i in 1:length(indices)) {
    if(i %% totalFolds == (nthFold-1)) {
      nthFoldIndices <- append(nthFoldIndices,indices[i])
    }
  }
  return(nthFoldIndices)
}

CalculateError <- function(prediction, data, testIndices, labelColumn) {
  # Computes the error based on using the Nth fold as the test data
  #
  # Args:
  #   prediction: the prediction vector
  #   data: the data frame
  #   testIndices: vector containing data indices that will be used for testing
  #   labelColumn: column that holds the data labels
  #
  # Returns:
  #   The error rate for using
  return(sum(prediction != data[testIndices, labelColumn]) / length(data[testIndices, labelColumn]))
}

data <- iris										# load iris data
set.seed(1)											# set seed for repeatability
max.folds = 10										# set the highest number of folds to test
max.k = 20											# set the highest number of k-nearest neighbors
dataLabelColumn = 5									# identify the column in data that holds the labels	
data.randomized.index <- sample(1:nrow(data), nrow(data))			# Create a randomized index for the data, essentially "shuffling" it
folds.err.rates = data.frame()

for(j in 2:max.folds){
  # Try all the different number of folds from 2 to max.folds
  numFolds = j
  for(i in 1:numFolds){
    # Run n-fold cross-validation for numFolds
    # Calculate generalization error for numFolds
    err.rates <- NULL
    test.index <- CalculateNthFoldIndices(data.randomized.index, numFolds, i)	# create the test indices; the train indices will be remaining indices
    for(l in 1:max.k){
      # For each fold, calculate the error for that k
      knn.model <- knn(train = data[-test.index, -dataLabelColumn], test = data[test.index, -dataLabelColumn], cl = data[-test.index, dataLabelColumn], k = l)  # create the KNN model
      prediction <- knn.model
      err.rates <- append(err.rates, CalculateError(prediction, data, test.index, dataLabelColumn))  # calculate the error rate using the KNN model
    }
    folds.err.rates <- rbind(folds.err.rates,err.rates)  # store all the error rates here
  }
  print(paste("KNN mean error rates where k is from 1 to", max.k, "and there are", j, "folds."))
  print(colMeans(folds.err.rates))  # take the mean of the error rates across all folds for each k
}