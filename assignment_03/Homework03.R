### 20130713_Homework03.R by dexlau@gmail.com (Dexter Lau) 20130720 ###

library("e1071")			# load the NB library

CalculateNthFoldIndices <- function(indices, total.folds, nthFold) {
  # Computes the indices of the nthFold
  #
  # Args:
  #   indices: the vector of indices, where the nth fold of indices come from
  #   total.folds: the total number of folds 
  #   nthFold: the fold of the desired indices, starting from 1
  #
  # Returns:
  #   The indices of the nthFold of indices
  nthFold.index <- NULL
  for(i in 1:length(indices)) {
    if(i %% total.folds == (nthFold-1)) {
      nthFold.index <- append(nthFold.index,indices[i])
    }
  }
  return(nthFold.index)
}

CalculateError <- function(prediction, data, test.index, label.column) {
  # Computes the error based on using the Nth fold as the test data
  #
  # Args:
  #   data: the data frame
  #   test.index: vector containing data indices that will be used for testing
  #   label.column: column that holds the data labels
  #
  # Returns:
  #   The error rate for using
  return(sum(prediction != data[test.index, label.column]) / length(data[test.index, label.column]))
}

data <- iris										# load iris data
set.seed(1)											# set seed for repeatability
max.folds = 10										# set the highest number of folds to test
dataLabelColumn = 5									# identify the column in data that holds the labels	
data.randomized.index <- sample(1:nrow(data), nrow(data))			# Create a randomized index for the data, essentially "shuffling" it

for(j in 2:max.folds){
  # Try all the different number of folds from 2 to max.folds
  err.rates <- NULL
  numFolds = j
  for(i in 1:numFolds){
    # Run n-fold cross-validation for numFolds
    # Calculate generalization error for numFolds
    test.index <- CalculateNthFoldIndices(data.randomized.index, numFolds, i)	# create the test indices; the train indices will be remaining indices
    NB.model <- naiveBayes(data[-test.index,-dataLabelColumn], data[-test.index, dataLabelColumn])  # create the naiveBayes model
    prediction <- predict(NB.model,data[test.index,])
    err.rates <- append(err.rates, CalculateError(prediction, data, test.index, dataLabelColumn))  # calculate the error rate using the naiveBayes model
  }
  print(paste(numFolds,"- fold generalization error = ", mean(err.rates)))
}