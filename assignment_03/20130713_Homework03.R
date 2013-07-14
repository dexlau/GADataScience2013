### 20130713_Homework03.R by dexlau@gmail.com (Dexter Lau) ###

library("e1071")			# load the NB library

data <- iris				# load iris data
set.seed(1)				# set seed for repeatability
train.percent <- 0.7		# set the training data size (%)

train.index <- sample(1:nrow(data), ceiling(nrow(data)*train.percent))	# set the train index
m <- naiveBayes(iris[train.index,-5], iris[train.index,5])		# create the NB model based on the training data alone
table(predict(m, iris[-train.index,]), iris[-train.index,5])	# use the NB model to predict on the test data