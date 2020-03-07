# Title     : MainRunner.R
# Objective : Handles the running of the project
# Created by: Tristan Miller (tem83)
# Created on: 3/6/2020

if( !require( "data.table" ) ){
    install.packages( "data.table" )
}

#load packages
library( data.table )
library( ggplot2 )
source("Project3/StocGradDescent.R")

#acquire data
if(!file.exists("spam.data") ){
    download.file("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data", "spam.data")
}

spam.dt <- data.table::fread("spam.data")

#-ncol(spam.dt) because the output is on the last col
data <- as.matrix( spam.dt[ , -ncol( spam.dt ), with=FALSE ] )

#seperates the outputs
outputs <- spam.dt[[ncol( spam.dt )]]

data.scaled <- scale( data )

set.seed(1)

is.train <- sample( x = rep( c( TRUE, FALSE) ), size = nrow( data.scaled ), prob = c( 0.8, 0.2), replace = TRUE )

trainCounts <- table( is.train )

numTrains <- trainCounts[ names( trainCounts ) == TRUE ]

is.subtrain <- sample( x = rep( c( TRUE, FALSE) ), size = numTrains, prob = c( 0.6, 0.4), replace = TRUE )

X.train <- data.scaled[ is.train, ]

y.train <- outputs[ is.train ]

X.test <- data.scaled[ !is.train, ]

y.test <- outputs[ !is.train ]

max.epochs <- 200

step.size <- 0.05

n.hidden.units <- 3

nn.out.many.epochs <- NNetOneSplit( X.train, y.train, max.epochs, step.size, n.hidden.units, is.subtrain )

loss.values.many.epochs <- nn.out.many.epochs[[1]]

V.mat.trained <- nn.out.many.epochs[[2]]

w.vec.trained <- nn.out.many.epochs[[3]]

min.dt.many.epochs <- loss.values.many.epochs[dataset=="validate"][which.min(mean.loss.value)]

nn.graph.many.epochs <- ggplot()+
    geom_line(aes(
        epoch, mean.loss.value, color=dataset),
              data=loss.values.many.epochs)+
    geom_point(aes(
        epoch, mean.loss.value, color=dataset),
               data=min.dt.many.epochs)

nn.graph.many.epochs


best.epochs <- min.dt.many.epochs[1,min.dt.many.epochs$epoch]

nn.out.best.epochs <- NNetOneSplit( X.train, y.train, best.epochs, step.size, n.hidden.units, is.subtrain = TRUE )

loss.values.best.epochs <- nn.out.best.epochs[[1]]

min.dt.best.epochs <- loss.values.best.epochs[dataset=="train"][which.min(mean.loss.value)]

train.loss.values.best.epochs <- loss.values.best.epochs[dataset=="train"]

nn.graph.best.epochs <- ggplot()+
    geom_line(aes(
        epoch, mean.loss.value),
              data=train.loss.values.best.epochs)+
    geom_point(aes(
        epoch, mean.loss.value),
               data=min.dt.best.epochs)

nn.graph.best.epochs

test.data.predicted.outputs <- GradientDescent(X.test, V.mat.trained, w.vec.trained)

test.data.accuracy <- CalculateAccuracy( test.data.predicted.outputs, y.test)

testCounts <- table(y.test)

baselineVal <- ifelse(testCounts[1]>testCounts[2], 0, 1)

baselinePredictions <- rep(baselineVal, length(y.test))

baseline.data.accuracy <- CalculateAccuracy(baselinePredictions, y.test)

sprintf("Trained data accuracy on test data: %f", mean(test.data.accuracy) )
sprintf("baseline data accuracy on test data: %f", mean( baseline.data.accuracy ) )

#(5 points) Finally use the learned V.mat/w.vec to make predictions on the test set. What is the prediction accuracy? (percent correctly predicted labels in the test set) What is the prediction accuracy of the baseline model which predicts the most frequent class in the train labels?

#EC:
# 10 points if your github repo includes a README.org (or README.md etc) file with a link to the source code of your NNOneSplit function, and an explanation about how to run it on the data sets.
# 10 points if you do 4-fold cross-validation instead of the single train/test split described above, and you make a plot of accuracy for both models for each split/fold.
# 10 points if you show GradientDescent (from project 1, logistic regression with number of iterations selected by a held-out validation set) in your test accuracy result figure.
# 10 points if you show NearestNeighborsCV (from project 2) in your test accuracy figure.
# 10 points if you compute and plot ROC curves for each (test fold, algorithm) combination. Make sure each algorithm is drawn in a different color, and there is a legend that the reader can use to read the figure. Example:
# 10 points if you compute area under the ROC curve (AUC) and include that as another evaluation metric (in a separate panel/plot) to compare the test accuracy of the algorithms.