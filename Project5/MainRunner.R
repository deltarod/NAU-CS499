# Title     : MainRunner
# Objective : The main runner for this project
# Created by: Tristan Miller (tem83)
# Created on: 4/13/2020

# tensorflow::install_tensorflow(version="2.1")

library( keras )
library( tensorflow )
library( data.table )
library( ggplot2 )

#import code for training model
source("Project5/Model.R")

#acquire data
if(!file.exists("spam.data") ){
    download.file("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data", "spam.data")
}

#load into data table
spam.dt <- data.table::fread("spam.data")

#column with the labels
spam.label <- ncol(spam.dt)

spam.y <- array(spam.dt[[spam.label]], nrow(spam.dt))

set.seed(1)

#split data into train and test
spam.dataset <- sample( x = rep( c( "train", "test" ) ), prob = c( .8, .2 ), replace = TRUE, size=nrow( spam.dt ) )
spam.isTest <- spam.dataset == "test"
spam.isTrain <- !spam.isTest

#scale data
spam.x <- scale(spam.dt[, -spam.label, with=FALSE])

#split into train and test datasets
spam.x.train <- spam.x[spam.isTrain, ]
spam.x.test <- spam.x[spam.isTest, ]

spam.y.train <- spam.y[spam.isTrain]
spam.y.test <- spam.y[spam.isTest]

spam.subtrain <- sample( rep( c( TRUE, FALSE), l=nrow( spam.x.train ) ) )
spam.validation <- !spam.subtrain

spam.x.subtrain <- spam.x.train[spam.subtrain, ]
spam.x.validation <- spam.x.train[spam.validation, ]

spam.y.subtrain <- spam.y.train[ spam.subtrain ]
spam.y.validation <- spam.y.train[ spam.validation ]


######################### Hidden Unit Training #########################

#train on up to 5 hidden units
hiddenUnits.output <- RunModel(spam.x.subtrain, spam.y.subtrain, list( spam.x.validation, spam.y.validation ), hiddenUnits = 5, hiddenUnits.train = TRUE )

#graph outputs
hiddenUnits.graph <- ggplot()+
    geom_line( aes( x = epoch, y = loss, color = factor( hiddenUnits ) ), linetype = 1, data = hiddenUnits.output[[1]] )+
    geom_line( aes( x = epoch, y = val_loss, color = factor( hiddenUnits ) ), linetype = 2, data = hiddenUnits.output[[1]])+
    geom_point( aes( x = epoch, y = val_loss, color = factor( hiddenUnits ) ), data = hiddenUnits.output[[2]] )+
    labs( color = "Number Of Hidden Units" )+
    ggtitle( "Training/Validation Loss of Number of Hidden Units")

#determine best parameter
hiddenUnits.best_parameter <- hiddenUnits.output[[2]][which.min(val_loss)]

hiddenUnits.best_parameter_value <- hiddenUnits.best_parameter[ 1, hiddenUnits.best_parameter$hiddenUnits ]

hiddenUnits.best_epoch <- hiddenUnits.best_parameter[ 1, hiddenUnits.best_parameter$epoch ]

#retrain
hiddenUnits.retrain.units <- RunModel( spam.x.train, spam.y.train, NULL, epochs = hiddenUnits.best_epoch, hiddenUnits = hiddenUnits.best_parameter_value, subtrain = FALSE )

hiddenUnits.retrain.eval.units <- EvaluateModel( hiddenUnits.retrain.units[[2]], spam.x.test, spam.y.test )

sprintf( "Hidden Units Trained Accuracy: %f", hiddenUnits.retrain.eval.units[[2]] )

######################### Hidden Layer Training #########################

hiddenLayers.output <- RunModel(spam.x.subtrain, spam.y.subtrain, list( spam.x.validation, spam.y.validation ), hiddenLayers = 5, hiddenLayers.train = TRUE )

#graph outputs
hiddenLayers.graph <- ggplot()+
    geom_line( aes( x = epoch, y = loss, color = factor( hiddenLayers ) ), linetype = 1, data = hiddenLayers.output[[1]] )+
    geom_line( aes( x = epoch, y = val_loss, color = factor( hiddenLayers ) ), linetype = 2, data = hiddenLayers.output[[1]])+
    geom_point( aes( x = epoch, y = val_loss, color = factor( hiddenLayers ) ), data = hiddenLayers.output[[2]] )+
    labs( color = "Number Of Hidden Layers" )+
    ggtitle( "Training/Validation Loss of Number of Hidden Layers")

#determine best parameter
hiddenLayers.best_parameter <- hiddenLayers.output[[2]][which.min(val_loss)]

hiddenLayers.best_parameter_value <- hiddenLayers.best_parameter[ 1, hiddenLayers.best_parameter$hiddenUnits ]

hiddenLayers.best_epoch <- hiddenLayers.best_parameter[ 1, hiddenLayers.best_parameter$epoch ]

#retrain
hiddenLayers.retrain <- RunModel( spam.x.train, spam.y.train, NULL, epochs = hiddenLayers.best_epoch, hiddenUnits = hiddenLayers.best_parameter_value, subtrain = FALSE )

hiddenLayers.retrain.eval <- EvaluateModel( hiddenLayers.retrain[[2]], spam.x.test, spam.y.test )

sprintf( "Hidden Layers Trained Accuracy: %f", hiddenLayers.retrain.eval[[2]] )


######################### Degree of L2/weight decay Training #########################

l2.output <- RunModel(spam.x.subtrain, spam.y.subtrain, list( spam.x.validation, spam.y.validation ), l2 = 0.01, l2.train = TRUE )

#graph outputs
l2.graph <- ggplot()+
    geom_line( aes( x = epoch, y = loss, color = factor( l2 ) ), linetype = 1, data = l2.output[[1]] )+
    geom_line( aes( x = epoch, y = val_loss, color = factor( l2 ) ), linetype = 2, data = l2.output[[1]])+
    geom_point( aes( x = epoch, y = val_loss, color = factor( l2 ) ), data = l2.output[[2]] )+
    labs( color = "L2 Value" )+
    ggtitle( "Training/Validation Loss of L2 Values")

#determine best parameter
l2.best_parameter <- l2.output[[2]][which.min(val_loss)]

l2.best_parameter_value <- l2.best_parameter[ 1, l2.best_parameter$l2 ]

l2.best_epoch <- l2.best_parameter[ 1, l2.best_parameter$epoch ]

#retrain
l2.retrain <- RunModel( spam.x.train, spam.y.train, NULL, epochs = l2.best_epoch, l2 = l2.best_parameter_value, subtrain = FALSE )

l2.retrain.eval <- EvaluateModel( l2.retrain[[2]], spam.x.test, spam.y.test )

sprintf( "L2 Trained Accuracy: %f", l2.retrain.eval[[2]] )

######################### Degree of Noise Training #########################

noise.output <- RunModel(spam.x.subtrain, spam.y.subtrain, list( spam.x.validation, spam.y.validation ), noise = 0.4, noise.train = TRUE )

#graph outputs
noise.graph <- ggplot()+
    geom_line( aes( x = epoch, y = loss, color = factor( noise ) ), linetype = 1, data = noise.output[[1]] )+
    geom_line( aes( x = epoch, y = val_loss, color = factor( noise ) ), linetype = 2, data = noise.output[[1]])+
    geom_point( aes( x = epoch, y = val_loss, color = factor( noise ) ), data = noise.output[[2]] )+
    labs( color = "Noise Value" )+
    ggtitle( "Training/Validation Loss of Noise Values")

#determine best parameter
noise.best_parameter <- noise.output[[2]][which.min(val_loss)]

noise.best_parameter_value <- noise.best_parameter[ 1, noise.best_parameter$noise ]

noise.best_epoch <- noise.best_parameter[ 1, noise.best_parameter$epoch ]

#retrain
noise.retrain <- RunModel( spam.x.train, spam.y.train, NULL, epochs = noise.best_epoch, noise = noise.best_parameter_value, subtrain = FALSE )

noise.retrain.eval <- EvaluateModel( noise.retrain[[2]], spam.x.test, spam.y.test )

sprintf( "Noise Trained Accuracy: %f", noise.retrain.eval[[2]] )


######################### Degree of DropOut Training #########################

dropOut.output <- RunModel(spam.x.subtrain, spam.y.subtrain, list( spam.x.validation, spam.y.validation ), dropOut = 0.4, dropOut.train = TRUE )

#graph outputs
dropOut.graph <- ggplot()+
    geom_line( aes( x = epoch, y = loss, color = factor( dropOut ) ), linetype = 1, data = dropOut.output[[1]] )+
    geom_line( aes( x = epoch, y = val_loss, color = factor( dropOut ) ), linetype = 2, data = dropOut.output[[1]])+
    geom_point( aes( x = epoch, y = val_loss, color = factor( dropOut ) ), data = dropOut.output[[2]] )+
    labs( color = "Drop Out Value" )+
    ggtitle( "Training/Validation Loss of Drop Out Values")

#determine best parameter
dropOut.best_parameter <- dropOut.output[[2]][which.min(val_loss)]

dropOut.best_parameter_value <- dropOut.best_parameter[ 1, dropOut.best_parameter$noise ]

dropOut.best_epoch <- dropOut.best_parameter[ 1, dropOut.best_parameter$epoch ]

#retrain
dropOut.retrain <- RunModel( spam.x.train, spam.y.train, NULL, epochs = dropOut.best_epoch, dropOut = dropOut.best_parameter_value, subtrain = FALSE )

dropOut.retrain.eval <- EvaluateModel( dropOut.retrain[[2]], spam.x.test, spam.y.test )

sprintf( "Drop Out Trained Accuracy: %f", dropOut.retrain.eval[[2]] )


#determine baseline accuracy
baselineAccuracy <- BaselineAccuracy( spam.y.test )
sprintf( "Baseline Accuracy: %f", baselineAccuracy )

ggsave("Project5/HiddenUnits.png", plot = hiddenUnits.graph)
ggsave("Project5/HiddenLayers.png", plot = hiddenLayers.graph)
ggsave("Project5/l2.png", plot = l2.graph)
ggsave("Project5/Noise.png", plot = noise.graph)
ggsave("Project5/DropOut.png", plot = dropOut.graph)
