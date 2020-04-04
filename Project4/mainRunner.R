# Title     : mainRunner
# Objective : Main runner script for project 4
# Created by: Tristan Miller (tem83)
# Created on: 4/3/2020


#load and install deps
if( !require( "tensorflow" ) ){
    install.packages( "tensorflow" )
}

if( !require( "keras" ) ){
    install.packages( "keras" )
}

# Uncomment this if nvidia graphics card
# install_tensorflow( version = "nightly-gpu" )

# Uncomment this otherwise
# install_tensorflow()

library( tensorflow )
library( keras )
library( data.table )
library( ggplot2 )

#import code for training model
source("Project4/Model.R")


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
spam.dataset <- sample( rep( c( "train", "test" ), l=nrow( spam.dt ) ) )
spam.isTest <- spam.dataset == "test"
spam.isTrain <- !spam.isTest

#scale data
spam.x <- scale(spam.dt[, -spam.label, with=FALSE])

#split into train and test datasets
spam.x.train <- spam.x[spam.isTrain, ]
spam.x.test <- spam.x[spam.isTest, ]

spam.y.train <- spam.y[spam.isTrain]
spam.y.test <- spam.y[spam.isTest]

graph.dt <- data.table()

epochs <- 25

#generate model for each unit size
tenUnits <- GenerateModel(10, spam.x.train, epochs )

oneHundredUnits <- GenerateModel(100, spam.x.train, epochs )

oneThousandUnits <- GenerateModel(1000, spam.x.train, epochs )

#train model

tenUnits.train <- TrainModel( tenUnits, spam.x.train, spam.y.train, epochs )

oneHundredUnits.train <- TrainModel( oneHundredUnits, spam.x.train, spam.y.train, epochs )

oneThousandUnits.train <- TrainModel( oneThousandUnits, spam.x.train, spam.y.train, epochs )

#generate info for graphing
tenUnits.info <- GenerateModelInfo( tenUnits.train, 10, epochs )

oneHundredUnits.info <- GenerateModelInfo( oneHundredUnits.train, 100, epochs )

oneThousandUnits.info <- GenerateModelInfo( oneThousandUnits.train, 1000, epochs )

#find unit size min validation loss
tenUnits.min <- tenUnits.info[which.min(val_loss)]

oneHundredUnits.min <- oneHundredUnits.info[which.min(val_loss)]

oneThousandUnits.min <- oneThousandUnits.info[which.min(val_loss)]

#graph these
loss.graph <- ggplot()+
    geom_line(aes(
    x = epoch, y = loss, color = factor(hiddenUnits) ), linetype = 1, data=tenUnits.info
    ) +
    geom_line(aes(
        x = epoch, y = val_loss, color = factor(hiddenUnits) ), linetype = 2, data=tenUnits.info
    ) +
    geom_line(aes(
        x = epoch, y = loss, color = factor(hiddenUnits) ), linetype = 1, data=oneHundredUnits.info
    ) +
    geom_line(aes(
        x = epoch, y = val_loss, color = factor(hiddenUnits) ), linetype = 2, data=oneHundredUnits.info
    ) +
    geom_line(aes(
        x = epoch, y = loss, color = factor(hiddenUnits) ), linetype = 1, data=oneThousandUnits.info
    ) +
    geom_line(aes(
        x = epoch, y = val_loss, color = factor(hiddenUnits) ), linetype = 2, data=oneThousandUnits.info
    ) +
    geom_point(aes(
        x = epoch, y = val_loss, color = factor(hiddenUnits) ), data = tenUnits.min ) +
    geom_point(aes(
        x = epoch, y = val_loss, color = factor(hiddenUnits) ), data = oneHundredUnits.min ) +
    geom_point(aes(
        x = epoch, y = val_loss, color = factor(hiddenUnits) ), data = oneThousandUnits.min ) +
    labs( color = "Number Of Hidden Units" )

loss.graph

#generate new models to be used to retrain
tenUnits.retrain <- GenerateModel(10, spam.x.train, tenUnits.min$epoch )

oneHundredUnits.retrain <- GenerateModel(100, spam.x.train, oneHundredUnits.min$epoch )

oneThousandUnits.retrain <- GenerateModel(1000, spam.x.train, oneThousandUnits.min$epoch )

#train the new models
tenUnits.retrain.train <- TrainModel( tenUnits.retrain, spam.x.train, spam.y.train, tenUnits.min$epoch, 0 )

oneHundredUnits.retrain.train <- TrainModel( oneHundredUnits.retrain, spam.x.train, spam.y.train, oneHundredUnits.min$epoch, 0 )

oneThousandUnits.retrain.train <- TrainModel( oneThousandUnits.retrain, spam.x.train, spam.y.train, oneThousandUnits.min$epoch, 0 )

#evaluate retrained models using the test data
print("Info for 10 hidden Units")
tenUnits.retrain %>%
    evaluate( spam.x.test, spam.y.test, verbose = 0 )

print("Info for 100 hidden Units")
oneHundredUnits.retrain %>%
    evaluate( spam.x.test, spam.y.test, verbose = 0 )

print("Info for 1000 hidden Units")
oneThousandUnits.retrain %>%
    evaluate( spam.x.test, spam.y.test, verbose = 0 )

#calculate baseline value
y.table <- table(spam.y.test)
baseline <- ifelse(y.table[1]>y.table[2], 0, 1)


baseline.accuracy <- mean(spam.y.train == baseline)
print("Baseline Accuracy: ")
print(baseline.accuracy)


