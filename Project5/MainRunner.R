# Title     : MainRunner
# Objective : The main runner for this project
# Created by: Tristan Miller (tem83)
# Created on: 4/13/2020

#load and install deps
if( !require( "tensorflow" ) ){
    install.packages( "tensorflow" )

}

if( !require( "keras" ) ){
    install.packages( "keras" )
    keras::install_keras(version="2.2.4")
}

library( keras )
library( data.table )
library( ggplot2 )

# Uncomment this if nvidia graphics card
#install_tensorflow( version = "nightly-gpu" )

# Uncomment this otherwise
# install_tensorflow()


#import code for training model
#source("Project5/Model.R")

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

model <- keras_model_sequential() %>%
    layer_flatten( input_shape = ncol( spam.x.subtrain ) ) %>%
    layer_dense( units = 5, activation = "sigmoid", use_bias = FALSE ) %>%
    layer_dense(1, activation = "sigmoid", use_bias = FALSE )

model %>%
    compile(
        loss = "binary_crossentropy",
        optimizer = "adam",
        metrics = "accuracy"
    )
model %>%
    fit(x=spam.x.subtrain, y=spam.y.subtrain,
        epochs=5,
        validation_data = list(spam.x.validation, spam.y.validation))

print("Test2")
