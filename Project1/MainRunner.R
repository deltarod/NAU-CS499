# Title     : Main Runner
# Objective : Handles the Running of the whole shabang
# Created by: Tristan Miller (tem83)
# Created on: 2/7/2020

#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
source("Project1/GradientDescent.R")

LoadFile <- function( filename )
{
    return( read.csv( filename ) )
}

MeanLogisticLoss <- function( predictedVals, actualVals )
{
    outputVector <- rep( 0, ncol(predictedVals) )

    iterations <- 1:nrow(predictedVals)

    for( iteration in 1:ncol( predictedVals ) )
    {
        output <- 0

        for( input in 1:nrow( predictedVals ) )
        {
            actual <- actualVals[input]

            predicted <- predictedVals[ input, iteration ]

            output <- output + ( -actual*log( predicted ) + ( 1 - actual )*log( 1-predicted ) )
        }

        #gets the mean
        outputVector[ iteration ] <- output/nrow(predictedVals)
    }

    return( data.frame( iterations, outputVector ) )
}



Sigmoid <- function( values )
{
    output <- matrix(0, nrow(values), ncol(values) )

    for(row in 0:nrow(values) )
    {
        for(col in 0:ncol(values) )
        {
            output[row, col] <- (1/( 1 + exp( -values[row, col] ) ) )
        }
    }

    return(output)
}

Hearts <- function()
{
    file <- LoadFile("Project1/SAheartShort.csv")

    iterationsForRegress <- 150

    stepSize <- 0.1

    matrixForm <- data.matrix(file)

    #remove name, useless data
    matrixForm <- matrixForm[,-1]

    #present = 2, accepted = 1 for famhist, subtracted by one to scale to what the github says
    for( row in 1:nrow( matrixForm ) )
    {
        matrixForm[ row, 5 ] <- matrixForm[ row, 5 ] - 1
    }

    #used to save the output values
    temp <- matrixForm

    matrixForm <- scale(matrixForm)

    for( row in 0:nrow( matrixForm ) )
    {
        matrixForm[row,10] <- temp[row,10]
    }

    #for reproduceable results
    set.seed(1)

    #way to split data into 3 different parts, one on github page didnt work
    #from https://stackoverflow.com/questions/22673335/check-if-each-row-of-a-data-frame-is-contained-in-another-data-frame
    idx <- sample(seq(1, 3), size = nrow(matrixForm), replace = TRUE, prob = c(.6, .2, .2))

    train <- matrixForm[idx == 1,]

    test <- matrixForm[idx == 2,]

    validate <- matrixForm[idx == 3,]

    inputs <- train[,-10]

    outputs <- train[,10]

    predictedOutputs <- GradientDescent(inputs, outputs, stepSize, iterationsForRegress )

    valuePerIteration <- predictedOutputs %*% t(inputs)

    sigmoided <- Sigmoid(valuePerIteration)

    MeanLossPerIter <- MeanLogisticLoss( sigmoided, outputs )

    HeartGraph <- ggplot(MeanLossPerIter, aes(x=iterations, y=outputVector) )



}

test <- Hearts()

