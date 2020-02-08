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
    size <- length(predictedVals)

    output <- 0

    #loop for calculating the sum of loss
    for( observation in 0:(size-1) )
        {
        predicted <- predictedVals[observation]

        actual <- actualVals[observation]

        #summation of the outputs
        output <- output + ( -actual*log( predicted ) - ( 1 - actual )*log( 1-predicted ) )
    }

    #finding the mean of the outputs
    output <- output / size

    return(output)
}

Hearts <- function()
    {
    file <- LoadFile("Project1/SAheart.csv")

    matrixForm <- data.matrix(file)

    #remove name, useless data
    matrixForm <- matrixForm[,-1]

    #present = 2, accepted = 1 for famhist, subtracted by one to scale to what the github says
    for( row in 0:( nrow( matrixForm ) - 1 ) )
        {
        matrixForm[ row, 5 ] <- matrixForm[ row, 5 ] - 1
    }

    matrixForm <- scale(matrixForm)

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

    test <- GradientDescent(inputs, outputs, 0.1, 100)

}

Hearts()

