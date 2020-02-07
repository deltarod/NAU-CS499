# Title     : Gradient Descent
# Objective : Project 1 for CS499
# Created by: Tristan Miller (tem83)
# Created on: 2/7/2020

install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

#instantiating e for later use
e <- exp(1)

GradiantDescent <- function(X, Y, stepSize, maxIterations)
{
  weightVector <- rep(0, ncol(X))

  weightMatrix <- matrix(0, , maxIterations)

  for( iterate in 0:(maxIterations-1) )
  {

  }

  return(weightMatrix)
}

ComputeGradient <- function( currentWeightVector, )
{

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
    output <- output + ( -actual*exp( predicted ) - ( 1 - actual )*exp( 1-predicted ) )
  }

  #finding the mean of the outputs
  output <- output / size

  return(output)
}

Sigmoid <- function( value )
{
  output <- 1

  output <- output/(1+e^value)

  return(output)
}

