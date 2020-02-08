# Title     : Gradient Descent
# Objective : Handles the GradientDescent Algorithm
# Created by: Tristan Miller (tem83)
# Created on: 2/7/2020



GradientDescent <- function(X, y, stepSize, maxIterations)
{
  weightVector <- rep(0, ncol(X))

  weightMatrix <- matrix(0, maxIterations ,ncol(X) )

  #X and Y same number of rows
  numInputs <- length(y)

  for( iterate in 0:( maxIterations - 1 ) )
  {
    #calculate the output from current weightVector, then subtract actual output
    output <- ( X %*% weightVector ) - y

    #calculate the gradient
    output <- t(X) %*% output

    weightVector <- weightVector - (stepSize/numInputs)*output

    #loop for populating the right weightMatrix part
    for( col in ncol( weightMatrix ) )
    {
      weightMatrix[iterate, col] <- weightVector[col,1]
    }

  }

  return(weightMatrix)
}

CalculateOutput <- function( weightVector, X, rowNum )
{
  currentInputs <- X[rowNum,]

  output <- weightVector %*% currentInputs
}



Sigmoid <- function( value )
{
  output <- 1

  output <- output/( 1+exp( -value ) )

  return(output)
}

