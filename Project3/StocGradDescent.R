# Title     : Stocastic Gradient Descent
# Objective : NN for Stocastic Gradient Descent
# Created by: Tristan Miller (tem83)
# Created on: 3/6/2020


NNetOneSplit <- function( X.mat, y.vec, max.epochs, step.size, n.hidden.units, is.subtrain )
{
    X.subtrain <- X.mat[is.subtrain, ]

    X.validation <- X.mat[!is.subtrain, ]

    y.subtrain <- y.vec[ is.subtrain ]

    y.validation <- y.vec[ !is.subtrain ]

    n_features <- ncol(X.mat)

    V.mat <- matrix( rnorm( n_features * n.hidden.units, mean = 0, sd = 1), n_features, n.hidden.units )

    w.vec <- rnorm( n.hidden.units )

    loss.values <- data.table()

    #calculate outputs for train and validate data
    trainOutput.epoch0 <- GradientDescent( X.subtrain, V.mat, w.vec )

    validateOutput.epoch0 <- GradientDescent( X.validation, V.mat, w.vec )

    #calculate error for train and validate
    trainError.epoch0 <- LogisticLoss( y.subtrain, trainOutput.epoch0 )

    validateError.epoch0 <- LogisticLoss( y.validation, validateOutput.epoch0 )

    #store values into data.table
    loss.values.mean.epoch0 <- c( mean(trainError.epoch0), mean(validateError.epoch0) )

    loss.values.sd.epoch0 <- c( sd(trainError.epoch0), sd(validateError.epoch0) )

    loss.values.epoch0 <- data.table(epoch=0,
                                     mean.loss.value=loss.values.mean.epoch0,
                                     sd.loss.value=loss.values.sd.epoch0,
                                     dataset=c("train", "validate"))

    loss.values <- rbind(loss.values, loss.values.epoch0)

    for( k in 1:max.epochs )
    {
        #vector use to decide which observation to use
        sampleObservation <- sample( seq_along( y.subtrain ) )

        for( datapoint in seq_along( sampleObservation ) )
        {
            #get the correct observation index
            observation <- sampleObservation[[datapoint]]

            #get inputs and outputs for that value
            observation.X <- X.subtrain[observation,]

            observation.Y <- y.subtrain[observation]

            output <- GradientDescent( observation.X, V.mat, w.vec )

            #back prop starts here

            #calculate loss on outputs
            loss <- LogisticLossDeriv( observation.Y, output )

            #calculate gradient for w.vec
            w.vec.grad <- w.vec%*%loss

            #calculate gradient for V.mat
            V.mat.grad <- V.mat*diag( w.vec.grad )

            #replace w.vec with new w.vec
            w.vec <- w.vec - (w.vec.grad * step.size)

            #replace V.mat with new V.mat
            V.mat <- V.mat - (V.mat.grad * step.size)
        }

        #calculate outputs for train and validate data
        trainOutput <- GradientDescent( X.subtrain, V.mat, w.vec )

        validateOutput <- GradientDescent( X.validation, V.mat, w.vec )

        #calculate error for train and validate
        trainError <- LogisticLoss( y.subtrain, trainOutput )

        validateError <- LogisticLoss( y.validation, validateOutput )

        #store values into data.table
        loss.values.mean <- c( mean(trainError), mean(validateError) )

        loss.values.sd <- c( sd(trainError), sd(validateError) )

        loss.values.epoch <- data.table( epoch=k,
                                         mean.loss.value=loss.values.mean,
                                         sd.loss.value=loss.values.sd,
                                         dataset=c("train", "validate") )

        loss.values <- rbind( loss.values, loss.values.epoch )

        #(10 points) the last thing you should do during each epoch is, after having gone through all the data points in the subtrain set,
        # compute the logistic loss on the subtrain/validation sets, and store those in a variable called loss.values.
        # Remember the logistic loss is: log[1+exp(-yf(x))] where y is a label in {-1,1} and f(x) is a real-valued prediction.
    }

    output <- list(loss.values, V.mat, w.vec)

    return( output )
#(10 points) Outputs should be a list/dictionary/etc with at least three named elements:
    #loss.values, a matrix/data.table/etc which stores the logistic loss with respect to the subtrain/validation set for each epoch.
    #V.mat, the weight matrix after having done gradient descent for the specified number of epochs (max.epochs).
    #w.vec, the weight vector after having done gradient descent for the specified number of epochs (max.epochs).
}

GradientDescent <- function( features, V.mat, w.vec )
{
    calculatedHiddenOutput <- features %*% V.mat

    output <- calculatedHiddenOutput %*% w.vec

    return(output)
}

LogisticLoss <- function( expected, actual )
{
    log(1+exp(-actual*expected))
}

LogisticLossDeriv <- function( expected, actual )
{
    -actual / ( 1+exp( actual*expected ) )
}

CalculateAccuracy <- function( actual, expected )
{
    abs( actual-expected )
}

