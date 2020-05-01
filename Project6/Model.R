# Title     : Model
# Objective : Contains all relevant code to project 6's model(s
# Created by: Tristan Miller (tem83)
# Created on: 5/1/2020

RunAlgorithm <- function( x, y, image.x = 16, image.y = 16, num_folds = 5,
                          epochs = 20, validation_split = 0.2)
{
    fold.id <- sample( rep( 1:num_folds, l=nrow(zip.x)))

    output.dt <- data.table()

    for( current_fold in 1:num_folds )
    {
        #determine what is in this fold
        is.fold <- fold.id == current_fold

        x.fold <- x[is.fold, ]

        y.fold <- y[is.fold]

        #split into train and test
        fold.isTrain <- sample( rep( c( TRUE, FALSE ),
                                     prob = c( .8, .2 ),
                                     replace = TRUE,
                                     size=nrow( x.fold ) ) )

        x_train <- x.fold[ fold.isTrain, ]

        y_train <- y.fold[ fold.isTrain ]

        x_test <- x.fold[ !fold.isTrain, ]

        y_train <- y.fold [ fold.isTrain ]
    }

}


BaselineAccuracy <- function( y )
{
    y.table <- table( y )

    mostCommonVal <- max(y.table)

    index <- 0

    for( val in 1:length( y.table ) )
    {
        if( mostCommonVal == y.table[val] )
        {
            index <- val
        }
    }

    accuracy <- mean( y == index - 1 )

}