# Title     : KFoldCV
# Objective : The file containing the code for the KFoldCV algorithm
# Created by: Tristan Miller (tem83)
# Created on: 2/20/2020

KFoldCV <- function( X_mat, y_vec, ComputePredictions, fold_vec )
{
    if( length( fold_vec ) != length( y_vec ) )
    {
        print("fold_vec and y_vec are not the same size")

        return()
    }

    #unique fold vectors
    uniqueFold <- unique(fold_vec)

    error_vec <- rep(0.0, length(uniqueFold))

    for( k in uniqueFold )
    {
        isValidate <- fold_vec == k

        isTrain <- !isValidate

        X_new <- X_mat[isValidate,]

        y_new <- y_vec[isValidate]

        X_train <- X_mat[isTrain,]

        y_train <- y_vec[isTrain]

        pred_new <- ComputePredictions( X_train, y_train, X_new )

        error_vec[ k ] <- MeanZeroOneLoss( pred_new[1], y_new )
    }

    return( error_vec )
}

NearestNeighborCV <- function( X_mat, y_vec, X_new, num_folds = 5, max_neighbors = 20 )
{
    #generate fold vector
    validation_fold_vec <- sample( rep( 1:num_folds, l=nrow( X_mat ) ) )

    #create matrix for storing error
    error_mat <- matrix(0.0, num_folds, max_neighbors )

    #calculate most optimal neighbor for 1 through max_neighbors
    for( num_neighbors in 1:max_neighbors )
    {
        knnWrap <- function( X_mat, y_vec, X_new )class::knn( X_mat, X_new, y_vec, k=num_neighbors )

        error_mat[,num_neighbors] <- KFoldCV( X_mat, y_vec, knnWrap, validation_fold_vec )
    }

    #calculate mean error
    mean_error_vec <- colMeans( error_mat )

    #get index of minimum error
    best_neighbors <- which.min( mean_error_vec ) + 1

    #calculate prediction for best number of neighbors
    predictions <- class::knn( X_mat, X_new, y_vec, k = best_neighbors )

    list( predictions, mean_error_vec, error_mat )
}

MeanZeroOneLoss <- function( predicted, actual )
{
    return( 1 - mean(predicted == actual ) )
}

GenerateError <- function( results )
{
    meanVals <- colMeans( results )

    sd <- ColSds( results )

    data.table( percent.error = meanVals*100, sd = sd, neighbors=1:ncol(results), test=results )
}

ColSds <- function( matrix )
{
    result <- rep( 0.0, ncol( matrix ) )

    for( col in 1:ncol( matrix ) )
    {
        result[ col ] <- sd( matrix[ ,col ] )
    }

    return( result )
}