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

    error_vec <- rep(0.0, length(fold_vec))

    for( k in fold_vec )
    {
        X_new

        y_new

        pred_new <- ComputePredictions(X_train, y_train, X_new)
    }

    return(error_vec)
}

PredictionWrapper <- function( X_train, y_train, X_new )
{

}