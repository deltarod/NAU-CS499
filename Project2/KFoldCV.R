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

    output.dt <- data.table()

    for( k in uniqueFold )
    {
        isValidate <- fold_vec == k

        isTrain <- !isValidate

        X_train <- X_mat[isTrain,]

        y_training <- y_vec[isTrain]

        pred_new <- ComputePredictions( X_train, y_training, X_mat )

        #instead of returning a vector of error_vec's I'm giving myself more information for later
        #contains data for every test in this fold for the specific neighbor
        fold.dt <- data.table(set=ifelse(isTrain, "train", "validation"),
        pred_new,
        fold=k,
        is.error= pred_new != y_vec)

        output.dt <- rbind(output.dt, fold.dt)
    }

    return( output.dt )
}

NearestNeighborCV <- function( X_mat, y_vec, num_folds = 5, max_neighbors = 20 )
{
    #generate fold vector
    validation_fold_vec <- sample( rep( 1:num_folds, l=nrow( X_mat ) ) )

    results.dt <- data.table()

    #calculate most optimal neighbor for 1 through max_neighbors
    for( num_neighbors in 1:max_neighbors )
    {
        knnWrap <- function( X_train, y_train, X_new )
        {
            #I am getting a weird bug here where y_train just goes missing, I don't understand..

            class::knn( X_train, X_new, y_train, k=num_neighbors )
        }

        output.dt <- KFoldCV( X_mat, y_vec, knnWrap, validation_fold_vec )

        #appends the data from that specific num_neighbors to results
        results.dt <- rbind(results.dt, cbind(output.dt, neighbors=num_neighbors ))
    }

    #calculate mean error
    mean_error.dt <- results.dt[, .(percent.error=100*mean(is.error)
    ), by=list(set,neighbors,fold)]

    return(mean_error.dt)

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

GenerateMeanSD <- function( results )
{
    meanSD.dt <- results[, .(
    mean.percent=mean(percent.error),
    sd=sd(percent.error)
    ), by=list(set,neighbors)]
}

RunAlgorithm <- function( algorithm, folds, data, outputs )
{
    if(algorithm == "baseline" )
    {
        algOutput <- Baseline( folds, data, outputs )
    }
    else if(algorithm == "1-NN" )
    {
        output <- NearestNeighborCV( data, outputs, max_neighbors = 1, num_folds = folds )

        accuracy.percent <- output[output$set=="validation"]

        algOutput <- data.table(fold=unique(output$fold), accuracy.percent=accuracy.percent[,percent.error])
    }
    else if(algorithm == "NNCV" )
    {
        algOutput <- NearestNeighborCV(data, outputs, num_folds = folds )

        print(algOutput)

        graphData <- GenerateMeanSD( algOutput )

        min <- graphData[set=="validation"][which.min(mean.percent)]

        numNeighbors <- min$neighbors

        algOutput <- data.table(fold=algOutput[neighbors==numNeighbors&set=="validation",fold], accuracy.percent=algOutput[neighbors==numNeighbors&set=="validation",percent.error] )

        print(algOutput)
    }
    else
    {
        print("invalid Algorithm")
    }

    return(algOutput)
}

Baseline <- function( folds, data, outputs )
{
    counts <- table( outputs )

    baselineVal <- which.max(counts)

    fold_vec <- sample( rep( 1:folds, l=nrow( data ) ) )

    outputTable <- data.table()

    for( fold in unique(fold_vec) )
    {
        isTrain <- fold_vec != fold

        foldOut <- outputs[isTrain]

        outputTable <- rbind(outputTable, cbind(fold, accuracy.percent=mean(foldOut == baselineVal)*100 ) )
    }

    return(outputTable)
}
