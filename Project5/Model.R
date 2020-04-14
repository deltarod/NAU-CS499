# Title     : Model
# Objective : Contains all relevant code for the training model
# Created by: Tristan Miller (tem83)
# Created on: 4/3/2020

GenerateModel <- function( hiddenUnits, data.x, epochs )
    {

    #setup the model
    model <- keras_model_sequential() %>%
        layer_flatten( input_shape = ncol( data.x ) ) %>%
        layer_dense( units = hiddenUnits, activation = "sigmoid", use_bias = FALSE ) %>%
        layer_dense(1, activation = "sigmoid", use_bias = FALSE )

    #compile the model with required functions and metrix
    model %>%
        compile(
            loss = "binary_crossentropy",
            optimizer = "adam",
            metrics = "accuracy"
        )

    return(model)
}

TrainModel <- function( model, data.x, data.y, epochs, validation_split = 0.4 )
{
#begin training the model
    result <- model %>%
        fit(
        x = data.x,
        y = data.y,
        epochs = epochs,
        validation_split = validation_split,
        verbose = 2
        )

    return(result)
}

GenerateModelInfo <- function( result, hiddenUnits, epochs )
{
    modelInfo <- do.call( data.table::data.table, result$metrics )
    modelInfo[, hiddenUnits := hiddenUnits ]
    modelInfo[, epoch := 1:epochs ]
}


RunModel <- function(x, y, validation, hiddenUnits = 5, epochs = 50 )
{
    model <- keras_model_sequential() %>%
        layer_flatten( input_shape = ncol( x ) ) %>%
        layer_dense( units = hiddenUnits, activation = "sigmoid", use_bias = FALSE ) %>%
        layer_dense(1, activation = "sigmoid", use_bias = FALSE )

    model %>%
        compile(
            loss = "binary_crossentropy",
            optimizer = "adam",
            metrics = "accuracy"
        )
    result <- model %>%
        fit(x=data.table(x), y=data.table(y),
            epochs=50,
            validation_data = validation,
            verbose=0)

    return(list(model, result))
}



