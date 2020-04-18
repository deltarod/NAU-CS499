# Title     : Model
# Objective : Contains all relevant code for the training model
# Created by: Tristan Miller (tem83)
# Created on: 4/3/2020


RunModel <- function( train.x, train.y,
                      validation,
                      hiddenUnits = 5, hiddenUnits.train = FALSE,
                      hiddenLayers = 3, hiddenLayers.train = FALSE,
                      l2 = 0, l2.train = FALSE,
                      noise = 0, noise.train = FALSE,
                      dropOut = 0, dropOut.train = FALSE,
                      epochs = 50,
                      subtrain = TRUE )
{
    if( subtrain )
    {
        #check if training hiddenUnits
        if( hiddenUnits.train )
        {
            regularization <- 1:hiddenUnits
        }

        #check if training hiddenLayers
        if( hiddenLayers.train )
        {
            regularization <- 1:hiddenLayers
        }

        if( l2.train )
        {
            regularization <- seq(0, l2, 0.002)
        }

        if( noise.train )
        {
            regularization <- seq(0, noise, 0.08)
        }

        if( dropOut.train )
        {
            regularization <- seq(0, dropOut, 0.08)
        }

        min.dt <- data.table()
    }
    else
    {
        regularization <- 1
    }

    info.dt <- data.table()

    for( regular in regularization )
    {
        #setup the model
        model <- keras_model_sequential()

        if(noise.train)
        {
            layer_gaussian_noise(model, noise )
        }
        if(dropOut.train)
        {
            layer_gaussian_dropout( model, dropOut )
        }

        #loop to setup number of hidden layers
        for( k in {if( hiddenLayers.train ) 1:regular else 1:hiddenLayers} )
        {
            layer_dense( model,
                         input_shape = ncol(train.x),
                         units = { if( hiddenUnits.train ) regular else hiddenUnits },
                         activation = "sigmoid",
                         kernel_regularizer=regularizer_l2(l = { if( l2.train ) regular else l2 } ),
                         use_bias = FALSE )
        }

        layer_dense(model,1, activation = "sigmoid", use_bias = FALSE ) #output layer

        #compile the model with required functions and metrix
        model %>%
            compile(
                loss = "binary_crossentropy",
                optimizer = optimizer_adam(lr=0.01),
                metrics = "accuracy"
            )

        if( subtrain )
            {
            result <- model %>%
                fit(
                    x = train.x,
                    y = train.y,
                    epochs = epochs,
                    validation_data = validation,
                    verbose = 2
                )
        }
        else
            {
            result <- model %>%
                fit(
                    x = train.x,
                    y = train.y,
                    epochs = epochs,
                    validation_split = 0,
                    verbose = 2
                )
        }

        modelInfo <- do.call( data.table::data.table, result$metrics )
        modelInfo[, hiddenUnits := { if(hiddenUnits.train) regular else hiddenUnits }  ]
        modelInfo[, hiddenLayers := { if( hiddenLayers.train) regular else hiddenLayers } ]
        modelInfo[, l2 := { if( l2.train) regular else l2 } ]
        modelInfo[, noise := { if( noise.train) regular else noise } ]
        modelInfo[, dropOut := { if( dropOut.train) regular else dropOut } ]
        modelInfo[, epoch := 1:epochs ]

        if( subtrain )
        {
            min.dt <- rbind( min.dt, modelInfo[which.min(val_loss)] )
        }

        info.dt <- rbind( info.dt, modelInfo )

    }

    if( subtrain )
    {
        return( list( info.dt, min.dt ) )
    }
    else
    {
        return( list( info.dt, model ) )
    }


}

EvaluateModel <- function( model, x, y )
{
    return( evaluate( model, x, y ) )
}

BaselineAccuracy <- function( y )
{
    y.table <- table( y )
    baseline <- { if( y.table[1] > y.table[2] ) 0 else 1 }
    baseline.accuracy <- mean( y == baseline )
}





