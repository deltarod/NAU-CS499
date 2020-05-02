# Title     : Model
# Objective : Contains all relevant code to project 6's model(s
# Created by: Tristan Miller (tem83)
# Created on: 5/1/2020

RunAlgorithms <- function( x, y, num_folds = 5, epochs = 20)
{
    fold.id <- sample( rep( 1:num_folds, l=nrow(zip.x)))

    baseline.dt <- data.table()

    dense.dt <- data.table()

    conv.dt <- data.table()

    deep.dt <- data.table()

    for( current_fold in 1:num_folds )
    {
        print(sprintf("Fold: %d", current_fold))

        #determine what is in this fold
        is.fold <- fold.id == current_fold

        x.fold <- x[is.fold, ]

        y.fold <- y[is.fold]

        is.train <- sample( x = rep( c( TRUE, FALSE ) ), prob = c( .8, .2 ), replace = TRUE, size=nrow( x.fold ) )

        x.train <- x.fold[ is.train, ]
        x.test <- x.fold[ !is.train, ]

        y.train <- y.fold[ is.train ]
        y.test <- y.fold[ !is.train ]

        #convolutional NN run
        conv.output <- setDT(RunConvolutionalModel( x.train, y.train, x.test, y.test, epochs )[2])

        conv.output[, fold := current_fold ]

        conv.output[, alg := "Convolutional"]

        conv.dt <- rbind( conv.dt, conv.output )

        #calculate dense rnn accuracy
        dense.output <- setDT( RunDenseModel(x.train, y.train, x.test, y.test, epochs )[2])

        dense.output[, fold := current_fold ]

        dense.output[, alg := "dense"]

        dense.dt <- rbind( dense.dt, dense.output )


        #calculate baseline accuracy
        baseline.output.dt <- data.table()

        baseline.output.dt[, accuracy := BaselineAccuracy(y.test) ]

        baseline.output.dt[, fold := current_fold ]

        baseline.output.dt[, alg := "baseline"]

        baseline.dt <- rbind( baseline.dt, baseline.output.dt)
    }

    output.dt <- rbind( conv.dt, baseline.dt )

    output.dt <- rbind( output.dt, dense.dt )

    output.dt <- rbind( output.dt, deep.dt )

    return(output.dt)
}

RunDenseModel <- function( x.train, y.train, x.test, y.test, epochs, layers=c(784, 270, 270, 128) )
{
    #change data types so program doesn't get mad anymore
    x.train.convert.dense <- array(
        unlist(x.train[seq_len(nrow(x.train)),-1]),
        c(nrow(x.train), 16, 16, 1))
    y.train.convert.dense <- to_categorical(y.train)

    x.test.convert.dense <- array(
        unlist(x.test[seq_len(nrow(x.test)),-1]),
        c(nrow(x.test), 16, 16, 1))
    y.test.convert.dense <- to_categorical(y.test)

    dense.model.first <- DenseModelGenerate(layers)

    dense.fit.first <- FitModel(dense.model.first, x.train.convert.dense, y.train.convert.dense )

    modelInfo.first <- do.call( data.table::data.table, dense.fit.first$metrics )

    modelInfo.first[, epoch := 1:epochs ]

    best_epoch <- modelInfo.first[which.min(val_loss)]

    train_epoch <- best_epoch[ 1, best_epoch$epoch ]

    model.best <- DenseModelGenerate(layers)

    FitModel( model.best, x.train.convert.dense, y.train.convert.dense, epochs = train_epoch, validation_split = 0.0 )

    result <- EvaluateModel( model.best, x.test.convert.dense, y.test.convert.dense )

}

DenseModelGenerate <- function( layers, num_classes = 10  )
{
    model <- keras_model_sequential() %>%
        layer_flatten(input_shape = c( 16, 16, 1 ))

    for( layer in seq(layers) )
    {
        layer_dense(model, units = layers[layer], activation = 'relu')
    }

    model %>% layer_dense( units = num_classes, activation = 'softmax') %>%
        CompileModel()

    return(model)

}

RunConvolutionalModel <- function( x.train, y.train, x.test, y.test, epochs )
{
    #change data types so program doesn't get mad anymore
    x.train.convert <- array(
        unlist(x.train[seq_len(nrow(x.train)),-1]),
        c(nrow(x.train), 16, 16, 1))
    y.train.convert <- to_categorical(y.train)

    x.test.convert <- array(
        unlist(x.test[seq_len(nrow(x.test)),-1]),
        c(nrow(x.test), 16, 16, 1))
    y.test.convert <- to_categorical(y.test)

    model.first <- ConvolutionalModelGenerate()

    fit.first <- FitModel( model.first, x.train.convert, y.train.convert )

    modelInfo.first <- do.call( data.table::data.table,fit.first$metrics )

    modelInfo.first[, epoch := 1:epochs ]

    best_epoch <- modelInfo.first[which.min(val_loss)]

    train_epoch <- best_epoch[ 1, best_epoch$epoch ]

    model.best <- ConvolutionalModelGenerate()

    FitModel( model.best, x.train.convert, y.train.convert, epochs = train_epoch, validation_split = 0.0 )

    result <- EvaluateModel( model.best, x.test.convert, y.test.convert )
}

#generates a convolutionalModel
ConvolutionalModelGenerate <- function( image.rows = 16, image.columns = 16, num_classes = 10 )
{
    k_clear_session()

    model <- keras_model_sequential() %>%
        layer_conv_2d( filters = 32, kernel_size = c( 3, 3 ), activation = 'relu',
                       input_shape = c( image.rows, image.columns, 1 ) ) %>%
        layer_conv_2d( filters = 64, kernel_size = c( 3, 3 ), activation = 'relu') %>%
        layer_max_pooling_2d( pool_size = c(2,2) ) %>%
        layer_dropout( rate = 0.25 ) %>%
        layer_flatten() %>%
        layer_dense( units = 128, activation = 'relu' ) %>%
        layer_dropout( rate = 0.5 ) %>%
        layer_dense( units = num_classes, activation = 'softmax' ) %>%
        CompileModel()
}


FitModel <- function( model, x.train, y.train, epochs = 20, validation_split = 0.2,  batch_size = 64 )
{
    result <- model %>%
        fit( x.train, y.train,
             batch_size = batch_size,
             epochs = epochs,
             validation_split = validation_split )

    k_clear_session()

    return(result)
}

CompileModel <- function( model )
{
    model %>% compile(loss = loss_categorical_crossentropy,
                      optimizer = optimizer_adadelta(),
                      metrics = c('accuracy') )
}

EvaluateModel <- function( model, x, y )
{
    return( evaluate( model, x, y ) )
}

BaselineAccuracy <- function( y.train )
{
    y.table <- table( y.train )

    mostCommonVal <- max(y.table)

    index <- 0

    for( val in seq_along(y.table) )
    {
        if( mostCommonVal == y.table[val] )
        {
            index <- val
        }
    }

    accuracy <- mean( y.train == index - 1 )

}