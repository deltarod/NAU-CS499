# Title     : TODO
# Objective : TODO
# Created by: craps
# Created on: 2/20/2020

## install package if it is not already.
if(!require("data.table")){
    install.packages("data.table")
}


## attach all functions provided by these packages.
library(data.table)
library(ggplot2)
source("Project2/KFoldCV.R")

## download spam data set to local directory, if it is not present.
if(!file.exists("spam.data")){
    download.file("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data", "spam.data")
}

## Read spam data set and conver to X matrix and y vector we need for
## gradient descent.
spam.dt <- data.table::fread("spam.data")
N.obs <- nrow(spam.dt)
X.raw <- as.matrix(spam.dt[, -ncol(spam.dt), with=FALSE])
y.vec <- spam.dt[[ncol(spam.dt)]]
X.sc <- scale(X.raw) #scaled X/feature/input matrix.

## compute and visualize validation error as a function of number of
## neighbors.
result <- NearestNeighborCV(X.sc, y.vec)
ggplot()+
    geom_line(aes(
        neighbors, error.percent, group=validation.fold),
              data=validation.error)

## compute and visualize test error results:
err.dt.list <- list()
## assign folds.
for(test.fold in 1:5){
    ## split into train/test sets.
    for(algorithm in c("baseline", "1-NN", "NNCV")){
        ## run algorithm and store test error.
        err.dt.list[[paste(test.fold, algorithm)]] <- data.table(
            test.fold, algorithm, error.percent)
    }
}
err.dt <- do.call(rbind, err.dt.list)

ggplot()+
    geom_point(aes(
        error.percent, algorithm),
               data=err.dt)