# Title     : MainRunner for Project 2
# Objective : Handles the running for project 2
# Created by: Tristan Miller (tem83)
# Created on: 2/20/2020

#check for package installation
if( !require( "data.table" ) ){
    install.packages( "data.table" )
}

#load packages
library( data.table )
library( ggplot2 )
source("Project2/KFoldCV.R")

#acquire data
if(!file.exists("spam.data") ){
    download.file("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data", "spam.data")
}

#temporarily shortened data for testing speed purposes
spam.dt <- data.table::fread("spam.data")[sample(.N, 100)]

#-ncol(spam.dt) because the output is on the last col
data <- as.matrix( spam.dt[ , -ncol( spam.dt ), with=FALSE ] )

#seperates the outputs
outputs <- spam.dt[[ncol( spam.dt )]]

data.scaled <- scale( data )

singleOutput <- NearestNeighborCV( data.scaled, outputs )

resultsSingle <- do.call( rbind, singleOutput[3] )

graphData <- GenerateError( resultsSingle )

#TODO: Figure out how the heck to graph this

#TODO: Add individual folds to graph, make graph less fucky

test1 <- ggplot()+
        geom_ribbon(aes(
        neighbors, ymin=percent.error-sd, ymax=percent.error+sd
        ), alpha=0.5, data=graphData )


