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

spam.dt <- data.table::fread("spam.data")

#-ncol(spam.dt) because the output is on the last col
data <- as.matrix( spam.dt[ , -ncol( spam.dt ), with=FALSE ] )

#seperates the outputs
outputs <- spam.dt[[ncol( spam.dt )]]

data.scaled <- scale( data )

singleOutput <- NearestNeighborCV( data.scaled, outputs )

#line graph for each fold
singleOutputGraph <- ggplot()+
    geom_line(aes(
        neighbors, percent.error, color=set, group=paste(set, fold)),
              data=singleOutput)


graphData <- GenerateMeanSD( singleOutput )

min.dt <- graphData[set=="validation"][which.min(mean.percent)]

#ribbon graph
gg <- ggplot()+
    geom_ribbon(aes(
        neighbors, ymin=mean.percent-sd, ymax=mean.percent+sd, fill=set),
                alpha=0.5,
                data=graphData)+
    geom_line(aes(
        neighbors, mean.percent, color=set),
              data=graphData)+
    geom_point(aes(
        neighbors, mean.percent, color=set),
               data=min.dt)+
    coord_cartesian(xlim=c(0, 25))

algorithmRun <- data.table()

test_fold_vec <- sample( rep( 1:4, l=nrow( data.scaled ) ) )

count <- table( test_fold_vec )

testOutput <- data.table()

for( algorithm in c("baseline", "1-NN", "NNCV"))
{
    numFolds <- 4

    algOutput <- RunAlgorithm( algorithm, numFolds, data.scaled, outputs )

    testOutput <- rbind(testOutput, cbind(algOutput, algorithm) )
}

#dot plot
ggplot()+
    geom_point(aes(accuracy.percent,algorithm),
    data=testOutput)
