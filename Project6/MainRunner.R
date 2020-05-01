# Title     : Main runner for project 6
# Objective : Handles the main pieces of the running
# Created by: Tristan Miller (tem83)
# Created on: 5/1/2020

# tensorflow::install_tensorflow(version="2.1")

library( keras )
library( tensorflow )
library( data.table )
library( ggplot2 )

source("Project6/Model.R")

#acquire data
if(!file.exists("zip.gz") ){
    download.file("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/zip.train.gz", "zip.gz")
}


zip.label <- 1


zip.dt <- data.table::fread("zip.gz")

zip.y <- array(zip.dt[[zip.label]], nrow(zip.dt))

zip.x <- zip.dt[, -zip.label, with=FALSE]


