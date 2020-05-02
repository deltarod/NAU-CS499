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
if(!file.exists("zip.train.gz") ){
    download.file("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/zip.train.gz", "zip.train.gz")
}


zip.label <- 1


zip.dt <- data.table::fread("zip.train.gz")

zip.y <- array(zip.dt[[zip.label]], nrow(zip.dt))

zip.x <- zip.dt[, -zip.label, with=FALSE]

output <- RunAlgorithms( x = zip.x, y = zip.y, num_folds = 5 )

dotPlot <- ggplot() +
    geom_dotplot( aes(x=accuracy, y=alg, color=alg), data=output, binaxis ="y") +
    ggtitle("Comparison of different algorithms accuracies") +
    labs(color="Algorithm")

ggsave("Project6/dotPlot.png", plot = dotPlot)