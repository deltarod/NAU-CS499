# Convolutional Neural Networks
In this project, we will use a convolutional neural network to learn how to determine a digit.
 
## Algorithm Code
The main runnner can be found here [here](https://github.com/deltarod/NAU-CS499/blob/master/Project6/MainRunner.R)

The meat can be found [here](https://github.com/deltarod/NAU-CS499/blob/master/Project6/Model.R)
 
## Requirements
R installed
Rstudio optional but recommended for running.

## Instructions
* Set working directory to the Projects Folder
* Install Tensorflow, Keras, data.table, and ggplot2
* run keras::install_keras() (or tensorflow="gpu" if using a nvidia graphics card) in R console, as keras doesn't work with the latest version of tensorflow
* run MainRunner.R
* If error `Error in py_get_attr_impl(x, name, silent) (Model.R#58): AttributeError: module 'kerastools' has no attribute 'progbar'` occurs,
rerun the program once. 

## Outputs

![Accuracies](https://github.com/deltarod/NAU-CS499/blob/master/Project6/dotPlot.png?raw=true)


