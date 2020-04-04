# neural network for binary classification (TensorFlow/keras)
This project is identical to that of Project 3, however instead of creating our own hidden layer neural network,
we are utilizing keras/tensorflow instead.
 
## Algorithm Code
The code for using Keras can be found [here.](https://github.com/deltarod/NAU-CS499/blob/master/Project4/Model.R)
 
## Requirements
R installed
Rstudio optional but recommended for running.

If using a nvidia graphics card, uncomment the install_tensorflow( version = "nightly-gpu" ) line. Otherwise,
uncomment the install_tensorflow() line.

## Instructions
* Set working directory to the Projects Folder
* run MainRunner.R

## Outputs

This is the graph for training and validation information:

![training and validation data graph](https://github.com/deltarod/NAU-CS499/blob/master/Project4/TrainGraph.png?raw=true)

For 10 hidden units, i got a loss of 0.2667879 and an accuracy of 0.9126087

For 100 units, 0.2503277 and 0.9126087

For 1000 units, 0.2802425 and 0.9

with a baseline accuracy of 0.617123



