# Demonstrating regularization/overfitting
In this project, we will use regularization techniques to determine what to do in regards to training.
We cannot use early stopping in this lab as that is what we have done in the past..
 
## Algorithm Code
Each different regularization paramater can be found [here](https://github.com/deltarod/NAU-CS499/blob/master/Project5/MainRunner.R)

The model definition can be found [here](https://github.com/deltarod/NAU-CS499/blob/master/Project5/Model.R)
 
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

![Hidden Units data graph](https://github.com/deltarod/NAU-CS499/blob/master/Project5/HiddenUnits.png?raw=true)

![Hidden Layers data graph](https://github.com/deltarod/NAU-CS499/blob/master/Project5/HiddenLayers.png?raw=true)

![L2 data graph](https://github.com/deltarod/NAU-CS499/blob/master/Project5/l2.png?raw=true)

![Noise data graph](https://github.com/deltarod/NAU-CS499/blob/master/Project5/Noise.png?raw=true)

![Drop Out data graph](https://github.com/deltarod/NAU-CS499/blob/master/Project5/DropOut.png?raw=true)

![Test Accuracies](https://github.com/deltarod/NAU-CS499/blob/master/Project5/Accuracies.png?raw=true)


