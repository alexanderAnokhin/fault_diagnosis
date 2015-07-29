#Failure Pattern Recognition Group Work

##Description
This repository is used for group work case study in the context of the seminar "Aplication of Data Analytics in Spare Parts Supply Chain Management".

##Data
Initial data are taken from Learnweb and transformed. The trasformed data comprises six different settings with three types of failures and three normal states. Each setting is presented in "data/" directory as dataframe and can be loaded into environment using load() function. Format of each dataframe:

1. "sensor_1" - measure from sensor 1;
2. "sensor_2" - measure from sensor 2;
3. "sensor_3" - measure from sensor 3; 
4. "cycle" - corresponding cycle.

Each sensor collects 2048 values every second (every 1/2048 second one value). In each cycle data is collected for around 46 seconds.

##Repository Structure
1. Thesis/ - folder with final thesis written with use of Latex 
2. data/ - folder with initial data for analysis
3. images/ - folder with images of analysis
4. resources/ - addition R scripts for analysis
5. exploratoryAnalysis.R - script with exploratory part of analysis 
6. analysisPart1.R - script with balancing part of analysis
7. analysisPart2.R - script with SVM part os analysis

In order to reproduce full analysis scripts exploratoryAnalysis.R, analysisPart1.R and analysisPart2.R  should be sourced in turn.
