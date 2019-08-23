# 2019_DogMovementCode
Code used for analysis of "Movement patterns of free-roaming dogs on heterogeneous urban landscapes: implications for rabies control"

#Data
This repository has GPS data collected from 23 free-ranging dogs in Arequipa, Peru.
-The raw GPS data is located in "2019_DogMovementCode/Data/RawData" folder
Also included: 
-A file of compiled, cleaned data "2019_DogMovementCode/Data/CleanedData"
    -Erroneous points removed
    -Points recoreded in the car on the way to or from the site removed
-A file of compiled, cleaned data with GPS points adjusted onto the street system 
(ie not in buildings): "2019_DogMovementCode/Data/CleanedData_Streets"
-a file of data with speed between points calculated:
"2019_DogMovementCode/Data/SpeedDF"
-a file with basic dog biological dat (age, sex, district etc): 
"2019_DogMovementCode/Data/SpeedDF"
-a folder of water channels location "2019_DogMovementCode/Data/WaterChannels"

#T_LoCoH
The spatial analysis was done using the t-locoh package. For more information about t-locoh visit: http://tlocoh.r-forge.r-project.org/
-Included in the T-LoCoH folder are the codes we used to run t-locoh "2019_DogMovementCode/T_LoCoH/Code" and the output (parameters, shapefiles, maps) "2019_DogMovementCode/T_LoCoH/Output"

#Analysis
Finally, included in the "Analysis" folder is the code we used to analyze the T-Locoh data. 
Included
-1_Grouper: Used to group dogs based on there usage of the torrenteras
-2_Speed: Analyzes speed data of dogs
-3_MainAnalysis: Runs the statistics for the different groups and calculates distance away from home
-"Output" folder. Contains results from analysis

#Not included
-Data of the dogs houses. These are withheld to protect the identity and privacy of the dogs' owners.




