# Overview
Analyzing athletes’ physical attributes across diverse Olympic sports reveals critical insights into the link between physiology and performance. Each sport demands unique traits — gymnastics prioritizes flexibility and strength, while basketball emphasizes height and agility. This analysis informs talent development, training optimization, injury prevention, and athlete well-being.

## Data Description
The dataset encompasses a total of 15 variables, encompassing ID, Name, Sex, Age, Height, Weight, Team, NOC, Games, Year, Season, City, Sport, Event, and Medal. Notably, ID and Name are served as labels and are excluded from the model’s feature set. This collection contains vital information concerning Olympic events.
We also propose a new variable, Body Mass Index (BMI), which is generated from the existing variables Height and Weight, to delve deeper into the examination of physical features.

## Methodologies 
- K-Means: clusters are established utilising selected physical qualities, exposing specific characteristics for each sport within these clusters
- Random Forest: used to determine feature importance
