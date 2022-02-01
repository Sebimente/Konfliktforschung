install.packages("InvariantCausalPrediction")
install.packages("CompareCausalNetworks")
install.packages("backShift")
library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table)
library(readxl)
library(scales)
library(InvariantCausalPrediction)
library(CompareCausalNetworks)

###### neuer beginn
#was wir brauche :

# X	=A matrix (or data frame) with the predictor variables for all experimental settings
#
#
# Y=The response or target variable of interest. Can be numeric for regression or a factor with two levels for binary classification.
#
# ExpInd= Indicator of the experiment or the intervention type an observation belongs to. Can be a numeric vector of the same length as Y with K unique entries if there are K different experiments (for example entry 1 for all observational data and entry 2 for intervention data). Can also be a list, where each element of the list contains the indices of all observations that belong to the corresponding grouping in the data (for example two elements: first element is a vector that contains indices of observations that are observational data and second element is a vector that contains indices of all observations that are of interventional type).
#
# alpha = The level of the test procedure. Use the default alpha=0.1 to obtain 90% confidence intervals.
#
# jahre auf den Zeitraum 2010-2019 Obergrenzen

Stat_Data_01_19 <- Stat_Data_geo%>%
  filter(Year>2009)
Data_ICP <- Stat_Data_01_19
  
  
  
X <- Stat_Data_01_19%>%
  select(-one_of(c("geometry","Country.Code","Year","Country.Name.x","classification_core","type", "economy", "continent", "region_un", "subregion","region_wb")))%>%
  as.matrix()

Y1 <- Stat_Data_01_19$Konflikt
Y2 <- Stat_Data_01_19$Sum_bd_best  
head(Y2)


ExpInd <- Stat_Data_01_19$region_wb
  
hiddenICP <- hiddenICP(X,Y1,ExpInd,alpha = 0.1, mode = "asymptotic") 
  
##### mal mit der getparents() funktion versuchen?

getParents()
