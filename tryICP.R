
# # source("http://bioconductor.org/biocLite.R")
# # biocLite("RBGL")
# 
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("graph")

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
  select(one_of("region_un","Country.Code","Year","Konflikt","region_wb","Sum_bd_best","HDI","GDI","EDI","Im_Ex","GNI","total_index_core","SG.GEN.PARL.ZS","SP.POP.2529.MA.5Y","CC.EST","GE.EST","RQ.EST","RL.EST","VA.EST","SP.POP.TOTL.FE.ZS"))%>%
  filter(Year>2009)

Data_ICP <- Stat_Data_01_19%>%
  replace_na(list(Sum_bd_best=0))



colSums(is.na(Data_ICP))


X <- Data_ICP%>%
  select(-one_of("Country.Code","Year","Konflikt","region_wb","region_un","Sum_bd_best"))%>%
  as.matrix
str(X)

Y1 <- Data_ICP%>%
  pull(Sum_bd_best)

Y2 <- Data_ICP%>%
  pull(Konflikt)%>%
  as.numeric(as.factor)



ExpInd <- Data_ICP%>%
  pull(region_wb)

ExpInd <- as.numeric(as.factor(ExpInd))%>%
  replace_na(0)

ICP_Y2 <- ICP(X,Y2,Exp_Ind_random)
print(ICP_Y2)
plot(ICP_Y2)

ICP_Y1 <- ICP(X,Y1,Exp_Ind_random)
print(ICP_Y1)
plot(ICP_Y1)


Exp_Ind_random <-floor(runif(1358, min=1, max=3))

hiddenICP2 <- hiddenICP(X,Y2,test)
summary(hiddenICP2)


### ohne fehlende Werte versuchen
Data_ICP_drop_na <- Data_ICP%>%
  drop_na()


X_na <- Data_ICP_drop_na%>%
  select(-one_of("Country.Code","Year","Konflikt","region_wb","region_un","Sum_bd_best"))%>%
  as.matrix

Y1_na <- Data_ICP_drop_na%>%
  pull(Sum_bd_best)

Y2_na <- Data_ICP_drop_na%>%
  pull(Konflikt)%>%
  as.numeric(as.factor)

ExpInd_na <- Data_ICP_drop_na%>%
  pull(region_wb)

ExpInd_na <- as.numeric(as.factor(ExpInd_na))%>%
  replace_na(0)


Exp_Ind_random_na <-floor(runif(2990, min=1, max=3))### wird nicht signifikant. Zu wenig interventions? 


ICP_Y1_na <- ICP(X_na,Y1_na,ExpInd_na)
print(ICP_Y1_na)
plot(ICP_Y1_na)

ICP_Y2_na <- ICP(X_na,Y2_na,ExpInd_na)
print(ICP_Y2_na)
plot(ICP_Y2_na)

hid_ICP_Y1_na<- hiddenICP(X_na,Y1_na,ExpInd_na, alpha=0.1)
print(hid_ICP_Y1_na)
print(hid_ICP)

hid_ICP_Y2_na<- hiddenICP(X_na,Y2_na,ExpInd_na, alpha=0.1)
print(hid_ICP_Y2_na)

print(hid_ICP$betahat)
print(hid_ICP$maximinCoefficients)
cat("true coefficients are:", beta)

nonlinearICP(X_na, Y2_na,ExpInd_na, condIndTest=InvariantResidualDistributionTest)

### hiddenICP funktioniert nicht. aber warum? Was sind genau die Intervention? Welche Variable ist die Intervention? Oder welche Form müss der ExpInd annehmen? 
## Ideen: NAs rausnehmen, aber dann haben wir nur noch einen kleinen Datensatz. Mehrere Spalten erstellen in den die Intervention region enthalten sind. 

##### mal mit der getparents() funktion versuchen? -> braucht für die meisten Funktionen auch einen ExpInd.Andere Funktionen, die Parents ausrechnen kann benötigen Packages, welche für diese Version von R nicht instalierbar sind.

getParents()

getParents(X,parentsOf = 1:ncol(X), method = "regression")



#### mit ein paar graphen rumspielen

#### directed Acylic Graphs

Y <- Data_ICP%>%
  pull(Sum_bd_best)
XG<- Data_ICP%>%
  pull(HDI)
XE <- Data_ICP%>%
  pull(EDI)
  

dagify(Y~~XG)%>%
  ggdag_canonical()
### mur zur visualisierung


### mit dem lavaan package eine SEM durchführen

model<-'
Y=~HDI+GDI+EDI+Im_Ex+GNI+total_index_core+SG.GEN.PARL.ZS+SP.POP.2529.MA.5Y+SP.POP.2529.FE.5Y+CC.EST+PV.EST+GE.EST+RQ.EST+RL.EST+VA.EST+SP.POP.TOTL.FE.ZS

'
fit <- sem(model,data= Data_ICP, meanstructure = TRUE)
summary(fit, standardized = TRUE, fit.measures = TRUE)
coef(fit)

semPaths(fit, what="paths",whatLabels = "par", style = "lisrel", layout = "tree", rotation = 2)
### mit dem SEM Model sollte die Struktur des Graphens vorer klar sein. Ist sie aber nicht. Rechnet aber dennoch Zusammenhänge aus 
 
