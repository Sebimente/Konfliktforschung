###### ein paar statistische Regressionen erstellen
### daten aufbereiten
### aufgrund der basisannahme des zentralen Grenzwertsatzes ist von einer Normalverteilung auszugehen
### post hoc test -> Sch�tzun empirischer Effektst�rke
#


Data_reg <- Stat_Data_geo%>%
  select(one_of(c("Sum_bd_best","Sum_bd_low","Sum_bd_high","Konflikt","HDI","GDI","EDI","Im_Ex","GNI","total_index_core","SG.GEN.PARL.ZS","SP.POP.2529.MA.5Y","SP.POP.2529.FE.5Y","CC.EST","PV.EST","GE.EST","RQ.EST","RL.EST","VA.EST","SP.POP.TOTL.FE.ZS")))
summary(Stat_Data_geo)
pairs(Stat_Data_geo) ### Daten kleiner machen. Du groß für Pairs

### Hauptkomponentenanalyse
## auch GINI usw. rausnehmen, da viele NAs und die Werte in anderen Idices enthalten sind. So wie GDI, HDI usw.
Data_prc <- Stat_Data_geo%>%
  filter(Konflikt== TRUE)%>%
  filter(Year>2009)

Data_prc2 <-Data_prc%>%  
  select(-one_of(c("Ineq_Coef","adjHDI","SE.ENR.PRSC.FM.ZS","SE.ENR.PRIM.FM.ZS","SE.ADT.1524.LT.FM.ZS","SE.ENR.SECO.FM.ZS","SE.ENR.TERT.FM.ZS","IQ.CPA.GNDR.XQ","IQ.CPA.PROP.XQ","SI.POV.GINI","Konflikt","Year","Country.Name.x","Country.Code","classification_core","type", "economy", "continent", "region_un", "subregion","region_wb")))

Data_prc3 <-Data_prc%>%
  select(one_of("SP.POP.TOTL.FE.ZS","SP.POP.2529.MA.5Y","SG.GEN.PARL.ZS", "GDI", "HDI","EDI","Im_Ex","total_index_core","CC.EST","GE.EST","RQ.EST","RL.EST","VA.EST","GNI"))


Data_prc_result <- prcomp(na.omit(Data_prc3),scale. = TRUE) 

Data_prc_result$rotation <- -1*Data_prc_result$rotation
 
Data_prc_result$rotation

Data_prc_result$x <- -1*Data_prc_result$x

Data_prc_result$x

biplot(Data_prc_result,scale=0)

## Varianz der Hauptkomponenten ausgeben

Data_prc_result$sdev^2/sum(Data_prc_result$sdev^2)

###Diagramm erstellen

Var_explaind <- Data_prc_result$sdev^2/sum(Data_prc_result$sdev^2)
Screenplot_prc <- qplot(c(1:14), Var_explaind) + 
  geom_line() + 
  xlab(" Principal Component") + 
  ylab(" Variance Explained") +
  ggtitle(" Scree Plot") +
  ylim(0, 1)

## Formula für die Rregression vorbereiten lm=liniare multiple Regression, glm=logistisch Binär

Formula_lm <-  Konflikt~SP.POP.TOTL.FE.ZS+SP.POP.2529.MA.5Y+SG.GEN.PARL.ZS+GDI+HDI+EDI+Im_Ex+total_index_core+CC.EST+GE.EST+RQ.EST+RL.EST+VA.EST+GNI

Formula_glm <-  Konflikt~SP.POP.TOTL.FE.ZS+SP.POP.2529.MA.5Y+SG.GEN.PARL.ZS+GDI+HDI+EDI+Im_Ex+total_index_core+CC.EST+GE.EST+RQ.EST+RL.EST+VA.EST+GNI

###linear Model
Data_reg_lm<- lm(Formula_lm,data = Data_reg)
Data_reg_glm <- glm(Formula_glm, data = Data_reg, family = binomial("logit"))

summary(Data_reg_lm)
summary(Data_reg_glm)





