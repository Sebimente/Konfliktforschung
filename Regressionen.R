###### ein paar statistische Regressionen erstellen
### daten aufbereiten
### aufgrund der basisannahme des zentralen Grenzwertsatzes ist von einer Normalverteilung auszugehen
### post hoc test -> Sch�tzun empirischer Effektst�rke

Data_reg <- Stat_Data_geo%>%
  select(-one_of(c("geometry","Country.Code","Year","Country.Name.x","classification_core","type", "economy", "continent", "region_un", "subregion","region_wb")))
summary(Stat_Data_geo)
pairs(Stat_Data_geo) ### Daten kleiner machen. Du groß für Pairs

## Formula für die Rregression vorbereiten lm=liniare multiple Regression, glm=logistisch Binär

Formula_lm <-  cbind(Sum_bd_best,Sum_bd_high,Sum_bd_low)~total_index_core+SP.POP.2529.MA.5Y+SP.POP.TOTL.FE.ZS+Im_Ex+Ineq_Coef+adjHDI+GDI+SL.TLF.CACT.FE.ZS+SL.TLF.TOTL.FE.ZS+SL.TLF.CACT.FM.ZS+CC.EST+PV.EST+GE.EST+RQ.EST+RL.EST+VA.EST+HDI+EDI+GNI+Investment

Formula_glm <-  Konflikt~total_index_core+SP.POP.2529.MA.5Y+SP.POP.TOTL.FE.ZS+Im_Ex+Ineq_Coef+adjHDI+GDI+SL.TLF.CACT.FE.ZS+SL.TLF.TOTL.FE.ZS+SL.TLF.CACT.FM.ZS+CC.EST+PV.EST+GE.EST+RQ.EST+RL.EST+VA.EST+HDI+EDI+GNI+Investment

Formula_glm_poly<- Konflikt ~ poly(Im_Ex,3)+ poly(adjHDI,4)+ poly(GDI,4)+poly(CC.EST,2)+ poly(PV.EST,2)+poly(GE.EST,2)+poly(RQ.EST,3)+poly(VA.EST,3)+poly(HDI,4)+poly(EDI,2)+poly(GNI,2)+Investment
### geht nicht 
Data_reg_dropna <-Data_reg%>%
  drop_na()
### warum droppen doch so viele NAs? 


###linear Model
Data_reg_lm<- lm(Formula_lm,data = Data_reg)
Data_reg_glm <- glm(Formula_glm, data = Data_reg, family = binomial("logit"))
Data_reg_poly <- glm(Formula_glm_poly, data = Data_reg_dropna)
summary(Data_reg_lm)
summary(Data_reg_glm)
summary(Data_reg_poly)

