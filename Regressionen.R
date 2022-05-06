###### ein paar statistische Regressionen erstellen
### daten aufbereiten
### aufgrund der basisannahme des zentralen Grenzwertsatzes ist von einer Normalverteilung auszugehen
### post hoc test -> Sch‰tzun empirischer Effektst‰rke
#



# ### Hauptkomponentenanalyse
# ## auch GINI usw. rausnehmen, da viele NAs und die Werte in anderen Idices enthalten sind. So wie GDI, HDI usw.
# Data_prc <- Stat_Data_geo%>%
#   filter(Konflikt== TRUE)%>%
#   filter(Year>2000)
# 
# Data_prc2 <-Data_prc%>%  
#   select(-one_of(c("Ineq_Coef","adjHDI","SE.ENR.PRSC.FM.ZS","SE.ENR.PRIM.FM.ZS","SE.ADT.1524.LT.FM.ZS","SE.ENR.SECO.FM.ZS","SE.ENR.TERT.FM.ZS","IQ.CPA.GNDR.XQ","IQ.CPA.PROP.XQ","SI.POV.GINI","Konflikt","Year","Country.Name.x","Country.Code","classification_core","type", "economy", "continent", "region_un", "subregion","region_wb")))
# 
# Data_prc3 <-Data_prc%>%
#   select(one_of("SP.POP.TOTL.FE.ZS","SP.POP.2529.MA.5Y","SG.GEN.PARL.ZS", "GDI", "HDI","EDI","Im_Ex","total_index_core","CC.EST","GE.EST","RQ.EST","RL.EST","VA.EST","GNI"))
# 
# 
# Data_prc_result <- prcomp(na.omit(Data_prc3),center = TRUE, scale = TRUE) 
# print(Data_prc_result)
# 
# Data_prc_result$rotation <- -1*Data_prc_result$rotation
#  
# Data_prc_result$rotation
# 
# Data_prc_result$x <- -1*Data_prc_result$x
# 
# Data_prc_result$x
# 
# biplot(Data_prc_result, scale=0)
# 
# ## Varianz der Hauptkomponenten ausgeben
# 
# Data_prc_result$sdev^2/sum(Data_prc_result$sdev^2)
# 
# ###Diagramm erstellen
# 
# Var_explaind <- Data_prc_result$sdev^2/sum(Data_prc_result$sdev^2)
# Screenplot_prc <- qplot(c(1:14), Var_explaind) + 
#   geom_line() + 
#   xlab(" Principal Component") + 
#   ylab(" Variance Explained") +
#   ggtitle(" Scree Plot") +
#   ylim(0, 1)
# print(Screenplot_prc)
#Daten vorauswh‰hlen wegen Missing Values

### Korrelation 
str(Data_cor)

Data_cor <- Stat_Data_geo%>%
  select(one_of("Konflikt","SP.DYN.CBRT.IN","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core", "GE.EST"))%>%
  drop_na()

Data_cor$Konflikt<- as.factor(as.numeric(Data_cor$Konflikt))

Data_cor_test <- Stat_Data_geo%>%
  select(one_of("Konflikt","SP.DYN.CBRT.IN","SP.DYN.CDRT.IN","SP.DYN.LE00.IN","Population_secondary_education_25.","NY.GNP.PCAP.PP.CD","SP.POP.GROW","EN.POP.DNST","SH.STA.MMRT","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","mean_years_schooling","equality_dim_index_core","control_dim_index_core","freedom_dim_index_core"))%>% 
  drop_na()
Data_cor_test$Konflikt<-as.numeric(as.factor(Data_cor_test$Konflikt))

cor.test(Data_cor$HDI, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$EDI, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$GDI, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$GII, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$GNI, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$Im_Ex, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$total_index_core, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$equality_dim_index_core, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$freedom_dim_index_core, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$control_dim_index_core, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$Unemployment_M_F, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$SP.POP.2024.MA.5Y, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$SP.POP.2024.FE.5Y, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$SP.POP.2529.MA.5Y, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$SP.POP.2529.FE.5Y, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$SP.POP.TOTL.MA.ZS, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$SP.POP.TOTL.FE.ZS, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$SG.GEN.PARL.ZS, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$exp_years_school, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$exp_years_school_female, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$exp_years_school_male, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$SL.TLF.ACTI.1524.MA.ZS, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$Population_secondary_education_25., Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$Population_secondary_education_male_25., Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$Population_secondary_education_female_25., Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$mean_years_schooling, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$mean_years_schooling_male, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$mean_years_schooling_female, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$Maternal_mortality_ratio, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$life_exp, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$life_exp_male, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$life_exp_female, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$Laborforce_male_15., Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$Laborforce_female_15., Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$Adolescent_birth_rate, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$GNI_per_capita_male, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$GNI_per_capita_female, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$SI.POV.GINI, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$SL.TLF.CACT.FE.ZS, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$SL.TLF.TOTL.FE.ZS, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$SL.TLF.CACT.FM.ZS, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$NY.GDP.MKTP.KN, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$NY.GDP.PCAP.CN, Data_cor$Sum_bd_best, method = "spearman")
cor.test(Data_cor$SE.ENR.SECO.FM.ZS, Data_cor$Sum_bd_best, method = "spearman")

cor.test(Data_cor_test$freedom_dim_index_core, Data_cor_test$Konflikt)
cor.test(Data_cor_test$control_dim_index_core, Data_cor_test$Konflikt)
cor.test(Data_cor_test$equality_dim_index_core, Data_cor_test$Konflikt)
cor.test(Data_cor_test$exp_years_school, Data_cor_test$Konflikt)
#cor.test(Data_cor_test$life_exp, Data_cor_test$Konflikt)
#cor.test(Data_cor_test$GNI, Data_cor_test$Konflikt)## sit ein Index, nochmal den genaueren rausuchen
cor.test(Data_cor_test$Im_Ex, Data_cor$Konflikt)
cor.test(Data_cor_test$NY.GDP.PCAP.CN, Data_cor_test$Konflikt)#GDP
cor.test(Data_cor_test$NE.EXP.GNFS.ZS, Data_cor_test$Konflikt)#Export of goods and servic
cor.test(Data_cor_test$SE.XPD.TOTL.GD.ZS, Data_cor_test$Konflikt)# Ausgaben f¸r Bildung am GDP
cor.test(Data_cor_test$TX.VAL.MMTL.ZS.UN, Data_cor_test$Konflikt)# Ores and metals exports (% of merchandise exports)
cor.test(Data_cor_test$SL.UEM.1524.ZS, Data_cor_test$Konflikt)#Unemployment, youth total (% of total labor force ages 15-24 
#cor.test(Data_cor_test$NY.GNP.PCAP.CD, Data_cor_test$Konflikt)# GNI per Capita
#cor.test(Data_cor_test$SM.POP.TOTL.ZS, Data_cor_test$Konflikt)# internatl migrant stock
cor.test(Data_cor_test$EN.POP.DNST, Data_cor_test$Konflikt)# Bevˆlkerungs Dichte
cor.test(Data_cor_test$SP.POP.GROW, Data_cor_test$Konflikt)# Bevˆlkerungswachstum
cor.test(Data_cor_test$SP.POP.TOTL, Data_cor_test$Konflikt)# Totale Bev.
#cor.test(Data_cor_test$Maternal_mortality_ratio, Data_cor_test$Konflikt)
#cor.test(Data_cor_test$Adolescent_birth_rate, Data_cor_test$Konflikt)
cor.test(Data_cor_test$SG.GEN.PARL.ZS, Data_cor_test$Konflikt)# Frauen im Parlament
cor.test(Data_cor_test$Population_secondary_education_25., Data_cor_test$Konflikt)
cor.test(Data_cor_test$SL.TLF.TOTL.FE.ZS, Data_cor_test$Konflikt)
cor.test(Data_cor_test$SP.POP.2024.MA.5Y, Data_cor_test$Konflikt)
cor.test(Data_cor_test$SH.STA.MMRT, Data_cor_test$Konflikt)
cor.test(Data_cor_test$SP.ADO.TFRT, Data_cor_test$Konflikt)
cor.test(Data_cor_test$SP.DYN.LE00.IN, Data_cor_test$Konflikt)
cor.test(Data_cor_test$NY.GNP.PCAP.CD, Data_cor_test$Konflikt)
cor.test(Data_cor_test$mean_years_schooling, Data_cor_test$Konflikt)
cor.test(Data_cor_test$TX.VAL.FUEL.ZS.UN, Data_cor_test$Konflikt)
cor.test(Data_cor_test$SP.URB.TOTL.IN.ZS, Data_cor_test$Konflikt)
cor.test(Data_cor_test$SP.DYN.CBRT.IN, Data_cor_test$Konflikt)
cor.test(Data_cor_test$SP.DYN.CDRT.IN, Data_cor_test$Konflikt)

##### logit Reg mit den neuen Variablen

Data_cor$Im_Ex<- log(Data_cor$Im_Ex)
Data_cor$NY.GDP.PCAP.CN<- log(Data_cor$NY.GDP.PCAP.CN)
Data_cor$EN.POP.DNST<- log(Data_cor$EN.POP.DNST)
Data_cor$SP.ADO.TFRT<- log(Data_cor$SP.ADO.TFRT)

Formula_logit<- Konflikt~SP.DYN.CBRT.IN+SP.DYN.CDRT.IN+Population_secondary_education_25.+SP.POP.GROW+EN.POP.DNST+SP.URB.TOTL.IN.ZS+SP.ADO.TFRT+Im_Ex+SP.POP.2024.MA.5Y+SL.TLF.TOTL.FE.ZS+SG.GEN.PARL.ZS+SL.UEM.1524.ZS+TX.VAL.MMTL.ZS.UN+TX.VAL.FUEL.ZS.UN+SE.XPD.TOTL.GD.ZS+NY.GDP.PCAP.CN+total_index_core

Data_logit_reg <- glm(Formula_logit, data = Data_cor, family = binomial("logit"))
summary(Data_logit_reg)
print(Data_logit_reg$coefficients)

### auf multikollieratit‰t testen. Vif werte d¸rfen nicht ¸ber 10 liegen. Wenn ja Multikollinearit‰t

vif(Data_logit_reg)
1/vif(Data_logit_reg) # Toleranzwerte m¸ssen ¸ber 0.1 liegen

#korrelationsmatrix
subset_cor <- subset(Data_cor, select = c(SP.DYN.CBRT.IN,SP.DYN.CDRT.IN, Population_secondary_education_25.,  SP.POP.GROW,EN.POP.DNST,SP.URB.TOTL.IN.ZS,SP.ADO.TFRT,Im_Ex,SP.POP.2024.MA.5Y,SL.TLF.TOTL.FE.ZS,SG.GEN.PARL.ZS,SL.UEM.1524.ZS,TX.VAL.MMTL.ZS.UN,TX.VAL.FUEL.ZS.UN,SE.XPD.TOTL.GD.ZS,NY.GDP.PCAP.CN,total_index_core))
cor_tab <- cor(subset_cor, method = "spearman")
cor_tab ## korrelation ¸ber 0.8 bei pearson ist nicht gut

## Auf validit‰t pr¸fen -> Subsets erstellen
test1 <- subset(Data_cor, split="TRUE")
test2 <- subset(Data_cor, split="FALSE")

res <- predict(Data_logit_reg,test1,type="response")
res
res <- predict(Data_logit_reg,test1,type="response")
res

confmatrix <- table(actual_value = test1$Konflikt, Predicted_value= res>0.5)
confmatrix
#accuracy
(confmatrix[[1,1]]+confmatrix[[2,2]])/sum(confmatrix) ### ist ein Vorhersagewert von 88 Prozent


## Omnibus test
glm_chi<- Data_logit_reg$null.deviance- Data_logit_reg$deviance
chiglm <- Data_logit_reg$df.null - Data_logit_reg$df.residual
chiglmsqd <- 1-pchisq(glm_chi,chiglm)

## Odds Ratio
exp(cbind(OR= coef(Data_logit_reg), confint(Data_logit_reg)))

## G¸temaﬂ
n<- length(Data_logit_reg$residuals)
R2cs <- 1-exp((Data_logit_reg$deviance - Data_logit_reg$null.deviance)/n)
R2n <- R2cs/(1-exp(-(Data_logit_reg$null.deviance/n))) ## abw ann ist das gut?



# auch eine lm

Formula_lm <- Konflikt~SP.DYN.LE00.IN+Population_secondary_education_25.+NY.GNP.PCAP.PP.CD+SP.POP.GROW+EN.POP.DNST+SP.URB.TOTL.IN.ZS+SH.STA.MMRT+SP.ADO.TFRT+Im_Ex+SP.POP.2024.MA.5Y+SL.TLF.TOTL.FE.ZS+SG.GEN.PARL.ZS+SL.UEM.1524.ZS+TX.VAL.MMTL.ZS.UN+TX.VAL.FUEL.ZS.UN+SE.XPD.TOTL.GD.ZS+NY.GDP.PCAP.CN+mean_years_schooling+equality_dim_index_core+control_dim_index_core+freedom_dim_index_core
Data_lm_reg <- lm(Formula_lm, data = Data_cor)
summary(Data_lm_reg)

## Daten f¸r die Regression

Data_reg <- Stat_Data_geo%>%
  select(one_of(c("SL.TLF.ACTI.1524.MA.ZS",
                                   "Unemployment_M_F",
                                   "Population_secondary_education_male_25.",
                                   "Population_secondary_education_female_25.",
                                   "Population_secondary_education_25.",
                                   "mean_years_schooling_male",
                                   "mean_years_schooling_female",
                                   "mean_years_schooling",
                                   "Maternal_mortality_ratio",
                                   "life_exp_male",
                                   "life_exp_female",
                                   "life_exp","Laborforce_male_15.",
                                   "Laborforce_female_15.",
                                   "Adolescent_birth_rate",
                                   "GNI_per_capita_male",
                                   "GNI_per_capita_female",
                                   "exp_years_school_female",
                                   "exp_years_school_male",
                                   "exp_years_school", "SE.ENR.SECO.FM.ZS","Sum_bd_best","Sum_bd_low","Sum_bd_high","Konflikt","HDI","GDI","EDI","Im_Ex","GNI","SL.TLF.CACT.FM.ZS","NY.GDP.PCAP.KD.ZG","equality_dim_index_core","control_dim_index_core","freedom_dim_index_core","total_index_core","SP.POP.2024.FE.5Y","SP.POP.2024.MA.5Y","SP.POP.TOTL.MA.ZS","SG.GEN.PARL.ZS","SP.POP.2529.MA.5Y","SP.POP.2529.FE.5Y","CC.EST","PV.EST","GE.EST","RQ.EST","RL.EST","VA.EST","SP.POP.TOTL.FE.ZS")))
summary(Stat_Data_geo)
pairs(Stat_Data_geo) ### Daten kleiner machen. Du gro√ü f√ºr Pairs

# ### Faktoren analyse
# ### geht nur mit daten, die keine NAs haben. 
# data_fak_test <- Stat_Data_geo%>%
#   select(where(is.numeric))%>%
#   drop_na()
# 
# data_fak<- Data_reg%>%
#   select(-one_of("Konflikt", "Sum_bd_best", "Sum_bd_high", "Sum_bd_low"))%>%
#   drop_na()
# describe(data_fak)
# cortest.bartlett(data_fak)
# KMO(data_fak)
# 
# nfactors(data_fak, rotate = "varimax", fm= "mle")
# 
# #Anzahl Fakrotren der Parallelanalyse
# 
# ev<-eigen(cor(data_fak))
# ap <- parallel(subject=nrow(data_fak), var= ncol(data_fak), rep=100, cent = .05)
# nS <- nScree(x=ev$values,aparallel = ap$eigen$qevpea)
# plotnScree(nS)
# ## ML Faktorenanalyse
# fit1 <- factanal(data_fak, 3, rotation = "varimax")
# print(fit1, digits=2, cutoff= .3)



## Formula f√ºr die Rregression vorbereiten lm=liniare multiple Regression, glm=logistisch Bin√§r

Formula_lm <-  Konflikt~SP.POP.TOTL.FE.ZS+
  SP.POP.2529.MA.5Y+
  SG.GEN.PARL.ZS+
  GDI+HDI+EDI+Im_Ex+
  total_index_core+CC.EST+
  GE.EST+RQ.EST+RL.EST+VA.EST+GNI
  

Formula_glm <-  Konflikt~SP.POP.TOTL.FE.ZS+SG.GEN.PARL.ZS+GDI+HDI+EDI+Im_Ex+equality_dim_index_core+control_dim_index_core+freedom_dim_index_core+GNI+SP.POP.2024.MA.5Y+SL.TLF.CACT.FM.ZS

Formula_lm_idi <- Konflikt~SL.TLF.ACTI.1524.MA.ZS+Unemployment_M_F+Population_secondary_education_male_25.+Population_secondary_education_female_25.+Population_secondary_education_25.+mean_years_schooling_male +mean_years_schooling_female + mean_years_schooling +Maternal_mortality_ratio +life_exp_male + life_exp_female + life_exp+ Laborforce_male_15.+Laborforce_female_15.+Adolescent_birth_rate +GNI_per_capita_male+GNI_per_capita_female +exp_years_school_female +exp_years_school_male+exp_years_school+ SG.GEN.PARL.ZS+SP.POP.2529.FE.5Y+ SP.POP.2529.MA.5Y +SP.POP.TOTL.FE.ZS+ SP.POP.TOTL.MA.ZS+ SP.POP.2024.MA.5Y+ SP.POP.2024.FE.5Y +Im_Ex+ freedom_dim_index_core+equality_dim_index_core +control_dim_index_core+ CC.EST + GE.EST+RQ.EST+RL.EST+VA.EST+NY.GDP.PCAP.KD.ZG+SL.TLF.CACT.FM.ZS 

Formula_glm_idi <- Konflikt~SL.TLF.ACTI.1524.MA.ZS+Unemployment_M_F+Population_secondary_education_male_25.+Population_secondary_education_female_25.+Population_secondary_education_25.+mean_years_schooling_male +mean_years_schooling_female + mean_years_schooling +Maternal_mortality_ratio +life_exp_male + life_exp_female + life_exp+ Laborforce_male_15.+Laborforce_female_15.+Adolescent_birth_rate +GNI_per_capita_male+GNI_per_capita_female +exp_years_school_female +exp_years_school_male+exp_years_school+ SG.GEN.PARL.ZS+SP.POP.2529.FE.5Y+ SP.POP.2529.MA.5Y +SP.POP.TOTL.FE.ZS+ SP.POP.2024.MA.5Y+ SP.POP.2024.FE.5Y +Im_Ex+ freedom_dim_index_core+equality_dim_index_core +control_dim_index_core+ CC.EST + GE.EST+RQ.EST+RL.EST+VA.EST+NY.GDP.PCAP.KD.ZG+SL.TLF.CACT.FM.ZS 

Formula_lm_idi_ogen <- Konflikt~SL.TLF.ACTI.1524.MA.ZS+Unemployment_M_F+Population_secondary_education_25. + mean_years_schooling +Maternal_mortality_ratio  + life_exp+Adolescent_birth_rate +GNI +exp_years_school+ SG.GEN.PARL.ZS+SP.POP.2529.FE.5Y+ SP.POP.2529.MA.5Y +SP.POP.TOTL.FE.ZS+ SP.POP.2024.MA.5Y+ SP.POP.2024.FE.5Y +Im_Ex+ freedom_dim_index_core+equality_dim_index_core +control_dim_index_core+ CC.EST + GE.EST+RQ.EST+RL.EST+VA.EST+NY.GDP.PCAP.KD.ZG+SL.TLF.CACT.FM.ZS 
Formula_lm_idi_ogen_sum <- Sum_bd_best~SL.TLF.ACTI.1524.MA.ZS+Unemployment_M_F+Population_secondary_education_25. + mean_years_schooling +Maternal_mortality_ratio  + life_exp+Adolescent_birth_rate +GNI +exp_years_school+ SG.GEN.PARL.ZS+SP.POP.2529.FE.5Y+ SP.POP.2529.MA.5Y +SP.POP.TOTL.FE.ZS+ SP.POP.2024.MA.5Y+ SP.POP.2024.FE.5Y +Im_Ex+ freedom_dim_index_core+equality_dim_index_core +control_dim_index_core+ CC.EST + GE.EST+RQ.EST+RL.EST+VA.EST+NY.GDP.PCAP.KD.ZG+SL.TLF.CACT.FM.ZS 

Formula_lm_idi_gen <- Konflikt~SL.TLF.ACTI.1524.MA.ZS+Unemployment_M_F+Population_secondary_education_male_25.+Population_secondary_education_female_25.+mean_years_schooling_male +mean_years_schooling_female +Maternal_mortality_ratio +life_exp_male + life_exp_female + Laborforce_male_15.+Laborforce_female_15.+Adolescent_birth_rate +GNI_per_capita_male+GNI_per_capita_female +exp_years_school_female +exp_years_school_male+ SG.GEN.PARL.ZS+SP.POP.2529.FE.5Y+ SP.POP.2529.MA.5Y +SP.POP.TOTL.FE.ZS+ SP.POP.TOTL.MA.ZS+ SP.POP.2024.MA.5Y+ SP.POP.2024.FE.5Y +Im_Ex+ freedom_dim_index_core+equality_dim_index_core +control_dim_index_core+ CC.EST + GE.EST+RQ.EST+RL.EST+VA.EST+NY.GDP.PCAP.KD.ZG+SL.TLF.CACT.FM.ZS 

Formula_lm_idi_gen_sum <- Sum_bd_best~SL.TLF.ACTI.1524.MA.ZS+Unemployment_M_F+Population_secondary_education_male_25.+Population_secondary_education_female_25.+mean_years_schooling_male +mean_years_schooling_female +Maternal_mortality_ratio +life_exp_male + life_exp_female + Laborforce_male_15.+Laborforce_female_15.+Adolescent_birth_rate +GNI_per_capita_male+GNI_per_capita_female +exp_years_school_female +exp_years_school_male+ SG.GEN.PARL.ZS+SP.POP.2529.FE.5Y+ SP.POP.2529.MA.5Y +SP.POP.TOTL.FE.ZS+ SP.POP.TOTL.MA.ZS+ SP.POP.2024.MA.5Y+ SP.POP.2024.FE.5Y +Im_Ex+ freedom_dim_index_core+equality_dim_index_core +control_dim_index_core+ CC.EST + GE.EST+RQ.EST+RL.EST+VA.EST+NY.GDP.PCAP.KD.ZG+SL.TLF.CACT.FM.ZS 


###linear Model mit den Indizes
Data_reg_lm<- lm(Formula_lm,data = Data_reg)
Data_reg_glm <- glm(Formula_glm, data = Data_reg, family = binomial("logit"))

summary(Data_reg_lm)
summary(Data_reg_glm)

## lineares Model mit allen Indikatoren


Data_reg_lm_indi <- lm(Formula_lm_idi, data= Data_reg)
Data_reg_glm_indi <- glm(Formula_glm_idi, data = Data_reg, family = binomial("logit"))
summary(Data_reg_lm_indi)
summary(Data_reg_glm_indi)
### lineares model ohne m‰nnlich und weibliche unterschiede


Data_reg_indi_ogen <- lm(Formula_lm_idi_ogen, data= Data_reg)
Data_reg_indi_ogen_sum <- lm(Formula_lm_idi_ogen_sum, data= Data_reg)
summary(Data_reg_indi_ogen)
summary(Data_reg_indi_ogen_sum)

### lineare Modele nur mit den Genderaufteilungen

Data_reg_indi_gen <- lm(Formula_lm_idi_gen, data= Data_reg)
Data_reg_indi_gen_sum <- lm(Formula_lm_idi_gen_sum, data= Data_reg)
summary(Data_reg_indi_gen)
summary(Data_reg_indi_gen_sum)
