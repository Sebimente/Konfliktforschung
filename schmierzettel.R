

install.packages("extrafont")
library(extrafont)


font_import()
loadfonts(device = "win", quiet = TRUE)


fonts()

library(extrafont)
font_import()
loadfonts(device="win")

### Manuelles Ver?ndern, damit die Joins besser funktionieren


Wdi_pivot[Wdi_pivot=="Egypt, Arab Rep."] <- "Egypt"
Wdi_pivot[Wdi_pivot=="Venezuela, RB"] <- "Venezuela"
Governance_pivot[Governance_pivot=="Egypt, Arab Rep."] <- "Egypt"
Governance_pivot[Governance_pivot=="Venezuela, RB"] <- "Venezuela"
ucdp_dpc[ucdp_dpc=="TZB"]<- "TZA"
ucdp_dpc[ucdp_dpc=="Myanmar (Burma)"]<- "Myanmar"
ucdp_dpc[ucdp_dpc=="DR Congo (Zaire)"] <-"Congo, Dem. Rep."
ucdp_dpc[ucdp_dpc== "Congo"] <-"Congo, Rep."
ucdp_dpc[ucdp_dpc== "Russia (Soviet Union)"] <-"Russian Federation"
ucdp_dpc[ucdp_dpc== "Syria"] <-"Syrian Arab Republic"
ucdp_dpc[ucdp_dpc== "United States of America"] <-"United States"
DemocracyMatrix_v4[DemocracyMatrix_v4=="Democratic Republic of Vietnam"] <- "Vietnam."
DemocracyMatrix_v4[DemocracyMatrix_v4=="Democratic Republic of Congo"] <- "Congo, Dem. Rep."
DemocracyMatrix_v4[DemocracyMatrix_v4=="Republic of the Congo"] <- "Congo, Rep"
DemocracyMatrix_v4[DemocracyMatrix_v4=="United States of America"] <- "United States"
DemocracyMatrix_v4[DemocracyMatrix_v4=="Burma/Myanmar"] <- "Myanmar"
DemocracyMatrix_v4[DemocracyMatrix_v4=="Hong Kong"] <- "Hong Kong Sar, China"
DemocracyMatrix_v4[DemocracyMatrix_v4=="Iran"] <- "Iran, Islamic Rep."
DemocracyMatrix_v4[DemocracyMatrix_v4=="Ivory Coast"] <- "C?te d'Ivoire"
DemocracyMatrix_v4[DemocracyMatrix_v4=="Laos"] <- "Laos PDR"
DemocracyMatrix_v4[DemocracyMatrix_v4=="North Korea"] <- "Korea, Dem. People's Rep."
DemocracyMatrix_v4[DemocracyMatrix_v4=="Palestine"] <- "West Bank and Gaza"
DemocracyMatrix_v4[DemocracyMatrix_v4=="Republic of the Congo"] <- "Congo, Rep."
DemocracyMatrix_v4[DemocracyMatrix_v4=="Russia"] <- "Russian Federation"
DemocracyMatrix_v4[DemocracyMatrix_v4=="Syria"] <- "Syrian Arab Republic"
DemocracyMatrix_v4[DemocracyMatrix_v4=="United States of America"] <- "United States"
Human_Development_Index_HDI_[Human_Development_Index_HDI_ == "Eswatini (Kingdom of)"] <- "Eswatini"
Human_Development_Index_HDI_[Human_Development_Index_HDI_ == "Iran (Islamic Republic of)"] <- "Iran, Islamic Rep"
Human_Development_Index_HDI_[Human_Development_Index_HDI_ == "Bolivia (Plurinational State of)"] <- "Bolivia"
Human_Development_Index_HDI_[Human_Development_Index_HDI_ == "Lao People's Democratic Republic"] <- "Lao PDR"
Human_Development_Index_HDI_[Human_Development_Index_HDI_ == "Korea (Republic of)"] <- "Korea, Rep."
Human_Development_Index_HDI_[Human_Development_Index_HDI_ == "Moldova (Republic of)"] <- "Moldova"
Human_Development_Index_HDI_[Human_Development_Index_HDI_ == "Palestine, State of"] <- "West Bank and Gaza"
Human_Development_Index_HDI_[Human_Development_Index_HDI_ == "Tanzania (United Republic of)"] <- "Tanzania"
Human_Development_Index_HDI_[Human_Development_Index_HDI_ == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
Human_Development_Index_HDI_[Human_Development_Index_HDI_ == "Viet Nam"] <- "Vietnam"

Gender_pivot[Gender_pivot == "Eswatini (Kingdom of)"] <- "Eswatini"
Gender_pivot[Gender_pivot == "Iran (Islamic Republic of)"] <- "Iran, Islamic Rep"
Gender_pivot[Gender_pivot == "Bolivia (Plurinational State of)"] <- "Bolivia"
Gender_pivot[Gender_pivot == "Lao People's Democratic Republic"] <- "Lao PDR"
Gender_pivot[Gender_pivot == "Korea (Republic of)"] <- "Korea, Rep."
Gender_pivot[Gender_pivot == "Moldova (Republic of)"] <- "Moldova"
Gender_pivot[Gender_pivot == "Palestine, State of"] <- "West Bank and Gaza"
Gender_pivot[Gender_pivot == "Tanzania (United Republic of)"] <- "Tanzania"
Gender_pivot[Gender_pivot == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
Gender_pivot[Gender_pivot == "Viet Nam"] <- "Vietnam"

Education_pivot[Education_pivot == "Eswatini (Kingdom of)"] <- "Eswatini"
Education_pivot[Education_pivot == "Iran (Islamic Republic of)"] <- "Iran, Islamic Rep"
Education_pivot[Education_pivot == "Bolivia (Plurinational State of)"] <- "Bolivia"
Education_pivot[Education_pivot == "Lao People's Democratic Republic"] <- "Lao PDR"
Education_pivot[Education_pivot == "Korea (Republic of)"] <- "Korea, Rep."
Education_pivot[Education_pivot == "Moldova (Republic of)"] <- "Moldova"
Education_pivot[Education_pivot == "Palestine, State of"] <- "West Bank and Gaza"
Education_pivot[Education_pivot == "Tanzania (United Republic of)"] <- "Tanzania"
Education_pivot[Education_pivot == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
Education_pivot[Education_pivot == "Viet Nam"] <- "Vietnam"

Income_pivot[Income_pivot == "Eswatini (Kingdom of)"] <- "Eswatini"
Income_pivot[Income_pivot == "Iran (Islamic Republic of)"] <- "Iran, Islamic Rep"
Income_pivot[Income_pivot == "Bolivia (Plurinational State of)"] <- "Bolivia"
Income_pivot[Income_pivot == "Lao People's Democratic Republic"] <- "Lao PDR"
Income_pivot[Income_pivot == "Korea (Republic of)"] <- "Korea, Rep."
Income_pivot[Income_pivot == "Moldova (Republic of)"] <- "Moldova"
Income_pivot[Income_pivot == "Palestine, State of"] <- "West Bank and Gaza"
Income_pivot[Income_pivot == "Tanzania (United Republic of)"] <- "Tanzania"
Income_pivot[Income_pivot == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
Income_pivot[Income_pivot == "Viet Nam"] <- "Vietnam"

Invest_pivot[Invest_pivot == "Eswatini (Kingdom of)"] <- "Eswatini"
Invest_pivot[Invest_pivot == "Iran (Islamic Republic of)"] <- "Iran, Islamic Rep"
Invest_pivot[Invest_pivot == "Bolivia (Plurinational State of)"] <- "Bolivia"
Invest_pivot[Invest_pivot == "Lao People's Democratic Republic"] <- "Lao PDR"
Invest_pivot[Invest_pivot == "Korea (Republic of)"] <- "Korea, Rep."
Invest_pivot[Invest_pivot == "Moldova (Republic of)"] <- "Moldova"
Invest_pivot[Invest_pivot == "Palestine, State of"] <- "West Bank and Gaza"
Invest_pivot[Invest_pivot == "Tanzania (United Republic of)"] <- "Tanzania"
Invest_pivot[Invest_pivot == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
Invest_pivot[Invest_pivot == "Viet Nam"] <- "Vietnam"


HDI_GII_EDI_GNI_Dem_Invest%>%
  select(Year) 

#group_by (Land Year) suumarize bd_best... 
Sums <- bind_rows(Landuse_fltr[-1,], Landcover_fltr) %>% 
  summarise(across(Y1993:Y2018, sum, na.rm = TRUE))
            
#listen statt matrix wenn schleifen

            
### option versuchen:
wdi_sf <- left_join(Wdi_pivot,countries_sf, by= c("Country Code" = "iso_a3"))

wdi_Gov<- left_join(wdi_sf,Governance_pivot, by= c("Year", "Country Code"))
view(wdi_Gov)

Wdi_Konflikt <- left_join(wdi_Gov, ucdp_dpc, by = c("Year", "Country Code"))
wdi_Konf_num <- Wdi_Konflikt
wdi_Konf_num$Konflikt<- Wdi_Konflikt$Konflikt%>%
  replace(is.na(Wdi_Konflikt$Konflikt),0)%>%
  as.logical(Wdi_Konflikt$Konflikt)
view(wdi_Konf_num)

Join_HDI <- left_join(wdi_Konf_num,HDI_pivot, by = c("Year", "Country Code"))
Demokratie <- left_join(Join_HDI, DemocracyMatrix_v4, by = c("Country Code","Year"))
HDI_GII_Konf<- left_join(Demokratie,Gender_pivot, by = c("Year", "Country Code"))
HDI_GII_EDI<- left_join(HDI_GII_Konf,Education_pivot, by = c( "Year", "Country Code"))
HDI_GII_EDI_GNI<- left_join(HDI_GII_EDI,Income_pivot, by = c( "Year", "Country Code"))
HDI_GII_EDI_GNI_Dem_Invest_Gov <- left_join(HDI_GII_EDI_GNI,Invest_pivot, by = c("Year","Country Code"))
Data_geo <- HDI_GII_EDI_GNI_Dem_Invest_Gov%>%
  select(one_of(c("Country Name.x", "Year","freedom_dim_index_core","rule_settlement_freedom_core","rights_freedom_core","communication_freedom_core","intermediate_freedom_core","decision_freedom_core","classification_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST","Country Code", "type", "economy", "continent", "region_un", "subregion","region_wb","geometry", "bd_best", "bd_low", "bd_high", "type_of_conflict","Konflikt", "HDI", "GII","EDI","GNI","Investment")))

Data_bd_best<-Data_geo%>%
  group_by(`Country Code`,`Year`)%>%
  summarise(Sum_bd_best=sum(bd_best))

Data_bd_high<-Data_geo%>%
  group_by(`Country Code`,`Year`)%>%
  summarise(Sum_bd_high=sum(bd_high))

Data_bd_low<-Data_geo%>%
  group_by(`Country Code`,`Year`)%>%
  summarise(Sum_bd_low=sum(bd_low))

Death<- inner_join(Data_bd_best,Data_bd_high, by=c("Country Code","Year"))

Death_join<-inner_join(Death,Data_bd_low,by=c("Country Code", "Year"))
view(Death_join)

#### Joinen damit jedes Land pro Jahr nur eine Zahl für Konflikttote hat
Death_wdi <- left_join(wdi_Gov,Death_join, by= c("Country Code","Year"))

Stat_Join_HDI <- left_join(Death_wdi,HDI_pivot, by = c("Year", "Country Code"))
Stat_Demokratie <- left_join(Stat_Join_HDI, DemocracyMatrix_v4, by = c("Country Code","Year"))
Stat_HDI_GII_Konf<- left_join(Stat_Demokratie,Gender_pivot, by = c("Year", "Country Code"))
Stat_HDI_GII_EDI<- left_join(Stat_HDI_GII_Konf,Education_pivot, by = c( "Year", "Country Code"))
Stat_HDI_GII_EDI_GNI<- left_join(Stat_HDI_GII_EDI,Income_pivot, by = c( "Year", "Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov <- left_join(Stat_HDI_GII_EDI_GNI,Invest_pivot, by = c("Year","Country Code"))
Stat_Data_geo <- Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov%>%
  select(one_of(c("Country Name.x","Country Code", "Year","freedom_dim_index_core","rule_settlement_freedom_core","rights_freedom_core","communication_freedom_core","intermediate_freedom_core","decision_freedom_core","classification_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST","Country Code", "type", "economy", "continent", "region_un", "subregion","region_wb","geometry", "Sum_bd_best", "Sum_bd_low", "Sum_bd_high", "HDI", "GII","EDI","GNI","Investment")))

stat.col.num<-c("Year","freedom_dim_index_core","rule_settlement_freedom_core","rights_freedom_core","communication_freedom_core","intermediate_freedom_core","classification_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST", "Sum_bd_best", "Sum_bd_low", "Sum_bd_high", "HDI", "GII","EDI","GNI","Investment")
Stat_Data_geo[stat.col.num]<-sapply(Stat_Data_geo[stat.col.num],as.numeric)


#### eine multivariate Regression
Stat_Data_geo <- Stat_Data_geo%>%
  select(-one_of(c("geometry","Country Code","Year","Country Name.x","classification_core","type", "economy", "continent", "region_un", "subregion","region_wb")))
summary(Stat_Data_geo)
pairs(Stat_Data_geo) ### Daten kleiner machen. Du groß für Pairs

Data_lm <-Stat_Data_geo%>%
  select("freedom_dim_index_core","rule_settlement_freedom_core","rights_freedom_core","communication_freedom_core","intermediate_freedom_core","classification_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST", "Sum_bd_best", "Sum_bd_low", "Sum_bd_high", "HDI", "GII","EDI","GNI","Investment")%>%
  tibble()
##SL.TLF.CACT.FE.ZS+SL.TLF.TOTL.FE.ZS+NY.GDP.MKTP.KN+NY.GDP.PCAP.CN+SE.ADT.1524.LT.FM.ZS+SE.ENR.PRIM.FM.ZS+SE.ENR.PRSC.FM.ZS+SE.ENR.SECO.FM.ZS+SE.ENR.TERT.FM.ZS
  


### Werte für die Regression vorbereiten
Formula_lmRegression <-  cbind(Sum_bd_best,Sum_bd_high,Sum_bd_low)~intermediate_freedom_core+communication_freedom_core+rights_freedom_core+rule_settlement_freedom_core+HDI+GII+EDI+GNI+Investment+freedom_dim_index_core+CC.EST+IQ.CPA.PROP.XQ+IQ.CPA.GNDR.XQ+PV.EST+GE.EST+RQ.EST+RL.EST+VA.EST
Formula_glmRegression <-  Konflikt~SL.TLF.CACT.FE.ZS+SL.TLF.TOTL.FE.ZS+NY.GDP.MKTP.KN+NY.GDP.PCAP.CN+SE.ADT.1524.LT.FM.ZS+SE.ENR.PRIM.FM.ZS+SE.ENR.PRSC.FM.ZS+SE.ENR.SECO.FM.ZS+SE.ENR.TERT.FM.ZS+intermediate_freedom_core+communication_freedom_core+rights_freedom_core+rule_settlement_freedom_core+HDI+GII+EDI+GNI+Investment+freedom_dim_index_core+CC.EST+IQ.CPA.PROP.XQ+IQ.CPA.GNDR.XQ+PV.EST+GE.EST+RQ.EST+RL.EST+VA.EST


###multivaraite Reggresion
Stat_data_lm<- lm(Formula_lmRegression,data = Data_lm)

summary(Stat_data_lm)

vcov(Stat_data_lm)

###logitsische Regression. Wir brauchen wieder die True und False werte
### brauch binäre Werte, wie True und False. Konflikt ja oder Nein

Data_glm <-Data_lm%>%
  transform(Konflikt=Sum_bd_best> 24)

Data_glm["Konflikt"][is.na(Data_glm["konflikt"])] <- FALSE
Data_glm <-Data_glm%>%
replace_na(list(Konflikt = FALSE))

Konflikt_Logit <-glm(Formula_glmRegression,data=Data_glm, family = binomial("logit"), na.action = na.exclude)
summary(Konflikt_Logit)
#### hier kann man evtl. nochmal die Family nach einer anderen Art als logit �berprfen




Formula_glmRegression_test <- cbind(Sum_bd_best,Sum_bd_high,Sum_bd_low)~HDI+GII+Investment+GNI+EDI
Test_lm <- lm(Formula_glmRegression_test, data=Data_lm)
summary(Test_lm)

ggtitel(paste(
  string
))

Labeler <- function(x){
  return(Meta26_11_Excel$`Indicator Name`[which(x==Meta26_11_Excel$`Code`)])
}



  

test<-ggplot(data=Data_geo,aes(IQ.CPA.PROP.XQ, IQ.CPA.GNDR.XQ, color = region_wb))
  
  
  geom_point(position = "jitter")+
  geom_smooth(stat= "smooth")+
  labs(x=Labeler("IQ.CPA.PROP.XQ"), y="Genderequality Rating")
  
  
  
  
  
test <-Data_geo%>%
    filter(Konflikt== TRUE)%>%
    ggplot(aes(IQ.CPA.PROP.XQ, IQ.CPA.GNDR.XQ, color = region_wb))+
    geom_point(position = "jitter")+
    geom_smooth(stat= "smooth")


test+labs(x=Labeler(test[["labels"]][["x"]]), y="Genderequality Rating")


## load the backShift package for data generation and plotting functionality
if(require(backShift) & require(pcalg))
  # Simulate data with connectivity matrix A with assumptions
  # 1) hidden variables present
  # 2) precise location of interventions is assumed unknown
  # 3) different environments can be distinguished
  
  ## simulate data
  myseed <- 1
  
  # sample size n
  n <- 10000
  
  # p=3 predictor variables and connectivity matrix A
  p <- 3
  labels <- c("1", "2", "3")
  A <- diag(p)*0
  A[1,2] <- 0.8
  A[2,3] <- 0.8
  A[3,1] <- -0.4
  
  # divide data in 10 different environments
  G <- 10
  
  # simulate
  simResult <- backShift::simulateInterventions(n, p, A, G, intervMultiplier = 3,
                                                noiseMult = 1, nonGauss = TRUE, hiddenVars = TRUE,
                                                knownInterventions = FALSE, fracVarInt = NULL, simulateObs = TRUE,
                                                seed = myseed)
  X <- simResult$X
  environment <- simResult$environment
