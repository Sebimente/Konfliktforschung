# Die Regionen in 1 und 2 aufgeteilt. South asia, mittlerer Oste und Nord Afrika, Subsahara Afrika sind 2; Europa und zentral Asien, lateinamerika und Karibik, Ost asien und pacifik und Nordamerika sind 2


data_cor_merge <- DataICP_sel_na%>%
  mutate(region_new= region_wb)
data_cor_merge$region_new <- as.numeric(as.factor(data_cor_merge$region_new))

data_cor_merge$region_new<- replace(data_cor_merge$region_new,data_cor_merge$region_new<4,1)
data_cor_merge$region_new<- replace(data_cor_merge$region_new,data_cor_merge$region_new>3,2)

data_cor_merge$Konflikt<-as.numeric(as.factor(data_cor_merge$Konflikt))
data_cor_merge$NY.GDP.PCAP.CN<- log(data_cor_merge$NY.GDP.PCAP.CN)
data_cor_merge$EN.POP.DNST<- log(data_cor_merge$EN.POP.DNST)
data_cor_merge$SP.ADO.TFRT<- log(data_cor_merge$SP.ADO.TFRT)
data_cor_merge$Im_Ex<- log(data_cor_merge$Im_Ex)

# jetzt eube ICP durchführen

# Matrix aus den Prediktor Variablen
X_sel_new<-data_cor_merge%>%
  select(-one_of("Country.Code","Year","Konflikt","region_wb","economy","region_new"))%>%
  as.matrix()
# Konflikte als Zielvariable
Y_sel_new<- data_cor_merge%>%
  pull(Konflikt)
# Regionen der World Bank als Interventionen  
ExpInd_sel_new <- data_cor_merge%>%
  pull(region_new)

#ExpInd_sel_new<- as.numeric(as.factor(ExpInd_sel_new))

hiddenICP(X_sel_new,Y_sel_new,ExpInd_sel_new) ### hier wird doch nichts signifikant

ICP(X_sel_new,Y_sel_new,ExpInd_sel_new)


### jetzt die einzelnen Regressionen durch die beiden environments
#Environment 1
data_cor_merge1 <- Data_cor_sel%>%
  mutate(region_new= region_wb)
data_cor_merge1$region_new <- as.numeric(as.factor(data_cor_merge1$region_new))

data_cor_merge1$region_new<- replace(data_cor_merge1$region_new,data_cor_merge1$region_new<4,1)
data_cor_merge1$region_new<- replace(data_cor_merge1$region_new,data_cor_merge1$region_new>3,2)

data_cor_logit1 <- data_cor_merge1%>%
  filter(region_new== 1)

Data_logit_new1 <- glm(Formula_logit, data_cor_logit1, family = binomial("logit"))
summary(Data_logit_new1)

# Environment 2
data_cor_merge2 <- Data_cor_sel%>%
  mutate(region_new= region_wb)
data_cor_merge2$region_new <- as.numeric(as.factor(data_cor_merge$region_new))

data_cor_merge2$region_new<- replace(data_cor_merge2$region_new,data_cor_merge2$region_new<4,1)
data_cor_merge2$region_new<- replace(data_cor_merge2$region_new,data_cor_merge2$region_new>3,2)

data_cor_logit2 <- data_cor_merge2%>%
  filter(region_new== 2)

Data_logit_new2 <- glm(Formula_logit, data_cor_logit2, family = binomial("logit"))
summary(Data_logit_new2)

#### Cross Validation

Data_logit_reg_cv <- train(Formula_logit, Data_cor_sel, family = binomial("logit"), trControl = trainControl(
  method = "cv", number = 10, verboseIter = TRUE))



Data_cor_osasiepaz <- Stat_Data_geo%>%
  filter(region_un == "Africa")%>%
  select(one_of("Konflikt","Year","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","economy","region_wb"))%>%
  drop_na()

# Konflikt in numerischen Faktor umwandeln und Variablen logarythmieren wegen hoher Spannweite der Werte
Data_cor_osasiepaz$Konflikt<- as.factor(as.numeric(Data_cor_osasiepaz$Konflikt))
Data_cor_osasiepaz$Im_Ex<- log(Data_cor_osasiepaz$Im_Ex)
Data_cor_osasiepaz$NY.GDP.PCAP.CN<- log(Data_cor_osasiepaz$NY.GDP.PCAP.CN)
Data_cor_southasia$EN.POP.DNST<- log(Data_cor_osasiepaz$EN.POP.DNST)
Data_cor_osasiepaz$SP.ADO.TFRT<- log(Data_cor_osasiepaz$SP.ADO.TFRT)

# logit Regression mit Koeffizienten ausgeben

Data_logit_osasiepaz <- glm(Formula_logit, data = Data_cor_osasiepaz, family = binomial("logit"))
summary(Data_logit_osasiepaz)

###### ICP mit den UN Regionen!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!







###################################### Tets

# Noch Konflikte und Jahre rausnehmen.
Type_of_conflict<- Data_geo%>%
  select(one_of("Country Code","Year","type_of_conflict"))%>%
  group_by(`Country Code`, `Year`)%>%
  distinct()

### distinct funktion?
Stat_data_type <- left_join(Stat_Data_geo,Type_of_conflict, by = c("Country.Code"="Country Code","Year"))%>%
  replace_na(list(type_of_conflict = 0))



Data_cor_sel_classification <- Stat_data_type%>%
  select(one_of("SP.DYN.CBRT.IN" ,"Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","Konflikt","type_of_conflict","region_wb","Year","Country.Code","economy","continent"))%>%
  drop_na()%>%
  filter(type_of_conflict!=c(2))%>%
  filter(Year> 2008)


Data_cor_sel_classification$Konflikt<- as.factor(as.numeric(Data_cor_sel_classification$Konflikt))
Data_cor_sel_classification$Im_Ex<- log(Data_cor_sel_classification$Im_Ex)
Data_cor_sel_classification$NY.GDP.PCAP.CN<- log(Data_cor_sel_classification$NY.GDP.PCAP.CN)
Data_cor_sel_classification$EN.POP.DNST<- log(Data_cor_sel_classification$EN.POP.DNST)
Data_cor_sel_classification$SP.ADO.TFRT<- log(Data_cor_sel_classification$SP.ADO.TFRT)

Formula_logit<- Konflikt~SP.DYN.CBRT.IN+Population_secondary_education_25.+SP.POP.GROW+EN.POP.DNST+SP.URB.TOTL.IN.ZS+SP.ADO.TFRT+Im_Ex+SP.POP.2024.MA.5Y+SL.TLF.TOTL.FE.ZS+SG.GEN.PARL.ZS+SL.UEM.1524.ZS+TX.VAL.MMTL.ZS.UN+TX.VAL.FUEL.ZS.UN+SE.XPD.TOTL.GD.ZS+NY.GDP.PCAP.CN+total_index_core


Data_logit_reg <- glm(Formula_logit, data = Data_cor_sel_classification, family = binomial("logit"))
summary(Data_logit_reg)

Data_cor_sel_classification_sel <- Data_cor_sel_classification

na_strings2 <- c("North America")
Data_cor_sel_classification_sel<-Data_cor_sel_classification_sel%>% 
  replace_with_na_at(.vars=c("region_wb"), condition = ~.x %in% na_strings2)

Data_cor_sel_classification_sel$Konflikt<-as.numeric(as.factor(Data_cor_sel_classification_sel$Konflikt))
Data_cor_sel_classification_sel<-Data_cor_sel_classification_sel%>%
  drop_na()

# Matrix aus den Prediktor Variablen
X_sel<-Data_cor_sel_classification_sel%>%
  select(-one_of("Country.Code","Year","Konflikt","region_wb","economy","continent","type_of_conflict"))%>%
  as.matrix()
# Konflikte als Zielvariable
Y_sel<- Data_cor_sel_classification_sel%>%
  pull(Konflikt)
# Regionen der World Bank als Interventionen  
ExpInd_sel_reg <- Data_cor_sel_classification_sel%>%
  pull(region_wb)
ExpInd_sel_reg<- as.numeric(as.factor(ExpInd_sel_reg))


# hidden ICP mit der Region
hidICP_sel<-hiddenICP(X_sel,Y_sel,ExpInd_sel_reg) ### ich hab irgendwas nicht logarithmieert
print(hidICP_sel)
##########################################################################

Data_ICP_sel_piv<- DataICP_sel_na%>%
  pivot_longer(cols = `SP.DYN.CDRT.IN`:`total_index_core`,
               names_to = "Indikatoren",
               names_transform = list(Indikatoren =as.character),
               values_drop_na= TRUE,
               values_to = "Value")



Dataa_cor_gen<-  Stat_Data_geo%>%
  select(one_of("Konflikt","GDI","SP.DYN.CBRT.IN","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core"))%>%
  drop_na()


subset_cor <- subset(Dataa_cor_gen, select = c(GDI,SP.DYN.CDRT.IN, Population_secondary_education_25.,  SP.POP.GROW,EN.POP.DNST,SP.URB.TOTL.IN.ZS,SP.ADO.TFRT,Im_Ex,SP.POP.2024.MA.5Y,SL.TLF.TOTL.FE.ZS,SG.GEN.PARL.ZS,SL.UEM.1524.ZS,TX.VAL.MMTL.ZS.UN,TX.VAL.FUEL.ZS.UN,SE.XPD.TOTL.GD.ZS,NY.GDP.PCAP.CN,total_index_core))
cor_tab <- cor(subset_cor, method = "spearman")
cor_tab
Formula_gen <-Konflikt ~  SP.DYN.CDRT.IN+Population_secondary_education_25.+  SP.POP.GROW+EN.POP.DNST+SP.URB.TOTL.IN.ZS+SP.ADO.TFRT+Im_Ex+SP.POP.2024.MA.5Y+SL.TLF.TOTL.FE.ZS+SG.GEN.PARL.ZS+SL.UEM.1524.ZS+TX.VAL.MMTL.ZS.UN+TX.VAL.FUEL.ZS.UN+SE.XPD.TOTL.GD.ZS+NY.GDP.PCAP.CN+total_index_core
Dataa_cor_gen$Konflikt<- as.factor(as.numeric(Dataa_cor_gen$Konflikt))
Dataa_cor_gen$Im_Ex<- log(Dataa_cor_gen$Im_Ex)
Dataa_cor_gen$NY.GDP.PCAP.CN<- log(Dataa_cor_gen$NY.GDP.PCAP.CN)
Dataa_cor_gen$EN.POP.DNST<- log(Dataa_cor_gen$EN.POP.DNST)
Dataa_cor_gen$SP.ADO.TFRT<- log(Dataa_cor_gen$SP.ADO.TFRT)

Logit_gen <- glm(Formula_gen, data= Dataa_cor_gen, family = binomial("logit"))
summary(Logit_gen)

vif(Logit_gen)
1/vif(Logit_gen)

### 


Dataa_ICP_gen<-  Stat_Data_geo%>%
  select(one_of("Konflikt","GDI","region_wb","Year","Country.Code","economy","GDI","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core"))%>%
  drop_na()



Dataa_ICP_gen_na <- Dataa_ICP_gen%>%
  drop_na()

Dataa_ICP_gen_na$Konflikt<-as.numeric(as.factor(Dataa_ICP_gen_na$Konflikt))
Dataa_ICP_gen_na$NY.GDP.PCAP.CN<- log(Dataa_ICP_gen_na$NY.GDP.PCAP.CN)
Dataa_ICP_gen_na$EN.POP.DNST<- log(Dataa_ICP_gen_na$EN.POP.DNST)
Dataa_ICP_gen_na$SP.ADO.TFRT<- log(Dataa_ICP_gen_na$SP.ADO.TFRT)

# Matrix aus den Prediktor Variablen
X_sel_gen<-Dataa_ICP_gen_na%>%
  select(-one_of("Country.Code","Year","Konflikt","region_wb","economy"))%>%
  as.matrix()
# Konflikte als Zielvariable
Y_sel_gen<- Dataa_ICP_gen_na%>%
  pull(Konflikt)
# Regionen der World Bank als Interventionen  
ExpInd_gen <- Dataa_ICP_gen_na%>%
  pull(region_wb)
ExpInd_gen<- as.numeric(as.factor(ExpInd_gen))


# hidden ICP mit der Region
hidICP_sel_gen<-hiddenICP(X_sel_gen,Y_sel_gen,ExpInd_gen)












stargazer(Data_logit_reg, titel = "Invarian Causal Projection mit versteckten Variablen", style = "default",decimal.mark = ",",
          out = "ICP.html")


test<-Data_geo%>%
  filter(classification_core== "Working Democracy")%>%
  filter(Konflikt==T)

data_cor_demo<- Stat_Data_geo%>%
  select(one_of("Country.Code","Year","Konflikt", "equality_dim_index_core","control_dim_index_core","freedom_dim_index_core"))

session_info()


Formulr_test_cor <- Konflikt~SP.DYN.LE00.IN+SP.DYN.CBRT.IN+SP.DYN.CDRT.IN+Population_secondary_education_25.+NY.GNP.PCAP.PP.CD+SP.POP.GROW+EN.POP.DNST+SP.URB.TOTL.IN.ZS+SH.STA.MMRT+SP.ADO.TFRT+Im_Ex+SP.POP.2024.MA.5Y+SL.TLF.TOTL.FE.ZS+SG.GEN.PARL.ZS+SL.UEM.1524.ZS+TX.VAL.MMTL.ZS.UN+TX.VAL.FUEL.ZS.UN+SE.XPD.TOTL.GD.ZS+NY.GDP.PCAP.CN+equality_dim_index_core+freedom_dim_index_core

glmt_tesdt_cor <-  glm(Formulr_test_cor, data = Data_cor, family = binomial("logit"))
summary(glmt_tesdt_cor)


#### Mit den Variablen, die unabhängig voneinander sind
Data_cor_multikor <- Stat_Data_geo%>%
  select(one_of("Konflikt","SP.DYN.CBRT.IN","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","GE.EST"))%>% 
  drop_na()

Data_cor_multikor$Konflikt<- as.factor(as.numeric(Data_cor_multikor$Konflikt))

Data_cor_multikor$Im_Ex<- log(Data_cor_multikor$Im_Ex)
Data_cor_multikor$NY.GDP.PCAP.CN<- log(Data_cor_multikor$NY.GDP.PCAP.CN)
Data_cor_multikor$EN.POP.DNST<- log(Data_cor_multikor$EN.POP.DNST)
Data_cor_multikor$SP.ADO.TFRT<- log(Data_cor_multikor$SP.ADO.TFRT)

Formula_multikor <-Konflikt~SP.DYN.CBRT.IN+SP.DYN.CDRT.IN+Population_secondary_education_25.+SP.POP.GROW+EN.POP.DNST+SP.URB.TOTL.IN.ZS+SP.ADO.TFRT+Im_Ex+SP.POP.2024.MA.5Y+SL.TLF.TOTL.FE.ZS+SG.GEN.PARL.ZS+SL.UEM.1524.ZS+TX.VAL.MMTL.ZS.UN+TX.VAL.FUEL.ZS.UN+SE.XPD.TOTL.GD.ZS+NY.GDP.PCAP.CN+total_index_core+GE.EST


glm_multicor <-  glm(Formula_multikor, data = Data_cor_multikor, family = binomial("logit"))
summary(glm_multicor)




subset_stat_data_geo <- subset(Stat_Data_geo, select= c(CC.EST, GE.EST,RQ.EST,RL.EST,VA.EST,total_index_core))%>%
  drop_na()
cor_gov<- cor(subset_stat_data_geo)
cor_gov

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

#### Joinen damit jedes Land pro Jahr nur eine Zahl fÃ¼r Konflikttote hat
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
pairs(Stat_Data_geo) ### Daten kleiner machen. Du groÃŸ fÃ¼r Pairs

Data_lm <-Stat_Data_geo%>%
  select("freedom_dim_index_core","rule_settlement_freedom_core","rights_freedom_core","communication_freedom_core","intermediate_freedom_core","classification_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST", "Sum_bd_best", "Sum_bd_low", "Sum_bd_high", "HDI", "GII","EDI","GNI","Investment")%>%
  tibble()
##SL.TLF.CACT.FE.ZS+SL.TLF.TOTL.FE.ZS+NY.GDP.MKTP.KN+NY.GDP.PCAP.CN+SE.ADT.1524.LT.FM.ZS+SE.ENR.PRIM.FM.ZS+SE.ENR.PRSC.FM.ZS+SE.ENR.SECO.FM.ZS+SE.ENR.TERT.FM.ZS
  


### Werte fÃ¼r die Regression vorbereiten
Formula_lmRegression <-  cbind(Sum_bd_best,Sum_bd_high,Sum_bd_low)~intermediate_freedom_core+communication_freedom_core+rights_freedom_core+rule_settlement_freedom_core+HDI+GII+EDI+GNI+Investment+freedom_dim_index_core+CC.EST+IQ.CPA.PROP.XQ+IQ.CPA.GNDR.XQ+PV.EST+GE.EST+RQ.EST+RL.EST+VA.EST
Formula_glmRegression <-  Konflikt~SL.TLF.CACT.FE.ZS+SL.TLF.TOTL.FE.ZS+NY.GDP.MKTP.KN+NY.GDP.PCAP.CN+SE.ADT.1524.LT.FM.ZS+SE.ENR.PRIM.FM.ZS+SE.ENR.PRSC.FM.ZS+SE.ENR.SECO.FM.ZS+SE.ENR.TERT.FM.ZS+intermediate_freedom_core+communication_freedom_core+rights_freedom_core+rule_settlement_freedom_core+HDI+GII+EDI+GNI+Investment+freedom_dim_index_core+CC.EST+IQ.CPA.PROP.XQ+IQ.CPA.GNDR.XQ+PV.EST+GE.EST+RQ.EST+RL.EST+VA.EST


###multivaraite Reggresion
Stat_data_lm<- lm(Formula_lmRegression,data = Data_lm)

summary(Stat_data_lm)

vcov(Stat_data_lm)

###logitsische Regression. Wir brauchen wieder die True und False werte
### brauch binÃ¤re Werte, wie True und False. Konflikt ja oder Nein

Data_glm <-Data_lm%>%
  transform(Konflikt=Sum_bd_best> 24)

Data_glm["Konflikt"][is.na(Data_glm["konflikt"])] <- FALSE
Data_glm <-Data_glm%>%
replace_na(list(Konflikt = FALSE))

Konflikt_Logit <-glm(Formula_glmRegression,data=Data_glm, family = binomial("logit"), na.action = na.exclude)
summary(Konflikt_Logit)
#### hier kann man evtl. nochmal die Family nach einer anderen Art als logit überprfen




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

  
  
  ProzentualChange <- function(x,y){
    x <- x[order(x$Year, dercreasing=TRUE),]
    pct_change <- -diff((x$y))/x$y[-1]*100
    data.frame(Year= x$Year[-length(x$Year)], pct_change=pct_change)
  }
  
  

is.na(Stat_Data_geo)  
# Stat_Data_geo%>%
#   select(where(is.numeric))%>%
#   filter(Year>2009)%>%
#   colSums(is.na(Stat_Data_geo))
# 
# colSums(is.na(DemocracyMatrix_v4))

# ### hiddenICP funktioniert nicht. aber warum? Was sind genau die Intervention? Welche Variable ist die Intervention? Oder welche Form müss der ExpInd annehmen? 
# ## Ideen: NAs rausnehmen, aber dann haben wir nur noch einen kleinen Datensatz. Mehrere Spalten erstellen in den die Intervention region enthalten sind. 
# 
# ##### mal mit der getparents() funktion versuchen? -> braucht für die meisten Funktionen auch einen ExpInd.Andere Funktionen, die Parents ausrechnen kann benötigen Packages, welche für diese Version von R nicht instalierbar sind.
# 
# getParents()
# 
# getParents(X,parentsOf = 1:ncol(X), method = "regression")
# 
# 
# 
# #### mit ein paar graphen rumspielen
# 
# #### directed Acylic Graphs
# 
# Y <- Data_ICP%>%
#   pull(Sum_bd_best)
# XG<- Data_ICP%>%
#   pull(HDI)
# XE <- Data_ICP%>%
#   pull(EDI)
#   
# 
# dagify(Y~~XG)%>%
#   ggdag_canonical()
# ### mur zur visualisierung
# 
# 
# ### mit dem lavaan package eine SEM durchführen
# 
# model<-'
# Y=~HDI+GDI+EDI+Im_Ex+GNI+total_index_core+SG.GEN.PARL.ZS+SP.POP.2529.MA.5Y+SP.POP.2529.FE.5Y+CC.EST+PV.EST+GE.EST+RQ.EST+RL.EST+VA.EST+SP.POP.TOTL.FE.ZS
# 
# '
# fit <- sem(model,data= Data_ICP, meanstructure = TRUE)
# summary(fit, standardized = TRUE, fit.measures = TRUE)
# coef(fit)
# 
# semPaths(fit, what="paths",whatLabels = "par", style = "lisrel", layout = "tree", rotation = 2)
### mit dem SEM Model sollte die Struktur des Graphens vorer klar sein. Ist sie aber nicht. Rechnet aber dennoch Zusammenhänge aus 


install.packages("VIM")
install.packages("FactoMineR")
install.packages("missMDA")
install.packages("naniar")
library(missMDA)
library(VIM)
library(FactoMineR)
library(naniar)

gg_miss_var(Stat_Data_geo)
res<-summary(aggr(Stat_Data_geo, sortVar=TRUE))$combinatio

head(res[rev(order(res[,2])),])
matrixplot(Stat_Data_geo, sortby = 2)

marginplot(Stat_Data_geo[,c("economy","region_wb")])

na_var_eco <- Stat_Data_geo%>%
  filter(Year>2009)%>%
  group_by(economy)%>%
  miss_var_summary()%>%
  pivot_wider(names_from = economy,values_from = c("n_miss","pct_miss"))

na_var_reg <- Stat_Data_geo%>%
  filter(Year>2000)%>%
  group_by(region_wb)%>%
  miss_var_summary()%>%
  pivot_wider(names_from = region_wb,values_from = c("n_miss","pct_miss"))


