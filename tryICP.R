
Data_ICP_sel <- Stat_Data_geo%>%
  select(one_of("SP.DYN.CBRT.IN","SP.DYN.CDRT.IN" ,"Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","Konflikt","region_wb","subregion","Year","Country.Code","continent","economy"))
  
#"SP.POP.TOTL","NE.EXP.GNFS.ZS", "SP.DYN.LE00.IN" hat einen stark kausalenzusammenhang


### naS??
gg_miss_var(Data_ICP_sel)
res<-summary(aggr(Data_ICP_sel, sortVar=TRUE))$combinatio

head(res[rev(order(res[,2])),])
matrixplot(Data_ICP_sel, sortby = 2)

marginplot(Data_ICP_sel[,c("economy","region_wb")])

na_data_ICP_sel <- Data_ICP_sel%>%
  filter(Year>2000)%>%
  miss_var_summary()
na_data_ICP_sel_year <- Data_ICP_sel%>%
  filter(Year>2000)%>%
  group_by(Year)%>%
  miss_var_summary()

### jetzt ICP

DataICP_sel_na <- Data_ICP_sel%>%
  drop_na()

DataICP_sel_na$Konflikt<-as.numeric(as.factor(DataICP_sel_na$Konflikt))
DataICP_sel_na$NY.GDP.PCAP.CN<- log(DataICP_sel_na$NY.GDP.PCAP.CN)
DataICP_sel_na$EN.POP.DNST<- log(DataICP_sel_na$EN.POP.DNST)
DataICP_sel_na$SP.ADO.TFRT<- log(DataICP_sel_na$SP.ADO.TFRT)

X_sel<-DataICP_sel_na%>%
  select(-one_of("Country.Code","Year","Konflikt","region_wb","continent","economy","subregion"))%>%
  as.matrix()

Y_sel<- DataICP_sel_na%>%
  pull(Konflikt)
  
ExpInd_sel_reg <- DataICP_sel_na%>%
  pull(region_wb)
ExpInd_sel_reg<- as.numeric(as.factor(ExpInd_sel_reg))

ExpInd_sel_eco <- DataICP_sel_na%>%
  pull(economy)
ExpInd_sel_eco<- as.numeric(as.factor(ExpInd_sel_eco))  

hidICP_sel<-hiddenICP(X_sel,Y_sel,ExpInd_sel_reg)

print(hidICP_sel)
print(hidICP_sel$betahat)
print(hidICP_sel$maximinCoefficients)

hidICP_sel_eco <- hiddenICP(X_sel,Y_sel,ExpInd_sel_eco)
print(hidICP_sel_eco)

#### eine normale ICP
Y_sel_t <- DataICP_sel_na%>%
  pull(Konflikt)%>%
  as.factor

ICP_normal<-ICP(X_sel,Y_sel_t, ExpInd_sel_reg, alpha= 0.1)
print(ICP_normal)


######
Data_ICP <- Stat_Data_geo%>%
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
                  "exp_years_school","Sum_bd_best","continent","Konflikt","subregion","economy","HDI","GDI","EDI","Im_Ex","GNI","SL.TLF.CACT.FM.ZS","NY.GDP.PCAP.KD.ZG","equality_dim_index_core","control_dim_index_core","freedom_dim_index_core","total_index_core","SP.POP.2024.FE.5Y","SP.POP.2024.MA.5Y","SP.POP.TOTL.MA.ZS","SG.GEN.PARL.ZS","SP.POP.2529.MA.5Y","SP.POP.2529.FE.5Y","CC.EST","PV.EST","GE.EST","RQ.EST","RL.EST","VA.EST","SP.POP.TOTL.FE.ZS","region_un","Country.Code","Year","Konflikt","region_wb")))%>%
  filter(Year>2009)%>%
  replace_na(list(Sum_bd_best=0))

Data_ICP$Konflikt<-as.factor(Data_ICP$Konflikt)




####### mit NAs

Data_ICP_1 <- Data_ICP%>%
  select(one_of("SP.POP.TOTL.FE.ZS","subregion","continent","SG.GEN.PARL.ZS","GDI","HDI","EDI","Im_Ex","equality_dim_index_core","control_dim_index_core","freedom_dim_index_core","GNI","SP.POP.2024.MA.5Y","SL.TLF.CACT.FM.ZS","region_un","Country.Code","Year","Konflikt","region_wb","Sum_bd_best", "Konflikt"))

Data_ICP_1_drop_na <- Data_ICP_1%>%
  drop_na()

X_na_1 <- Data_ICP_1_drop_na%>%
  select(-one_of("Country.Code","Year","Konflikt","region_wb","region_un","Sum_bd_best","continent"))%>%
  as.matrix

Y1_na_1 <- Data_ICP_1_drop_na%>%
  pull(Sum_bd_best)

Y2_na_1 <- Data_ICP_1_drop_na%>%
  pull(Konflikt)%>%
  as.numeric(as.factor)

ExpInd_na_1 <- Data_ICP_1_drop_na%>%
  pull(region_wb)

ExpInd_na_1 <- as.numeric(as.factor(ExpInd_na_1))%>%
  replace_na(0)

hiddenICP(X_na_1,Y1_na_1,ExpInd_na_1)

#### was wenn mir Jahren weil Region einen Einfluss hat

ExpInd_na_1_Year <- Data_ICP_1_drop_na%>%
  pull(Year)
ExpInd_na_1_Year <- 

hiddenICP(X_na_1,Y1_na_1,ExpInd_na_1_Year)

### ohne fehlende Werte versuchen mit aufgeteilter demokratie
Data_ICP_Indizes <- Data_ICP%>%
  select(one_of("HDI","GDI","EDI","Im_Ex","GNI","Konflikt","equality_dim_index_core","control_dim_index_core","freedom_dim_index_core","SP.POP.2024.MA.5Y","SG.GEN.PARL.ZS","SP.POP.TOTL.FE.ZS","region_un","Country.Code","Year","Konflikt","region_wb","Sum_bd_best"))


Data_ICP_Indeizes_drop_na <- Data_ICP_Indizes%>%
  drop_na()


X_na <- Data_ICP_Indeizes_drop_na%>%
  select(-one_of("Country.Code","Year","Konflikt","region_wb","region_un","Sum_bd_best"))%>%
  as.matrix

Y1_na <- Data_ICP_Indeizes_drop_na%>%
  pull(Sum_bd_best)

Y2_na <- Data_ICP_Indeizes_drop_na%>%
  pull(Konflikt)%>%
  as.numeric(as.factor)

ExpInd_na <- Data_ICP_Indeizes_drop_na%>%
  pull(region_wb)

ExpInd_na <- as.numeric(as.factor(ExpInd_na))%>%
  replace_na(0)


#Exp_Ind_random_na <-floor(runif(2990, min=1, max=3))### wird nicht signifikant. Zu wenig interventions? 


hiddenICP(X_na,Y1_na,ExpInd_na)

hiddenICP(X_na,Y2_na,ExpInd_na)



## hidden ICP  mit Indikatoren anstatt der Indizes

Data_ICP_ind <- Stat_Data_geo%>%
  select(one_of(c("SL.TLF.ACTI.1524.MA.ZS",
                  "Population_secondary_education_25.",
                  "mean_years_schooling",
                  "Maternal_mortality_ratio",
                  "life_exp","Adolescent_birth_rate",
                  "exp_years_school","Sum_bd_best","Konflikt","Im_Ex","GNI","SL.TLF.CACT.FM.ZS","equality_dim_index_core","control_dim_index_core","freedom_dim_index_core","SP.POP.2024.MA.5Y","SP.POP.TOTL.FE.ZS","SG.GEN.PARL.ZS","region_un","Country.Code","Year","Konflikt","region_wb","Sum_bd_best")))%>%
  filter(Year>2009)%>%
  replace_na(list(Sum_bd_best=0))

Data_ICP_ind$Konflikt<- as.factor(Data_ICP_ind$Konflikt)

drop_na_DataICP_ind <-Data_ICP_ind %>%
  drop_na()

X_na_ind <- drop_na_DataICP_ind%>%
  select(-one_of("Country.Code","Year","Konflikt","region_wb","region_un","Sum_bd_best"))%>%
  as.matrix

Y1_na_ind <- drop_na_DataICP_ind%>%
  pull(Sum_bd_best)

Y2_na_ind <- drop_na_DataICP_ind%>%
  pull(Konflikt)%>%
  as.numeric(as.factor)

ExpInd_na_ind <- drop_na_DataICP_ind%>%
  pull(region_wb)

ExpInd_na_ind <- as.numeric(as.factor(ExpInd_na_ind))%>%
  replace_na(0)

hiddenICP(X_na_ind,Y1_na_ind,ExpInd_na_ind)
hiddenICP(X_na_ind,Y2_na_ind,ExpInd_na_ind)

### hidden ICP  mit den Indicatoren auf Gender angepasst
Data_ICP_gen <-Stat_Data_geo%>%
  select(one_of(c("Population_secondary_education_male_25.",
                  "Population_secondary_education_female_25.",
                  "SL.TLF.ACTI.1524.MA.ZS",
                  "Adolescent_birth_rate",
                  "GNI_per_capita_male",
                  "GNI_per_capita_female",
                  "exp_years_school_female",
                  "exp_years_school_male",
                  "Sum_bd_best","Konflikt",
                  "SL.TLF.CACT.FM.ZS",
                  "freedom_dim_index_core","control_dim_index_core","equality_dim_index_core",
                  "life_exp_female",
                  "life_exp_male",
                  "mean_years_schooling_female",
                  "mean_years_schooling_male",
                  "SP.POP.2024.MA.5Y",
                  "Maternal_mortality_ratio",
                  "SG.GEN.PARL.ZS",
                  "SP.POP.TOTL.FE.ZS","region_un","Country.Code","Year","Konflikt","region_wb","Sum_bd_best")))%>%
  filter(Year>2009)%>%
  replace_na(list(Sum_bd_best=0))

Data_ICP_gen$Konflikt<- as.factor(Data_ICP_gen$Konflikt)

drop_na_DataICP_gen <-Data_ICP_gen %>%
  drop_na()

X_na_gen <- drop_na_DataICP_gen%>%
  select(-one_of("Country.Code","Year","Konflikt","region_wb","region_un","Sum_bd_best"))%>%
  as.matrix

Y1_na_gen <- drop_na_DataICP_gen%>%
  pull(Sum_bd_best)

Y2_na_gen <- drop_na_DataICP_gen%>%
  pull(Konflikt)%>%
  as.numeric(as.factor)

ExpInd_na_gen <- drop_na_DataICP_gen%>%
  pull(region_wb)

ExpInd_na_gen <- as.numeric(as.factor(ExpInd_na_gen))%>%
  replace_na(0)



test <-hiddenICP(X_na_gen,Y1_na_gen,ExpInd_na_gen)
print(test)
print(signif(test$betahat,3))
print(signif(test$maximinCoefficients,3))
cat("True coefficiens are:", beta,0)

hiddenICP(X_na_gen,Y2_na_gen,ExpInd_na_gen)

#### Nicht lineare ICP

ExpInd_na_test <- Data_ICP_1_drop_na%>%
  pull(region_wb)
ExpInd_na_test<- as.factor(ExpInd_na_test)
resultnonlinICP <-nonlinearICP(X=X_na_1, Y= Y1_na_1, environment = ExpInd_na_test)
summary(resultnonlinICP)
print(resultnonlinICP)

resultnonlinICP_indikator <-nonlinearICP(X=X_na_ind, Y= Y1_na_ind, environment = as.factor(ExpInd_na_ind), condIndTest = "InvariantResidualDistributionTest")

resultnonlinICP_gender <-nonlinearICP(X=X_na_gen, Y= Y1_na_gen, environment = as.factor(ExpInd_na_gen), condIndTest = "InvariantResidualDistributionTest")

##### Untersuchung der NAs 

gg_miss_var(Data_ICP)
res<-summary(aggr(Data_ICP, sortVar=TRUE))$combinatio

head(res[rev(order(res[,2])),])
matrixplot(Data_ICP, sortby = 2)

marginplot(Data_ICP[,c("economy","region_wb")])

na_data_ICP_eco <- Data_ICP%>%
  filter(Year>2009)%>%
  group_by(economy)%>%
  miss_var_summary()%>%
  pivot_wider(names_from = economy,values_from = c("n_miss","pct_miss"))

na_data_ICP__reg <- Data_ICP%>%
  filter(Year>2009)%>%
  group_by(region_wb)%>%
  miss_var_summary()%>%
  pivot_wider(names_from = region_wb,values_from = c("n_miss","pct_miss"))



