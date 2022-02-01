library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table)
library(readxl)
library(scales)
library(rnaturalearthdata)
library(rnaturalearth)
library(sf)
library(cartography)

wdi_2611 <- read_csv("assets/wdi_2611.csv")
DemocracyMatrix_v4 <- read_excel("assets/DemocracyMatrix_v4.xlsx")
ucdp_dpc <- read_excel("assets/ucdp_dpc.xlsx")
Human_Development_Index_HDI_ <- read_excel("assets/Human Development Index (HDI).xlsx", 
                                           sheet = "Human Development Index (HD (2)")
Gender_Inequality_Index <- read_excel("assets/Gender_Inequality_Index.xlsx")
Education_index <- read_excel("assets/Education index.xlsx")
Income_Index <- read_excel("assets/Income Indec GNI.xlsx")
Investment_Index <- read_excel("assets/Investment Index.xlsx")
Governance_Indicators <- read_csv("assets/Governance Indicators.csv")
Inequality_adjHDI <- read_excel("assets/Inequality_adjHDI.xlsx")
Gender_development_Index <- read_excel("assets/Gender_development_Index.xlsx")
Inequality_Coef <- read_excel("assets/Inequality_Coef.xlsx")
In_Ex <- read_excel("assets/In_Ex.xlsx")
Demographie <- read_excel("assets/Demographie.xlsx")
Meta26_11_Excel <- read_excel("assets/Meta26_11_Excel.xlsx")

### Falls die Indizes neu eingelesen werden muss die erste Zeile zum Spalten Namen werden
colnames(In_Ex)<- In_Ex[1,]
In_Ex<- In_Ex[-1,]

colnames(Inequality_adjHDI)<- Inequality_adjHDI[1,]
Inequality_adjHDI<- Inequality_adjHDI[-1,]

colnames(Gender_development_Index)<- Gender_development_Index[1,]
Gender_development_Index<- Gender_development_Index[-1,]

colnames(Inequality_Coef)<- Inequality_Coef[1,]
Inequality_Coef<- Inequality_Coef[-1,]

colnames(Investment_Index)<- Investment_Index[1,]
Investment_Index<- Investment_Index[-1,]

colnames(Human_Development_Index_HDI_) <- Human_Development_Index_HDI_[1,]
Human_Development_Index_HDI_<- Human_Development_Index_HDI_[-1,]

colnames(Gender_Inequality_Index)<- Gender_Inequality_Index[1,]
Gender_Equality_Index<- Gender_Inequality_Index[-1,]

colnames(Education_index)<- Education_index[1,]
Education_index<- Education_index[-1,]

colnames(Income_Index)<- Income_Index[1,]
Income_Index<- Income_Index[-1,]

colnames(Meta26_11_Excel)<- Meta26_11_Excel[1,]
Meta26_11_Excel<- Meta26_11_Excel[-1,]



###Pivots
### vor den Pivots ausf?hren
setnames(wdi_2611, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(DemocracyMatrix_v4, old = "Country", new = "Country Name")
setnames(Governance_Indicators, old = c("1996 [YR1996]","1998 [YR1998]","2000 [YR2000]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1996", "1998","2000","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(Demographie, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))


Wdi_pivot <- wdi_2611 %>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = TRUE
  )%>%
  pivot_wider(c("Country Code", "Country Name","Year"), "Series Code")

HDI_pivot <- Human_Development_Index_HDI_ %>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_to = "HDI",
    values_drop_na = TRUE
  )

Gender_pivot <- Gender_Inequality_Index %>%
  pivot_longer(
    cols = `1995`:`2019`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_to = "GII",
    values_drop_na = TRUE
  )

Education_pivot <- Education_index %>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_to = "EDI",
    values_drop_na = TRUE
  )

Income_pivot <- Income_Index %>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_to = "GNI",
    values_drop_na = TRUE
  )
Invest_pivot <- Investment_Index %>%
  pivot_longer(
    cols = `1990`:`2011-2019`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_to = "Investment",
    values_drop_na = TRUE
  )

Governance_pivot <- Governance_Indicators %>%
  pivot_longer(
    cols = `1996`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = TRUE
  )%>%
  pivot_wider(c("Country Code", "Country Name","Year"), "Series Code")

Inequ_adjHDI_pivot <- Inequality_adjHDI%>%
  pivot_longer(
    cols=`2010`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "adjHDI",
  )

GDI_pivot<- Gender_development_Index%>%
  pivot_longer(
    cols = `1995`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "GDI",
  )

Ineq_coef_pivot <- Inequality_Coef%>%
  pivot_longer(
    cols = `2010`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Ineq_Coef",
  )

demographie_pivot <- Demographie %>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = TRUE
  )%>%
  pivot_wider(c("Country Code", "Country Name","Year"), "Series Code")

Im_Ex_pivot <- In_Ex%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Im_Ex",
  )

### Die Joins zur ?bersicht
setnames(Gender_pivot, old= "Country", new = "Country Name")
setnames(HDI_pivot, old= "Country", new = "Country Name")
setnames(Income_pivot, old= "Country", new = "Country Name")
setnames(Education_pivot, old= "Country", new = "Country Name")
setnames(Invest_pivot, old= "Country", new = "Country Name")
setnames(Ineq_coef_pivot, old= "Country", new = "Country Name")
setnames(GDI_pivot, old= "Country", new = "Country Name")
setnames(Inequ_adjHDI_pivot, old= "Country", new = "Country Name")
setnames(Im_Ex_pivot, old = "Country", new = "CountryCode")
###normale Version


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
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI <-left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov, GDI_pivot, by= c("Year", "Country Code"))
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI <- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI, Inequ_adjHDI_pivot, by= c("Year", "Country Code"))
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef <- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI, Ineq_coef_pivot, by = c("Year","Country Code"))
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex <- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef, Im_Ex_pivot,by = c("Year","Country Code"))
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie <- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex,demographie_pivot, by = c("Year","Country Code"))


Data_geo <- HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie%>%
  select(one_of(c("Konflikt","type_of_conflict","SP.POP.2529.FE.5Y","SP.POP.2529.MA.5Y","SP.POP.TOTL.FE.ZS","SP.POP.TOTL.MA.ZS", "SP.POP.2024.MA.5Y", "SP.POP.2024.FE.5Y","Im_Ex","Ineq_Coef","adjHDI","GDI","Country Name.x","Country Code", "Year","freedom_dim_index_core","rule_settlement_freedom_core","rights_freedom_core","communication_freedom_core","intermediate_freedom_core","decision_freedom_core","classification_core","freedom_dim_index_core","equality_dim_index_core", "control_dim_index_core", "total_index_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST","Country Code", "type", "economy", "continent", "region_un", "subregion","region_wb","geometry", "bd_best", "bd_low", "bd_high", "HDI", "GII","EDI","GNI","Investment")))



#### wenn alles gejoint, die Variabeln in numerische Werte umformen

col.num.all <-c("Im_Ex","equality_dim_index_core", "control_dim_index_core", "total_index_core","SP.POP.2529.FE.5Y","SP.POP.2529.MA.5Y","SP.POP.TOTL.FE.ZS","SP.POP.TOTL.MA.ZS", "SP.POP.2024.MA.5Y", "SP.POP.2024.FE.5Y","Ineq_Coef","adjHDI","GDI","Year","freedom_dim_index_core","rule_settlement_freedom_core","rights_freedom_core","communication_freedom_core","intermediate_freedom_core","classification_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST", "bd_best", "bd_low", "bd_high", "HDI", "GII","EDI","GNI","Investment")
Data_geo[col.num.all] <- sapply(Data_geo[col.num.all],as.numeric)



###### FÜr eine erste statistische Auswertung darf pro Jahr und Land nur eine Zeile existieren. 
###### Sonst wird dieser Wert bei Regressionen und Trendanalysen gewichtet.
###### Es gibt mehrere Zeilen für ein Jahr pro Land, der Konfliktdatensätze mehrere Konflikte pro Jahr aufzählt.

### Gruppeirung der Länder und Jahre plus die Aufsummierung der Todesopfer in den Jahren
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


#### Diesen Datensatz mit den summierten Todesopfern mit den restlichen Daten zusammenfügen

Death_wdi <- left_join(wdi_Gov,Death_join, by= c("Country Code","Year"))

Stat_Join_HDI <- left_join(Death_wdi,HDI_pivot, by = c("Year", "Country Code"))
Stat_Demokratie <- left_join(Stat_Join_HDI, DemocracyMatrix_v4, by = c("Country Code","Year"))
Stat_HDI_GII_Konf<- left_join(Stat_Demokratie,Gender_pivot, by = c("Year", "Country Code"))
Stat_HDI_GII_EDI<- left_join(Stat_HDI_GII_Konf,Education_pivot, by = c( "Year", "Country Code"))
Stat_HDI_GII_EDI_GNI<- left_join(Stat_HDI_GII_EDI,Income_pivot, by = c( "Year", "Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov <- left_join(Stat_HDI_GII_EDI_GNI,Invest_pivot, by = c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI <-left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov, GDI_pivot, by= c("Year", "Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI <- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI, Inequ_adjHDI_pivot, by= c("Year", "Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef <- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI, Ineq_coef_pivot, by = c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex <- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef, Im_Ex_pivot,by = c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie <- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex,demographie_pivot, by = c("Year","Country Code"))

Stat_Data_geo <- Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie%>%
  select(one_of(c("SP.POP.2529.FE.5Y","SP.POP.2529.MA.5Y","SP.POP.TOTL.FE.ZS","SP.POP.TOTL.MA.ZS", "SP.POP.2024.MA.5Y", "SP.POP.2024.FE.5Y","Im_Ex","Ineq_Coef","adjHDI","GDI","Country Name.x","Country Code", "Year","classification_core","freedom_dim_index_core","equality_dim_index_core", "control_dim_index_core", "total_index_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST","Country Code", "type", "economy", "continent", "region_un", "subregion","region_wb","geometry", "Sum_bd_best", "Sum_bd_low", "Sum_bd_high", "HDI", "GII","EDI","GNI","Investment")))

Stat_Data_geo <-Stat_Data_geo%>%
  transform(Konflikt=Sum_bd_best> 24)%>%
  replace_na(list(Konflikt = FALSE))

stat.col.num<-c("equality_dim_index_core", "control_dim_index_core", "total_index_core","SP.POP.2529.FE.5Y","SP.POP.2529.MA.5Y","SP.POP.TOTL.FE.ZS","SP.POP.TOTL.MA.ZS", "SP.POP.2024.MA.5Y", "SP.POP.2024.FE.5Y","Im_Ex","Ineq_Coef","adjHDI","GDI","Year","freedom_dim_index_core","classification_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST", "Sum_bd_best", "Sum_bd_low", "Sum_bd_high", "HDI", "GII","EDI","GNI","Investment")
Stat_Data_geo[stat.col.num]<-sapply(Stat_Data_geo[stat.col.num],as.numeric)




### Multivariate Regressionen durchführen, um erste Zusammenhänge 
## Daten Vorbereitung.
Stat_Data_geo <- Stat_Data_geo%>%
  select(-one_of(c("geometry","Country Code","Year","Country Name.x","classification_core","type", "economy", "continent", "region_un", "subregion","region_wb")))
summary(Stat_Data_geo)
pairs(Stat_Data_geo) ### Daten kleiner machen. Du groß für Pairs

Data_lm <-Stat_Data_geo%>%
  select("freedom_dim_index_core","rule_settlement_freedom_core","rights_freedom_core","communication_freedom_core","intermediate_freedom_core","classification_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST", "Sum_bd_best", "Sum_bd_low", "Sum_bd_high", "HDI", "GII","EDI","GNI","Investment")%>%
  tibble()
## Formula für die Rregression vorbereiten lm=liniare multiple Regression, glm=logistisch Binär

Formula_lmRegression <-  cbind(Sum_bd_best,Sum_bd_high,Sum_bd_low)~SL.TLF.CACT.FE.ZS+SL.TLF.TOTL.FE.ZS+NY.GDP.PCAP.CN+SE.ENR.PRIM.FM.ZS+SE.ENR.PRSC.FM.ZS+SE.ENR.SECO.FM.ZS+SE.ENR.TERT.FM.ZS+intermediate_freedom_core+communication_freedom_core+rights_freedom_core+rule_settlement_freedom_core+HDI+GII+EDI+GNI+Investment+freedom_dim_index_core+CC.EST+IQ.CPA.PROP.XQ+IQ.CPA.GNDR.XQ+PV.EST+GE.EST+RQ.EST+RL.EST+VA.EST

Formula_glmRegression <-  Konflikt~SL.TLF.CACT.FE.ZS+SL.TLF.TOTL.FE.ZS+NY.GDP.PCAP.CN+SE.ENR.PRIM.FM.ZS+SE.ENR.PRSC.FM.ZS+SE.ENR.SECO.FM.ZS+SE.ENR.TERT.FM.ZS+intermediate_freedom_core+communication_freedom_core+rights_freedom_core+rule_settlement_freedom_core+HDI+GII+EDI+GNI+Investment+freedom_dim_index_core+CC.EST+IQ.CPA.PROP.XQ+IQ.CPA.GNDR.XQ+PV.EST+GE.EST+RQ.EST+RL.EST+VA.EST



###linear Model
Stat_data_lm<- lm(Formula_lmRegression,data = Data_lm)

summary(Stat_data_lm)

vcov(Stat_data_lm)

## für eine logarithmische binäre Regression werden die Werte 0 und 1 benötigt. oder True und False. Welche Variablen haben einen Einfluss auf das Auftreten eines Konfliktes

## Generalized Linear Models
Konflikt_Logit <-glm(Formula_glmRegression,data=Data_glm, family = binomial("logit"),maxit=100)
summary(Konflikt_Logit)

### Warum nur stop das bei vielen Variablen?
### nicht lineare Regressionen? -> nlm
### überprüfen nach homoskedastizität und Residuen sind normalverteilt und Test aauf Linearität -> residuen Diagramm 
### multiplre Regression mit Moderator Variable (macht evtl. nicht so viel sinn)
### polynomiale Regression 
### stückweise linerare Regression-> wann ändert sich die Steigung und die Entwicklung. Tritt da szufällig mit einem Konflikt auf? 
### Bei Regressionen muss man R2 angeben: Varianzerklärung. Mehr PRediktoren erhöhren Unterraum. Adjustierre R2 passt den Unterraum an
















