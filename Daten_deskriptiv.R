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

wdi_2611 <- read_csv("C:/Users/sebas/Desktop/GeographieMA/Masterarbeit/data/wdi_2611.csv")
DemocracyMatrix_v4 <- read_excel("C:/Users/sebas/Desktop/GeographieMA/Masterarbeit/data/DemocracyMatrix_v4.xlsx")
ucdp_dpc <- read_excel("C:/Users/sebas/Desktop/GeographieMA/Masterarbeit/data/ucdp_dpc.xlsx")
Human_Development_Index_HDI_ <- read_excel("C:/Users/sebas/Desktop/GeographieMA/Masterarbeit/data/Human Development Index (HDI).xlsx", 
                                           sheet = "Human Development Index (HD (2)")
Gender_Equality_Index <- read_excel("C:/Users/sebas/Desktop/GeographieMA/Masterarbeit/data/Gender_Equality_Index.xlsx")
Education_index <- read_excel("C:/Users/sebas/Desktop/GeographieMA/Masterarbeit/data/Education index.xlsx")
Income_Index <- read_excel("C:/Users/sebas/Desktop/GeographieMA/Masterarbeit/data/Income Indec GNI.xlsx")
Investment_Index <- read_excel("C:/Users/sebas/Desktop/GeographieMA/Masterarbeit/data/Investment Index.xlsx")
Governance_Indicators <- read_csv("C:/Users/sebas/Desktop/GeographieMA/Masterarbeit/data/WDI_DAta/Governance Indicators.csv")

### Falls die Indizes neu eingelesen werden muss die erste Zeile zum Spalten Namen werden

colnames(Investment_Index)<- Investment_Index[1,]
Investment_Index<- Investment_Index[-1,]

colnames(Human_Development_Index_HDI_) <- Human_Development_Index_HDI_[1,]
Human_Development_Index_HDI_<- Human_Development_Index_HDI_[-1,]

colnames(Gender_Equality_Index)<- Gender_Equality_Index[1,]
Gender_Equality_Index<- Gender_Equality_Index[-1,]

colnames(Education_index)<- Education_index[1,]
Education_index<- Education_index[-1,]

colnames(Income_Index)<- Income_Index[1,]
Income_index<- Income_Index[-1,]

colnames(Income_Index)<- Income_Index[1,]
Income_index<- Income_Index[-1,]

### Manuelles Verändern, damit die Joins besser funktionieren


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
DemocracyMatrix_v4[DemocracyMatrix_v4=="Ivory Coast"] <- "Côte d'Ivoire"
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

###Pivots
### vor den Pivots ausführen
setnames(wdi_2611, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(DemocracyMatrix_v4, old = "Country", new = "Country Name")
setnames(Governance_Indicators, old = c("1996 [YR1996]","1998 [YR1998]","2000 [YR2000]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1996", "1998","2000","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))


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

Gender_pivot <- Gender_Equality_Index %>%
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

  
### Die Joins zur Übersicht
setnames(Gender_pivot, old= "Country", new = "Country Name")
setnames(HDI_pivot, old= "Country", new = "Country Name")
setnames(Income_pivot, old= "Country", new = "Country Name")
setnames(Education_pivot, old= "Country", new = "Country Name")
setnames(Invest_pivot, old= "Country", new = "Country Name")
wdi_sf <- inner_join(countries_sf,Wdi_pivot, by= "Country Code" == "iso_a3")
wdi_Gov<- left_join(Wdi_pivot,Governance_pivot, by= c("Year", "Country Code", "Country Name"))
  

Wdi_Konflikt <- left_join(wdi_Gov, ucdp_dpc, by = c("Year", "Country Code"))
WDI_Konf_num <- Wdi_Konflikt
WDI_Konf_num$Konflikt<- Wdi_Konflikt$Konflikt%>%
  replace(is.na(Wdi_Konflikt$Konflikt),0)%>%
  as.logical(Wdi_Konflikt$Konflikt)

Join_HDI <- left_join(WDI_Konf_num,HDI_pivot, by = c("Year", "Country Code"))
Demokratie <- left_join(Join_HDI, DemocracyMatrix_v4, by = c("Country Code","Year"))
HDI_GII_Konf<- left_join(Join_HDI,Gender_pivot, by = c("Country Code", "Year", "HDI Rank"))
HDI_GII_EDI<- left_join(HDI_GII_Konf,Education_pivot, by = c("Country Code", "Year", "HDI Rank"))
HDI_GII_EDI_GNI<- left_join(HDI_GII_EDI,Income_pivot, by = c("Country Code", "Year", "HDI Rank"))
HDI_GII_EDI_GNI_Dem_Invest_Gov <- left_join(HDI_GII_EDI_GNI,Invest_pivot, by = c("Year","Country Code","HDI Rank"))
Data_geo <- inner_join(countries_sf,HDI_GII_EDI_GNI_Dem_Invest_Gov, by = c("iso_a3"="Country Code"))
HDI_trend_data <- inner_join(countries_sf, HDI_Trend, by= c("iso_a3"="Country Code"))


#### wenn alles gejoint, die Variabeln in numerische Werte umformen

col.num.all <-c("Investment", "GII", "HDI", "EDI", "GNI","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST")
HDI_GII_EDI_GNI_Dem_Invest_Gov[col.num.all] <- sapply(HDI_GII_EDI_GNI_Dem_Invest_Gov[col.num.all],as.numeric)

### ein bisschen aufräumen
HDI_GII_EDI_GNI_Dem_Invest_Gov<- HDI_GII_EDI_GNI_Dem_Invest_Gov%>%
  select(-one_of(c("RL.STD.ERR","RL.PER.RNK.UPPER","RL.PER.RNK.LOWER","RL.PER.RNK","RL.NO.SRC", "RQ.STD.ERR","RQ.PER.RNK.UPPER","RQ.PER.RNK.LOWER","RQ.PER.RNK","RQ.NO.SRC","PV.STD.ERR","PV.PER.RNK.UPPER", "PV.PER.RNK.LOWER","PV.PER.RNK","PV.NO.SRC", "GE.STD.ERR","GE.PER.RNK.UPPER", "GE.PER.RNK.LOWER","GE.PER.RNK","GE.NO.SRC","CC.STD.ERR","CC.PER.RNK.UPPER","CC.PER.RNK.LOWER","CC.PER.RNK","CC.NO.SRC","SE.ENR.TERT.FM.ZS","SE.ENR.SECO.FM.ZS","SE.ENR.PRSC.FM.ZS","NY.GDP.PCAP.KD.ZG","conflict_id","dyad_id","side_a_id","side_b_id","gwno_a","gwno_a_2nd","gwno_b","gwno_b_2nd","gwno_loc","gwno_battle","version")
))

#### die Daten deskiptiv untersuchen ##### Plots

### Bar Diagramm mit Konflikten nach Region 3=Asien, 1=Europa, 4=afrika, 2= naher Osten, 5 = Amerika

### 
Data_geo%>%
  filter(Konflikt==TRUE)%>%
  ggplot(aes(subregion, Konflikt, na.rm=TRUE))+
  geom_jitter(na.rm = TRUE)+
  labs(title =  "Verteilung der Konflikte nach den Regionen")

Data_geo%>%
  filter(Konflikt==TRUE)%>%
  ggplot(aes(subregion))+
  geom_bar()+
  labs(title = "Verteilung der Konflikte nach den Regionen")+
  theme(text = element_text(size = 10),
                axis.text.x = element_text(angle = 30, hjust=1))

Data_geo%>%
  filter(Konflikt == TRUE)%>%
  ggplot(aes(type_of_conflict))+
  geom_bar()

"1 = extrasystemic (between a state and a non-state group outside its own territory, where the government side is fighting to retain control of a territory outside the state system)
2 = interstate (both sides are states in the Gleditsch and
Ward membership system).
3 = intrastate (side A is always a government; side B is always one or more rebel groups; there is no involvement of foreign governments with troops, i.e. there is no side_a_2nd or side_b_2nd coded).
4 = internationalized intrastate (side A is always a
government; side B is always one or more rebel
groups; there is involvement of foreign
governments with troops, i.e. there is at least ONE
side_a_2nd or side_b_2nd coded)."

## jitterplot mit CIPA rating 
Data_geo%>%
  filter(Konflikt== TRUE)%>%
  ggplot(aes(IQ.CPA.PROP.XQ, IQ.CPA.GNDR.XQ, color = subregion))+
  geom_point(position ="Jitter")+
  labs(x="Governance Rating", y="Genderequality Rating")
 
  
### GDP per Capita und Ratio of femal to male labor force. 

Data_geo%>%
  filter(Konflikt== TRUE) %>%
  ggplot(aes(x=SL.TLF.CACT.FM.ZS, y =NY.GDP.PCAP.CN, color = subregion))+
  geom_point()+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x="ratio labor force m und f", y= "GDP per capita(log)")
  
  


### mit der Konfliktzahl. X-achsenabschnitt beliebig verändern
Data_geo%>%
  ggplot(aes(x=NY.GDP.PCAP.CN, y= bd_best, color= subregion))+
  geom_point()+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  scale_x_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x= "GDP per Capita",y="Anzahl der Toten pro Konflikt")+
  theme_minimal()

### Demokratie mit Demokratie matrix
## wie viel Konflikte in der Bewertung von Demokratie

Data_geo%>%
  filter(Konflikt==TRUE)%>%
  ggplot(aes(classification_context))+
  geom_bar()+
  labs(x="Einteilung in politische Systeme")+
  theme_minimal()

## Zusammenhang zwischen Beteiligung von weiblicher Arbeitskraft und dem GDP des. Nach Regionen und Konflikten aufgeteilt in Farbe und FOrm
Data_geo%>%
  filter(Konflikt== TRUE)%>%
  ggplot(aes(NY.GDP.MKTP.KN, SL.TLF.CACT.FE.ZS,color= subregion, shape= Konflikt, na.rm = TRUE))+
  geom_point(size= 1, position = "jitter")+
  scale_x_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x= "GDP", y= "Labor force participant rate, female (% of female population 15+)")


## erster Plot mit den hdi daten
Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(HDI,bd_best,color=subregion, shape= Konflikt))+
  geom_point()+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x="HDI",y="Anzahl der geschätzen Toten pro Konflikt")



##Darstellung mit dem GII
Data_geo%>%
  #drop_na()%>%
  filter(Konflikt==TRUE)%>%
  ggplot(aes(GII,HDI,color=subregion,shape=Konflikt))+
  geom_point(position = "jitter")+
  labs(x="Gender Inequality Index", y= "Human Development Index")


###Plotten mit Education Index Daten 
Data_geo%>%
  #drop_na()%>%
  filter(Konflikt==TRUE)%>%
  filter(type_of_conflict== 3)%>%
  ggplot(aes(GII,EDI,color=subregion,shape=Konflikt))+
  geom_point(position = "jitter")+
  labs(x="Gender Inequality Index", y= "Education Index")


### filtern nach Konflikttyp 3 = Inerstaatlicher Konflikt
Data_geo%>%
  #drop_na()%>%
  filter(Konflikt==TRUE)%>%
  filter(type_of_conflict== 3)%>%
  ggplot(aes(HDI,EDI,color=subregion,shape=Konflikt))+
  geom_point(position = "jitter")+
  labs(x="Human Development Index", y= "Education Index")


### Income Index


Data_geo %>%
  filter(type_of_conflict== 3)%>%
  ggplot(aes(GNI, GII, color= subregion, shape = Konflikt))+
  geom_point()+
  labs(x="Bruttoeinkommensprodukt",y="Gender Inequality Index")

Data_geo %>%
  ggplot(aes(GNI, SI.POV.GINI, color= subregion, shape = Konflikt))+
  geom_point()+
  labs(x="Bruttoeinkommensprodukt",y="GINI Index")



## Plotten mit Invest Daten

Data_geo%>%
  filter(Konflikt== TRUE)%>%
  ggplot(aes(Investment, GII, color= subregion, shape = Konflikt))+
  geom_point(position = "Jitter")+
  scale_x_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x="Investment aus dem Ausland", y= "Gender Inequality Index")
  
#### Plotten mit Governance Daten

Data_geo%>%
  filter(Konflikt== TRUE)%>%
  ggplot(aes(GII,PV.EST,color=subregion, shape = Konflikt))+
  geom_point(position = "jitter")+
  labs(x="Gender Inequality Index", y= "Political Stability and Absence of Violence/Terrorism: Estimate")+
  theme_minimal()


#### Werte in regions anpassen, so dass keine NAs drin sind

## Trends Vriaben ausrechenen

na_on_error <- function(expr){
  result<- tryCatch(expr,
                    error=function(e){
                      return(NA)
                    })
  return(result)
}

### HDI Trend berechnen

HDI_Trend <- HDI_GII_EDI_GNI_Dem_Invest%>% select(`HDI`, `Year`, `Country Code`, `Country Name`) %>% 
  group_by(`Country Code`)%>%
  summarise(HDI = na_on_error(coef(lm(HDI ~ Year))[2]))
HDI_trend_data <- inner_join(countries_sf, HDI_Trend, by= c("iso_a3"="Country Code"))

HDI_Trend_map <- HDI_trend_data%>%
  ggplot(aes(fill=HDI))+
  geom_sf(col="black", size=0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  scale_fill_viridis_c(direction = -1)+
  theme_minimal()
HDI_Trend_map

## Kartographische Darstellung von Konflikten

countries_sf <- ne_countries(returnclass = "sf")
countries_sf %>%
  ggplot() +
  geom_sf()


data_map <- inner_join(countries_sf,HDI_GII_EDI_GDI_Dem_Invest, by = c("iso_a3"="Country Code"))
head(data_map)
## Konflikte auf Länder in 2019
Konflikt_map <- data_map %>%
  filter(Year== 2019)%>%
  ggplot(aes(fill= Konflikt))+
  geom_sf(col= "black", size= 0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  theme_minimal()

Konflikt_map  ### irgendwie noch die type_of_conflict filtern

HDI_map <- Data_geo %>%
  filter(Year == 2011)%>%
  ggplot(aes(fill= HDI))+
  geom_sf(col="black", size = 0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  scale_fill_viridis_c(direction = -1)+
  theme_minimal()
HDI_map

GII_map <- Data_geo %>%
  filter(Year == 2019)%>%
  ggplot(aes(fill= GII))+
  geom_sf(col="black", size = 0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  scale_fill_viridis_c(direction = -1)+
  theme_minimal()
GII_map

EDI_map <- Data_geo %>%
  filter(Year == 2019)%>%
  ggplot(aes(fill= EDI))+
  geom_sf(col="black", size = 0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  scale_fill_viridis_c(direction = -1)+
  theme_minimal()
EDI_map


GINI_map<- Data_geo%>%
  filter(Year==2019)%>%
  ggplot(aes(fill= SI.POV.GINI))+
  geom_sf(col="black", size = 0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  scale_fill_viridis_c(direction = -1)+
  theme_minimal()
GINI_map  

Gov_map <- Data_geo%>%
  filter(Year==2019)%>%
  ggplot(aes(fill=PV.EST))+
  geom_sf(col="black", size = 0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  scale_fill_viridis_c(direction = -1)+
  theme_minimal()
Gov_map
