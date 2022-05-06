install.packages("ggdag")
install.packages("lavaan",dependencies = TRUE)
install.packages("InvariantCausalPrediction")
install.packages("nonlinearICP",dependencies = TRUE)
install.packages("CompareCausalNetworks")
install.packages("backShift")
install.packages("pcalg")
install.packages("RBGL")
install.packages("graph",dependencies = TRUE)
install.packages("semPlot")
install.packages("randomForest",dependencies = TRUE)
install.packages(urlPackage, repos=NULL, type="source")## geht nicht 'NAMESPACE' file ist ben�tigt
install.packages("devtools")
install.packages("cartography")
install.packages("rnaturalearthdata")
install.packages("rnaturalearth")
install.packages("tidyverse")
install.packages("psy")
install.packages("nFactors")
install.packages("ggpubr")
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
library(ggdag)
library(dagitty)
library(lavaan)
library(InvariantCausalPrediction)
library(CompareCausalNetworks)
#ibrary(pcalg)
#library(RBGL)
library(semPlot)
library(nonlinearICP)
library(randomForest)
library(psych)
library(psy)
library(nFactors)
library(missMDA)
library(VIM)
library(FactoMineR)
library(naniar)
library(ggpubr)
library(car)

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
Female_in_par <- read_excel("assets/Female_in_par.xlsx")
Export1803 <- read_csv("assets/Export1803.csv")
Export2203 <- read_csv("assets/Export2203.csv")
Export2403 <- read_csv("assets/Export2403.csv")
Export2403_2 <- read_csv("assets/Export2403_2.csv")
Export2603 <- read_csv("assets/Export2603.csv")

Labor_force_participation_rate_for_ages_15_24_male_ <- read_csv("assets/Labor force participation rate for ages 15-24, male (%).csv")

Estimated_gross_national_income_per_capita_male_2017_PPP_ <- read_delim("assets/Estimated gross national income per capita, male (2017 PPP $).csv", 
                                                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)%>%
  select(-one_of("Column28"))

Estimated_gross_national_income_per_capita_female_2017_PPP_ <- read_delim("assets/Estimated gross national income per capita, female (2017 PPP $).csv", 
                                                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)%>%
  select(-one_of("Column28"))

Adolescent_birth_rate_births_per_1_000_women_ages_15_19_ <- read_delim("assets/Adolescent birth rate (births per 1,000 women ages 15-19).csv", 
                                                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select(-one_of("Column1","Column14","Column16","Column18","Column20","Column24","Column26","Column28", "Column30"))%>%
  select_if(is.character)


Expected_years_of_schooling_years_ <- read_delim("assets/Expected years of schooling (years).csv", 
                                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select(-one_of("_60","_58","_56","_54","_52","_50","_48","_46","_44","_42","_40","_38","_36","_34","_32","_30","_28","_26","_24","_22","_20","_18","_16","_14","_12","_10","_8","_6","_4","_2"))

Expected_years_of_schooling_female_years_ <- read_delim("assets/Expected years of schooling, female (years).csv", 
                                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)%>%
  select(-one_of("Column30"))

Expected_years_of_schooling_male_years_ <- read_delim("assets/Expected years of schooling, male (years).csv", 
                                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)%>%
  select(-one_of("Column30"))

Labour_force_participation_rate_ages_15_and_older_female <- read_delim("assets/Labour force participation rate (% ages 15 and older), female.csv", 
                                                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)%>%
  select(-one_of("Column30"))

Labour_force_participation_rate_ages_15_and_older_male <- read_delim("assets/Labour force participation rate (% ages 15 and older), male.csv", 
                                                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)%>%
  select(-one_of("Column30"))

Labour_force_participation_rate_ages_15_and_older_ <- read_delim("assets/Labour force participation rate (% ages 15 and older).csv", 
                                                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)%>%
  select(-one_of("Column30"))

Life_expectancy_at_birth_years_ <- read_delim("assets/Life expectancy at birth (years).csv", 
                                              delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)%>%
  select(-one_of("_60"))

Life_expectancy_at_birth_female_years_ <- read_delim("assets/Life expectancy at birth, female (years).csv", 
                                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)

Life_expectancy_at_birth_male_years_ <- read_delim("assets/Life expectancy at birth, male (years).csv", 
                                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)

Maternal_mortality_ratio_deaths_per_100_000_live_births_ <- read_delim("assets/Maternal mortality ratio (deaths per 100,000 live births).csv", 
                                                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)%>%
  select(-one_of("Column26"))

Mean_years_of_schooling_years_ <- read_delim("assets/Mean years of schooling (years).csv", 
                                             delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)%>%
  select(-one_of("Column62"))

Mean_years_of_schooling_female_years_ <- read_delim("assets/Mean years of schooling, female (years).csv", 
                                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)%>%
  select(-one_of("Column30"))

Mean_years_of_schooling_male_years_ <- read_delim("assets/Mean years of schooling, male (years).csv", 
                                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)%>%
  select(-one_of("Column30"))

Population_with_at_least_some_secondary_education_ages_25_and_older_ <- read_delim("assets/Population with at least some secondary education (% ages 25 and older).csv", 
                                                                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)%>%
  select(-one_of("Column30"))

Population_with_at_least_some_secondary_education_female_ages_25_and_older_ <- read_delim("assets/Population with at least some secondary education, female (% ages 25 and older).csv", 
                                                                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)%>%
  select(-one_of("Column28","Column30"))

Population_with_at_least_some_secondary_education_male_ages_25_and_older_ <- read_delim("assets/Population with at least some secondary education, male (% ages 25 and older).csv", 
                                                                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)%>%
  select(-one_of("Column28","Column30"))
  
Total_unemployment_rate_female_to_male_ratio_ <- read_delim("assets/Total unemployment rate (female to male ratio).csv", 
                                                            delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select_if(is.character)


### Falls die Indizes neu eingelesen werden muss die erste Zeile zum Spalten Namen werden

Adolescent_birth_rate_births_per_1_000_women_ages_15_19_[1,]<-Adolescent_birth_rate_births_per_1_000_women_ages_15_19_[194,]
Adolescent_birth_rate_births_per_1_000_women_ages_15_19_<-Adolescent_birth_rate_births_per_1_000_women_ages_15_19_[-194,]

colnames(Adolescent_birth_rate_births_per_1_000_women_ages_15_19_)<- Adolescent_birth_rate_births_per_1_000_women_ages_15_19_[1,]
Adolescent_birth_rate_births_per_1_000_women_ages_15_19_<- Adolescent_birth_rate_births_per_1_000_women_ages_15_19_[-1,]


colnames(Estimated_gross_national_income_per_capita_male_2017_PPP_)<- Estimated_gross_national_income_per_capita_male_2017_PPP_[1,]
Estimated_gross_national_income_per_capita_male_2017_PPP_<- Estimated_gross_national_income_per_capita_male_2017_PPP_[-1,]

colnames(Estimated_gross_national_income_per_capita_female_2017_PPP_)<- Estimated_gross_national_income_per_capita_female_2017_PPP_[1,]
Estimated_gross_national_income_per_capita_female_2017_PPP_<- Estimated_gross_national_income_per_capita_female_2017_PPP_[-1,]

colnames(Expected_years_of_schooling_female_years_)<- Expected_years_of_schooling_female_years_[1,]
Expected_years_of_schooling_female_years_<- Expected_years_of_schooling_female_years_[-1,]

colnames(Expected_years_of_schooling_male_years_)<- Expected_years_of_schooling_male_years_[1,]
Expected_years_of_schooling_male_years_<- Expected_years_of_schooling_male_years_[-1,]


col.num2<- c("2017","2018","2019")
Expected_years_of_schooling_years_[col.num2] <- sapply(Expected_years_of_schooling_years_[col.num2],as.character)
colnames(Expected_years_of_schooling_years_)<- Expected_years_of_schooling_years_[1,]
Expected_years_of_schooling_years_<- Expected_years_of_schooling_years_[-1,]

colnames(Labour_force_participation_rate_ages_15_and_older_female)<- Labour_force_participation_rate_ages_15_and_older_female[1,]
Labour_force_participation_rate_ages_15_and_older_female<- Labour_force_participation_rate_ages_15_and_older_female[-1,]

colnames(Labour_force_participation_rate_ages_15_and_older_male)<- Labour_force_participation_rate_ages_15_and_older_male[1,]
Labour_force_participation_rate_ages_15_and_older_male<- Labour_force_participation_rate_ages_15_and_older_male[-1,]

colnames(Life_expectancy_at_birth_years_)<- Life_expectancy_at_birth_years_[1,]
Life_expectancy_at_birth_years_<- Life_expectancy_at_birth_years_[-1,]

colnames(Labour_force_participation_rate_ages_15_and_older_)<- Labour_force_participation_rate_ages_15_and_older_[1,]
Labour_force_participation_rate_ages_15_and_older_<- Labour_force_participation_rate_ages_15_and_older_[-1,]

colnames(Life_expectancy_at_birth_female_years_)<- Life_expectancy_at_birth_female_years_[1,]
Life_expectancy_at_birth_female_years_<- Life_expectancy_at_birth_female_years_[-1,]

colnames(Life_expectancy_at_birth_male_years_)<- Life_expectancy_at_birth_male_years_[1,]
Life_expectancy_at_birth_male_years_<- Life_expectancy_at_birth_male_years_[-1,]

colnames(Maternal_mortality_ratio_deaths_per_100_000_live_births_)<- Maternal_mortality_ratio_deaths_per_100_000_live_births_[1,]
Maternal_mortality_ratio_deaths_per_100_000_live_births_<- Maternal_mortality_ratio_deaths_per_100_000_live_births_[-1,]

colnames(Mean_years_of_schooling_years_)<- Mean_years_of_schooling_years_[1,]
Mean_years_of_schooling_years_<- Mean_years_of_schooling_years_[-1,]

colnames(Mean_years_of_schooling_female_years_)<- Mean_years_of_schooling_female_years_[1,]
Mean_years_of_schooling_female_years_<- Mean_years_of_schooling_female_years_[-1,]

colnames(Mean_years_of_schooling_male_years_)<- Mean_years_of_schooling_male_years_[1,]
Mean_years_of_schooling_male_years_<- Mean_years_of_schooling_male_years_[-1,]

colnames(Population_with_at_least_some_secondary_education_ages_25_and_older_)<- Population_with_at_least_some_secondary_education_ages_25_and_older_[1,]
Population_with_at_least_some_secondary_education_ages_25_and_older_<- Population_with_at_least_some_secondary_education_ages_25_and_older_[-1,]

colnames(Population_with_at_least_some_secondary_education_female_ages_25_and_older_)<- Population_with_at_least_some_secondary_education_female_ages_25_and_older_[1,]
Population_with_at_least_some_secondary_education_female_ages_25_and_older_<- Population_with_at_least_some_secondary_education_female_ages_25_and_older_[-1,]

colnames(Population_with_at_least_some_secondary_education_male_ages_25_and_older_)<- Population_with_at_least_some_secondary_education_male_ages_25_and_older_[1,]
Population_with_at_least_some_secondary_education_male_ages_25_and_older_<- Population_with_at_least_some_secondary_education_male_ages_25_and_older_[-1,]

colnames(Total_unemployment_rate_female_to_male_ratio_)<- Total_unemployment_rate_female_to_male_ratio_[1,]
Total_unemployment_rate_female_to_male_ratio_<- Total_unemployment_rate_female_to_male_ratio_[-1,]


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

colnames(Female_in_par)<- Female_in_par[1,]
Female_in_par<- Female_in_par[-1,]



###Pivots
### vor den Pivots ausf?hren
setnames(wdi_2611, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(DemocracyMatrix_v4, old = "Country", new = "Country Name")
setnames(Governance_Indicators, old = c("1996 [YR1996]","1998 [YR1998]","2000 [YR2000]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1996", "1998","2000","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(Demographie, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(Female_in_par, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(Labor_force_participation_rate_for_ages_15_24_male_, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(Export1803, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(Export2203, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(Export2603, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))


Export2403[ , 5:24]<- list(NULL)
setnames(Export2403, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(Export2403_2, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))

Exp2603_piv<- Export2603%>%
  select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = T
  )%>%
  pivot_wider(names_from = "Series Code", values_from = "value")

Exp2403_2_piv<- Export2403_2%>%
  select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = T
  )%>%
  pivot_wider(names_from = "Series Code", values_from = "value")

Exp2403_piv<- Export2403%>%
  select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = T
  )%>%
  pivot_wider(names_from = "Series Code", values_from = "value")

Exp2203_piv<- Export2203%>%
  select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = T
  )%>%
  pivot_wider(names_from = "Series Code", values_from = "value")

Exp1803_piv<- Export1803%>%
  select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = T
  )%>%
  pivot_wider(names_from = "Series Code", values_from = "value")

Wdi_pivot <- wdi_2611 %>%
  select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = TRUE
  )%>%
  pivot_wider(names_from = "Series Code", values_from = "value")

lfp_male_1524_pivot <- Labor_force_participation_rate_for_ages_15_24_male_ %>%
  select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = TRUE
  )%>%
  pivot_wider(names_from = c("Series Code"), values_from = "value")



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
  select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1996`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = TRUE
  )%>%
  pivot_wider(names_from = c("Series Code"), values_from = "value")

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
  select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = TRUE
  )%>%
  pivot_wider(c(names_from = c("Series Code"), values_from = "value"))

Im_Ex_pivot <- In_Ex%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Im_Ex",
  )

F_i_p_pivot <- Female_in_par %>%
  select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = TRUE
  )%>%
  pivot_wider(names_from = c("Series Code"), values_from = "value")

t_unemp_mf_pivot <- Total_unemployment_rate_female_to_male_ratio_%>%
  pivot_longer(
    cols = `1991`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Unemployment_M_F",
  )

Pop_secedu_m_25 <- Population_with_at_least_some_secondary_education_male_ages_25_and_older_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Population_secondary_education_male_25+",
  )

Pop_secedu_f_25 <- Population_with_at_least_some_secondary_education_female_ages_25_and_older_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Population_secondary_education_female_25+",
  )

Pop_secedu_25 <- Population_with_at_least_some_secondary_education_ages_25_and_older_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Population_secondary_education_25+",
  )

mean_years_schooling_male <- Mean_years_of_schooling_male_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "mean_years_schooling_male",
  )

mean_years_schooling_female <- Mean_years_of_schooling_female_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "mean_years_schooling_female",
  )

mean_years_schooling <- Mean_years_of_schooling_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "mean_years_schooling",
  )

Maternal_mortality_ratio <- Maternal_mortality_ratio_deaths_per_100_000_live_births_%>%
  pivot_longer(
    cols = `1990`:`2017`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Maternal_mortality_ratio",
  )

life_exp_male <- Life_expectancy_at_birth_male_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "life_exp_male",
  )

life_exp_female <- Life_expectancy_at_birth_female_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "life_exp_female",
  )

life_exp <- Life_expectancy_at_birth_years_%>%
  pivot_longer(
    cols = `1991`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "life_exp",
  )

Laborforce_male_15 <- Labour_force_participation_rate_ages_15_and_older_male%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Laborforce_male_15+",
  )

Laborforce_female_15 <- Labour_force_participation_rate_ages_15_and_older_female%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Laborforce_female_15+",
  )


exp_years_school <- Expected_years_of_schooling_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "exp_years_school",
  )
exp_years_school_male <- Expected_years_of_schooling_male_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "exp_years_school_male",
  )

exp_years_school_female <- Expected_years_of_schooling_female_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "exp_years_school_female",
  )

GNI_per_capita_female <- Estimated_gross_national_income_per_capita_female_2017_PPP_%>%
  pivot_longer(
    cols = `1995`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "GNI_per_capita_female",
  )

GNI_per_capita_male <- Estimated_gross_national_income_per_capita_male_2017_PPP_%>%
  pivot_longer(
    cols = `1995`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "GNI_per_capita_male",
  )

Adolescent_birth_rate <- Adolescent_birth_rate_births_per_1_000_women_ages_15_19_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Adolescent_birth_rate",
  )


### ein extra join f�r die neu hinzugef�gten Variablen. welche die Indizes aufteilen.

df_list<- list(mean_years_schooling,
               Adolescent_birth_rate,
               GNI_per_capita_male,
               GNI_per_capita_female,
               exp_years_school_female,
               exp_years_school_male,
               exp_years_school,
               Laborforce_female_15,
               Laborforce_male_15,
               life_exp,
               life_exp_female,
               life_exp_male,
               Maternal_mortality_ratio,
               mean_years_schooling_female,
               mean_years_schooling_male,
               Pop_secedu_25,
               Pop_secedu_f_25,
               Pop_secedu_m_25,
               t_unemp_mf_pivot)

Join_Index_Indicator <- df_list%>%
  reduce(full_join,by=c("Year", "Country","Country Code"))

Join_Index_Indicator_lfp <- left_join(Join_Index_Indicator,lfp_male_1524_pivot, by = c("Year","Country Code"))

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

Wdi_Konflikt <- left_join(wdi_Gov, ucdp_dpc, by = c("Year", "Country Code"))
wdi_Konf_num <- Wdi_Konflikt
wdi_Konf_num$Konflikt<- Wdi_Konflikt$Konflikt%>%
  replace(is.na(Wdi_Konflikt$Konflikt),0)%>%
  as.logical(Wdi_Konflikt$Konflikt)

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
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip <- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie, F_i_p_pivot, by= c("Year","Country Code"))
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi <-left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip, Join_Index_Indicator_lfp, by= c("Year","Country Code"))
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803 <- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi, Exp1803_piv, by= c("Year","Country Code"))
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203<-left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803, Exp2203_piv, by=c("Year","Country Code"))
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403<- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203,Exp2403_piv, by = c("Year","Country Code"))
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2<- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403,Exp2403_2_piv,by = c("Year","Country Code"))
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2_2603<-left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2, Exp2603_piv,by = c("Year","Country Code"))

Data_geo <- HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2_2603%>%
  select(one_of(c("SP.DYN.CBRT.IN","SP.DYN.CDRT.IN","SE.SEC.CUAT.LO.ZS","NY.GNP.PCAP.PP.CD","TX.VAL.FUEL.ZS.UN","SH.STA.MMRT","SP.ADO.TFRT","SP.DYN.LE00.IN","NE.EXP.GNFS.ZS","SE.XPD.TOTL.GD.ZS","TX.VAL.MMTL.ZS.UN","SL.UEM.1524.ZS","NY.GNP.PCAP.CD","SM.POP.TOTL.ZS","EN.POP.DNST","SP.POP.GROW","SP.POP.TOTL","SL.TLF.ACTI.1524.MA.ZS", "SL.TLF.ACTI.1524.FE.ZS",
                  "Unemployment_M_F",
                  "Population_secondary_education_male_25+",
                  "Population_secondary_education_female_25+",
                  "Population_secondary_education_25+",
                  "mean_years_schooling_male",
                  "mean_years_schooling_female",
                  "mean_years_schooling",
                  "Maternal_mortality_ratio",
                  "life_exp_male",
                  "life_exp_female",
                  "life_exp","Laborforce_male_15+",
                  "Laborforce_female_15+",
                  "Adolescent_birth_rate",
                  "GNI_per_capita_male",
                  "GNI_per_capita_female",
                  "exp_years_school_female",
                  "exp_years_school_male",
                  "exp_years_school",
                  "SG.GEN.PARL.ZS",
                  "Konflikt",
                  "type_of_conflict",
                  "SP.POP.2529.FE.5Y",
                  "SP.POP.2529.MA.5Y",
                  "SP.POP.TOTL.FE.ZS",
                  "SP.POP.TOTL.MA.ZS", 
                  "SP.POP.2024.MA.5Y", 
                  "SP.POP.2024.FE.5Y",
                  "Im_Ex","Ineq_Coef",
                  "adjHDI",
                  "GDI",
                  "Country Name.x",
                  "Country Code", 
                  "Year",
                  "freedom_dim_index_core",
                  "rule_settlement_freedom_core",
                  "rights_freedom_core",
                  "communication_freedom_core",
                  "intermediate_freedom_core",
                  "decision_freedom_core",
                  "classification_core",
                  "freedom_dim_index_core",
                  "equality_dim_index_core", 
                  "control_dim_index_core", 
                  "total_index_core",
                  "SI.POV.GINI",
                  "SL.TLF.ACTI.1524.FE.ZS",
                  "SL.TLF.CACT.FE.ZS",
                  "SL.TLF.TOTL.FE.ZS",
                  "SL.TLF.CACT.FM.ZS",
                  "NY.GDP.MKTP.KN",
                  "NY.GDP.MKTP.KD.ZG",
                  "NY.GDP.PCAP.CN",
                  "NY.GDP.PCAP.KD.ZG",
                  "IQ.CPA.PROP.XQ",
                  "IQ.CPA.GNDR.XQ",
                  "SE.ADT.1524.LT.FM.ZS",
                  "SE.ENR.PRIM.FM.ZS", 
                  "SE.ENR.PRSC.FM.ZS", 
                  "SE.ENR.SECO.FM.ZS", 
                  "SE.ENR.TERT.FM.ZS",
                  "CC.EST", "PV.EST",
                  "GE.EST", "RQ.EST", 
                  "RL.EST",
                  "VA.EST","Country Code", 
                  "type", 
                  "economy",
                  "geometry", "continent", 
                  "region_un", "subregion",
                  "region_wb", "bd_best", 
                  "bd_low", "bd_high", 
                  "HDI", "GII","EDI","GNI","Investment")))



#### wenn alles gejoint, die Variabeln in numerische Werte umformen

col.num.all <-c("SP.DYN.CBRT.IN","SP.DYN.CDRT.IN","SE.SEC.CUAT.LO.ZS","NY.GNP.PCAP.PP.CD","TX.VAL.FUEL.ZS.UN","SH.STA.MMRT","SP.ADO.TFRT","SP.DYN.LE00.IN","NE.EXP.GNFS.ZS","SE.XPD.TOTL.GD.ZS","TX.VAL.MMTL.ZS.UN","SL.UEM.1524.ZS","NY.GNP.PCAP.CD","SM.POP.TOTL.ZS","EN.POP.DNST","SP.POP.GROW","SP.POP.TOTL","SL.TLF.ACTI.1524.MA.ZS",
                "Unemployment_M_F",
                "Population_secondary_education_male_25+",
                "Population_secondary_education_female_25+",
                "Population_secondary_education_25+",
                "mean_years_schooling_male",
                "mean_years_schooling_female",
                "mean_years_schooling",
                "Maternal_mortality_ratio",
                "life_exp_male",
                "life_exp_female",
                "life_exp","Laborforce_male_15+",
                "Laborforce_female_15+",
                "Adolescent_birth_rate",
                "GNI_per_capita_male",
                "GNI_per_capita_female",
                "exp_years_school_female",
                "exp_years_school_male",
                "exp_years_school","SG.GEN.PARL.ZS","Im_Ex","equality_dim_index_core", "control_dim_index_core", "total_index_core","SP.POP.2529.FE.5Y","SP.POP.2529.MA.5Y","SP.POP.TOTL.FE.ZS","SP.POP.TOTL.MA.ZS", "SP.POP.2024.MA.5Y", "SP.POP.2024.FE.5Y","Ineq_Coef","adjHDI","GDI","Year","freedom_dim_index_core","rule_settlement_freedom_core","rights_freedom_core","communication_freedom_core","intermediate_freedom_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST", "bd_best", "bd_low", "bd_high", "HDI", "GII","EDI","GNI","Investment")
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
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip <- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie, F_i_p_pivot, by= c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi <-left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip, Join_Index_Indicator_lfp, by= c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803 <- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi, Exp1803_piv, by= c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203<-left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803, Exp2203_piv, by=c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403<- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203,Exp2403_piv, by = c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2<- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403, Exp2403_2_piv, by = c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2_2603 <- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2, Exp2603_piv,by = c("Year","Country Code") )

Stat_Data_geo <- Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2_2603%>%
  select(one_of(c("SP.DYN.CBRT.IN","SP.DYN.CDRT.IN","SE.SEC.CUAT.LO.ZS","SP.URB.TOTL.IN.ZS","NY.GNP.PCAP.PP.CD","TX.VAL.FUEL.ZS.UN","SH.STA.MMRT","SP.ADO.TFRT","SP.DYN.LE00.IN","NE.EXP.GNFS.ZS","SE.XPD.TOTL.GD.ZS","TX.VAL.MMTL.ZS.UN","SL.UEM.1524.ZS","NY.GNP.PCAP.CD","SM.POP.TOTL.ZS","EN.POP.DNST","SP.POP.GROW","SP.POP.TOTL","SL.TLF.ACTI.1524.MA.ZS",
                  "Unemployment_M_F",
                  "Population_secondary_education_male_25+",
                  "Population_secondary_education_female_25+",
                  "Population_secondary_education_25+",
                  "mean_years_schooling_male",
                  "mean_years_schooling_female",
                  "mean_years_schooling",
                  "Maternal_mortality_ratio",
                  "life_exp_male",
                  "life_exp_female",
                  "life_exp","Laborforce_male_15+",
                  "Laborforce_female_15+",
                  "Adolescent_birth_rate",
                  "GNI_per_capita_male",
                  "GNI_per_capita_female",
                  "exp_years_school_female",
                  "exp_years_school_male",
                  "exp_years_school","SG.GEN.PARL.ZS","SP.POP.2529.FE.5Y","SP.POP.2529.MA.5Y","SP.POP.TOTL.FE.ZS","SP.POP.TOTL.MA.ZS", "SP.POP.2024.MA.5Y", "SP.POP.2024.FE.5Y","Im_Ex","Ineq_Coef","adjHDI","GDI","Country Name.x","Country Code", "Year","classification_core","freedom_dim_index_core","equality_dim_index_core", "control_dim_index_core", "total_index_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST","Country Code", "type", "economy", "continent", "region_un", "subregion","region_wb","geometry", "Sum_bd_best", "Sum_bd_low", "Sum_bd_high", "HDI", "GII","EDI","GNI","Investment")))



stat.col.num<-c("SP.DYN.CBRT.IN","SP.DYN.CDRT.IN","SE.SEC.CUAT.LO.ZS","SP.URB.TOTL.IN.ZS","NY.GNP.PCAP.PP.CD","TX.VAL.FUEL.ZS.UN","SH.STA.MMRT","SP.ADO.TFRT","SP.DYN.LE00.IN","NE.EXP.GNFS.ZS","SE.XPD.TOTL.GD.ZS","TX.VAL.MMTL.ZS.UN","SL.UEM.1524.ZS","NY.GNP.PCAP.CD","SM.POP.TOTL.ZS","EN.POP.DNST","SP.POP.GROW","SP.POP.TOTL","SL.TLF.ACTI.1524.MA.ZS",
                "Unemployment_M_F",
                "Population_secondary_education_male_25+",
                "Population_secondary_education_female_25+",
                "Population_secondary_education_25+",
                "mean_years_schooling_male",
                "mean_years_schooling_female",
                "mean_years_schooling",
                "Maternal_mortality_ratio",
                "life_exp_male",
                "life_exp_female",
                "life_exp","Laborforce_male_15+",
                "Laborforce_female_15+",
                "Adolescent_birth_rate",
                "GNI_per_capita_male",
                "GNI_per_capita_female",
                "exp_years_school_female",
                "exp_years_school_male",
                "exp_years_school","SG.GEN.PARL.ZS","equality_dim_index_core", "control_dim_index_core", "total_index_core","SP.POP.2529.FE.5Y","SP.POP.2529.MA.5Y","SP.POP.TOTL.FE.ZS","SP.POP.TOTL.MA.ZS", "SP.POP.2024.MA.5Y", "SP.POP.2024.FE.5Y","Im_Ex","Ineq_Coef","adjHDI","GDI","Year","freedom_dim_index_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST", "Sum_bd_best", "Sum_bd_low", "Sum_bd_high", "HDI", "GII","EDI","GNI","Investment")
Stat_Data_geo[stat.col.num]<-sapply(Stat_Data_geo[stat.col.num],as.numeric)

Stat_Data_geo <-Stat_Data_geo%>%
  transform(Konflikt=Sum_bd_best> 24)%>%
  replace_na(list(Konflikt = FALSE))


#### to do's
### nicht lineare Regressionen? -> nlm
### überprüfen nach homoskedastizität und Residuen sind normalverteilt und Test auf Linearität -> residuen Diagramm 
### multiplre Regression mit Moderator Variable (macht evtl. nicht so viel sinn)
### polynomiale Regression 
### stückweise linerare Regression-> wann ändert sich die Steigung und die Entwicklung. Tritt da szufällig mit einem Konflikt auf? 
### Bei Regressionen muss man R2 angeben: Varianzerklärung. Mehr PRediktoren erhöhren Unterraum. Adjustierre R2 passt den Unterraum an



# in welchen Spalten sind wie viele NAs?





Labeler <- function(x){
  return(Meta26_11_Excel$`Indicator Name`[which(x==Meta26_11_Excel$`Code`)])
}





