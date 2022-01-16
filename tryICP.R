install.packages("InvariantCausalPrediction")

library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table)
library(readxl)
library(scales)
library(InvariantCausalPrediction)
#hellotest

setnames(wdi_2611, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(results1, old = "year", new = "Year")

Wdi_pivot <- wdi_2611 %>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    #names_ptypes = list(Year = integer()),
    values_drop_na = TRUE,
  )%>%
  pivot_wider( c("Country Code", "Country Name","Year"), "Series Code")

wdi_ucdp <-merge(Wdi_pivot, ucdp_dpc, by = c("Year", "Country Code"), all= TRUE)
wdi_ucdp_result <- merge(wdi_ucdp, results1, by = c("Year", "Country Code"), all = TRUE) 

hiddenICP(df2[4:19],df2$Konflikt,df2$Region, alpha = 0.01, mode = "asymptotic", intercept = FALSE)











df <- data.frame(var1= Wdi_pivot$SI.POV.GINI,
                  var2= Wdi_pivot$SL.TLF.ACTI.1524.FE.ZS,
                  var3= Wdi_pivot$SL.TLF.CACT.FE.ZS,
                 var4=Wdi_pivot$SL.TLF.CACT.FM.ZS,
                 var5=Wdi_pivot$NY.GDP.MKTP.KN,
                 var6=Wdi_pivot$NY.GDP.MKTP.KD.ZG,
                 var7=Wdi_pivot$NY.GDP.PCAP.CN,
                 var8=Wdi_pivot$NY.GDP.PCAP.KD.ZG,
                 var9=Wdi_pivot$SE.ENR.PRIM.FM.ZS,
                 var10=Wdi_pivot$IQ.CPA.PROP.XQ,
                 var11=Wdi_pivot$IQ.CPA.GNDR.XQ,
                 var12=Wdi_pivot$SE.ADT.1524.LT.FM.ZS,
                 var13=Wdi_pivot$SE.ENR.PRSC.FM.ZS,
                 var14=Wdi_pivot$SE.ENR.SECO.FM.ZS,
                 var15=Wdi_pivot$SE.ENR.TERT.FM.ZS)
df2<- data.frame(wdi_ucdp_result)
trix <- as.matrix(wdi_ucdp_result)
summary(df)
## ändern von character in numerisch (Mode)

df2_num <- as.numeric(df)
summary(df2_num)
