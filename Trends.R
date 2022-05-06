#### Werte in regions anpassen, so dass keine NAs drin sind

## Trends Vriaben ausrechenen

na_on_error <- function(expr){
  result<- tryCatch(expr,
                    error=function(e){
                      return(NA)
                    })
  return(result)
}

#### neuer Versuch
## Was ich möchte. den Druchscnitt der Variablen nach Region und jahr ausrechenen. und dann plotten

Tren_cor <- Data_cor_sel%>%
  group_by(region_wb,Year)%>%
  summarise(across(SP.DYN.CDRT.IN:total_index_core, mean))

Tren_cor%>%
  ggplot(aes(Year,SP.DYN.CDRT.IN, color = region_wb))+
  geom_line()

Tren_cor%>%
  ggplot(aes(Year,Population_secondary_education_25., color = region_wb))+
  geom_line()

Tren_cor%>%
  ggplot(aes(Year,SP.POP.GROW, color = region_wb))+
  geom_line()

Tren_cor%>%
  ggplot(aes(Year,EN.POP.DNST, color = region_wb))+
  geom_line()

Tren_cor%>%
  ggplot(aes(Year,SP.URB.TOTL.IN.ZS, color = region_wb))+
  geom_line()

Tren_cor%>%
  ggplot(aes(Year,Im_Ex, color = region_wb))+
  geom_line()

Tren_cor%>%
  ggplot(aes(Year,SP.POP.2024.MA.5Y, color = region_wb))+
  geom_line()




Trend_neu <- Stat_Data_geo%>%
  select(one_of("SP.URB.TOTL.IN.ZS","NY.GNP.PCAP.CD","TX.VAL.FUEL.ZS.UN","SP.DYN.LE00.IN" ,"Population_secondary_education_25.","GNI","SP.POP.GROW","EN.POP.DNST","SH.STA.MMRT","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","NY.GDP.PCAP.CN","exp_years_school","mean_years_schooling","equality_dim_index_core","control_dim_index_core","freedom_dim_index_core","Konflikt","region_wb","subregion","Year","Country.Code","continent","economy"))%>%
  group_by(region_wb,Year)%>%
  summarise(across(SP.URB.TOTL.IN.ZS:freedom_dim_index_core, mean))


# Trend über die Welt. Durchscnittliche steigung der Variablen über die Jahre global
Trend_global <-Stat_Data_geo%>%
  select(one_of("TX.VAL.FUEL.ZS.UN","SP.DYN.LE00.IN" ,"Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SH.STA.MMRT","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","NY.GDP.PCAP.CN","exp_years_school","mean_years_schooling","equality_dim_index_core","control_dim_index_core","freedom_dim_index_core","Konflikt","region_wb","subregion","Year","Country.Code","continent","economy"))%>%
  group_by(Year)%>%
  summarise(across(TX.VAL.FUEL.ZS.UN:freedom_dim_index_core, mean)) ### Warum nur NAs?

Trend_life_exp <-Trend_neu%>%
  ggplot(aes(Year,SP.DYN.LE00.IN, color= region_wb))+
  geom_line()+
  scale_color_viridis_d("Regionen (World Bank)")+
  labs(x="Jahre",y= "Lebenserwartung")+
  ggtitle("Durschnittliche Entwicklung der Lebenserwartung nach Region")+
  theme_minimal()

Trend_GNI_per_capita <-Trend_neu%>%
  ggplot(aes(Year,NY.GNP.PCAP.CD, color= region_wb))+
  geom_line()+
  scale_color_viridis_d("Regionen (World Bank)")+
  labs(x="Jahre",y= "GNI pro Kopf")+
  ggtitle("Durschnittliche Entwicklung vom GNI pro Kopf nach Region")+
  theme_minimal() #zu viele NAs

Trend_pop_sec_25 <-Trend_neu%>%
  ggplot(aes(Year,Population_secondary_education_25., color= region_wb))+
  geom_line()+
  scale_color_viridis_d("Regionen (World Bank)")+
  labs(x="Jahre",y= "Bevölkerung ü. 25 mit mind. secondary Abschluss")+
  ggtitle("Durschnittliche Entwicklung vom der Bevölkerung mit einer schulischen Grundausbildung")+
  theme_minimal()# zu viele NAs

Trend_urb_Bev <-Trend_neu%>%
  ggplot(aes(Year,SP.URB.TOTL.IN.ZS, color= region_wb))+
  geom_line()+
  scale_color_viridis_d("Regionen (World Bank)")+
  labs(x="Jahre",y= "Anteil der urbanen Bevölkerung")+
  ggtitle("Durschnittliche Entwicklung vom Anteil der urbanen Bevölkerung")+
  theme_minimal()
Trend_urb_Bev <-Trend_neu%>%
  ggplot(aes(Year,SP.URB.TOTL.IN.ZS, color= region_wb))+
  geom_line()+
  scale_color_viridis_d("Regionen (World Bank)")+
  labs(x="Jahre",y= "Anteil der urbanen Bevölkerung")+
  ggtitle("Durschnittliche Entwicklung vom Anteil der urbanen Bevölkerung")+
  theme_minimal()



### hat die Variablen schon gespeichert

PCTTREND_Data <-Stat_Data_geo%>%
  select(one_of("Year", "Country.Code","Sum_bd_best","Sum_bd_low","Sum_bd_high","Konflikt","HDI","GDI","EDI","Im_Ex","GNI","total_index_core","SG.GEN.PARL.ZS","SP.POP.2529.MA.5Y","SP.POP.2529.FE.5Y","CC.EST","PV.EST","GE.EST","RQ.EST","RL.EST","VA.EST","SP.POP.TOTL.FE.ZS"))%>%
  filter(Year>1989)


##HDI
Hdi_pct <-PCTTREND_Data%>%
  group_by(`Country.Code`)%>%
  arrange(Year)%>%
  mutate(pct_chg_hdi = na_on_error(HDI-lag(HDI))/lag(HDI))

HDI_pct_y <- Hdi_pct%>%
  filter(Year>1989)%>%
  group_by(`Year`)%>%
  mutate(pct_chg_year_hdi= na_on_error(mean(pct_chg_hdi, na.rm=TRUE)))
##EDI
EDI_pct <-HDI_pct_y%>%
  group_by(`Country.Code`)%>%
  arrange(Year)%>%
  mutate(pct_chg_edi = na_on_error(EDI-lag(EDI))/lag(EDI))

EDI_pct_y <- EDI_pct%>%
  filter(Year>1989)%>%
  group_by(`Year`)%>%
  mutate(pct_chg_year_edi= na_on_error(mean(pct_chg_edi, na.rm=TRUE)))
##GDI
GDI_pct <-EDI_pct_y%>%
  group_by(`Country.Code`)%>%
  arrange(Year)%>%
  mutate(pct_chg_gdi = na_on_error(GDI-lag(GDI))/lag(GDI))

GDI_pct_y <- GDI_pct%>%
  filter(Year>1989)%>%
  group_by(`Year`)%>%
  mutate(pct_chg_year_gdi= na_on_error(mean(pct_chg_gdi, na.rm=TRUE)))
## GNI
GNI_pct <-GDI_pct_y%>%
  group_by(`Country.Code`)%>%
  arrange(Year)%>%
  mutate(pct_chg_gni = na_on_error(GNI-lag(GNI))/lag(GNI))

GNI_pct_y <- GNI_pct%>%
  filter(Year>1989)%>%
  group_by(`Year`)%>%
  mutate(pct_chg_year_gni= na_on_error(mean(pct_chg_gni, na.rm=TRUE)))

## Im_Ex
Im_Ex_pct <-GNI_pct_y%>%
  group_by(`Country.Code`)%>%
  arrange(Year)%>%
  mutate(pct_chg_Imex = na_on_error(Im_Ex-lag(Im_Ex))/lag(Im_Ex))

Im_Ex_pct_y <- Im_Ex_pct%>%
  filter(Year>1989)%>%
  group_by(`Year`)%>%
  mutate(pct_chg_year_imex= na_on_error(mean(pct_chg_Imex, na.rm=TRUE)))

## Sum_bd_Best
Sum_bd_best_pct <-Im_Ex_pct_y%>%
  group_by(`Country.Code`)%>%
  arrange(Year)%>%
  mutate(pct_chg_sum_bd_best = na_on_error(Sum_bd_best-lag(Sum_bd_best))/lag(Sum_bd_best))

Sum_bd_best_pct_y <- Sum_bd_best_pct%>%
  filter(Year>1989)%>%
  group_by(`Year`)%>%
  mutate(pct_chg_year_sum_bd_best= na_on_error(mean(pct_chg_sum_bd_best, na.rm=TRUE)))
## Demokratie
demo_pct <-Sum_bd_best_pct_y%>%
  group_by(`Country.Code`)%>%
  arrange(Year)%>%
  mutate(pct_chg_demo = na_on_error(total_index_core-lag(total_index_core))/lag(total_index_core))

demo_pct_y <- demo_pct%>%
  filter(Year>1989)%>%
  group_by(`Year`)%>%
  mutate(pct_chg_year_demo= na_on_error(mean(pct_chg_demo, na.rm=TRUE)))

## Population female

popf_pct <-demo_pct_y%>%
  group_by(`Country.Code`)%>%
  arrange(Year)%>%
  mutate(pct_chg_popf = na_on_error(SP.POP.TOTL.FE.ZS-lag(SP.POP.TOTL.FE.ZS))/lag(SP.POP.TOTL.FE.ZS))

popf_pct_y <- popf_pct%>%
  filter(Year>1989)%>%
  group_by(`Year`)%>%
  mutate(pct_chg_year_popf= na_on_error(mean(pct_chg_popf, na.rm=TRUE)))
## pop Männlich 25-29

pop_m2529_pct <-popf_pct_y%>%
  group_by(`Country.Code`)%>%
  arrange(Year)%>%
  mutate(pct_chg_popm2529 = na_on_error(SP.POP.2529.MA.5Y-lag(SP.POP.2529.MA.5Y))/lag(SP.POP.2529.MA.5Y))

pop_m2529pct_y <- pop_m2529_pct%>%
  filter(Year>1989)%>%
  group_by(`Year`)%>%
  mutate(pct_chg_year_popm2529= na_on_error(mean(pct_chg_popm2529, na.rm=TRUE)))
## Frauen im Parlament

fipar_pct <-pop_m2529pct_y%>%
  group_by(`Country.Code`)%>%
  arrange(Year)%>%
  mutate(pct_chg_fipar = na_on_error(SG.GEN.PARL.ZS-lag(SG.GEN.PARL.ZS))/lag(SG.GEN.PARL.ZS))

fipar_pct_y <- fipar_pct%>%
  filter(Year>1989)%>%
  group_by(`Year`)%>%
  mutate(pct_chg_year_fipar= na_on_error(mean(pct_chg_fipar, na.rm=TRUE)))

### Governemnt effectivnes

GE_pct <-fipar_pct_y%>%
  group_by(`Country.Code`)%>%
  arrange(Year)%>%
  mutate(pct_chg_ge = na_on_error(GE.EST-lag(GE.EST))/lag(GE.EST))

GE_pct_y <- GE_pct%>%
  filter(Year>1989)%>%
  group_by(`Year`)%>%
  mutate(pct_chg_year_ge= na_on_error(mean(pct_chg_ge, na.rm=TRUE)))

## Rule of Law
RL_pct <-GE_pct_y%>%
  group_by(`Country.Code`)%>%
  arrange(Year)%>%
  mutate(pct_chg_rl = na_on_error(RL.EST-lag(RL.EST))/lag(RL.EST))

RL_pct_y <- RL_pct%>%
  filter(Year>1989)%>%
  group_by(`Year`)%>%
  mutate(pct_chg_year_rl= na_on_error(mean(pct_chg_rl, na.rm=TRUE)))

## Voice accountability

VA_pct <-RL_pct_y%>%
  group_by(`Country.Code`)%>%
  arrange(Year)%>%
  mutate(pct_chg_va = na_on_error(VA.EST-lag(VA.EST))/lag(VA.EST))

VA_pct_y <- VA_pct%>%
  filter(Year>1989)%>%
  group_by(`Year`)%>%
  mutate(pct_chg_year_va= na_on_error(mean(pct_chg_va, na.rm=TRUE)))
## Regulatory Quality
RQ_pct <-VA_pct_y%>%
  group_by(`Country.Code`)%>%
  arrange(Year)%>%
  mutate(pct_chg_rq = na_on_error(RQ.EST-lag(RQ.EST))/lag(RQ.EST))

RQ_pct_y <- RQ_pct%>%
  filter(Year>1989)%>%
  group_by(`Year`)%>%
  mutate(pct_chg_year_rq= na_on_error(mean(pct_chg_rq, na.rm=TRUE)))

### Prozentuale Veränderung der Variablen über die Jahre

PLOT_trend_Year_index <- RQ_pct_y%>%
  ggplot()+
  geom_line(aes(Year,pct_chg_year_hdi, color= "HDI"))+
  geom_line(aes(Year,pct_chg_year_edi, color= "EDI"))+
  geom_line(aes(Year,pct_chg_year_gdi, color= "GDI"))+
  geom_line(aes(Year,pct_chg_year_gni, color= "GNI"))+
  labs(y= "prozentuale globale Veränderung", x= "Jahre", titel= "Prozentuale Veränderungen der prediktor-Variablen pro Jahr")+
  theme_minimal()

PLOT_Trend_Year<-RQ_pct_y%>%
  ggplot()+
  geom_line(aes(Year,pct_chg_year_imex, color= "Im_Ex"))+
  geom_line(aes(Year,pct_chg_year_demo, color= "Demokratie"))+
  geom_line(aes(Year,pct_chg_year_popf, color= "Population Female"))+
  geom_line(aes(Year,pct_chg_year_popm2529, color= "Männliche Pop 25-29"))+
  #geom_line(aes(Year,pct_chg_year_fipar, color= "Frauen im Parlament"))+
  geom_line(aes(Year,pct_chg_year_rl, color= "Rule of Law"))+
  geom_line(aes(Year,pct_chg_year_ge, color= "Government Efectivnis"))+
  #geom_line(aes(Year,pct_chg_year_rq, color= "Regulatory Quality"))+
  geom_line(aes(Year,pct_chg_year_va, color= "Voice Accountability"))+
  geom_line(aes(Year,pct_chg_year_sum_bd_best, color= "Tote pro Jahr in Konflikten"))


 
#### Trend Berechnung über Zeit. Keine Prozentualen Angaben

HDI_Trend_time <- Stat_Data_geo%>% select(`HDI`, `Year`, `Country.Code`, `Country.Name.x`) %>% 
  group_by(`Year`)%>%
  summarise(HDI = na_on_error(coef(lm(HDI ~ Country.Code))[2]))

EDI_Trend_time <- Stat_Data_geo%>% select(`EDI`, `Year`, `Country.Code`, `Country.Name.x`)%>%
  group_by(`Year`)%>%
  summarise(EDI = na_on_error(coef(lm(EDI ~ Country.Code))[2]))

GDI_Trend_time <- Stat_Data_geo%>% select(`GDI`, `Year`, `Country.Code`, `Country.Name.x`)%>%
  group_by(`Year`)%>%
  summarise(GDI = na_on_error(coef(lm(GDI ~ Country.Code))[2]))

Im_Ex_Trend_time <- Stat_Data_geo%>% select(`Im_Ex`, `Year`, `Country.Code`, `Country.Name.x`)%>%
  group_by(`Year`)%>%
  summarise(Im_Ex = na_on_error(coef(lm(Im_Ex ~ Country.Code))[2]))

GNI_Trend_time <- Stat_Data_geo%>% select(`GNI`, `Year`, `Country.Code`, `Country.Name.x`)%>%
  group_by(`Year`)%>%
  summarise(GNI = na_on_error(coef(lm(GNI ~ Country.Code))[2]))

CC.EST_Trend_time <- Stat_Data_geo%>% select(`CC.EST`, `Year`, `Country.Code`, `Country.Name.x`)%>%
  group_by(`Year`)%>%
  summarise(CC.EST = na_on_error(coef(lm(CC.EST ~ Country.Code))[2]))

VA.EST_Trend_time <- Stat_Data_geo%>% select(`VA.EST`, `Year`, `Country.Code`, `Country.Name.x`)%>%
  group_by(`Year`)%>%
  summarise(VA.EST = na_on_error(coef(lm(VA.EST ~ Country.Code))[2]))

RL.EST_Trend_time <- Stat_Data_geo%>% select(`RL.EST`, `Year`, `Country.Code`, `Country.Name.x`)%>%
  group_by(`Year`)%>%
  summarise(RL.EST = na_on_error(coef(lm(RL.EST ~ Country.Code))[2]))

GE.EST_Trend_time <- Stat_Data_geo%>% select(`GE.EST`, `Year`, `Country.Code`, `Country.Name.x`)%>%
  group_by(`Year`)%>%
  summarise(GE.EST = na_on_error(coef(lm(GE.EST ~ Country.Code))[2]))

demo_Trend_time <- Stat_Data_geo%>% select(`total_index_core`, `Year`, `Country.Code`, `Country.Name.x`)%>%
  group_by(`Year`)%>%
  summarise(total_index_core = na_on_error(coef(lm(total_index_core ~ Country.Code))[2]))

pop_f_Trend_time <- Stat_Data_geo%>% select(`SP.POP.TOTL.FE.ZS`, `Year`, `Country.Code`, `Country.Name.x`)%>%
  group_by(`Year`)%>%
  summarise(SP.POP.TOTL.FE.ZS = na_on_error(coef(lm(SP.POP.TOTL.FE.ZS ~ Country.Code))[2]))

pop_m2529_Trend_time <- Stat_Data_geo%>% select(`SP.POP.2529.MA.5Y`, `Year`, `Country.Code`, `Country.Name.x`)%>%
  group_by(`Year`)%>%
  summarise(SP.POP.2529.MA.5Y = na_on_error(coef(lm(SP.POP.2529.MA.5Y ~ Country.Code))[2]))

fiP_Trend_time <- Stat_Data_geo%>% select(`SG.GEN.PARL.ZS`, `Year`, `Country.Code`, `Country.Name.x`)%>%
  group_by(`Year`)%>%
  summarise(SG.GEN.PARL.ZS = na_on_error(coef(lm(SG.GEN.PARL.ZS ~ Country.Code))[2]))

## Zusammführen
fip_popm2529 <- full_join(fiP_Trend_time,pop_m2529_Trend_time, by = c("Year"))
fip_popm2529_popf <- full_join(fip_popm2529, pop_f_Trend_time, by = c("Year"))
fip_popm2529_popf_demo <-full_join(fip_popm2529_popf, demo_Trend_time, by = c("Year"))
fip_popm2529_popf_demo_GE <-full_join(fip_popm2529_popf_demo, GE.EST_Trend_time, by = c("Year"))
fip_popm2529_popf_demo_GE_RL <- full_join(fip_popm2529_popf_demo_GE, RL.EST_Trend_time, by = c("Year"))
fip_popm2529_popf_demo_GE_RL_VA <- full_join(fip_popm2529_popf_demo_GE_RL, VA.EST_Trend_time, by = c("Year"))
fip_popm2529_popf_demo_GE_RL_VA_CC <- full_join(fip_popm2529_popf_demo_GE_RL_VA, CC.EST_Trend_time, by = c("Year"))
fip_popm2529_popf_demo_GE_RL_VA_CC_HDI <- full_join(fip_popm2529_popf_demo_GE_RL_VA_CC, HDI_Trend_time, by = c("Year"))
fip_popm2529_popf_demo_GE_RL_VA_CC_HDI_EDI <- full_join(fip_popm2529_popf_demo_GE_RL_VA_CC_HDI, EDI_Trend_time, by = c("Year"))
fip_popm2529_popf_demo_GE_RL_VA_CC_HDI_EDI_GDI <- full_join(fip_popm2529_popf_demo_GE_RL_VA_CC_HDI_EDI, GDI_Trend_time, by = c("Year"))
fip_popm2529_popf_demo_GE_RL_VA_CC_HDI_EDI_GDI_GNI <- full_join(fip_popm2529_popf_demo_GE_RL_VA_CC_HDI_EDI_GDI, GNI_Trend_time, by = c("Year"))
fip_popm2529_popf_demo_GE_RL_VA_CC_HDI_EDI_GDI_GNI_ImEx <- full_join(fip_popm2529_popf_demo_GE_RL_VA_CC_HDI_EDI_GDI_GNI, Im_Ex_Trend_time, by = c("Year"))

Trend_data <- fip_popm2529_popf_demo_GE_RL_VA_CC_HDI_EDI_GDI_GNI_ImEx%>%
  filter(Year>2000)

Trend_data%>%
  ggplot()+
  geom_path(aes(Year,HDI, color= "HDI"))+
  geom_path(aes(Year,EDI, color= "EDI"))+
  geom_path(aes(Year,GDI, color= "GDI"))+
  #geom_path(aes(Year,Im_Ex, color = "yellow"))+
  geom_path(aes(Year, GNI, color="GNI"))+
  geom_path(aes(Year, total_index_core, color="Demokratie"))+
  geom_path(aes(Year, SP.POP.TOTL.FE.ZS, color = "Female Pop"))+
  geom_path(aes(Year, SP.POP.2529.MA.5Y, color = "Male Pop 25-29"))+
  geom_path(aes(Year, GE.EST, color= "Government Effektivnis"))+
  geom_path(aes(Year, VA.EST, color= "Voice Accouintability"))+
  geom_path(aes(Year, RL.EST, color= "Regulatory of Law"))+
  geom_path(aes(Year, CC.EST, color= "control of Corruption"))+
  geom_path(aes(Year, SG.GEN.PARL.ZS, color=  "female in Parlament"))


test<- Stat_Data_geo%>%
  group_by(Country.Code)%>%
  mutate(HDIprozent= (HDI))


### Interessant
Trend_data%>%
  ggplot(aes(Year,HDI/EDI/GNI/GDI))+
  geom_path()



### HDI Trend berechnen

HDI_Trend_Country <- Stat_Data_geo%>% select(`HDI`, `Year`, `Country.Code`, `Country.Name.x`) %>% 
  group_by(`Country.Code`)%>%
  summarise(HDI = na_on_error(coef(lm(HDI ~ Year))[2]))
HDI_trend_data <- inner_join(countries_sf, HDI_Trend, by= c("iso_a3"="Country.Code"))
HDI_Trend_map <- HDI_trend_data%>%
  ggplot(aes(fill=HDI))+
  geom_sf(col="black", size=0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  scale_fill_viridis_c(direction = -1)+
  theme_minimal()
HDI_Trend_map

### Import Export

Im_Ex_Trend <- Stat_Data_geo%>% select(`Im_Ex`, `Year`, `Country.Code`, `Country.Name.x`) %>% 
  group_by(`Country.Code`)%>%
  summarise(Im_Ex = na_on_error(coef(lm(Im_Ex ~ Year))[2]))
Im_Ex_trend_data <- inner_join(countries_sf, Im_Ex_Trend, by= c("iso_a3"="Country.Code"))

Im_Ex_Trend_map <- Im_Ex_trend_data%>%
  ggplot(aes(fill=Im_Ex))+
  geom_sf(col="black", size=0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  scale_fill_viridis_c(direction = -1)+
  theme_minimal()
Im_Ex_Trend_map
