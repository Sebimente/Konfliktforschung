#################################### die Daten deskiptiv untersuchen ##### Plots
### schneller Zugang


Konf_regio
Konf_type
GDPpCapiat_p
freedom_p
HDI_p
GDI_p
GII_p
Im_Ex_p
Ineq_Coef_p
adjHDI_p
EDI_p
GNI_p
Investment_p
Demo_total_p
Demo_control_p
demo_equal_p
pop_20_24f_p
pop_20_24m_p
pop_m_p
pop_f_p
pop_25_29f_p
pop_25_29m_p
Corruption_p
CPIA_gen_p # evtl. Dummy weil kategoriale Werte
CPIA_gov_p # evtl. Dummy weil kategoriale Werte
GDP_growth_p
GDP_p
GDPpCapiat_p
Government_p
GPI_school_pri_p
GPI_school_ter_p
lfp_15_24f_p
lfp_15f_p
lfp_fm_p
lf_f_p
Regulatory_p
Government_p
ROL_p
Demo_control_p
Demo_total_p
stability_p
voice_p
Literacy_rate_p


###### BAlkendiagramme
### UN Regionen und die Verteilung von Konflikten
Konf_regio <-Data_geo%>%
  filter(Konflikt==TRUE)%>%
  ggplot(aes(region_wb))+
  geom_bar()+
  labs(title = "Verteilung der Konflikte nach den Regionen")+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 30, hjust=1))
### Subregionen und die Verteilung von Konflikten (differenzierter)
Data_geo%>%
  filter(Konflikt==TRUE)%>%
  ggplot(aes(subregion))+
  geom_bar()+
  labs(title = "Verteilung der Konflikte nach den Regionen")+
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 30, hjust=1))

Konf_type <-Data_geo%>%
  filter(Konflikt == TRUE)%>%
  ggplot(aes(type_of_conflict))+
  geom_bar()+
  labs(x="Anzahl an Konflikten",y = "Konflikttyp", titel="Art der Konflikte")
##### Interesse an typ 3

Data_geo%>%
  filter(Konflikt==TRUE)%>%
  ggplot(aes(classification_core))+
  geom_bar()+
  labs(x="Einteilung in politische Systeme")+
  theme_minimal()#   nochmal prüfen

"1 = extrasystemic (between a state and a non-state group outside its own territory, where the government side is fighting to retain control of a territory outside the state system)
2 = interstate (both sides are states in the Gleditsch and
Ward membership system).
3 = intrastate (side A is always a government; side B is always one or more rebel groups; there is no involvement of foreign governments with troops, i.e. there is no side_a_2nd or side_b_2nd coded).
4 = internationalized intrastate (side A is always a
government; side B is always one or more rebel
groups; there is involvement of foreign
governments with troops, i.e. there is at least ONE
side_a_2nd or side_b_2nd coded)."

# ## jitterplot mit CIPA rating 
# Data_geo%>%
#   filter(Konflikt== TRUE)%>%
#   ggplot(aes(IQ.CPA.PROP.XQ, IQ.CPA.GNDR.XQ, color = region_wb))+
#   geom_point(position = "jitter")+
#   geom_smooth(stat= "smooth")+
#   labs(x=Labeler("IQ.CPA.PROP.XQ"), y=Labeler("IQ.CPA.GNDR.XQ"))
# 
# 
# ### GDP per Capita und Ratio of femal to male labor force. 
# 
# Data_geo%>%
#   filter(Konflikt== TRUE) %>%
#   ggplot(aes(x=SL.TLF.CACT.FM.ZS, y =NY.GDP.PCAP.CN))+
#   geom_point()+
#   geom_smooth(stat="smooth")+
#   scale_y_continuous(trans='log10',
#                      breaks=trans_breaks('log10', function(x) 10^x),
#                      labels=trans_format('log10', math_format(10^.x)))+
#   labs(x=Labeler("SL.TLF.CACT.FM.ZS"), y= Labeler("NY.GDP.PCAP.CN"))
# 
# 
# 
# ### Demokratie mit Demokratie matrix
# ## wie viel Konflikte in der Bewertung von Demokratie
# 
# 
# 
# 
# 
# 
# ## Zusammenhang zwischen Beteiligung von weiblicher Arbeitskraft und dem GDP des. Nach Regionen und Konflikten aufgeteilt in Farbe und FOrm
# Data_geo%>%
#   filter(Konflikt== TRUE)%>%
#   ggplot(aes(NY.GDP.MKTP.KN, SL.TLF.CACT.FE.ZS, na.rm = TRUE))+
#   geom_point(size= 1, position = "jitter")+
#   geom_smooth(stat="smooth")+
#   scale_x_continuous(trans='log10',
#                      breaks=trans_breaks('log10', function(x) 10^x),
#                      labels=trans_format('log10', math_format(10^.x)))+
#   labs(x=Labeler("NY.GDP.MKTP.KN"), y= Labeler("SL.TLF.CACT.FE.ZS"))
# 
# 
# ## erster Plot mit den hdi daten
# 
# 
# 
# 
# ##Darstellung mit dem GII
# Data_geo%>%
#   #drop_na()%>%
#   filter(Konflikt==TRUE)%>%
#   ggplot(aes(GII,HDI,shape=Konflikt))+
#   geom_point(position = "jitter")+
#   geom_smooth(stat = "smooth")+
#   labs(x=Labeler("GII"), y=Labeler("HDI"))
# 
# 
# ###Plotten mit Education Index Daten 
# Data_geo%>%
#   #drop_na()%>%
#   filter(Konflikt==TRUE)%>%
#   filter(type_of_conflict== 3)%>%
#   ggplot(aes(GII,EDI,shape=Konflikt))+
#   geom_point(position = "jitter")+
#   geom_smooth(stat = "smooth")+
#   labs(x=Labeler("GII"), y= Labeler("EDI"))
# 
# 
# 
# ### filtern nach Konflikttyp 3 = Inerstaatlicher Konflikt
# Data_geo%>%
#   #drop_na()%>%
#   filter(Konflikt==TRUE)%>%
#   filter(type_of_conflict== 3)%>%
#   ggplot(aes(HDI,EDI,shape=Konflikt))+
#   geom_point(position = "jitter")+
#   geom_smooth(stat = "smooth")+
#   labs(x=Labeler("HDI"), y= Labeler("EDI"))
# 
# 
# ### Income Index
# 
# 
# Data_geo %>%
#   filter(type_of_conflict== 3)%>%
#   ggplot(aes(GNI, GII, shape = Konflikt))+
#   geom_point()+
#   geom_smooth(stat = "smooth")+
#   labs(x=Labeler("GNI"),y=Labeler("GII"))
# 
# Data_geo %>%
#   filter(Konflikt== TRUE)%>%
#   ggplot(aes(GNI, SI.POV.GINI))+
#   geom_point()+
#   geom_smooth(stat="smooth")+
#   labs(x=Labeler("GNI"),y=Labeler("SI.POV.GINI"))
# 
# 
# 
# ## Plotten mit Invest Daten
# 
# Data_geo%>%
#   filter(Konflikt== TRUE)%>%
#   ggplot(aes(Investment, GII))+
#   geom_point(position = "Jitter")+
#   geom_smooth(stat = "smooth")+
#   scale_x_continuous(trans='log10',
#                    breaks=trans_breaks('log10', function(x) 10^x),
#                    labels=trans_format('log10', math_format(10^.x)))+
#   labs(x=Labeler("Investment"), y= Labeler("GII"))
# 
# #### Plotten mit Governance Daten
# 
# Data_geo%>%
#   filter(Konflikt== TRUE)%>%
#   ggplot(aes(GII,PV.EST))+
#   geom_point(position = "jitter")+
#   geom_smooth(stat="smooth")+
#   labs(x=Labeler("GII"), y= Labeler("PV.EST"))+
#   theme_minimal()

###### nur plotten mit den geschätzen Todeszahlen. Ich benutze die statistischen Werte um mehrfache Konflikte in einemJahr zu verändern


###GDP per capita

GDPpCapiat_p <-Stat_Data_geo%>%
  #filter(Konflikt== TRUE)%>%
 # filter(type_of_conflict== 3)%>%
  ggplot(aes(x=NY.GDP.PCAP.CN, y= Sum_bd_best))+
  geom_point()+
  geom_smooth(stat="smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  scale_x_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x= Labeler("NY.GDP.PCAP.CN"),y=Labeler("Sum_bd_best"))+
  theme_minimal()


### Sekbstbestimmung im politischen System
freedom_p <-Stat_Data_geo%>%
  #filter(Konflikt== TRUE)%>%
  #filter(type_of_conflict==3)%>%
  ggplot(aes(freedom_dim_index_core,Sum_bd_best))+
  geom_point(position="jitter")+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x= Labeler("freedom_dim_index_core"), y= Labeler("Sum_bd_best"))


### Human development Index
HDI_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  #filter(type_of_conflict==3)%>%
  ggplot(aes(HDI,Sum_bd_best, shape= Konflikt))+
  geom_point()+
  geom_smooth(method = "glm")+
  geom_smooth(stat= "smooth")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("HDI"),y=Labeler("Sum_bd_best"))

### Gender Development Index
GDI_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  #filter(type_of_conflict==3)%>%
  ggplot(aes(GDI,Sum_bd_best, shape= Konflikt))+
  geom_point()+
  geom_smooth(stat= "smooth")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("GDI"),y=Labeler("Sum_bd_best"))

### Gender Inequality index -> fast identisch zum GDI. Der GDi hat mehr Werte
GII_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  #filter(type_of_conflict==3)%>%
  ggplot(aes(GII,Sum_bd_best, shape= Konflikt))+
  geom_point()+
  geom_smooth(stat= "smooth")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  scale_x_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("GII"),y=Labeler("Sum_bd_best"))

### Import und Export
Im_Ex_p <-Stat_Data_geo%>%
  filter(Konflikt ==TRUE)%>%
  ggplot(aes(Im_Ex,Sum_bd_best, shape= Konflikt))+
  geom_point()+
  geom_smooth(stat= "smooth")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  scale_x_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("Im_Ex"),y=Labeler("Sum_bd_best"))

### Inequality Koeffizient

Ineq_Coef_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(Ineq_Coef,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat= "smooth")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("Ineq_Coef"),y=Labeler("Sum_bd_best"))

### adjusted HDI

adjHDI_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(adjHDI,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat= "smooth")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("adjHDI"),y=Labeler("Sum_bd_best"))

### Education Index
EDI_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(EDI,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat= "smooth")+ 
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("EDI"),y=Labeler("Sum_bd_best"))

### Bruttonationaleinkommen

GNI_p <- Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(GNI,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  #geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("GNI"),y=Labeler("Sum_bd_best"))

### Investment

Investment_p <- Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(Investment,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
   scale_x_continuous(trans='log10',
                      breaks=trans_breaks('log10', function(x) 10^x),
                      labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("Investment"),y=Labeler("Sum_bd_best"))


### totaler Demokratie Index
Demo_total_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(total_index_core,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("total_index_core"),y=Labeler("Sum_bd_best"))

### Kontrolle über Politik
Demo_control_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(control_dim_index_core,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("control_dim_index_core"),y=Labeler("Sum_bd_best"))

### Gleicheit im politischen System
demo_equal_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(equality_dim_index_core,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("equality_dim_index_core"),y=Labeler("Sum_bd_best"))

### Freiheit in der Demokratie
Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(freedom_dim_index_core,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("freedom_dim_index_core"),y=Labeler("Sum_bd_best"))

### Population ages 20-24, female (% of female population)

pop_20_24f_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SP.POP.2024.FE.5Y,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SP.POP.2024.FE.5Y"),y=Labeler("Sum_bd_best"))

### Population ages 20-24, male (% of male population)
pop_20_24m_p<- Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SP.POP.2024.MA.5Y,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SP.POP.2024.MA.5Y"),y=Labeler("Sum_bd_best"))

### Population Male

pop_m_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SP.POP.TOTL.MA.ZS,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SP.POP.TOTL.MA.ZS"),y=Labeler("Sum_bd_best"))

### Population female
pop_f_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SP.POP.TOTL.FE.ZS,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SP.POP.TOTL.FE.ZS"),y=Labeler("Sum_bd_best"))

### Population ages 25-29, female (% of female population)
pop_25_29f_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SP.POP.2529.FE.5Y,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SP.POP.2529.FE.5Y"),y=Labeler("Sum_bd_best"))

### Population ages 25-29, male (% of male population)
pop_25_29m_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SP.POP.2529.MA.5Y,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SP.POP.2529.MA.5Y"),y=Labeler("Sum_bd_best"))

### Control of corruption
Corruption_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(CC.EST,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("CC.EST"),y=Labeler("Sum_bd_best"))

### Government Effectiveness
Government_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(GE.EST,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("GE.EST"),y=Labeler("Sum_bd_best"))

### Political Stability

stability_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(PV.EST,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("PV.EST"),y=Labeler("Sum_bd_best"))

### Regulatory Quality

Regulatory_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(RQ.EST,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("RQ.EST"),y=Labeler("Sum_bd_best"))

### Rule of Law
ROL_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(RL.EST,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("RL.EST"),y=Labeler("Sum_bd_best"))

### Voice and accountability

voice_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(VA.EST,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("VA.EST"),y=Labeler("Sum_bd_best"))


### Labor force participation rate for ages 15-24, female (%) (modeled ILO estimate)
lfp_15_24f_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SL.TLF.ACTI.1524.FE.ZS,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SL.TLF.ACTI.1524.FE.ZS"),y=Labeler("Sum_bd_best"))

### Labor force participation rate, female (% of female population ages 15+) (modeled ILO estimate)

lfp_15f_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SL.TLF.CACT.FE.ZS,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SL.TLF.CACT.FE.ZS"),y=Labeler("Sum_bd_best"))

### Labor force, female (% of total labor force)
lf_f_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SL.TLF.TOTL.FE.ZS,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SL.TLF.TOTL.FE.ZS"),y=Labeler("Sum_bd_best"))

### Ratio of female to male labor force participation rate (%) (modeled ILO estimate)

lfp_fm_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SL.TLF.CACT.FM.ZS,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SL.TLF.CACT.FM.ZS"),y=Labeler("Sum_bd_best"))

### GDP (constant LCU)

GDP_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(NY.GDP.MKTP.KN,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  scale_x_continuous(trans='log10',
                      breaks=trans_breaks('log10', function(x) 10^x),
                      labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("NY.GDP.MKTP.KN"),y=Labeler("Sum_bd_best"))

### GDP growth (annual %)

GDP_growth_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(NY.GDP.MKTP.KD.ZG,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  scale_x_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("NY.GDP.MKTP.KD.ZG"),y=Labeler("Sum_bd_best"))

### CPIA property rights and rule-based governance rating (1=low to 6=high)

CPIA_gov_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(IQ.CPA.PROP.XQ,Sum_bd_best))+
  geom_point(position = "jitter")+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("IQ.CPA.PROP.XQ"),y=Labeler("Sum_bd_best"))

### CPIA gender equality rating (1=low to 6=high)
CPIA_gen_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(IQ.CPA.GNDR.XQ,Sum_bd_best))+
  geom_point(position = "jitter")+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("IQ.CPA.GNDR.XQ"),y=Labeler("Sum_bd_best"))

### Literacy rate, youth (ages 15-24), gender parity index (GPI) -> komische darstellung und sagt nichts aus
Literacy_rate_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SE.ADT.1524.LT.FM.ZS,Sum_bd_best))+
  geom_point(position = "jitter")+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SE.ADT.1524.LT.FM.ZS"),y=Labeler("Sum_bd_best"))

### School enrollment, primary (gross), gender parity index (GPI)

GPI_school_pri_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SE.ENR.PRIM.FM.ZS,Sum_bd_best))+
  geom_point(position = "jitter")+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SE.ENR.PRIM.FM.ZS"),y=Labeler("Sum_bd_best"))

### School enrollment, tertiary (gross), gender parity index (GPI)

GPI_school_ter_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SE.ENR.TERT.FM.ZS,Sum_bd_best))+
  geom_point(position = "jitter")+
  geom_smooth(stat = "smooth")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SE.ENR.TERT.FM.ZS"),y=Labeler("Sum_bd_best"))
