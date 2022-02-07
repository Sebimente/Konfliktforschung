#################################### die Daten deskiptiv untersuchen ##### Plots
### schneller Zugang


### Demokratie
Demo_total_p
Demo_total_box_regio 
Demo_total_box 
Demo_total_vio
demo_equal_p
demo_equal_box_regio 
demo_equal_box 
demo_equal_vio
Demo_control_p
Demo_control_box_regio 
Demo_control_box 
Demo_control_vio 

### Governance
CPIA_gov_p # evtl. Dummy weil kategoriale Werte
CPIA_gov_vio 
CPIA_gov_box 
CPIA_gov_box_regio
freedom_p
freedom_box_regio
freedom_box 
freedom_vio
Corruption_p
Corruption_box_regio 
Corruption_box 
Corruption_vio
Regulatory_p
Regulatory_box_regio 
Regulatory_box 
Regulatory_vio
Government_p
Government_vio 
Government_box 
Government_box_regio
ROL_p
ROL_box_regio 
ROL_box 
ROL_vio
stability_p
stability_vio 
stability_box 
stability_box_regio
voice_p
voice_box_regio 
voice_box 
voice_vio     
### Gender
GDI_p
GDI_vio 
GDI_box 
GDI_box_regio
GII_p
GII_vio 
GII_box 
GII_box_regio 
FiParlament_p
FiParlament_box_regio 
FiParlament_box
FiParlament_vio
CPIA_gen_p # evtl. Dummy weil kategoriale Werte
CPIA_gen_vio 
CPIA_gen_box 
CPIA_gen_box_regio
### Wirtschaft
GDPpCapiat_p
GDPpCapiat_box_regio 
GDPpCapiat_box 
GDPpCapiat_vio
Im_Ex_p
Im_Ex_box_regio 
Im_Ex_box 
Im_Ex_vio
GNI_p
GNI_box_regio 
GNI_box
GNI_vio
Investment_p
Investment_box_regio
Investment_box
Investment_vio
GDP_growth_p
GDP_growth_box_regio
GDP_growth_box
GDP_growth_vio
GDP_p
GDP_box_regio 
GDP_box 
GDP_vio
   
### Arbeitsmakrt
lfp_15_24f_p
lfp_15_24f_box_regio 
lfp_15_24f_box 
lfp_15_24f_vio
lfp_15f_p
lfp_15f_box_regio 
lfp_15f_box 
lfp_15f_vio
lfp_fm_p
lfp_fm_vio 
lfp_fm_box 
lfp_fm_box_regio
lf_f_p
lf_f_box_regio 
lf_f_box 
lf_f_vio  
### Bildung
EDI_p
EDI_box_regio 
EDI_box 
EDI_vio
GPI_school_pri_p
GPI_school_ter_p
Literacy_rate_p
### Demographie
pop_20_24f_p
pop_20_24f_box_regio 
pop_20_24f_box 
pop_20_24f_vio
pop_20_24m_p
pop_20_24m_box_regio 
pop_20_24m_box 
pop_20_24m_vio
pop_m_p
pop_m_box_regio 
pop_m_box 
pop_m_vio
pop_f_p
pop_f_box_regio 
pop_f_box 
pop_f_vio
pop_25_29f_p
pop_25_29f_box_regio 
pop_25_29f_box 
pop_25_29f_vio
pop_25_29m_p
pop_25_29m_box_regio 
pop_25_29m_box 
pop_25_29m_vio     
### Index für allgemien ENtwicklung
HDI_p
HDI_box_regio 
HDI_box 
HDI_vio 
Ineq_Coef_p
Ineq_Coef_box_regio 
Ineq_Coef_box 
Ineq_Coef_vio
adjHDI_p
adjHDI_vio 
adjHDI_box 
adjHDI_box_regio  
### Balkendiagramme 
Konf_regio
Konf_type
demokratie_Konf



###### BAlkendiagramme
### UN Regionen und die Verteilung von Konflikten
Konf_regio <-Data_geo%>%
  filter(Konflikt==TRUE)%>%
  ggplot(aes(region_un))+
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
Data_geo%>%
  filter(Konflikt==TRUE)%>%
  ggplot(aes(region_wb))+
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

demokratie_Konf <-Data_geo%>%
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


###GDP per capita

GDPpCapiat_p <-Stat_Data_geo%>%
  #filter(Konflikt== TRUE)%>%
 # filter(type_of_conflict== 3)%>%
  ggplot(aes(x=NY.GDP.PCAP.CN, y= Sum_bd_best))+
  geom_point(position = "jitter")+
  geom_smooth(stat="smooth", color="red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  scale_x_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x= Labeler("NY.GDP.PCAP.CN"),y=Labeler("bd_best"))+
  theme_minimal()


GDPpCapiat_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,NY.GDP.PCAP.CN))+
  geom_violin(scale= "area")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x= Labeler("Konflikt"),y=Labeler("NY.GDP.PCAP.CN"))+
  theme_minimal()

GDPpCapiat_box <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,NY.GDP.PCAP.CN))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("NY.GDP.PCAP.CN"))+
  theme_minimal()

GDPpCapiat_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,NY.GDP.PCAP.CN))+
  geom_boxplot()+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x= Labeler("Konflikt"),y=Labeler("NY.GDP.PCAP.CN"))+
  theme_minimal()

### Sekbstbestimmung im politischen System
freedom_p <-Stat_Data_geo%>%
  #filter(Konflikt== TRUE)%>%
  #filter(type_of_conflict==3)%>%
  ggplot(aes(freedom_dim_index_core,Sum_bd_best))+
  geom_point(position="jitter")+
  geom_smooth(stat = "smooth", color= "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x= Labeler("freedom_dim_index_core"), y= Labeler("Sum_bd_best"))+
  theme_minimal()

freedom_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,freedom_dim_index_core))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("freedom_dim_index_core"))+
  theme_minimal()

freedom_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,freedom_dim_index_core))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("freedom_dim_index_core"))+
  theme_minimal()



freedom_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,freedom_dim_index_core))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("freedom_dim_index_core"))+
  theme_minimal()



### Human development Index
HDI_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  #filter(type_of_conflict==3)%>%
  ggplot(aes(HDI,Sum_bd_best))+
  geom_point()+
  geom_smooth(method = "glm")+
  geom_smooth(stat= "smooth", color= "red")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("HDI"),y=Labeler("Sum_bd_best"))+
  theme_minimal()

HDI_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,HDI))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("HDI"))+
  theme_minimal()


HDI_box <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,HDI))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("HDI"))+
  theme_minimal()

HDI_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,HDI))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("HDI"))+
  theme_minimal()

### Gender Development Index
GDI_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  #filter(type_of_conflict==3)%>%
  ggplot(aes(GDI,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat= "smooth", color= "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("GDI"),y=Labeler("Sum_bd_best"))
GDI_box_regio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,GDI))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("GDI"))+
  theme_minimal()

GDI_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,GDI))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("GDI"))+
  theme_minimal()

GDI_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,GDI))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("GDI"))+
  theme_minimal()

### Gender Inequality index -> fast identisch zum GDI. Der GDi hat mehr Werte
GII_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  #filter(type_of_conflict==3)%>%
  ggplot(aes(GII,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat= "smooth", color= "red")+
  geom_smooth(method = "lm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("GII"),y=Labeler("Sum_bd_best"))

GII_box_regio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,GII))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("GII"))+
  theme_minimal()

GII_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,GII))+
  geom_boxplot()+
  labs(x=Labeler("Konflikt"),y=Labeler("GII"))+
  theme_minimal()

GII_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,GII))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("GII"))+
  theme_minimal()

### Import und Export
Im_Ex_p <-Stat_Data_geo%>%
  ggplot(aes(Im_Ex,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat= "smooth", color= "red")+
  geom_smooth(method = "lm")+
   scale_y_continuous(trans='log10',
                      breaks=trans_breaks('log10', function(x) 10^x),
                      labels=trans_format('log10', math_format(10^.x)))+
   scale_x_continuous(trans='log10',
                      breaks=trans_breaks('log10', function(x) 10^x),
                      labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("Im_Ex"),y=Labeler("Sum_bd_best"))

Im_Ex_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,Im_Ex))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("Im_Ex"))+
  theme_minimal()

Im_Ex_box <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,Im_Ex))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("Im_Ex"))+
  theme_minimal()

Im_Ex_vio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,Im_Ex))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("Im_Ex"))+
  theme_minimal()

### Inequality Koeffizient

Ineq_Coef_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(Ineq_Coef,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat= "smooth", color= "red")+
  geom_smooth(method = "lm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("Ineq_Coef"),y=Labeler("Sum_bd_best"))

Ineq_Coef_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,Ineq_Coef))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("Ineq_Coef"))+
  theme_minimal()

Ineq_Coef_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,Ineq_Coef))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("Ineq_Coef"))+
  theme_minimal()

Ineq_Coef_vio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,Ineq_Coef))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("Ineq_Coef"))+
  theme_minimal()

### adjusted HDI

adjHDI_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(adjHDI,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat= "smooth", color = "red")+
  geom_smooth(method = "lm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("adjHDI"),y=Labeler("Sum_bd_best"))

adjHDI_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,adjHDI))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("adjHDI"))+
  theme_minimal()

adjHDI_box <- Stat_Data_geo%>%
  ggplot(aes(Konflikt,adjHDI))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("adjHDI"))+
  theme_minimal()

adjHDI_vio <- Stat_Data_geo%>%
  ggplot(aes(Konflikt,adjHDI))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("adjHDI"))+
  theme_minimal()


### Education Index
EDI_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(EDI,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat= "smooth", color= "red")+
  geom_smooth(method="lm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("EDI"),y=Labeler("Sum_bd_best"))

EDI_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,EDI))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("EDI"))+
  theme_minimal()

EDI_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,EDI))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("EDI"))+
  theme_minimal()

EDI_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,EDI))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("EDI"))+
  theme_minimal()


### Bruttonationaleinkommen

GNI_p <- Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(GNI,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color = "red")+
  geom_smooth(method = "lm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("GNI"),y=Labeler("Sum_bd_best"))

GNI_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,GNI))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("GNI"))+
  theme_minimal()

GNI_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,GNI))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("GNI"))+
  theme_minimal()

GNI_vio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,GNI))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("GNI"))+
  theme_minimal()

### Investment

Investment_p <- Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(Investment,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color= "red")+
  geom_smooth(method = "lm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
   scale_x_continuous(trans='log10',
                      breaks=trans_breaks('log10', function(x) 10^x),
                      labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("Investment"),y=Labeler("Sum_bd_best"))

Investment_box_regio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,Investment))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("Investment"))+
  theme_minimal()

## Ausreißer einfangen
Investment_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,Investment))+
  geom_boxplot()+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x= Labeler("Konflikt"),y=Labeler("Investment"))+
  theme_minimal()

Investment_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,Investment))+
  geom_violin(scale= "area")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x= Labeler("Konflikt"),y=Labeler("Investment"))+
  theme_minimal()

### totaler Demokratie Index
Demo_total_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(total_index_core,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color= "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("total_index_core"),y=Labeler("Sum_bd_best"))

Demo_total_box_regio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,total_index_core))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("total_index_core"))+
  theme_minimal()


Demo_total_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,total_index_core))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("total_index_core"))+
  theme_minimal()

Demo_total_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,total_index_core))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("total_index_core"))+
  theme_minimal()

### Kontrolle über Politik
Demo_control_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(control_dim_index_core,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color = "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("control_dim_index_core"),y=Labeler("Sum_bd_best"))

Demo_control_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,control_dim_index_core))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("control_dim_index_core"))+
  theme_minimal()

Demo_control_box <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,control_dim_index_core))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("control_dim_index_core"))+
  theme_minimal()

Demo_control_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,control_dim_index_core))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("control_dim_index_core"))+
  theme_minimal()

### Gleicheit im politischen System
demo_equal_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(equality_dim_index_core,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color = "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("equality_dim_index_core"),y=Labeler("Sum_bd_best"))

demo_equal_box_regio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,equality_dim_index_core))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("equality_dim_index_core"))+
  theme_minimal()

demo_equal_box <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,equality_dim_index_core))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("equality_dim_index_core"))+
  theme_minimal()

demo_equal_vio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,equality_dim_index_core))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("equality_dim_index_core"))+
  theme_minimal()

### Population ages 20-24, female (% of female population)

pop_20_24f_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SP.POP.2024.FE.5Y,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color= "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SP.POP.2024.FE.5Y"),y=Labeler("Sum_bd_best"))

pop_20_24f_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.2024.FE.5Y))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.2024.FE.5Y"))+
  theme_minimal()

pop_20_24f_box <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.2024.FE.5Y))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.2024.FE.5Y"))+
  theme_minimal()

pop_20_24f_vio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.2024.FE.5Y))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.2024.FE.5Y"))+
  theme_minimal()


### Population ages 20-24, male (% of male population)
pop_20_24m_p<- Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SP.POP.2024.MA.5Y,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color= "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SP.POP.2024.MA.5Y"),y=Labeler("Sum_bd_best"))

pop_20_24m_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.2024.MA.5Y))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.2024.MA.5Y"))+
  theme_minimal()

pop_20_24m_box <- Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.2024.MA.5Y))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.2024.MA.5Y"))+
  theme_minimal()

pop_20_24m_vio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.2024.MA.5Y))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.2024.MA.5Y"))+
  theme_minimal()

### Population Male

pop_m_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SP.POP.TOTL.MA.ZS,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color = "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SP.POP.TOTL.MA.ZS"),y=Labeler("Sum_bd_best"))

pop_m_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,PV.EST))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.TOTL.MA.ZS"))+
  theme_minimal()

Stat_Data_geo%>%
  ggplot(aes(SP.POP.TOTL.MA.ZS))+
  geom_density(kernel = "gaussian")

pop_m_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.TOTL.MA.ZS))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.TOTL.MA.ZS"))+
  theme_minimal()

pop_m_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.TOTL.MA.ZS))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.TOTL.MA.ZS"))+
  theme_minimal()


### Population female
pop_f_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SP.POP.TOTL.FE.ZS,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color= "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SP.POP.TOTL.FE.ZS"),y=Labeler("Sum_bd_best"))

Stat_Data_geo%>%
  ggplot(aes(SP.POP.TOTL.FE.ZS))+
  geom_density(kernel = "gaussian")

pop_f_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.TOTL.FE.ZS))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.TOTL.FE.ZS"))+
  theme_minimal()

pop_f_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.TOTL.FE.ZS))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.TOTL.FE.ZS"))+
  theme_minimal()

pop_f_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.TOTL.FE.ZS))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.TOTL.FE.ZS"))+
  theme_minimal()

### Population ages 25-29, female (% of female population)
pop_25_29f_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SP.POP.2529.FE.5Y,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color= "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SP.POP.2529.FE.5Y"),y=Labeler("Sum_bd_best"))

pop_25_29f_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.2529.FE.5Y))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.2529.FE.5Y"))+
  theme_minimal()

pop_25_29f_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.2529.FE.5Y))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.2529.FE.5Y"))+
  theme_minimal()

pop_25_29f_vio<- Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.2529.FE.5Y))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.2529.FE.5Y"))+
  theme_minimal()



### Population ages 25-29, male (% of male population)
pop_25_29m_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SP.POP.2529.MA.5Y,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color = "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SP.POP.2529.MA.5Y"),y=Labeler("Sum_bd_best"))

pop_25_29m_box_regio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.2529.MA.5Y))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.2529.MA.5Y"))+
  theme_minimal()

pop_25_29m_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.2529.MA.5Y))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.2529.MA.5Y"))+
  theme_minimal()

pop_25_29m_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.POP.2529.MA.5Y))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("SP.POP.2529.MA.5Y"))+
  theme_minimal()

### Control of corruption
Corruption_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(CC.EST,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color = "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("CC.EST"),y=Labeler("Sum_bd_best"))

Corruption_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,CC.EST))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("CC.EST"))+
  theme_minimal()

Corruption_box <- Stat_Data_geo%>%
  ggplot(aes(Konflikt,CC.EST))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("CC.EST"))+
  theme_minimal()

Corruption_vio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,CC.EST))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("CC.EST"))+
  theme_minimal()

### Government Effectiveness
Government_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(GE.EST,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color = "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("GE.EST"),y=Labeler("Sum_bd_best"))

Government_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,GE.EST))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("GE.EST"))+
  theme_minimal()

Government_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,GE.EST))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("GE.EST"))+
  theme_minimal()

Government_vio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,GE.EST))+
  geom_violin(scale= "area")+
  labs(x= Labeler("Konflikt"),y=Labeler("GE.EST"))+
  theme_minimal()

### Political Stability

stability_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(PV.EST,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color= "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("PV.EST"),y=Labeler("Sum_bd_best"))

stability_box_regio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,PV.EST))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("PV.EST"))+
  theme_minimal()

stability_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,PV.EST))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("PV.EST"))+
  theme_minimal()

stability_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,PV.EST))+
  geom_violin(scale= "area", draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(x= Labeler("Konflikt"),y=Labeler("PV.EST"))+
  theme_minimal()

### Regulatory Quality

Regulatory_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(RQ.EST,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color = "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("RQ.EST"),y=Labeler("Sum_bd_best"))

Regulatory_box_regio<- Stat_Data_geo%>%
  ggplot(aes(Konflikt,RQ.EST))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("RQ.EST"))+
  theme_minimal()

Regulatory_box<- Stat_Data_geo%>%
  ggplot(aes(Konflikt,RQ.EST))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("RQ.EST"))+
  theme_minimal()

Regulatory_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,RQ.EST))+
  geom_violin(scale= "area", draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(x= Labeler("Konflikt"),y=Labeler("RQ.EST"))+
  theme_minimal()

### Rule of Law
ROL_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(RL.EST,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color = "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("RL.EST"),y=Labeler("Sum_bd_best"))

ROL_box_regio<- Stat_Data_geo%>%
  ggplot(aes(Konflikt,RL.EST))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("RL.EST"))+
  theme_minimal()

ROL_box<- Stat_Data_geo%>%
  ggplot(aes(Konflikt,RL.EST))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("RL.EST"))+
  theme_minimal()

ROL_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,RL.EST))+
  geom_violin(scale= "area", draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(x= Labeler("Konflikt"),y=Labeler("RL.EST"))+
  theme_minimal()

### Voice and accountability

voice_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(VA.EST,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color = "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("VA.EST"),y=Labeler("Sum_bd_best"))

voice_box_regio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,VA.EST))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("VA.EST"))+
  theme_minimal()

voice_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,VA.EST))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("VA.EST"))+
  theme_minimal()

voice_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,VA.EST))+
  geom_violin(scale= "area", draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(x= Labeler("Konflikt"),y=Labeler("VA.EST"))+
  theme_minimal()

### Labor force participation rate for ages 15-24, female (%) (modeled ILO estimate)
lfp_15_24f_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SL.TLF.ACTI.1524.FE.ZS,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color= "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SL.TLF.ACTI.1524.FE.ZS"),y=Labeler("Sum_bd_best"))

lfp_15_24f_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SL.TLF.ACTI.1524.FE.ZS))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("SL.TLF.ACTI.1524.FE.ZS"))+
  theme_minimal()

lfp_15_24f_box <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SL.TLF.ACTI.1524.FE.ZS))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("SL.TLF.ACTI.1524.FE.ZS"))+
  theme_minimal()

lfp_15_24f_vio<- Stat_Data_geo%>%
  ggplot(aes(Konflikt,SL.TLF.ACTI.1524.FE.ZS))+
  geom_violin(scale= "area", draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(x= Labeler("Konflikt"),y=Labeler("SL.TLF.ACTI.1524.FE.ZS"))+
  theme_minimal()


### Labor force participation rate, female (% of female population ages 15+) (modeled ILO estimate)

lfp_15f_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SL.TLF.CACT.FE.ZS,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color = "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SL.TLF.CACT.FE.ZS"),y=Labeler("Sum_bd_best"))

lfp_15f_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SL.TLF.CACT.FE.ZS))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("SL.TLF.CACT.FE.ZS"))+
  theme_minimal()

lfp_15f_box <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SL.TLF.CACT.FE.ZS))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("SL.TLF.CACT.FE.ZS"))+
  theme_minimal()

lfp_15f_vio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SL.TLF.CACT.FE.ZS))+
  geom_violin(scale= "area", draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(x= Labeler("Konflikt"),y=Labeler("SL.TLF.CACT.FE.ZS"))+
  theme_minimal()

### Labor force, female (% of total labor force)
lf_f_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SL.TLF.TOTL.FE.ZS,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color= "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SL.TLF.TOTL.FE.ZS"),y=Labeler("Sum_bd_best"))

lf_f_box_regio <- Stat_Data_geo%>%
  ggplot(aes(Konflikt,SL.TLF.TOTL.FE.ZS))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("SL.TLF.TOTL.FE.ZS"))+
  theme_minimal()

lf_f_box <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SL.TLF.TOTL.FE.ZS))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("SL.TLF.TOTL.FE.ZS"))+
  theme_minimal()

lf_f_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SL.TLF.TOTL.FE.ZS))+
  geom_violin(scale= "area", draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(x= Labeler("Konflikt"),y=Labeler("SL.TLF.TOTL.FE.ZS"))+
  theme_minimal()

### Ratio of female to male labor force participation rate (%) (modeled ILO estimate)

lfp_fm_p <-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SL.TLF.CACT.FM.ZS,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color = "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SL.TLF.CACT.FM.ZS"),y=Labeler("Sum_bd_best"))


lfp_fm_box_regio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SL.TLF.CACT.FM.ZS))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("SL.TLF.CACT.FM.ZS"))+
  theme_minimal()

lfp_fm_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SL.TLF.CACT.FM.ZS))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("SL.TLF.CACT.FM.ZS"))+
  theme_minimal()

lfp_fm_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SL.TLF.CACT.FM.ZS))+
  geom_violin(scale= "area", draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(x= Labeler("Konflikt"),y=Labeler("SL.TLF.CACT.FM.ZS"))+
  theme_minimal()

### GDP (constant LCU)

GDP_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(NY.GDP.MKTP.KN,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color = "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  scale_x_continuous(trans='log10',
                      breaks=trans_breaks('log10', function(x) 10^x),
                      labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("NY.GDP.MKTP.KN"),y=Labeler("Sum_bd_best"))

GDP_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,NY.GDP.MKTP.KN))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("NY.GDP.MKTP.KN"))+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  theme_minimal()

GDP_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,NY.GDP.MKTP.KN))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("NY.GDP.MKTP.KN"))+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  theme_minimal()

GDP_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,NY.GDP.MKTP.KN))+
  geom_violin(scale= "area", draw_quantiles = c(0.25, 0.5, 0.75))+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x= Labeler("Konflikt"),y=Labeler("NY.GDP.MKTP.KN"))+
  theme_minimal()


### GDP growth (annual %)

GDP_growth_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(NY.GDP.MKTP.KD.ZG,Sum_bd_best))+
  geom_point()+
  geom_smooth(stat = "smooth", color= "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  scale_x_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("NY.GDP.MKTP.KD.ZG"),y=Labeler("Sum_bd_best"))

GDP_growth_box_regio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,NY.GDP.MKTP.KD.ZG))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("NY.GDP.MKTP.KD.ZG"))+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  theme_minimal()

GDP_growth_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,NY.GDP.MKTP.KD.ZG))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("NY.GDP.MKTP.KD.ZG"))+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  theme_minimal()

GDP_growth_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,NY.GDP.MKTP.KD.ZG))+
  geom_violin(scale= "area", draw_quantiles = c(0.25, 0.5, 0.75))+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  labs(x= Labeler("Konflikt"),y=Labeler("NY.GDP.MKTP.KD.ZG"))+
  theme_minimal()

### CPIA property rights and rule-based governance rating (1=low to 6=high)

CPIA_gov_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(IQ.CPA.PROP.XQ,Sum_bd_best))+
  geom_point(position = "jitter")+
  geom_smooth(stat = "smooth", color = "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("IQ.CPA.PROP.XQ"),y=Labeler("Sum_bd_best"))


CPIA_gov_box_regio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,IQ.CPA.PROP.XQ))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("IQ.CPA.PROP.XQ"))+
  theme_minimal()

CPIA_gov_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,IQ.CPA.PROP.XQ))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("IQ.CPA.PROP.XQ"))+
  theme_minimal()

CPIA_gov_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,IQ.CPA.PROP.XQ))+
  geom_violin(scale= "area", draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(x= Labeler("Konflikt"),y=Labeler("IQ.CPA.PROP.XQ"))+
  theme_minimal()


### CPIA gender equality rating (1=low to 6=high)
CPIA_gen_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(IQ.CPA.GNDR.XQ,Sum_bd_best))+
  geom_point(position = "jitter")+
  geom_smooth(stat = "smooth", color = "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("IQ.CPA.GNDR.XQ"),y=Labeler("Sum_bd_best"))

CPIA_gen_box_regio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,IQ.CPA.GNDR.XQ))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("IQ.CPA.GNDR.XQ"))+
  theme_minimal()

CPIA_gen_box<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,IQ.CPA.GNDR.XQ))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("IQ.CPA.GNDR.XQ"))+
  theme_minimal()

CPIA_gen_vio<-Stat_Data_geo%>%
  ggplot(aes(Konflikt,IQ.CPA.GNDR.XQ))+
  geom_violin(scale= "area", draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(x= Labeler("Konflikt"),y=Labeler("IQ.CPA.GNDR.XQ"))+
  theme_minimal()


### Anteil an Frauen im Parlament

FiParlament_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SG.GEN.PARL.ZS,Sum_bd_best))+
  geom_point(position = "jitter")+
  geom_smooth(stat = "smooth", color= "red")+
  geom_smooth(method = "glm")+
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  # scale_x_continuous(trans='log10',
  #                    breaks=trans_breaks('log10', function(x) 10^x),
  #                    labels=trans_format('log10', math_format(10^.x)))+
  labs(x=Labeler("SG.GEN.PARL.ZS"),y=Labeler("Sum_bd_best"))

FiParlament_box_regio <-Stat_Data_geo%>%
  ggplot(aes(Konflikt,SG.GEN.PARL.ZS))+
  geom_boxplot(aes(colour = region_wb))+
  labs(x= Labeler("Konflikt"),y=Labeler("SG.GEN.PARL.ZS"))+
  theme_minimal()

FiParlament_box <- Stat_Data_geo%>%
  ggplot(aes(Konflikt,SG.GEN.PARL.ZS))+
  geom_boxplot()+
  labs(x= Labeler("Konflikt"),y=Labeler("SG.GEN.PARL.ZS"))+
  theme_minimal()

FiParlament_vio<- Stat_Data_geo%>%
  ggplot(aes(Konflikt,SG.GEN.PARL.ZS))+
  geom_violin(scale= "area", draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(x= Labeler("Konflikt"),y=Labeler("SG.GEN.PARL.ZS"))+
  theme_minimal()


### Literacy rate, youth (ages 15-24), gender parity index (GPI) -> komische darstellung und sagt nichts aus
Literacy_rate_p<-Stat_Data_geo%>%
  #filter(Konflikt ==TRUE)%>%
  ggplot(aes(SE.ADT.1524.LT.FM.ZS,Sum_bd_best))+
  geom_point(position = "jitter")+
  geom_smooth(stat = "smooth", color = "red")+
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

