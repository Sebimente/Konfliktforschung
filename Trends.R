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

HDI_Trend <- Stat_Data_geo%>% select(`HDI`, `Year`, `Country.Code`, `Country.Name.x`) %>% 
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
