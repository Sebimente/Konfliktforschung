## Kartographische Darstellung von Konflikten

countries_sf <- ne_countries(returnclass = "sf")
countries_sf %>%
  ggplot() +
  geom_sf()


data_map <- inner_join(countries_sf,Data_geo, by = c("iso_a3"="Country Code"))
head(data_map)
## Konflikte auf L?nder in 2019
Konflikt_map <- data_map %>%
  filter(Year== 2020)%>%
  ggplot(aes(fill= Konflikt))+
  geom_sf(col= "black", size= 0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  theme_minimal()

Konflikt_map  ### irgendwie noch die type_of_conflict filtern

HDI_map <- data_map %>%
  filter(Year == 2011)%>%
  ggplot(aes(fill= HDI))+
  geom_sf(col="black", size = 0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  scale_fill_viridis_c(direction = -1)+
  theme_minimal()
HDI_map

GII_map <- data_map %>%
  filter(Year == 2019)%>%
  ggplot(aes(fill= GII))+
  geom_sf(col="black", size = 0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  scale_fill_viridis_c(direction = -1)+
  theme_minimal()
GII_map

EDI_map <- data_map %>%
  filter(Year == 2019)%>%
  ggplot(aes(fill= EDI))+
  geom_sf(col="black", size = 0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  scale_fill_viridis_c(direction = -1)+
  theme_minimal()
EDI_map


GINI_map<- data_map%>%
  filter(Year==2019)%>%
  ggplot(aes(fill= SI.POV.GINI))+
  geom_sf(col="black", size = 0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  scale_fill_viridis_c(direction = -1)+
  theme_minimal()
GINI_map  ### kaum Daten

Gov_map <- data_map%>%
  filter(Year==2019)%>%
  ggplot(aes(fill=PV.EST))+
  geom_sf(col="black", size = 0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  scale_fill_viridis_c(direction = -1)+
  theme_minimal()
Gov_map