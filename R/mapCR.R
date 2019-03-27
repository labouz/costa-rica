library(rgdal)
library(sp)
library(dplyr)
library(tmap)
library(leaflet)
library(ggmap)
library(viridis)

cr <- readOGR("maps/CRI_adm","CRI_adm0")
regions <- readOGR("maps/CRI_adm","CRI_adm1")
#cantons <- readOGR("maps/CRI_adm","CRI_adm2")


plot(cr)
plot(regions)
#plot(cantons)

x <- viridis_pal()(10)

pal <- colorFactor(x, levels = regions$ID_1)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = regions, weight = 2, 
              fillColor = ~pal(regions$ID_1),
              fillOpacity = 0.5, color = "blue")  %>% 
  addLegend(pal = pal, values = regions$NAME_1)



#another way to map
x <- tm_shape(regions) +
  tm_borders(lwd = 2) +
  tm_fill(col = "NAME_1", popup.vars = "NAME_1")


save_tmap(x, filename = "CRregions.html")
