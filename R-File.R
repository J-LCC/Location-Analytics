rm(list=ls())
setwd("C:/Users/Jose/Desktop/RSpatialTutorial")
 
is.installed <- function(package) is.element(package, installed.packages())

if(!is.installed('rgdal'))
  install.packages('rgdal')
library(rgdal)

if(!is.installed('rgeos'))
  install.packages('rgeos')
library(rgeos)

if(!is.installed('tmap'))
  install.packages('tmap')
library(tmap)

if(!is.installed('dplyr'))
  install.packages('dplyr')
library(dplyr)



i_data_gs <- readOGR(dsn = "data", layer = "london_sport")
i_data_gs$Pop_2001 <- as.numeric(as.character(i_data_gs$Pop_2001))

LonCos <- read.csv("C:/Users/Jose/Desktop/RSpatialTutorial/data/LondonCustomer.csv", 
                      header = TRUE, sep = ";")

LonCos$NETPRICE_PRO14_AMT <- as.integer(LonCos$NETPRICE_PRO14_AMT)





attach(LonCos)
col <- c("NETPRICE_PRO11_AMT", "NETPRICE_PRO12_AMT", "NETPRICE_PRO13_AMT",
         "NETPRICE_PRO14_AMT", "NETPRICE_PRO15_AMT", "NETPRICE_PRO16_AMT",
         "NETPRICE_PRO17_AMT")

# Sumamos el total de ganancias
LonCos$NETPRICE_TOTAL <- rowSums(LonCos[,col])

# Sustituimos los NA por 0 en la columna de ganancias totales 
LonCos$NETPRICE_TOTAL[is.na(LonCos$NETPRICE_TOTAL)] <- 0

# Agrupamos todos los clientes por la provincia
ag_clien <- aggregate(LonCos$CONTACT_ID, by=list(LonCos$name), FUN = length)
ag_clien <- ag_clien %>% rename(clientes = x)
ag_clien <- ag_clien %>% rename(name = Group.1)

# Sumamos las ganancias totales por distrito
ag_net <- LonCos %>% group_by(name) %>% summarise(beneficio = sum(NETPRICE_TOTAL))

# Ahora calculamos la media de edad de los clientes de cada distrito
ag_edad <- LonCos %>% group_by(name) %>% summarise(media_edad = mean(AGE))








# Unimos las columnas creadas a los datos espaciales
data <- merge(i_data_gs@data, c(ag_edad, ag_net, ag_clien), by = "name", all.x = T )
data <- select(data, -c(6,8))

# Ahora vamos a construir un objeto espacial nuevo que tenga los mismos datos espaciales

data_gs<-merge(i_data_gs, data[,c(1,5:7)], by = "name", all.x=TRUE)







as_tibble(head(data_gs@data))








tm_shape(data_gs) +
  tm_borders(col = 'black', lwd = 0.3) +
  tm_fill(col = 'beneficio', title = 'Beneficios', legend.hist = TRUE, palette = "Blues") +
  tm_legend(legend.outside = TRUE) +
  tm_layout(frame = FALSE) 

tm_shape(data_gs) +
  tm_borders(col = 'black', lwd = 0.3) +
  tm_fill(col = 'media_edad', title = 'Media de edad', legend.hist = TRUE, palette = "Blues") +
  tm_legend(legend.outside = TRUE) +
  tm_layout(frame = FALSE) 








sel1 <- data_gs$name == "Haringey"
sel2 <- data_gs$name == "Wandsworth"
sel3 <- data_gs$name == "Hammersmith and Fulham"
sel4 <- data_gs$name == "Bromley"
sel5 <- data_gs$name == "Sutton"
cent_data_gs_ha <- gCentroid(data_gs[sel1,])
cent_data_gs_wan <- gCentroid(data_gs[sel2,])
cent_data_gs_hf <- gCentroid(data_gs[sel3,])
cent_data_gs_bro <- gCentroid(data_gs[sel4,])
cent_data_gs_sut <- gCentroid(data_gs[sel5,])

# Filtramos y coloremos las regiones que vamos a eliminar
filtro <- data_gs$beneficio < 9000 & data_gs$media_edad < 46

plot(i_data_gs, col = "azure1")
plot(i_data_gs[filtro, ], col = "darkgoldenrod1", add = TRUE) 
text(coordinates(cent_data_gs_ha), "Haringey")
text(coordinates(cent_data_gs_wan), "Wandsworth")
text(coordinates(cent_data_gs_hf), "Hammersmith and Hulham")
text(coordinates(cent_data_gs_bro), "Bromley")
text(coordinates(cent_data_gs_sut), "Sutton")








tm_shape(data_gs) +
  tm_borders(col = 'black', lwd = 0.3) +
  tm_fill(col = 'clientes', title = 'Nº de clientes', legend.hist = TRUE, palette = "Blues") +
  tm_legend(legend.outside = TRUE) +
  tm_layout(frame = FALSE) +
  tm_text("name", size = 0.7)








tm_shape(data_gs[filtro,]) +
  tm_polygons("beneficio", title = "Beneficio", palette = "Blues") + tm_text("name", size = 0.7) +
  tm_legend(legend.position = c("right", "top"), frame = TRUE, legend.text.size = 0.9)
tm_shape(data_gs[filtro,]) +
  tm_polygons("media_edad", title = "Media de edad", palette = "Blues") + tm_text("name", size = 0.7) +
  tm_legend(legend.position = c("right", "top"), frame = TRUE, legend.text.size = 0.9)





# Vamos a localizar a los clientes de las zonas clausuradas a un distrito cercano
# Seleccionamos las zonas geograficas que esten a 2km del centroide geografico
data_gs_buffer_ha <- gBuffer(spgeom = cent_data_gs_ha, width = 2000)
data_gs_buffer_wan <- gBuffer(spgeom = cent_data_gs_wan, width = 2000)
data_gs_buffer_hf <- gBuffer(spgeom = cent_data_gs_hf, width = 2000)

# Seleccionamos las provincias dentro de esta zona
data_gs_central_ha = data_gs[data_gs_buffer_ha,]
data_gs_central_wan = data_gs[data_gs_buffer_wan,]
data_gs_central_hf = data_gs[data_gs_buffer_hf,]

# Pintamos los graficos con sus respectivas zonas

# Zona de Haringey
plot(data_gs, col = "azure2")
plot(data_gs_central_ha, col = "coral", add = T)
plot(data_gs_buffer_ha, add = T)
text(coordinates(cent_data_gs_ha), "Haringey", cex=0.8)
# Zona de Wandsworth
plot(data_gs_central_wan, col = "coral", add = T)
plot(data_gs_buffer_wan, add = T)
text(coordinates(cent_data_gs_wan), "Wandsworth", cex=0.8)
# Zona de Hammersmith and Fulham
plot(data_gs_central_hf, col = "coral", add = T)
plot(data_gs_buffer_hf, add = T)
text(coordinates(cent_data_gs_hf), "Hammersmith", cex=0.8)

# Por último, pintamos el nombre de las regiones donde serán localizados
sel6 <- data_gs$name == "Islington"
sel7 <- data_gs$name == "Kensington and Chelsea"
sel8 <- data_gs$name == "Merton"

cent_data_gs_is <- gCentroid(data_gs[sel6,])
cent_data_gs_ho <- gCentroid(data_gs[sel7,])
cent_data_gs_mer <- gCentroid(data_gs[sel8,])

text(coordinates(cent_data_gs_is), "Islington", cex=0.8)
text(coordinates(cent_data_gs_ho), "Kensington", cex=0.8,pos=3)
text(coordinates(cent_data_gs_mer), "Merton", cex=0.8)
