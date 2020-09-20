library(raster)
library(tidyverse)
library(mapview)
# descarga de la divisi?n administrativa
adm <- getData(name = "GADM", 
               country = "per",
               level = 1)

# selecci?n del departamento Loreto
lor <- adm[adm@data$NAME_1 %in% "Loreto",]
crd <- as.data.frame(coordinates(lor))

# descarga de la  temperatur promedio
# La base datos de Worldclim son grados celcius, pero la variable se debe multiplicar por un factor de escala de 0.1
tmean <- getData(name = "worldclim",
                 var = "tmean",
                 res = 0.5,
                 lon = crd[1,1],
                 lat = crd[1,2])

mapview(tmean)
tmean_lor <- crop(tmean, lor) %>%
  mask(., lor)

tmean_lor_promedio <- mean(tmean_lor)
mapview(tmean_lor_promedio)



# altitud
alt <- getData(name = "SRTM",
               lon = crd[1,1],
               lat = crd[1,2])

alt_lor <- crop(alt, lor) %>%
  mask(., lor)
mapview(alt_lor)
