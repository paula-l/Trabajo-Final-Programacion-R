library(remotes)
remotes::install_github("r-spatial/rgee")

library(sf)
library(mapview)
library(mapedit)
library(rgee)
library(dplyr)


ee_install()
ee_Initialize("luarobin")

loreto1<- mapview() %>% editMap()
loreto1_sf<-loreto1$all
mapview(loreto1_sf)


lrt_ee<-sf_as_ee(loreto1_sf)

#Extraer datos de pp con rgee

imagen_lrt<-ee$ImageCollection("TRMM/3B43V7")$ 
  filterDate("2016-01-01","2017-01-01")$first()
dem_stack<-ee_as_raster(image=imagen_lrt,
                        region=lrt_ee$geometry())

dem_lrt<-dem_stack[[1]]
plot(dem_lrt)

mapview(list(dem_lrt,loreto1_sf))

#Crear puntos con mapedit

puntos<-mapview(dem_lrt) %>% editMap()
puntos_sf<- puntos$all

#Extraer valor de precipitacion  para los puntos

puntos_sf$pp=raster::extract(dem_lrt,puntos_sf)
puntos_sf

#Extraer latitud y longitud de campo de objeto sf

puntos_with_geometry<- puntos_sf %>%
  mutate(lon=unlist(map(puntos_sf$geometry,1)),
         lat=unlist(map(puntos_sf$geometry,2))
  )

head(puntos_with_geometry)

comparar<- puntos_with_geometry %>%
  as_tibble() %>%
  select(z,temp)

pairs(comparar)
cor(comparar)

#Extraer datos de temperatura con rgee

loreto2<- mapview() %>% editMap()
loreto2_sf<-loreto2$all
mapview(loreto2_sf)

lrt2_ee<-sf_as_ee(loreto2_sf)


imagen_temp<-ee$ImageCollection("NOAA/GFS0P25")$ 
  filterDate("2016-01-01","2017-01-01")$first()
tem_stack<-ee_as_raster(image=imagen_temp,
                        region=lrt2_ee$geometry())

dem_temp<-tem_stack[[1]]
plot(dem_temp)


#Crear puntos con mapedit

puntos2<-mapview(dem_lrt) %>% editMap()
puntos_sf2<- puntos2$all

#Extraer valor de temperatura para los puntos

puntos_sf2$temp=raster::extract(dem_temp,puntos_sf2)
puntos_sf2

#Extraer latitud y longitud de campo de objeto sf

puntos_with_geometry2<- puntos_sf2 %>%
  dplyr::mutate(lon=unlist(map(puntos_sf2$geometry,1)),
         lat=unlist(map(puntos_sf2$geometry,2))
  )

comparar<- puntos_with_geometry2 %>%
  as_tibble() %>%
  select(z,temp)

pairs(comparar)
cor(comparar)


regresion<- lm(pp-z,data=comparar)

plot(comparar$z,comparar$temp , xlab=precipitacion, ylab="temperarura")
