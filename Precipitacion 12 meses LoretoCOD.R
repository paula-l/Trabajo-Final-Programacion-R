library(mapview)
library(sp)
library(dplyr)
library(ncdf4)
library(raster)
library(rgdal)
library(rgeos)
library(gtools)
library(tidyverse)
library(RColorBrewer)
# data
mps <- shapefile('Departamentos del Peru/DEPARTAMENTOS.shp')
dpt <- aggregate(mps, 'DEPARTAMEN')
lbl <- data.frame(month_abb = month.abb, mes = 1:12)

#loreto
lor <- mps[mps@data$ DEPARTAMEN %in% 'LORETO',]

#mascara
prec <- raster::getData('worldclim', 
                        var = 'prec', 
                        res = 0.5, 
                        lon = coordinates(lor)[1], 
                        lat = coordinates(lor)[2])
prec <- raster::crop(prec, lor) %>% 
  raster::mask(., lor)
plot(prec)
# mapa
vls <- rasterToPoints(prec) %>% 
  as_tibble() %>% 
  gather(var, value, -x, -y) %>% 
  mutate(mes = parse_number(var)) %>% 
  inner_join(., lbl, by = 'mes') %>% 
  dplyr::select(x, y, month_abb, value) %>% 
  mutate(month_abb = factor(month_abb, levels = month.abb))
#plot

gg <- ggplot(vls)  +
  geom_tile(aes(x = x, y =  y, fill = value)) +
  facet_wrap(~ month_abb) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = "GnBu"), 
                       na.value = 'white', limits = c(0, 500), breaks = seq(0, 500, 100)) +
  geom_polygon(data = mps, aes(x=long, y = lat, group = group), color = 'black', fill='NA') +
  theme_bw() +
  scale_x_continuous(breaks = c(-78, -77, -76, -75)) +
  coord_equal(xlim = extent(lor)[1:2], ylim = extent(lor)[3:4]) +
  labs(title = 'PRECIPITACI?N MENSUAL - DEPARTAMENTO DE LORETO', fill = 'mm',  x = 'Longitud', y = 'Latitud') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.width = unit(5, 'line')) +
  guides(shape = guide_legend(override.aes = list(size = 10)))

# guardando el plot 
ggsave(plot = gg, filename = 'PRECIPITACIONLORETO.png', width = 11, height = 9, units = 'in', dpi = 300)
