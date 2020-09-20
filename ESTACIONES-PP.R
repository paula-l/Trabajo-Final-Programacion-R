#-----LIBRERIAS
library(tidyverse)
library(dplyr)
library(readxl)
library(Hmisc)


#----DATOS HISTÓRICOS DE LAS ESTACIONES
##BAGAZAN - 01/01/1964-30/04/2014
##JUANCITO - 01/09/1964-31/01/2014
##SAN RAMON - 01/05/1969-31/07/2014
##TIMICURILLO - 01/01/1992-30/04/2014

#----SE TOMAN LAS FECHAS COMUNES
#01/01/1992 - 2014


#----ESTACION BAGAZAN
##Leemos el Excel
Bagazan = read_excel("Loreto_Bagazan.xlsx")
View(Bagazan)
##Filtramos los años en común con las demás estaciones
Bagazan_pp = Bagazan %>% 
  filter(Column1 >= 1992 & Column4)
View(Bagazan_pp)

##La precipitación máxima es 151.8
max(Bagazan_pp$Column4)
##La precipitación mínima es 1
  min(Bagazan_pp$Column4)

##Ploteamos la precipitación de la estación
ggplot(Bagazan_pp, aes(x=Column1, y = Column4)) + 
  geom_point(aes(color = Column4)) +
  geom_smooth(color= "Red") +
  theme_classic() +
  ggtitle("GRÁFICA DE PRECIPITACIÓN DE LA ESTACIÓN BAGAZÁN
          Dist: Nauta, Prov: Loreto, Dpto: Loreto
          para el periodo del 01/01/1992 al 30/04/2014") +
  theme(plot.title = element_text(size=rel(1.3),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#81132D",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)",
       caption = "Max: 151.8 en 2007
                  Min: 1",
       color = "  Rango de
    valores 
    para la 
Precipitación(mm)") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="orange",
                                    size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="orange",
                                    size=rel(1.5)))
               
#----ESTACIO JUANCITO
##Leemos el Excel
Juancito = read_excel("Loreto_Juancito.xlsx")
View(Juancito)
##Filtramos los años en común con las demás estaciones y eliminamos los valores
##de los que no hubo registros
Juancito_pp = Juancito %>% 
  filter(Column1 >= 1992 & Column4 %nin% "-99.9")
View(Juancito_pp)

##La precipitación máxima es 204
max(Juancito_pp$Column4)
##La precipitación mínima es 0
min(Juancito_pp$Column4)

##Ploteamos la precipitación de la estación
ggplot(Juancito_pp, aes(x=Column1, y = Column4)) + 
  geom_point(aes(color = Column4)) +
  geom_smooth(color= "Red") +
  theme_classic() +
  ggtitle("GRÁFICA DE PRECIPITACIÓN DE LA ESTACIÓN JUANCITO
          Dist: Sarayacu, Prov: Ucayali, Dpto: Loreto
          para el periodo del 01/01/1992 al 30/04/2014") +
  theme(plot.title = element_text(size=rel(1.3),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#81132D",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)",
       caption = "Max: 204 en 2002
                  Min: 0",
       color = "  Rango de
    valores 
    para la 
Precipitación(mm)") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="orange",
                                    size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="orange",
                                    size=rel(1.5)))

#----ESTACIÓN SAN RAMON
##Leemos el Excel
San_Ramon = read_excel("Loreto_SanRamon.xlsx")
View(San_Ramon)
##Filtramos los años en común con las demás estaciones y eliminamos los valores
##de los que no hubo registros
San_Ramon_pp = San_Ramon %>% 
  filter(Column1 >= 1992 & Column2 <= 04 & Column4 %nin% "-99.9")
View(San_Ramon_pp)

##La precipitación máxima es 163.3
max(San_Ramon_pp$Column4)
##La precipitación mínima es 0
min(San_Ramon_pp$Column4)

##Ploteamos la precipitación de la estación
ggplot(San_Ramon_pp, aes(x = Column1, y = Column4)) + 
  geom_point(aes(color = Column4)) + 
  geom_smooth(color= "Red") +
  theme_classic() +
  ggtitle("GRÁFICA DE PRECIPITACIÓN DE LA ESTACIÓN SAN RAMÓN
          Dist: Yurimaguas, Prov: Alto Amazonas, Dpto: Loreto
          para el periodo del 01/01/1992 al 31/03/2014") +
  theme(plot.title = element_text(size=rel(1.3),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#81132D",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)",
       caption = "Max: 163.3 en 2010
                  Min: 0",
       color = "  Rango de
    valores 
    para la 
Precipitación(mm)") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="orange",
                                    size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="orange",
                                    size=rel(1.5)))


#----ESTACIÓN TIMICURILLO
##Leemos el Excel
Timicurillo = read_excel("Loreto_Timicurillo.xlsx")
View(Timicurillo)
##Filtramos los años en común con las demás estaciones y eliminamos los valores
##de los que no hubo registros
Timicurillo_pp = Timicurillo %>% 
  filter(Column4 %nin% "-100")
View(Timicurillo_pp)

##La precipitación máxima es 215
max(Timicurillo_pp$Column4)
##La precipitación mínima es 0
min(Timicurillo_pp$Column4)

##Ploteamos la precipitación de la estación
ggplot(Timicurillo_pp, aes(x=Column1, y = Column4)) + 
  geom_point(aes(color = Column4),
             alpha = 0.8,
             size = 2) +
  geom_smooth(color= "Red") +
  theme_classic() +
  ggtitle("GRÁFICA DE PRECIPITACIÓN DE LA ESTACIÓN TIMICURILLO
          Dist: Indiana, Prov: Maynas, Dpto: Loreto
          para el periodo del 01/01/1992 al 30/04/2014") +
  theme(plot.title = element_text(size=rel(1.3),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#81132D",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)",
       caption = "Max: 215 en 2006
                  Min: 0",
       color = "  Rango de
    valores 
    para la 
Precipitación(mm)") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="orange",
                                    size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="orange",
                                    size=rel(1.5)))

#----HALLANDO LA MEDIA POR AÑO DE CADA ESTACIÓN----#
##ESTACIÓN BAGAZAN
Bagazan_media = Bagazan_pp %>% 
  group_by(Column1) %>% 
  summarise(Media = mean(Column4))
View(Bagazan_media)

max(Bagazan_media$Media)
### La media máxima es 29.23 en 1992 
min(Bagazan_media$Media)
### La media mínima es 6.11 en 2003
ggplot(Bagazan_media, aes(x = Column1, y = Media)) + 
  geom_smooth(color = "yellow") +
  geom_line(colour="red") +
  geom_point(size = 3,
             colour = "red",
             alpha = 0.5) + 
  ggtitle("GRÁFICA DE LA MEDIA DE PRECIPITACIONES POR AÑO
          DE LA ESTACIÓN BAGAZAN
          Años: 1992-2014") +
  theme_light() +
  theme(plot.title = element_text(size=rel(1.25),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#633100",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)",
       caption = "Max: 29.23 en 1992
                  Min: 6.11 en 2003") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2)))
##ESTACIÓN JUANCITO
Juancito_media = Juancito_pp %>% 
  group_by(Column1) %>% 
  summarise(Media = mean(Column4))
View(Juancito_media)

max(Juancito_media$Media)
### La media máxima es 9.24 en 2002
min(Juancito_media$Media)
### La media mínima es 1.90 en 1995

ggplot(Juancito_media, aes(x = Column1, y = Media)) + 
  geom_smooth(color = "#F75C9D") +
  geom_line(colour="purple") +
  geom_point(size = 3,
             colour = "purple",
             alpha = 0.5) + 
  ggtitle("GRÁFICA DE LA MEDIA DE PRECIPITACIONES POR AÑO
          DE LA ESTACIÓN JUANCITO
          Años: 1992-2014") +
  theme_light() +
  theme(plot.title = element_text(size=rel(1.25),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#633100",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)",
       caption = "Max: 9.24 en 2002
                  Min: 1.90 en 1995") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2)))
##ESTACIÓN SAN RAMÓN
SanRamon_media = San_Ramon_pp %>% 
  group_by(Column1) %>% 
  summarise(Media = mean(Column4))
View(SanRamon_media)

max(SanRamon_media$Media)
### La media máxima es  11.89 en 2007
min(SanRamon_media$Media)
### La media mínima es 5.21 en 2004

ggplot(SanRamon_media, aes(x = Column1, y = Media)) + 
  geom_smooth(color = "orange") +
  geom_line(colour="green") +
  geom_point(size = 3,
             colour = "green",
             alpha = 0.5) + 
  ggtitle("GRÁFICA DE LA MEDIA DE PRECIPITACIONES POR AÑO
          DE LA ESTACIÓN SAN RAMÓN
          Años: 1992-2014") +
  theme_light() +
  theme(plot.title = element_text(size=rel(1.25),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#633100",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)",
       caption = "Max: 11.89 en 2007
                  Min: 5.21 en 2004") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2)))

##ESTACIÓN TIMICURILLO
Timicurillo_media = Timicurillo_pp %>% 
  group_by(Column1) %>% 
  summarise(Media = mean(Column4))
View(Timicurillo_media)

max(Timicurillo_media$Media)
### La media máxima es 10.57 en 2014
min(Timicurillo_media$Media)
### La media mínima es 5.38 en 2010

ggplot(Timicurillo_media, aes(x = Column1, y = Media)) + 
  geom_smooth(color = "#87CEFA") +
  geom_line(colour="blue") +
  geom_point(size = 3,
             colour = "blue",
             alpha = 0.5) + 
  ggtitle("GRÁFICA DE LA MEDIA DE PRECIPITACIONES POR AÑO
          DE LA ESTACIÓN TIMICURILLO
          Años: 1992-2014") +
  theme_light() +
  theme(plot.title = element_text(size=rel(1.25),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#633100",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)",
       caption = "Max: 10.57 en 2014
                  Min: 5.38 en 2010") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2)))

#----HALLANDO EL MAXIMO POR AÑO DE CADA ESTACIÓN
##ESTACIÓN BAGAZAN
Bagazan_max = Bagazan_pp %>% 
  group_by(Column1) %>% 
  summarise(Máximo = max(Column4))
View(Bagazan_max)

max(Bagazan_max$Máximo)
### La máximo es 151.8 en 2007
min(Bagazan_max$Máximo)
### El mínimo es 32 en 1992

ggplot(Bagazan_max, aes(x = Column1, y = Máximo)) + 
  geom_smooth(color = "yellow") +
  geom_line(colour="red") +
  geom_point(size = 3,
             colour = "red",
             alpha = 0.5) + 
  ggtitle("GRÁFICA DEL MÁXIMO DE PRECIPITACIONES POR AÑO
          DE LA ESTACIÓN BAGAZAN
          Años: 1992-2014") +
  theme_light() +
  theme(plot.title = element_text(size=rel(1.25),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#633100",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)",
       caption = "Max: 151.8 en 2007
                  Min: 32 en 1992") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2)))

##ESTACIÓN JUANCITO
Juancito_max = Juancito_pp %>% 
  group_by(Column1) %>% 
  summarise(Máximo = max(Column4))
View(Juancito_max)

max(Juancito_max$Máximo)
### La máximo es  204 en 2002
min(Juancito_max$Máximo)
### El mínimo es 18.7 en 2014

ggplot(Juancito_max, aes(x = Column1, y = Máximo)) + 
  geom_smooth(color = "#F75C9D") +
  geom_line(colour="purple") +
  geom_point(size = 3,
             colour = "purple",
             alpha = 0.5) + 
  ggtitle("GRÁFICA DEL MÁXIMO DE PRECIPITACIONES POR AÑO
          DE LA ESTACIÓN JUANCITO
          Años: 1992-2014") +
  theme_light() +
  theme(plot.title = element_text(size=rel(1.25),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#633100",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)",
       caption = "Max: 204 en 2002
                  Min: 18.7 en 2014") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2)))
##ESTACIÓN SAN RAMÓN
SanRamon_max = San_Ramon_pp %>% 
  group_by(Column1) %>% 
  summarise(Máximo = max(Column4))
View(SanRamon_max)

max(SanRamon_max$Máximo)
### La máximo es  163.3 en 2010
min(SanRamon_max$Máximo)
### El mínimo es 59.8 en 1992

ggplot(SanRamon_max, aes(x = Column1, y = Máximo)) + 
  geom_smooth(color = "orange") +
  geom_line(colour="green") +
  geom_point(size = 3,
             colour = "green",
             alpha = 0.5) + 
  ggtitle("GRÁFICA DEL MÁXIMO DE PRECIPITACIONES POR AÑO
          DE LA ESTACIÓN SAN RAMÓN
          Años: 1992-2014") +
  theme_light() +
  theme(plot.title = element_text(size=rel(1.25),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#633100",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)",
       caption = "Max: 163.3 en 2010
                  Min: 59.8 en 1992") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2)))

##ESTACIÓN TIMICURILLO
Timicurillo_max = Timicurillo_pp %>% 
  group_by(Column1) %>% 
  summarise(Máximo = max(Column4))
View(Timicurillo_max)

max(Timicurillo_max$Máximo)
### La máximo es  215 en 2000
min(Timicurillo_max$Máximo)
### El mínimo es 80 en 2006

ggplot(Timicurillo_max, aes(x = Column1, y = Máximo)) + 
  geom_smooth(color = "#87CEFA") +
  geom_line(colour="blue") +
  geom_point(size = 3,
             colour = "blue",
             alpha = 0.5) + 
  ggtitle("GRÁFICA DEL MÁXIMO DE PRECIPITACIONES POR AÑO
          DE LA ESTACIÓN TIMICURILLO
          Años: 1992-2014") +
  theme_light() +
  theme(plot.title = element_text(size=rel(1.25),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#633100",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)",
       caption = "Max: 215 en 200
                  Min: 80 en 2006") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2)))

#----HALLANDO EL MINIMO POR AÑO DE CADA ESTACIÓN
##ESTACIÓN BAGAZAN
Bagazan_min = Bagazan_pp %>% 
  group_by(Column1) %>% 
  summarise(Mínimo = min(Column4))
View(Bagazan_min)

max(Bagazan_min$Mínimo)
### La máximo es 5.2 en 2014
min(Bagazan_min$Mínimo)
### El mínimo es 1

ggplot(Bagazan_min, aes(x = Column1, y = Mínimo)) + 
  geom_smooth(color = "yellow") +
  geom_line(colour="red") +
  geom_point(size = 3,
             colour = "red",
             alpha = 0.5) + 
  ggtitle("GRÁFICA DEL MÍNIMO DE PRECIPITACIONES POR AÑO
          DE LA ESTACIÓN BAGAZAN
          Años: 1992-2014") +
  theme_light() +
  theme(plot.title = element_text(size=rel(1.25),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#633100",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)",
       caption = "Max: 5.2 en 2014
                  Min: 1") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2)))

##ESTACIÓN JUANCITO
Juancito_min = Juancito_pp %>% 
  group_by(Column1) %>% 
  summarise(Mínimo = min(Column4))
View(Juancito_min)

ggplot(Juancito_min, aes(x = Column1, y = Mínimo)) + 
  geom_smooth(color = "#F75C9D") +
  geom_line(colour="purple") +
  geom_point(size = 3,
             colour = "purple",
             alpha = 0.5) + 
  ggtitle("GRÁFICA DEL MÍNIMO DE PRECIPITACIONES POR AÑO
          DE LA ESTACIÓN JUANCITO
          Años: 1992-2014") +
  theme_light() +
  theme(plot.title = element_text(size=rel(1.25),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#633100",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2)))

##ESTACIÓN SAN RAMÓN
SanRamon_min = San_Ramon_pp %>% 
  group_by(Column1) %>% 
  summarise(Mínimo = min(Column4))
View(SanRamon_min)

ggplot(SanRamon_min, aes(x = Column1, y = Mínimo)) + 
  geom_smooth(color = "orange") +
  geom_line(colour="green") +
  geom_point(size = 3,
             colour = "green",
             alpha = 0.5) + 
  ggtitle("GRÁFICA DEL MÍNIMO DE PRECIPITACIONES POR AÑO
          DE LA ESTACIÓN SAN RAMÓN
          Años: 1992-2014") +
  theme_light() +
  theme(plot.title = element_text(size=rel(1.25),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#633100",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2)))

##ESTACIÓN TIMICURILLO
Timicurillo_min = Timicurillo_pp %>% 
  group_by(Column1) %>% 
  summarise(Mínimo = min(Column4))
View(Timicurillo_min)

ggplot(Timicurillo_min, aes(x = Column1, y = Mínimo)) + 
  geom_smooth(color = "#87CEFA") +
  geom_line(colour="blue") +
  geom_point(size = 3,
             colour = "blue",
             alpha = 0.5) + 
  ggtitle("GRÁFICA DEL MÍNIMO DE PRECIPITACIONES POR AÑO
          DE LA ESTACIÓN TIMICURILLO
          Años: 1992-2014") +
  theme_light() +
  theme(plot.title = element_text(size=rel(1.25),
                                  face="bold",
                                  vjust=0.5,
                                  hjust=0.5,
                                  color="#633100",
                                  lineheigh=1.0)) + 
  labs(x="Años", y="Precipitación(mm)") +
  theme(axis.title.x = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2))) +
  theme(axis.title.y = element_text(face="bold",
                                    vjust=0.5,
                                    color="#00007C",
                                    size=rel(1.2)))
