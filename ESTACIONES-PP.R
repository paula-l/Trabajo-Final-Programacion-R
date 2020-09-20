library(tidyverse)
library(dplyr)
library(readxl)
library(ncdf4)
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
##Ploteamos la precipitación de la estación
B_p = ggplot(Bagazan_pp, aes(x=Column1, y = Column4)) + 
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
##Ploteamos la precipitación de la estación
J_p = ggplot(Juancito_pp, aes(x=Column1, y = Column4)) + 
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
##Ploteamos la precipitación de la estación
S_p = ggplot(San_Ramon_pp, aes(x = Column1, y = Column4)) + 
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
##Ploteamos la precipitación de la estación
T_p = ggplot(Timicurillo_pp, aes(x=Column1, y = Column4)) + 
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

