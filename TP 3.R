# Trabajo Práctico Nº3 -  Representación gráfica de datos


# Primero vamos a cargar las librerias necesarias para realizar el trabajo. En primer lugar vamos a cargars tidyverse y luego para este caso puntual sera necesario que instalaemos y carguemos rvest.
# En este TP puntualmente necesitaremos SF 

#install.packages("tidyverse")
library(tidyverse) # Easily Install and Load the 'Tidyverse' 
#install.packages(sf)
library(sf) # Simple Features for R 
#install.packages(datos)
library(datos) # Traduce al Español Varios Conjuntos de Datos de Práctica 
#install.packages(janitor)
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data 
#install.packages(zip)
library(zip) # Cross-Platform 'zip' Compression 
#install.packages(skimr)
library(skimr) # Compact and Flexible Summaries of Data 

#Tambien configurames de antemano para que no hayan notaciones cientificas
options(scipen = 999) 


# Para este trabajo vamos a utilizar los datos obtenidos el en TP Nº2  relacionados a los casos de Covid19 para la Ciudad de Buenos Aires.
# En el TP Nº2 puntualmente se diferenciaron los casos positivos en villas y asentamientos con respecto al resto de la ciudad. 

# Primero vamos a cargar la base de datos obtenida 
datos <- read.csv("PCT2_CiudadFvsCInformal", encoding = "ASCII")

# Vamos a estudiar la estructura de nuestos datos.
skim(datos)


# Si bien en el TP Nº2 habiamos realizado una corrección y limpieza de datos para dejar el csv lo mas limpia posible, ahora haremos unas breves modificaciones
# Eliminaremos una columna que no brinda información relevante para el TP y renombraremos de manera mas práctica 3 columnas
datos_00 <-datos %>%
  select( -X) %>% 
  rename("CasosTotal"=Casosconfirmados, "PCTFormal" = PCT_CiudadFormal, "PCTInformal" = PCT_CiudadInformal)


# En el primer gráfico queremos ver como es la relación entre la población del barrio y los casos de Covid
ggplot(datos_00)+
  geom_label(aes(x = Población, y = CasosTotal, label = factor(Barrio)),size=3, color="mediumorchid2")+
  geom_point(aes(x = Población, y = CasosTotal, size = CasosCInformal), alpha= 0.25)+ #Agregamos el punto para facilitar la lectura, y aprovechamos para referenciar con su tamaño el numero de casos en las villas y asentamientos de cada barrio
  labs(title ="Casos Covid19 x Barrio", subtitle="CABA", x="NºPoblacion", y="Nº de Casos",  caption= "Fuente: Wikipedia")+
  theme_classic()


# Ahora queremos ver puntualmente como es la relación entre la cantidad de casos positivos en la ciudad informal y los positivos de la ciudad formal 
ggplot(data=datos_00, aes(x = CasosCInformal, y = CasosCFormal))+
  geom_label(aes(label = factor(Barrio)),size=3, color="seashell4")+
  geom_point(aes(color=Barrio, size=PCTInformal), alpha=0.5)+
  labs(title ="Casos Covid19 - Relación Ciudad Formal/Ciudad Informal Barrio", subtitle="CABA", x="Casos Ciudad Informal", y="Casos Ciudad Formal",  caption= "Fuente: Wikipedia")+
  theme_classic()

# Para verlo mas claro vamos a realizar un facetado por cada barrio
ggplot(datos_00)+
  geom_point(aes(x=CasosCInformal, y=CasosCFormal, color=PCTInformal), size=5)+
  scale_color_fermenter()+
  facet_wrap(~Barrio, order(decreasing = FALSE), nrow = 5)+
  labs(title ="Casos Covid19 - Relación Ciudad Formal/Ciudad Informal Barrio", subtitle="CABA", x="Casos Ciudad Informal", y="Casos Ciudad Formal",  caption= "Fuente: Wikipedia")

# Ahora vamos a ver especificamente los datos para las villas y asentamientos
# Primero vamos a ordenarlos de manera decreciente segun el numero de casos en villas y asentamientos para cada barrio y eliminaremos algunas columnas 
datos_01 <- datos_00 %>% 
  arrange(desc(CasosCInformal)) %>% 
  select( -CasosTotal, -PCTFormal, -PCTInformal)

# Visualizamos los datos: 
ggplot(datos_01)+
  geom_bar(aes(x=reorder(Barrio, -CasosCInformal), weight = CasosCInformal, fill=CasosCInformal))+
  scale_fill_viridis_c()+ 
  labs(title ="Casos Covid 19 - Ciudad Informal", subtitle="CABA", x="Barrios", y="Nº de Casos", fill= "Nº de Casos", caption= "Fuente: Wikipedia")+
  theme_light()+
  theme_classic()

# Ahora ordenamos los datos segun casos positivos por barrio para la ciudad formal.
datos_011 <- datos_00 %>% 
  arrange(desc(CasosCFormal)) %>% 
  select( -Población, -CasosTotal, -CasosCInformal, -PCTFormal, -PCTInformal)

# Con estos datos, se los sumamos al grafico anterior para ver como se solapan los casos por barrio entre la ciudad formal y la informal
ggplot()+
  geom_bar(data=datos_01, 
           aes(x=reorder(Barrio, -CasosCInformal),
               weight = CasosCInformal, fill=CasosCInformal))+
  geom_point(data=datos_011,
             aes(x=Barrio, y= CasosCFormal), colour="red")+
  scale_fill_viridis_c()+ 
  labs(title ="Casos Covid 19 - Ciudad Informal y Ciudad Formal", subtitle="CABA", x="Barrios", y="Nº de Casos", fill= "Nº de Casos Ciudad Informal", caption= "Fuente: Wikipedia")+
  theme_light()+
  theme_classic()


# Ahora queremos hacer foco en como se distribuyen los casos positivos en las villas y asentamientos con respecto al resto de la ciudad
# Para esto vamos a ver los porcentajes de positivos en la ciudad informal y lo ordenaremos de manera decreciente 
datos_02 <- datos_00 %>% 
  arrange(desc(PCTInformal)) %>% 
  select(-CasosTotal, -CasosCFormal, -CasosCInformal) #descartamos algunas columnas que en este caso no nos interesan

# Visualizamos los datos
ggplot(datos_02)+
  geom_bar(aes(x=reorder(Barrio, -PCTInformal), weight = PCTInformal, fill= Barrio))+
  labs(title ="% Casos Covid 19 en la Ciudad Informal", subtitle="CABA", x="Barrio", y="% de positivos en villas",  caption= "Fuente: Wikipedia")+
  coord_flip() #Giramos los ejes para poder visualizar y leer mejor los barrios.

