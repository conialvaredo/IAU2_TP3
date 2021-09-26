# Trabajo Práctico Nº3 -  Representación gráfica de datos


# Primero vamos a cargar las librerias necesarias para realizar el trabajo. En primer lugar vamos a cargars tidyverse y luego para este caso puntual sera necesario que instalaemos y carguemos rvest.
# En este TP puntualmente necesitaremos SF 


library(tidyverse) # Easily Install and Load the 'Tidyverse' 
library(sf) # Simple Features for R 
library(datos) # Traduce al Español Varios Conjuntos de Datos de Práctica 
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data 
library(zip) # Cross-Platform 'zip' Compression 
library(skimr) # Compact and Flexible Summaries of Data 
library(leaflet)
library(lwgeom)
library(ggmap)
library(osmdata)

#Tambien configurames de antemano para que no hayan notaciones cientificas
options(scipen = 999) 


# Para este trabajo vamos a utilizar los datos obtenidos el en TP Nº2  relacionados a los casos de Covid19 para la Ciudad de Buenos Aires.
# En el TP Nº2 puntualmente se diferenciaron los casos positivos en villas y asentamientos con respecto al resto de la ciudad. 

# Primero vamos a cargar la base de datos obtenida 
datos <- read.csv("PCT2_CiudadFvsCInformal", encoding = "ASCII")

# Vamos a estudiar la estructura de nuestos datos.
skim(datos)


# Como vamos a querer volcar la informacion en mapas de la ciudad, dejamos cargada la base de datos del Gobierno de la ciudad con los barrios de CABA
barrios <- read_sf("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson")

# Vemos su estructura
skim(barrios)

barrios

# Antes de empezar a trabjar con los datos, ordenamos ambas bases para poder unirlas

# Primero acomodamos algunas cosas para poder hacer la union
# Como las vamos a unir por los barrios necesitamos que esten escritos de igual manera.

# Las modificaciones las haremos sobre nuestra base Datos, ya que no tiene todos los barrios y es esta con la que nos interesa trabajar
# Hacemos que estos figuren en mayuscula
datos_ok <- datos %>% 
  mutate( Barrio = toupper(Barrio))

#Tambien detectamos que hay dos barrios que en nuestro dataset figuran con un articulo que no tienen en el dataset del GCBA
datos_ok2 <- datos_ok %>% 
  mutate(Barrio = str_replace_all(Barrio, "LA PATERNAL", "PATERNAL"),
         Barrio = str_replace_all(Barrio, "LA BOCA", "BOCA"))


# Ahora ya podemos unir ambas bases
datos_ <- left_join (datos_ok2, barrios, by=c("Barrio"= "BARRIO"))




# Comenzaremos a graficar

# Si bien en el TP Nº2 habiamos realizado una corrección y limpieza de datos para dejar el csv lo mas limpia posible, ahora haremos unas breves modificaciones
# Eliminaremos una columna que no brinda información relevante para el TP y renombraremos de manera mas práctica 3 columnas
datos_00 <-datos_ %>%
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


# Ahora queremos ver la informacion volcada en mapas
# Para esto trabajaremos con informacion geograficca obtenida en la base de datos de los barrios 

ggplot(datos_)+
  geom_sf(aes(fill=Casosconfirmados), size=0.2)

# Armamos la caja de coordenadas para descargar un mapa


bbox_CABA <- getbb("CABA, Buenos Aires")

#Ahora descarguemos el mapa de fondo a partir de get_stamenmap():
mapa <- get_stamenmap(bbox = bbox_CABA,
                            maptype = "toner-lite",
                            zoom=12)

# Lo visualizamos
ggmap(mapa)+
  labs(title="CABA",
       caption="Fuente: get_stamenmap ")


ggmap(mapa)+
  geom_point(data=datos_, aes(color= Casosconfirmados), alpha=0.5)+
  labs(title = "Estaciones para carga de autos electricos X Estado",
       subtitle = "Mes de julio",
       color = "Estado",
       caption = "Imagen: 5
         Fuente: Open Data Paris")

