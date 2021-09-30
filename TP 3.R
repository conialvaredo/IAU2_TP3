# Trabajo Práctico Nº3 -  Representación gráfica de datos


# Primero vamos a cargar las librerias necesarias para realizar el trabajo.
# En este TP puntualmente necesitaremos SF para realizar los gráficos


library(tidyverse) # Easily Install and Load the 'Tidyverse' 
library(sf) # Simple Features for R 
library(datos) # Traduce al Español Varios Conjuntos de Datos de Práctica 
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data 
library(skimr) # Compact and Flexible Summaries of Data 

#Tambien configurames de antemano para que no hayan notaciones científicas
options(scipen = 999) 


# Para este trabajo vamos a utilizar los datos obtenidos el en TP Nº2  relacionados a los casos de Covid19 para la Ciudad de Buenos Aires.
# En el TP Nº2 puntualmente diferenciamos los casos positivos en villas y asentamientos con respecto al resto de la ciudad. 

# Primero vamos a cargar la base de datos obtenida 
datos <- read.csv("PCT2_CiudadFvsCInformal", encoding = "ASCII")

# Vamos a estudiar la estructura de nuestos datos.
skim(datos)

# Como vamos a querer volcar la información en mapas de la ciudad, dejamos cargada la base de datos de los barrios de CABA
barrios <- read_sf("barrios_caba/barrios_caba.shp")

# Vamos a crear la variable con la información sobre el CRS proyectado para la Ciudad Autonoma de Buenos Aires, para poder configurar de la misma manera a todos los datos espaciales.
caba_proj = "+proj=tmerc +lat_0=-34.6297166 +lon_0=-58.4627 +k=1 +x_0=100000 +y_0=100000 +ellps=intl +units=m +no_defs"

# Transformamos el dataset de barrios al sistema de coordenadas creado para la CABA
barrios <- st_transform(barrios, crs=caba_proj)

# Vemos su estructura
skim(barrios)

# Antes de empezar a trabjar con los datos, ordenamos ambas bases para poder unirlas y ya dejarlas listas para el momento que necesitemos realziar la representación en mapas
# Primero acomodamos algunas cosas para poder hacer la unión
# Como las vamos a unir por los barrios necesitamos que esten escritos de igual manera.

# Las modificaciones las haremos sobre nuestra base Datos, ya que no tiene todos los barrios y es esta con la que nos interesa trabajar
# Hacemos que estos figuren en mayúscula
datos_ok <- datos %>% 
  mutate( Barrio = toupper(Barrio))

#Tambien detectamos que hay dos barrios que en nuestro dataset figuran con un artículo que no tienen en el dataset del GCBA. Lo modificamos
datos_ok2 <- datos_ok %>% 
  mutate(Barrio = str_replace_all(Barrio, "LA PATERNAL", "PATERNAL"),
         Barrio = str_replace_all(Barrio, "LA BOCA", "BOCA"))

# Ya tenemos las bases acomodadas para poder unirlas. 
# Antes vamos a graficar los datos y hacer un primer análisis sin georeferenciar la información

# Comenzamos a graficar

# Si bien en el TP Nº2 habiamos realizado una corrección y limpieza de datos para dejar el csv lo mas limpia posible, ahora haremos unas breves modificaciones
# Eliminaremos una columna que no brinda información relevante para el TP y renombraremos de manera mas práctica 3 columnas
datos_00 <-datos_ok2 %>%
  select( -X) %>% 
  rename("CasosTotal"=Casosconfirmados, "PCTFormal" = PCT_CiudadFormal, "PCTInformal" = PCT_CiudadInformal)


# En el primer gráfico queremos ver como es la relación entre la población del barrio y los casos de Covid
ggplot(datos_00)+
  geom_label(aes(x = Población, y = CasosTotal, label = factor(Barrio)),size=3, color="mediumorchid2")+ #Volcamos en el gráfico el nombre de los barrios 
  geom_point(aes(x = Población, y = CasosTotal, size = CasosCInformal), color= "orchid", alpha= 0.25)+ #Agregamos el punto para facilitar la lectura, y aprovechamos para referenciar con su tamaño el numero de casos en las villas y asentamientos de cada barrio
  labs(title ="Casos Covid19 x Barrio", 
       subtitle="CABA", 
       x="NºPoblacion",
       y="Nº de Casos",  
       caption= "Grafico Nº1
        Fuente: Wikipedia")+
  theme_light()

# Ahora queremos ver puntualmente como es la relación entre la cantidad de casos positivos en la ciudad informal y los de la ciudad formal 
ggplot(data=datos_00, aes(x = CasosCInformal, y = CasosCFormal))+
  geom_point(color="orchid4")+
  geom_label(aes(label = factor(Barrio)),size=3, color="seashell4", alpha=0.5)+
  labs(title ="Casos Covid19 - Relación Ciudad Formal/Ciudad Informal Barrio", 
       subtitle="CABA",
       x="Casos Ciudad Informal", 
       y="Casos Ciudad Formal",  
       caption= "Gráfico Nº2
        Fuente: Wikipedia")+
  theme_classic() #Probamos otro estilo de fondo

# Este gráfico es muy interesante ya que podemos detectar dos barrios puntuales  (Retiro y Flores) en donde contraste mucho la diferencia entre los casos
# Tambien podemos ver casos como Flores o Villa Lugano que tuvieron una alta cantidad de casos en ambas realidades

# Para verlo mas claro vamos a realizar un facetado por cada barrio OJO
ggplot(datos_00)+
  geom_point(aes(x=CasosCInformal, y=CasosCFormal), color="red3", size=2)+
    facet_wrap(~Barrio, order(decreasing = FALSE), nrow = 5)+
  labs(title ="Casos Covid19 - Relación Ciudad Formal/Ciudad Informal Barrio", 
       subtitle="CABA",
       x="Casos Ciudad Informal",
       y="Casos Ciudad Formal", 
       caption= "Gráfico Nº3
          Fuente: Wikipedia")

# Ahora vamos a ver especificamente los datos para las villas y asentamientos
# Primero vamos a ordenarlos de manera decreciente según el número de casos en villas y asentamientos para cada barrio y eliminaremos algunas columnas 
datos_01 <- datos_00 %>% 
  arrange(desc(CasosCInformal)) %>% 
  select( -CasosTotal, -PCTFormal, -PCTInformal)

# Visualizamos los datos: 
ggplot(datos_01)+
  geom_bar(aes(x=reorder(Barrio, -CasosCInformal), weight = CasosCInformal, fill=CasosCInformal))+
  scale_fill_viridis_c()+ # Configuramos la escala de colores
  labs(title ="Casos Covid 19 - Ciudad Informal", 
       subtitle="CABA", 
       x="Barrios", 
       y="Nº de Casos", 
       fill= "Nº de Casos", 
       caption= "Gráfico Nº4
        Fuente: Wikipedia")+
  theme_light()+
  coord_flip() #Giramos los ejes para poder leer mejor los barrios

#Podemos ver, al igual que el gráfico Nº2, que efectivamente los barrios con mas casos en villas y asentamientos son Flores, Retiro y Villa Lugano

# Ahora ordenamos los datos según casos positivos por barrio para la ciudad formal.
datos_011 <- datos_00 %>% 
  arrange(desc(CasosCFormal)) %>% 
  select( -Población, -CasosTotal, -CasosCInformal, -PCTFormal, -PCTInformal)

# Con estos datos, se los sumamos al gráfico anterior para ver como se solapan los casos por barrio entre la ciudad formal y la informal
ggplot()+
  geom_bar(data=datos_01, 
           aes(x=reorder(Barrio, -CasosCInformal),
               weight = CasosCInformal, fill=CasosCInformal))+
  geom_point(data=datos_011,
             aes(x=Barrio, y= CasosCFormal), colour="red")+
  scale_fill_viridis_c()+ 
  labs(title ="Casos Covid 19 - Ciudad Informal y Ciudad Formal", 
       subtitle="CABA",
       x="Barrios", 
       y="Nº de Casos", 
       fill= "Nº de Casos Ciudad Informal", 
       caption= "Gráfico Nº5
        Fuente: Wikipedia")+
  theme_light()+
  coord_flip()

# Podemos volver a observar la relacion entre los casos en villas y asentamientos con el resto de la ciudad. 
# En este caso volvemos a ver el contrastre en Retiro y en Barracas. 
# Tambien podemos ver con mayor claridad que en Flores y Barracas, si bien la distribucion es mas pareja, los positivos son mayores en la ciudad formal
# Retiro, al margen del contraste tan grande, es el único caso con mas positivos en las villas que en el resto de la ciudad


# Ahora queremos hacer foco en como se distribuyen los casos positivos en las villas y asentamientos con respecto al resto de la ciudad
# Para esto vamos a ver los porcentajes de positivos en la ciudad informal y lo ordenaremos de manera decreciente 
datos_02 <- datos_00 %>% 
  arrange(desc(PCTInformal)) %>% 
  select(-CasosTotal, -CasosCFormal, -CasosCInformal) #descartamos algunas columnas que en este caso no nos interesan

# Visualizamos los datos
ggplot(datos_02)+
  geom_bar(aes(x=reorder(Barrio, -PCTInformal), weight = PCTInformal, fill= Barrio))+
  labs(title ="% Casos Covid 19 en la Ciudad Informal",
       subtitle="CABA", 
       x="Barrio", 
       y="% de positivos en villas",  
       caption= "Gráfico Nº6
        Fuente: Wikipedia")+
  coord_flip() #Giramos los ejes para poder visualizar y leer mejor los barrios.

#Vemos que efectivamente en Retiro tenemos cerca del 80% de los casos

####

# Ahora queremos ver la información volcada en mapas

# Para esto trabajaremos con información geográficca obtenida en la base de datos de los barrios. 
# Unimos la base de datos que nos itneresa con nuetro dataset de barrios.
mapas1 <- left_join (barrios, datos_00, by=c( "BARRIO"="Barrio"))

# Corroboramos haciendo un mapa limpio con la base de datos
ggplot(mapas1)+
  geom_sf()+
  labs(title ="Ciudad Autonoma de Buenos Aires (CABA)",
       subtitle="Argentina",
       caption= "Mapa Nº1
        Fuente: Buenos Aires Data")

# Ahora ajustamos algunas cosas estéticas y le agregamos el nombre de los barrios
ggplot(mapas1)+
  geom_sf(aes(colors = "darkolivegreen1"), fill="darkolivegreen3")+ #Cambiamos el color de fondo de los barrios y el de las lineas de borde
  geom_sf_label(aes(label = BARRIO), size=2)+ #Agregamos el nombre de los barrios para facilitar la lectura
  labs(title ="Barrios", 
       subtitle="CABA", 
       caption= "Mapa Nº2
        Fuente: Buenos Aires Data")+
  theme_void() #Le sacamos el fondo con las grillas de latitud y longitud

# Para continuar con la visualización de nuestros datos primero queremos identificar solo aquellos barrios con villas o asentamientos
ggplot(filter(mapas1, CasosTotal>=1))+ #Filtramos aquellos barrios que no tenemos datos de casos (que por nuestro dataset seran aquellos sin villas/asentamientos)
    geom_sf(data=filter(mapas1, CasosTotal >= 1), aes(colors = "chocolate1"), fill="cornsilk1")+ #Configuramos el color que queremos para los barrios
  geom_sf_label(aes(label = BARRIO), size=2)+ #Agregamos el nombre de los barrios para facilitar la lectura
  theme_minimal()+ #Como no visualizamos la totalidad de la ciudad, volvemos a agregar lat y long
  labs(title ="Barrios con villas/asentamientos",
       y="Latitud", x="Longitud",
       subtitle="CABA", 
       caption= "Mapa Nº3
        Fuente: Buenos Aires Data")

# Ahora si volcamos a nuestro mapa la cantidad de casos total por barrio 
# En este mapa vamos a configurar cuestiones esteticas del fondo para ver mejor los datos
ggplot()+
  geom_sf(data=mapas1, aes(fill=CasosTotal), color="lavender", size=1)+ #Configuramos el color de la linea de borde los barrios 
  geom_sf_label(data=filter(mapas1, CasosTotal >= 1), aes(label = BARRIO), size=2)+ #Agregamos la etiqueta de los barrios en los que estamos volcando datos
  scale_fill_viridis_c(trans = "log2", na.value = "whitesmoke") + #Configuramos escala de colores y el color para los barrios sin datos
  theme_inset() + #Volvemos a sacar los ejes de long y lat
  labs(title ="Casos de Covid por Barrio", 
       subtitle="En barrios con asentamientos o villas",
       fill= "Cantidad de casos",
       caption= "Mapa Nº4
            Fuente: Buenos Aires Data")+
  theme(legend.position = "bottom", #Configuramos para que la referencia aparezca en la parte inferior del mapa
        legend.key.width = unit(2,"cm")) #Configuramos el ancho de la barra de escala
#Vemos que los barrios del sur tienen entre valores medios y altos de positivos de covid. Los del Norte tienen valores bajos

#Ahora queremos ver los casos en la ciudad formal, a ver si la distribución se mantiene
ggplot()+
  geom_sf(data=mapas1, aes(fill=CasosCFormal), color="lavender", size=0.2)+
  geom_sf_label(data=filter(mapas1, CasosTotal >= 1), aes(label = BARRIO), size=2)+
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "whitesmoke") + #Probamos otra escala de colores, pero la invertimos ya que de este modo es mas acorde a las referencias que queremos ver
  theme_light() + #Probamos otro estilo de fondo
  labs(title ="Casos de Covid por Barrio", 
       subtitle="En la ciudad formal",
       fill= "Cantidad de casos",
       y="Latitud", x="Longitud",
       caption= "Mapa Nº5
            Fuente: Buenos Aires Data")+
  theme(legend.direction = "vertical", #Ponemos las referencias al costado
        legend.position = "right", #especificamos para que aparezcan a la derecha
        legend.key.height = unit(2, "cm"), #configuramos el tamaño de la escala
        legend.key.width = unit(1,"cm"))

# Vemos que en lineas generales la distribución se mantiene, cambia mas que nada el orden de los barrios del sur y Retiro. 
# En el total de casos teniamos primero al barrio de Flores, ahora es Barracas y disminuye tambien el barrio de Retiro, esto nos da a entnder que la mayoria de los casos en Retiro se daban la villa 31

# Entonces queremos ver cual es el porcentaj de de casos en las villas y asentamientos para cada barrio 
ggplot(mapas1)+
  geom_sf(aes(fill=CasosCInformal), size=0.2)+ #Configuramos para que la escala de colores sea segun la cantidad de casos 
  geom_sf_label(data=filter(mapas1, CasosTotal >= 1), aes(label = round(PCTInformal,1)), size=3)+ #En este caso queremos que se vea el porcentaje de casos en villas y asentamientos
  scale_fill_distiller(palette = "Spectral", na.value = "whitesmoke") + #Probamos otra escala de colores
  theme_classic() +
  labs(title ="Casos en villas y asentamientos x barrio", 
       subtitle="Cantidad de casos y porcentaje de casos x barrio",
       fill= "Cantidad de casos en villas y asentamientos",
       y="Latitud", x="Longitud",
       caption= "Mapa Nº6
            Fuente: Buenos Aires Data")+
  theme(legend.direction = "horizontal", 
        legend.position = "bottom", 
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(2,"cm"))

# Con este grafico podemos ver con mayor claridad como es el caso de retiro
# El 80% de los casos en el Barrio de Retiro se da en la villa 31 y estos son al rededor de 3000 casos. 
# Esto explica el cambio que veiamos entre el mapa Nº4 y el Nº5 

