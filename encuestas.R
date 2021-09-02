
# Panorama general --------------------------------------------------------
# 
# Importar datos (excel, csv, zip, datos abiertos)
# Las relaciones entre tablas (llave de la encuesta)
# Crear el diseño muestral
# Etiquetas de variables y de categorías
# Crear variables
# Condiciones
# Ciclos For
# Seleccionar casos
# Fundir tablas
# Tablas con diseño (elección de tipo de letra, color, etc.)
# Precisiones estadísticas
# Gráfica


# I. Descarga, decompresión y lectura de archivos -------------------------



# 1. Librerías ------------------------------------------------------------

library(tidyverse)


# 2. Descarga de archivos -------------------------------------------------


# 2.1 Definimos las rutas de los archivos ---------------------------------
# Carpeta destino del archivo
carpeta <- '/home/julio/Desktop/Documents/datos/enigh'
# Enlace de descarga, tomado de la página de INEGI
enlace <- 'https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_concentradohogar_csv.zip'
archivo <- 'enigh2020_ns_concentradohogar_csv.zip'

# 2.2 Descargamos el archivo ----------------------------------------------
download.file(url = enlace, destfile = paste0(carpeta,'/',archivo))

# 3. Descomprimimos el archivo --------------------------------------------
# Cambiamos el directorio a la carpeta en donde guardamos el archivo.
setwd(carpeta)
# Descomprimimos el archivo
base_archivo <- unzip(archivo)
# Eliminamos el archivo que descomprimimos
file.remove(archivo)
  


# 4. Leemos la base -------------------------------------------------------

# 4.1 Para leer archivos csv ----------------------------------------------
concentradohogar <- read_csv(base_archivo)
# 4.2 Para leer archivos de excel
library(readxl)

tabulados_archivo <- 'enigh2020_ns_basicos_tabulados.xlsx'
ingreso_corriente <- read_excel(tabulados_archivo, sheet = 'Cuadro 2.1', range = "A4:E17")

View(concentradohogar)
names(concentradohogar)




# II. Encuestas  ----------------------------------------------------------

# 1. Declaración de encuestas complejas -----------------------------------


# 1.1 Creación de nuevas variables ----------------------------------------

# La variable ubicageo contiene información de estado y municipio
unique(concentradohogar$ubica_geo)
# Los primeros dos dígitos corresponden a la clave de la entidad
concentradohogar$cve_edo <- substr(concentradohogar$ubica_geo, 1,2)
table(concentradohogar$cve_edo)
# Convertimos la variable en numérica
concentradohogar$cve_edo <- as.integer(concentradohogar$cve_edo)
# COnvertimos la variable en factor y Asociamos etiquetas
concentradohogar$cve_edo <- factor(concentradohogar$cve_edo,
                                   labels = c(
                                     "Ags.",
                                     "BC",
                                     "BCS",
                                     "Camp.",
                                     "Coah.",
                                     "Col.",
                                     "Chis.",
                                     "Chih.",
                                     "CDMX",
                                     "Dgo.",
                                     "Gto.",
                                     "Gro.",
                                     "Hgo.",
                                     "Jal.",
                                     "Mex.",
                                     "Mich.",
                                     "Mor.",
                                     "Nay.",
                                     "NL",
                                     "Oax.",
                                     "Pue.",
                                     "Qro.",
                                     "Q. Roo",
                                     "SLP",
                                     "Sin.",
                                     "Son.",
                                     "Tab.",
                                     "Tamps.",
                                     "Tlax.",
                                     "Ver.",
                                     "Yuc.",
                                     "Zac."
                                   ))

concentradohogar$cve_edo %>% table()


concentradohogar %>% group_by(cve_edo) %>% summarise(sum(p12_64))

# Vamos a emplear una librería para estimar encuestas con diseño complejo
library(srvyr)
library(survey)

# Declaramos el diseño de la encuesta compleja
concentradohogar_svyset <- as_survey_design(concentradohogar,strata = est_dis, weights = factor, id = upm, nest=TRUE)

concentradohogar_svyset  %>% 
  group_by(cve_edo) %>% 
  summarize( total = survey_total(p12_64),
             promedio = survey_mean(p12_64))



concentradohogar_svyset  %>% 
  group_by(cve_edo) %>% 
  summarize( total = survey_total(ing_cor),
             promedio = survey_mean(ing_cor))

