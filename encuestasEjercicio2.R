library(tidyverse)
library(srvyr)
library(survey)

# I. Leer archivos dbf ----------------------------------------------------


library(foreign)
setwd('/home/julio/Desktop/Documents/datos/enigh_ejercicio2')
liga <- 'https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_viviendas_dbf.zip'
download.file(liga, 'enigh2020_ns_viviendas_dbf.zip')
bade_dbf <- unzip('enigh2020_ns_viviendas_dbf.zip')
viviendas <- read.dbf(bade_dbf)

# pdf
# install.packages('pdftools')
library(pdftools)
viviendas <- pdf_text('ZM_2021T2.pdf')

archivo_pdf <- 'https://www.gob.mx/cms/uploads/attachment/file/635349/_ndice_SHF_T1_2021.pdf'
download.file(archivo_pdf, 'indice_SHF_T1_2021.pdf')

archivo_pdf <- pdf_text('_ndice_SHF_T1_2021.pdf')

# Separamos las líneas
archivo_pdf <- archivo_pdf %>% str_split('\n')

hoja1 <- archivo_pdf[[1]]

hoja1 <- hoja1[5:54]

hoja1_ <- hoja1 %>% str_split_fixed('\\s\\s+',27)

hoja1_ %>% View()
# 2. Unión de encuestas ---------------------------------------------------

# Vamos a unir las encuestas viviendas, hogares y población

# 1. Descargamos las encuenstas
viviendas_liga <- 'https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_viviendas_csv.zip'
hogares_liga <- 'https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_hogares_csv.zip'
poblacion_liga <- 'https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_poblacion_csv.zip'

# Nos ubicamos en el directorio en el que deseamos hacer la descarga
carpeta <- '/home/julio/Desktop/Documents/datos/enigh_ejercicio2'
setwd(carpeta)

# Descargamos los archivos
# Para viviendas:
viviendas_archivo <- 'enigh2020_ns_viviendas_csv.zip'
download.file( url =  viviendas_liga, destfile =  viviendas_archivo)
# Para hogares:
hogares_archivo <- str_extract(hogares_liga, pattern = '/(\\w|\\.|\\_)+$') %>% str_remove('/')
download.file( url = hogares_liga, destfile = hogares_archivo)
# Para población
poblacion_archivo <- str_extract(poblacion_liga, pattern = '/(\\w|\\.|\\_)+$') %>% str_remove('/')
download.file( url = poblacion_liga, destfile = poblacion_archivo)
# options(timeout=60)
# getOption('timeout')
# options(timeout=500)

# Descomprimimos los archivos
viviendas_base <- unzip(viviendas_archivo)
hogares_base <- unzip(hogares_archivo)
poblacion_base <- unzip(poblacion_archivo)

# Eliminamos los archivos zip
file.remove(viviendas_archivo)
file.remove(hogares_archivo)
file.remove(poblacion_archivo)

# Leemos las bases
viviendas <- read_csv(viviendas_base)
hogares <- read_csv(hogares_base)
poblacion <- read_csv(poblacion_base)

# Llave para pegar la encuesta
# https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/nueva_estruc/889463901242.pdf
# P. 13


# Para unir las bases de viviendas y hogares se emplean las variables llave
# folioviv

nrow(viviendas)
nrow(hogares)
viviendas_hogares <- left_join(viviendas, hogares, by = c('folioviv'))

# Para unir la base con la base de población empleamos las variables llave
# folioviv
# foliohog
nrow(viviendas_hogares)
nrow(poblacion)
viviendas_hogares_poblacion <- left_join(viviendas_hogares, poblacion, by = c('folioviv', 'foliohog'))

enigh_srv <- as_survey_design(viviendas_hogares_poblacion, strata = est_dis, weights = factor, id = upm)

# Tabulados básicos Cuadro 1.1

enigh_srv %>% mutate(poblacion = 1) %>% 
  summarise(pob_tot = survey_total(poblacion, vartype = c("cv")))

enigh_srv %>% group_by(sexo) %>% 
  summarise(pob_tot = survey_total(vartype = c("cv","ci")))

enigh_srv <- enigh_srv %>% mutate(rango_edad = '')
enigh_srv <- enigh_srv %>% mutate(rango_edad = ifelse(edad <= 4, '1. 0-4 AÑOS',rango_edad))
enigh_srv <- enigh_srv %>% mutate(rango_edad = ifelse(edad >= 5 & edad <= 11, '2. 5-11 AÑOS',rango_edad))
enigh_srv <- enigh_srv %>% mutate(rango_edad = ifelse(edad >= 12 & edad <= 19, '3. 12-19 AÑOS',rango_edad))
enigh_srv <- enigh_srv %>% mutate(rango_edad = ifelse(edad >= 20 & edad <= 29, '4. 20-29 AÑOS',rango_edad))
enigh_srv <- enigh_srv %>% mutate(rango_edad = ifelse(edad >= 30 & edad <= 39, '5. 30-39 AÑOS',rango_edad))
enigh_srv <- enigh_srv %>% mutate(rango_edad = ifelse(edad >= 40 & edad <= 49, '6. 40-49 AÑOS',rango_edad))
enigh_srv <- enigh_srv %>% mutate(rango_edad = ifelse(edad >= 50 & edad <= 59, '7. 50-59 AÑOS',rango_edad))
enigh_srv <- enigh_srv %>% mutate(rango_edad = ifelse(edad >= 60 , '8. 60 Y MÁS AÑOS',rango_edad))

enigh_srv <- enigh_srv %>% mutate(sexo_et = factor(as.integer(sexo), labels = c('HOMBRES','MUJERES')))

setwd('/home/julio/Desktop/Documents/datos/enigh_ejercicio3')

saveRDS(enigh_srv, 'enigh.rds')

enigh_srv %>% group_by(rango_edad) %>% 
  summarise(pob_tot = survey_total(vartype = c("cv","ci")))

tabla_edad <- enigh_srv %>% group_by(sexo_et, rango_edad) %>% 
  summarise(pob_tot = survey_total(vartype = c("cv")))

tabla_edad

tabla_edad %>% 
  pivot_wider(
    names_from = sexo_et,
    names_sep = '_',
    values_from = c(pob_tot, pob_tot_cv)
  )

tabla_edad <- tabla_edad %>% 
  pivot_wider(
    names_from = sexo_et,
    names_sep = '_',
    values_from = c(pob_tot, pob_tot_cv)
  ) %>% select(rango_edad, pob_tot_HOMBRES, pob_tot_cv_HOMBRES, pob_tot_MUJERES, pob_tot_cv_MUJERES)


write_excel_csv(tabla_edad,'edad.csv')

# Por tamaño de localidad

# Localidades con menos de 2,500 habitantes
tabla_edad_men2500 <- enigh_srv %>% filter(tam_loc ==4) %>% 
  group_by(sexo_et, rango_edad) %>% 
  summarise(pob_tot_menos2500 = survey_total(vartype = c("cv"))) %>% 
  pivot_wider(
    names_from = sexo_et,
    names_sep = '_',
    values_from = c(pob_tot_menos2500, pob_tot_menos2500_cv)
  ) %>% select(rango_edad,pob_tot_menos2500_HOMBRES,pob_tot_menos2500_cv_HOMBRES,pob_tot_menos2500_MUJERES,pob_tot_menos2500_cv_MUJERES)


# Localidades con más de 2,500 habitantes
tabla_edad_mas2500 <- enigh_srv %>% filter(tam_loc !=4) %>% 
  group_by(sexo_et, rango_edad) %>% 
  summarise(pob_tot_mas2500 = survey_total(vartype = c("cv"))) %>% 
  pivot_wider(
    names_from = sexo_et,
    names_sep = '_',
    values_from = c(pob_tot_mas2500, pob_tot_mas2500_cv)
  ) %>% select(rango_edad,pob_tot_mas2500_HOMBRES,pob_tot_mas2500_cv_HOMBRES,pob_tot_mas2500_MUJERES,pob_tot_mas2500_cv_MUJERES)


# Unimos las tablas
tabla_edad_s <- left_join(tabla_edad, tabla_edad_mas2500, by = 'rango_edad') %>% 
  left_join(tabla_edad_men2500, by = 'rango_edad')


write_excel_csv(tabla_edad_s,'edad_2.csv')


# Loop para todas las localidas
# Para entidades


