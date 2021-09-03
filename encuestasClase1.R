# Este es un comentario

# Este es otro comentario


# 1. Descarga -------------------------------------------------------------
liga <- 'https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_concentradohogar_csv.zip'
carpeta <- '/home/julio/Desktop/Documents/datos/enigh_ejercicio1'
archivo <- 'enigh2020_ns_concentradohogar_csv.zip'

setwd(carpeta)

download.file(url = liga, destfile = archivo)

concentradohogar_base <- unzip(archivo)

file.remove(archivo)


library(tidyverse)
library(readxl)

concentradohogar <- read_csv(concentradohogar_base)

names(concentradohogar)

(tabulado <- read_excel('enigh2020_ns_basicos_tabulados.xlsx', sheet = 'Cuadro 1.1', range = 'A5:C15'))

# dbf
# pdf

names(concentradohogar)

library(srvyr)
library(survey)

sum(concentradohogar$ing_cor)

sum(concentradohogar['ing_cor'])

concentradohogar %>% summarise(mean(ing_cor))
# compendios estadisticos
summarise()
mean()
sum()
# Agrupamiento
concentradohogar %>% group_by(sexo_jefe) %>% summarise(mean(ing_cor))

# Crear nuevas variables
concentradohogar <- concentradohogar %>% 
  mutate(cve_edo = substr(ubica_geo, 1,2) )

# Selección de variables
concentradohogar %>% select(cve_edo, ubica_geo) %>% 
  View()
# Filtrado
# != diferente
# >
# <
# >=
# <=
# %>%
# %>% 
concentradohogar %>% filter(sexo_jefe == 2) %>% select(sexo_jefe, ing_cor) %>% View()



concentradohogar_svr <- as_survey_design(concentradohogar, strata = est_dis, weights = factor, id = upm)

concentradohogar_svr %>% group_by(cve_edo) %>% 
  summarise(ing_corr_tot = survey_total(ing_cor),
            ing_corr_prom = survey_mean(ing_cor,vartype = c("cv","ci"))
            ) %>% View()




