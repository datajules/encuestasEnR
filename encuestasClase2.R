
# I. Agregando más de una condición ---------------------------------------


data_frame %>% filter(var1 == 1 | var2 == "hombre")
# ==
# !=
# >
# <
# <=
# >=

# & ->  Con un solo falso que se presente todo va a ser falso
data_frame  
# FALSO & FALSO  & FALSO & FALSO = FALSO
# FALSO & VERDADERO = FALSO
# VERDADERO & VERDADERO & FALSO & VERDADERO = FALSO
# |

# !


# II. Verbos de transformación de datos -----------------------------------


# agrupamiento
data_frame %>% group_by(sexo)

# summarise summarize
data_frame %>% group_by(sexo) %>% summarise(survey_total(ingreso_corr))

# Crear variables nuevas
data_frame %>% mutate(variable_nueva = var1 * var2)



# III. Leer archivos en dbf -----------------------------------------------

library(tidyverse)
library(foreign)

liga <- 'https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_viviendas_dbf.zip'
carpeta <- '/home/julio/Desktop/Documents/datos/ejercicio2'
archivo <- 'enigh2020_ns_viviendas_dbf.zip'
setwd(carpeta)
download.file(liga, archivo)

archivo_base <- unzip(archivo)

viviendas <- read.dbf("./viviendas.dbf")


# IV. Leer pdf -----------------------------------------------------------

library(pdftools)

liga <- 'https://www.gob.mx/cms/uploads/attachment/file/635349/_ndice_SHF_T1_2021.pdf'

# download.file(liga, 'indice_SHF_T1_2021.pdf')

tabla_shf <- pdf_text('_ndice_SHF_T1_2021.pdf')

tabla_shf <- tabla_shf %>% str_split(pattern = '\n')

hoja1 <- tabla_shf[[1]]

hoja1_tabla <- hoja1[5:54] %>% str_split_fixed(pattern = '\\s\\s+',27)



hoja2 <- tabla_shf[[2]]

hoja2_tabla <- hoja1[1:56] %>% str_split_fixed(pattern = '\\s\\s+',27)




