
# I. Librerías ------------------------------------------------------------

library(tidyverse)
library(survey)
library(srvyr)

# II. Bases de datos ------------------------------------------------------
carpeta <- '/home/julio/Desktop/Documents/datos/enigh_ejercicio3'
setwd(carpeta)
enigh_srv <- readRDS('enigh.rds')

# Los primeros dos dígitos corresponden a la clave de la entidad
enigh_srv <- enigh_srv %>% mutate(cve_edo = substr(ubica_geo, 1,2))
# Convertimos la variable en numérica
enigh_srv <- enigh_srv %>% mutate(cve_edo = as.integer(cve_edo))
# COnvertimos la variable en factor y Asociamos etiquetas
entidades <- c(
  "Ags",
  "BC",
  "BCS",
  "Camp",
  "Coah",
  "Col",
  "Chis",
  "Chih",
  "CDMX",
  "Dgo",
  "Gto",
  "Gro",
  "Hgo",
  "Jal",
  "Mex",
  "Mich",
  "Mor",
  "Nay",
  "NL",
  "Oax",
  "Pue",
  "Qro",
  "Q Roo",
  "SLP",
  "Sin",
  "Son",
  "Tab",
  "Tamps",
  "Tlax",
  "Ver",
  "Yuc",
  "Zac"
)

enigh_srv <- enigh_srv %>% 
  mutate(cve_edo = factor(cve_edo,
                          labels = entidades))



# III. For loops ----------------------------------------------------------

# Para crear una lista de números se emplea la función c() y dentro de ella
#  se agregan los elementos de la lista separados por coma.
numeros <- c(1,2,3,4,5)
numeros[3]

# Otra forma de crear listas de números consecutvos es separándolos por 
# ':'

numeros_2 <- 1:100
numeros_2

variable <- 2

print(variable)

for (num in numeros) {
  try({
    'a' + 1
    'b' + num
  }
    )
  print(num)
}


for (variable in entidades) {
  'a' + 
  print(variable)
}

for (i in numeros) {
  print(i)
}

# El for nos permite automatizar operaciones repetitivas
for (valor in numeros) {
  (texto <- paste(valor, 'elefantes se columpiaban sobre la tela de una araña.'))
  # (texto)
  print(texto)
}

# También podemos emplear el for para realizar operaciones que modifiquen
# alguna variable existente.
sumaDeNumeros <- 0
for (n in numeros_2) {
  sumaDeNumeros <- n + sumaDeNumeros
}

sumaDeNumeros

1+2+3

100 + 1
99 + 2

# IV. Gráficas ----------------------------------------------------------------

tabla_edad <- enigh_srv %>% group_by(sexo_et, rango_edad) %>% 
  summarise(pob = survey_total(vartype = c("cv"))) 
 
addmargins(tabla_edad)

grafica1 <- ggplot(data = tabla_edad, aes(x = rango_edad, y = pob/1000000, fill = sexo_et)) +
  geom_col()

grafica1

grafica1 <- grafica1 + coord_flip() +
  labs(title = 'México: Población por rango de edad (2020)',
       y = 'Millones de personas',
       x = '',
       legend = 'Género',
       caption = 'Elaboración propia con base en ENIGH (2020)'
       )
grafica1

# Salvamos la gráfica
# Formatos disponibles:
# jpeg
# png
# pdf
jpeg('pob_edad_f.jpg')
grafica1
dev.off()

png('pob_edad.png')
grafica1
dev.off()

pdf('pob_edad.pdf')
grafica1
dev.off()

grafica1 + theme_dark()
# V. Loops + gráficas --------------------------------------------------------

# Guardamos las gráficas que vamos a generar en un directorio
carpeta_graficas <- '/home/julio/Desktop/Documents/datos/enigh_ejercicio3/graficas_estados'
setwd(carpeta_graficas)

estados <- 1:32
# Primero generamos la tabla
tabla_edad <- enigh_srv %>% 
  group_by(cve_edo, sexo_et, rango_edad) %>% 
  summarise(pob = survey_total(vartype = c("cv"))) %>% ungroup()

for (i in estados) {
  # Filtramos la entidad
  try(tabla_edad_ent <- tabla_edad %>% filter(cve_edo == entidades[i]))
  # Generamos la gráfica
  grafica <- ggplot(data = tabla_edad_ent, aes(x = rango_edad, y = pob / 100000, fill = sexo_et)) +
    geom_col()
  # Damos formato a la gráfica
  grafica <- grafica + coord_flip() +
    labs(title = paste(entidades[i],': Población por rango de edad (2020)'),
         y = 'Cientos de miles de personas',
         x = '',
         legend = 'Género',
         caption = 'Elaboración propia con base en ENIGH (2020)'
    ) +
    scale_fill_discrete(name = 'GÉNERO')
  # Salvamos la gráfica
  archivo <- paste0(entidades[i],'_pob_edad.png')
  
  png(archivo)
  print(grafica)
  dev.off()

  cat('\r','Estado ', i, 'de 32.')
}


# VI. Tablas con diseño ---------------------------------------------------

library(kableExtra)

# Tomaremos la tabla de la CDMX como ejemplo
dt <- tabla_edad %>% filter(cve_edo == "CDMX") %>% 
  select(-cve_edo) %>% 
  pivot_wider(
    names_from = sexo_et,
    names_sep = '_',
    values_from = c(pob, pob_cv)
  ) %>% select(rango_edad, pob_HOMBRES, pob_cv_HOMBRES, pob_MUJERES, pob_cv_MUJERES)

# Cambiamos los nombres a las columnas
dt <- dt %>% mutate(
  pob_HOMBRES = format(pob_HOMBRES, decimal.mark=".", big.mark=" ", digits = 2),
  pob_cv_HOMBRES = format(pob_cv_HOMBRES, decimal.mark=".", big.mark=" ", digits = 2),
  pob_MUJERES = format(pob_MUJERES, decimal.mark=".", big.mark=" ", digits = 2),
  pob_cv_MUJERES = format(pob_cv_MUJERES, decimal.mark=".", big.mark=" ", digits = 2)
)

colnames(dt) <- c('Edad','Hombres','CV H','Mujeres','CV M')
kbl(dt)



# Existen 6 estilos predefiidos
# kable_paper
# kable_classic
# kable_classic_2 
# kable_minimal
# kable_material 
# kable_material_dark

salida1 <- dt %>% mutate(Total = pob_HOMBRES  + pob_MUJERES  )

salida2 <- dt %>% summarise(pob_HOMBRES = sum(pob_HOMBRES), pob_MUJERES = sum(pob_MUJERES)  ) %>% 
  mutate(rango_edad = "Total",pob_cv_HOMBRES = 0, pob_cv_MUJERES = 0, Total = 0 ) %>% 
  select(rango_edad,       pob_HOMBRES, pob_cv_HOMBRES, pob_MUJERES, pob_cv_MUJERES,   Total)

salida3 <- bind_rows(salida1, salida2) 
salida3 %>% View()

dt %>%
  kbl() %>%
  kable_classic_2()

# pander
# pandoc

# Fuente y tamaño de letra
# https://www.w3schools.com/cssref/css_websafe_fonts.asp
# Arial (sans-serif)
# Verdana (sans-serif)
# Helvetica (sans-serif)
# Tahoma (sans-serif)
# Trebuchet MS (sans-serif)
# Times New Roman (serif)
# Georgia (serif)
# Garamond (serif)
# Courier New (monospace)
# Brush Script MT (cursive)

dt %>%
  kbl() %>%
  kable_paper(html_font = 'Georgia', font_size = 12)

# Cambio de color
dt %>%
  kbl() %>%
  kable_classic_2() %>% 
  column_spec(2, color = 'white', background = 'black')



# Si se desea colorear varias columnas se especifican como lista
dt %>%
  kbl() %>%
  kable_classic_2() %>% 
  column_spec(c(2,4), color = 'white', background = 'black')

# En caso de querer colorear una sola celda
celda <- dt$Edad == '5. 30-39 AÑOS'
dt %>%
  kbl() %>%
  kable_classic_2() %>% 
  column_spec(2, color = ifelse(celda,'white', 'black'), background = ifelse(!celda,'white', 'black'))


dt %>% addmargins()
