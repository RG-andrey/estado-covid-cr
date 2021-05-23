# Carga de ggplot2
library(ggplot2)

# Carga de dplyr
library(dplyr)

# Carga de datos desde un archivo CSV (separado por tabuladores)
registros_mammalia_cr <- 
  read.csv(
    file='https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/gbif/mammalia-cr-registros.csv', 
    sep = "\t"
  )

# Despliegue de los datos
View(registros_mammalia_cr)

#Por año
# Generación de un nuevo data frame
registros_agrupados_x_anyo <-
  registros_mammalia_cr %>%
  dplyr::count(year)

# Gráfico de barras verticales
barplot(
  registros_agrupados_x_anyo$n, 
  names.arg = registros_agrupados_x_anyo$year,
  main = "Cantidad de registros de mamíferos por año",
)

# Gráfico de barras horizontales
barplot(
  registros_agrupados_x_anyo$n, 
  names.arg = registros_agrupados_x_anyo$year,
  main = "Cantidad de registros de mamíferos por año",
  horiz = TRUE,
  las=1
)

#por orden
# Generación de un nuevo data frame
registros_agrupados_x_orden <-
  registros_mammalia_cr %>%
  dplyr::count(order)

# Gráfico de barras verticales
barplot(
  registros_agrupados_x_orden$n, 
  names.arg = registros_agrupados_x_orden$order,
  main = "Cantidad de registros por orden taxonómico de mamíferos",
)

# Gráfico de barras horizontales
par(mai=c(1, 2, 1, 1))
barplot(
  registros_agrupados_x_orden$n, 
  names.arg = registros_agrupados_x_orden$order,
  main = "Cantidad de registros por orden taxonómico de mamíferos",
  horiz = TRUE,
  las=1
)

#Agrupación por familia de carnivoros

# Generación de un nuevo data frame
registros_carnivora_agrupados_x_familia <-
  registros_mammalia_cr %>%
  filter(order == "Carnivora") %>%
  dplyr::count(family)

# Gráfico de barras horizontales
par(mai=c(1, 2, 1, 1))
barplot(
  registros_carnivora_agrupados_x_familia$n, 
  names.arg = registros_carnivora_agrupados_x_familia$family,
  main = "Cantidad de registros por familia de carnívoros",
  horiz = TRUE,
  las=1
)

#Agrupación por especie de Felinos

# Generación de un nuevo data frame
registros_felidae_agrupados_x_especie <-
  registros_mammalia_cr %>%
  filter(family == "Felidae") %>%
  dplyr::count(species)

# Gráfico de barras horizontales
par(mai=c(1, 2, 1, 1))
barplot(
  registros_felidae_agrupados_x_especie$n, 
  names.arg = registros_felidae_agrupados_x_especie$species,
  main = "Cantidad de registros por especie de felinos",
  horiz = TRUE,
  las=1
)

#gráfico de barras de felinos y caninos

registros_felidae_agrupados_x_especie <-
  registros_mammalia_cr %>%
  filter(family == "Felidae"| family== "canidae") %>%
  dplyr::count(species)

par(mai=c(1, 2, 1, 1))
barplot(
  registros_felidae_agrupados_x_especie$n, 
  names.arg = registros_felidae_agrupados_x_especie$species,
  main = "Cantidad de registros por especie de felinos",
  horiz = TRUE,
  las=1
)


#Funciones de ggplot2

#Barras apiladas

#Registro de felinos por especie y por tipo de registro

# Generación de un nuevo data frame
registros_felidae_agrupados_x_especie_tiporegistro <-
  registros_mammalia_cr %>%
  filter(family == "Felidae") %>%
  dplyr::count(species, basisOfRecord)

# Gráfico de barras apiladas
ggplot(
  registros_felidae_agrupados_x_especie_tiporegistro, 
  aes(x = species, y = n, fill = basisOfRecord)
) +
  geom_col() +
  coord_flip()

#gráfico de barras agrupadas 

ggplot(
  registros_felidae_agrupados_x_especie_tiporegistro, 
  aes(x = species, y = n, fill = basisOfRecord)
) +
  geom_col(position = "dodge") +  coord_flip() 

