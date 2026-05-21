# Libraries
library(tidyverse)
library(jsonlite)

# Data ----
df_hrvb <- readxl::read_xlsx("RR_HRVB_V2.xlsx")

# Preparacion de los datos

## Igualamos los registros y limpiamos ----
df_hrvb <- df_hrvb %>%
  mutate(across(contains("rr"), ~ lag(.x, default = 0))) %>% #desplazamos los valor hacia abajo y el primer valor es un 0
  mutate(across(contains("time"), ~ as.numeric(gsub("'", "", .)))) #~ nos indica que funcion vamos a aplicar a cada elemento

## Asignamos una variable factor por tiempo de registro----
## La variable tiempo hace referencia al tiempo de cada cambio

step <- function(columna, tiempo){
  resp <- numeric()
  stage <- 1
  limite <- tiempo 
  
  for(idx in seq_along(columna)){
    if(is.na(columna[idx])){
      resp[idx] <- NA
    } else if(columna[idx] <= limite){
      resp[idx] <- stage
      } else{
      stage <- stage + 1
      limite <- tiempo * stage
      resp[idx] <- stage
    }
  }
  return(resp)
}

df_hrvb <- df_hrvb %>% 
  mutate(across(contains("time"), 
                ~ step(.x, 120),
                .names = "resp_{.col}"))


## Segmentamos la serie en funcion del factor correspondiente ----
segmentar <- function(dataframe){
  col_rr <- grep("rr", names(dataframe))
  col_resp <- grep("resp", names(dataframe))
  
  lista_listas <- list()
  
  for(k in seq_along(col_rr)){
    nombre <- paste(names(dataframe)[col_rr[k]], "segmented",names(dataframe)[col_resp[k]], sep = "_")
    lista_listas[[nombre]] <- split(dataframe[[col_rr[k]]], dataframe[[col_resp[k]]])
  }
  return(lista_listas)
}

# Medir tiempo (~0.19s)
system.time(
  df_segmentado <- segmentar(df_hrvb)
)

## Creamos un TIBBLE con todos los datos que tenemos ----
tibble_de_listas <- function(lista_de_listas) {
  tibble(
    nombre_completo = rep(names(lista_de_listas), lengths(lista_de_listas)), 
    lista = unlist(lista_de_listas, recursive = FALSE)  
  )
}

df_listado <- tibble_de_listas(df_segmentado)


df_indicadores <- tibble(id = substr(df_listado$nombre_completo, 4, 6),
                         sesion = substr(df_listado$nombre_completo, 8, 9),
                         situacion = as.factor(rep(1:10, times = 208)),
                         output = (as.factor(rep(1:0, times = 1040)))
                         )

df_final <- cbind(df_indicadores, df_listado)

### Guardamos el tibble en RDS y JSON ----
saveRDS(df_final, "data_hrvb_segmentada.rds")
write_json(df_final, "data_hrvb_segmentada.json", pretty = TRUE)


## Cambio de formato (si fuera necesario)----
### Long and Wide

formato <- function(datos){
  
  #cambiamos el formato de los datos
  df_longer <- datos %>%
    pivot_longer(cols = everything(), # todas las columnas
                 names_to = c("variable", "Participante", "Sesion"), # nuevos nombres
                 names_sep = "_", #divide los nombres de las columnas por las etiquetas originales separadas por _
                 values_to = "valor")
  #desde el formato long lo volvemos wide con los datos en formato de lista
  df_wide_lists <- na.omit(df_longer) %>%
    pivot_wider(names_from = variable, # convertimos esta variable en columnas
                values_from = valor,
                values_fn = list) # Los valores son de la columna valor y los guardamos en listas
  
  return(df_wide_lists) 
}

# Segmentamos las series por observaciones-----

# Se le pasa la columna y el num de paquetes que queremos
# Columna o vector numerico
#Omite los NA y calcula cada cuantos num se debe calcular la media
#ELIMINAR LOS PRIMEROS REGISTROS (FALTA)

paquetes <- function(datos, num_paq) {
  dt<- na.omit(datos) #omitimos de cada columna los NA
  n <- length(dt)
  n_medias <- floor(n/num_paq) #cada cuantos n se calcula
  medias<- numeric()
  
  for (i in 1:num_paq) {
    inicio <- (i-1)*n_medias + 1 #el inicio es cada n_medias
    fin <- i*n_medias #el final es la posición i de los paquet * n_medias 
    medias[i] <- mean(dt[inicio:fin], na.rm= T) 
  }
  
  return(medias)
}

# Graficacion

xx <- df_final$lista %>% 
  enframe(name = "nombre", value= "valores") %>% 
  unnest(valores)

xx <- xx %>% 
  group_by(as.factor(nombre)) %>% 
  mutate(Indice = row_number())



plot(x = seq_along(df_final$lista[[1]]), df_final$lista[[1]], type = "l")
lines(x = seq_along(df_final$lista[[2]]), df_final$lista[[2]], col="red")
lines(x = seq_along(df_final$lista[[3]]), df_final$lista[[3]], col="blue")










