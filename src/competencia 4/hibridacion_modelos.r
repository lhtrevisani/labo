# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("purrr")
require("readr")
require("stringr")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "ZZ1292_ganancias_semillerio"
PARAM$exp_input <- c("ZZ9411_semillerio", "ZZ9421_semillerio", "ZZ9431_semillerio")

# PARAM$corte <- 11000 # cantidad de envios
# FIN Parametros del script

options(error = function() {
    traceback(20)
    options(error = NULL)
    stop("exiting after script error")
})

#base_dir <- "~/buckets/b1/"
base_dir <- "~/Documents/Maestria/"


## creo la ruta de todos los modelos y semillas, y una lista de los modelos a hibridar
archivos_final = c()
modelos_final = c()

for (exp in PARAM$exp_input){
  
  path_experimento_semillerio <- paste0(base_dir, "exp/", exp)
  archivos <- list.files(path = path_experimento_semillerio, pattern = "_resultados.csv")
  modelos = list.files(path = path_experimento_semillerio, pattern = "ksemillas.csv")
  modelos = paste0(exp,"_", str_sub(modelos,1,-15))
  archivos = paste0(path_experimento_semillerio, "/", archivos)
  archivos_final = c(archivos_final, archivos)
  modelos_final = c(modelos_final, modelos)
}


## itero sobre cada modelo, y promedio el ranking de las distintas semillas (resultado: 1 vector para cada modelo):

modelos_semillerio = list()

for (i in modelos_final) {
  
  data_join <- archivos_final[str_detect(archivos_final, "ZZ9411_semillerio_modelo1")] %>%
    lapply(read_csv) %>%                              # Store all files in list
    reduce(full_join, by = "numero_de_cliente") %>%   # Full-join data sets into one data set
    select(numero_de_cliente,contains("rank"))
  
  modelos_semillerio[[i]] <- data.table(
    data_join[, 1],
    prediccion = rowMeans(data_join[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
  )
  
}


## joineo los vectores de cada modelo, promedio los ranks y queda un único vector de probabilidades.

modelos_semillerio_join = modelos_semillerio %>%
  reduce(full_join, by = "numero_de_cliente")


# Esta es la predicción del semillerio para la semilla i-esima
tb_prediccion_semillerio_mean <- data.table(
  modelos_semillerio_join[, 1],
  prediccion = rowMeans(modelos_semillerio_join[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
)

## podría intentar hibridar de otra manera que no sea mean de ranking (ej: votación o )


## guardo el ensamble en la carpeta del experimento
fwrite(  tb_prediccion_semillerio_mean,
         file= paste0( PARAM$experimento, "_mean.csv"),
         sep= "," )


fwrite(  modelos_semillerio_join,
         file= paste0( PARAM$experimento, "_rank.csv"),
         sep= "," )

## genero distintos cortes para el ensamble

cortes  <- seq( from=  7000,
                to=   15000,
                by=     500 )


setorder( tb_prediccion_semillerio_mean, prediccion )

for( corte in cortes )
{
  tb_prediccion_semillerio_mean[  , Predicted := 0L ]
  tb_prediccion_semillerio_mean[ 1:corte, Predicted := 1L ]
  
  nom_submit  <- paste0( PARAM$experimento, 
                         "_",
                         sprintf( "%05d", corte ),
                         ".csv" )
  
  fwrite(  tb_prediccion_semillerio_mean[ , list( numero_de_cliente, Predicted ) ],
           file= nom_submit,
           sep= "," )
  #print(nom_submit)
}
