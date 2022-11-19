# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("purrr")
require("readr")

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

base_dir <- "~/buckets/b1/"
#base_dir <- "~/Documents/Maestria/"


archivos_final = c()

for (exp in PARAM$exp_input){
  
  path_experimento_semillerio <- paste0(base_dir, "exp/", exp)
  archivos <- list.files(path = path_experimento_semillerio, pattern = "_resultados.csv")
  archivos = paste0(path_experimento_semillerio, "/", archivos)
  archivos_final = c(archivos_final, archivos)
}


data_join <- archivos_final %>% 
  lapply(read_csv) %>%                              # Store all files in list
  reduce(full_join, by = "numero_de_cliente") %>%   # Full-join data sets into one data set
  select(numero_de_cliente,contains("rank"))

# Esta es la predicción del semillerio para la semilla i-esima
tb_prediccion_semillerio <- data.table(
  data_join[, 1],
  prediccion = rowMeans(data_join[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
)


## guardo el ensamble en la carpeta del experimento
fwrite(  tb_prediccion_semillerio,
         file= paste0( PARAM$experimento, ".csv"),
         sep= "," )


## genero distintos cortes para el ensamble

cortes  <- seq( from=  7000,
                to=   15000,
                by=     500 )


setorder( tb_prediccion_semillerio, prediccion )

for( corte in cortes )
{
  tb_prediccion_semillerio[  , Predicted := 0L ]
  tb_prediccion_semillerio[ 1:corte, Predicted := 1L ]
  
  nom_submit  <- paste0( PARAM$experimento, 
                         "_",
                         sprintf( "%05d", corte ),
                         ".csv" )
  
  fwrite(  tb_prediccion_semillerio[ , list( numero_de_cliente, Predicted ) ],
           file= nom_submit,
           sep= "," )
  #print(nom_submit)
}
