#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("dplyr")
require("lightgbm")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "ZZ9412a"
PARAM$exp_input  <- "HT9412a"

#PARAM$modelos  <- 2

# FIN Parametros del script

vector_semillas <- c(700423, 700429, 700433, 700459, 700471, 700475, 700479, 700499, 700992, 700564)

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

base_dir <- "~/Documents/Maestria/"  # cloud:  "~/buckets/b1/"   #maestria

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
#setorder( tb_log, -ganancia )
tb_log$ganancia_max_acum = cummax(tb_log$ganancia)

#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn=FALSE )

#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_train_final.csv.gz" )
dataset  <- fread( arch_dataset )

#leo el dataset donde voy a aplicar el modelo final
arch_future  <- paste0( base_dir, "exp/", TS, "/dataset_future.csv.gz" )
dfuture <- fread( arch_future )

#defino la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]

campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria", "clase01") )

resultado_iteracion <- c()
ganancia_acumulada_iteracion <- 0


for( i in  c(seq(1, nrow(tb_log), 5), nrow(tb_log))  ) {  ## cada 5 iteraciones, calculo la ganancia en test con 10 semillas

  ## para la primera iteración, armo el armo el modelo, y 
  parametros  <- as.list( copy( tb_log[ i ] ) )
  iteracion_bayesiana  <- parametros$iteracion_bayesiana
  
  if(parametros$ganancia_max_acum > ganancia_acumulada_iteracion){
    
    parametros <- as.list(tb_log %>% filter(ganancia == parametros$ganancia_max_acum))
    
    #creo CADA VEZ el dataset de lightgbm
    dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with=FALSE] ),
                          label=   dataset[ , clase01],
                          weight=  dataset[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                          free_raw_data= FALSE)
    
    ganancia  <- parametros$ganancia
    envios  <- parametros$estimulos
    prob_corte <- parametros$prob_corte
    ganancia_acumulada <- parametros$ganancia_max_acum
  
    #elimino los parametros que no son de lightgbm
    parametros$experimento  <- NULL
    parametros$cols         <- NULL
    parametros$rows         <- NULL
    parametros$fecha        <- NULL
    parametros$prob_corte   <- NULL
    parametros$estimulos    <- NULL
    parametros$ganancia     <- NULL
    parametros$iteracion_bayesiana  <- NULL
    parametros$ganancia_max_acum <- NULL
    
    ganancia_iter <- c()
    
    for (j in vector_semillas) {
      
      #Utilizo la semilla definida en este script
      parametros$seed  <- j
      
      #genero el modelo entrenando en los datos finales
      set.seed( parametros$seed )
      
      modelo_final  <- lightgbm( data= dtrain,
                                param=  parametros,
                                verbose= -100 )
    
      #genero la prediccion, Scoring
      prediccion  <- predict( modelo_final,
                              data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )
    
      tbl  <- dfuture[ , list(clase_ternaria) ]
    
      tbl[ , prob := prediccion ]
    
      ganancia_test  <- tbl[ prob >= prob_corte,
                             sum( ifelse(clase_ternaria=="BAJA+2", 78000, -2000 ) )]
    
      ganancia_iter <- rbind( ganancia_iter, ganancia_test)
    
    }
    
    ganancia_modelo <- mean(ganancia_iter)
    resultado_iteracion = rbind(resultado_iteracion, list(i, ganancia_modelo))
    ganancia_acumulada_iteracion = ganancia_acumulada
  
  } else {
    
    resultado_iteracion = rbind(resultado_iteracion, list(i, resultado_iteracion[-1,2]))
    
  }
  
}

colnames(resultado_iteracion) <- c("iteracion", "ganancia")


fwrite(  resultado_iteracion,
         file= "ganancias_iteracion.csv",
         sep= "," )


    
