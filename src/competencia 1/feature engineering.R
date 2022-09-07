# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("dplyr")
require("mlrMBO")

options(scipen=999)

# Poner la carpeta de la materia de SU computadora local
setwd("/home/lucas/Maestria/DMEyF")

# Poner sus semillas
semillas <- c(700423, 700429, 700433, 700459, 700471)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")


######################  Realizo feature engineering sobre el dataset (operaciones entre columnas) ##################################

## Tiempo de vida en el banco 
dataset[, vida_banco := (cliente_antiguedad/12) / (cliente_edad)]

## Total Pasivos
dataset[, total_deuda := mprestamos_personales + mprestamos_prendarios + mprestamos_hipotecarios + Visa_msaldototal + Master_msaldototal]

## Total Activos
dataset[, total_activos := mplazo_fijo_dolares + mplazo_fijo_pesos + minversion1_pesos + minversion1_dolares + minversion2 + mcuentas_saldo]

## Balance en el banco
dataset[, balance := total_activos - total_deuda]
dataset[, ratio_deuda := total_deuda / (total_activos + 1)]

## Cantidad de tarjetas total
dataset[, ctarjetas := ctarjeta_visa + ctarjeta_master]

## Total seguros
dataset[, cseguros := cseguro_vida + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales]

## Total payroll
dataset[, mpayroll_total := mpayroll + mpayroll2]

## Total débitos automáticos
dataset[, debitos_automaticos := mcuenta_debitos_automaticos + mttarjeta_visa_debitos_automaticos + mttarjeta_master_debitos_automaticos]

## Total Consumos y gastos
dataset[, total_consumos := mautoservicio + mtarjeta_visa_consumo + mtarjeta_master_consumo + mpagodeservicios + debitos_automaticos]

## Total descuentos (sobre total de consumos?)
dataset[, total_descuentos := mtarjeta_visa_descuentos + mtarjeta_master_descuentos + mcajeros_propios_descuentos]

## descuentos sobre consumos?
dataset[, total_descuentos_sobre_consumos := ifelse(dataset$total_consumos == 0, NA, total_descuentos / total_consumos)]

## Total comisiones
dataset[, total_comisiones := mcomisiones_mantenimiento + mcomisiones_otras]

## Balance transferencias
dataset[, balance_transferencias := mtransferencias_recibidas - mtransferencias_emitidas]

## ¿hace más transacciones en cajeros de otros bancos?
dataset[, cajeros_ajenos := ifelse(dataset$matm < dataset$matm_other, 1, 0)]

## ctrx quarter / cantidad de productos?
dataset[, ctrx_x_producto := ctrx_quarter / cproductos]

## comisiones / ctrx_quarter?
dataset[, comisiones_x_trx := total_comisiones / (ctrx_quarter + 1) ]

## total limite de compra
dataset[, limite_compra_total := Visa_mlimitecompra + Master_mlimitecompra]
dataset[, limite_compra_promedio := ifelse(dataset$ctarjetas == 0, NA, limite_compra_total / ctarjetas) ]

## limite de compra sobre ingresos
dataset[, limite_compra_sobre_ingresos := ifelse(dataset$mpayroll_total == 0, NA, limite_compra_total / mpayroll_total) ]
dataset[, limite_compra_sobre_activos := ifelse(dataset$total_activos == 0, NA, limite_compra_total / total_activos) ]

## limite de compra real vs esperado según ingreso
limite_esperado = median(dataset[mpayroll_total > 0, limite_compra_total / mpayroll_total], na.rm=TRUE)
dataset[, limite_compra_real_sobre_esperado := ifelse(dataset$total_activos == 0, NA, mpayroll_total * limite_esperado - limite_compra_total) ]

## suma de columnas null
dataset$na_count <- apply(dataset, 1, function(x) sum(is.na(x)))

#tapply(ifelse(dataset$clase_binaria == 'evento', 1, 0), cut(dataset$comisiones_x_trx, breaks = 10), mean, na.rm = TRUE)
#tapply(dataset$Visa_Fvencimiento/30, dataset$clase_binaria, summary)
#View(dataset[sample(nrow(dataset), 15), ])


############################################## separo enero de marzo ##################################################


# Nos quedamos solo con el 202101
dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

# Creamos una clase binaria
dtrain[, clase_binaria := ifelse(
  clase_ternaria == "CONTINUA",
  "noevento",
  "evento"
)]

# Borramos el target viejo
dtrain[, clase_ternaria := NULL]
dapply[, clase_ternaria := NULL]

############################################ particiono en train y "validación"  ######################################################

#set.seed(semillas[1])

# Particionamos de forma estratificada
#in_training <- caret::createDataPartition(dataset_enero$clase_binaria, p = 0.70, list = FALSE)

#dtrain  <-  dataset_enero[in_training, ]
#dvalid   <-  dataset_enero[-in_training, ]

#calcular_ganancia <- function(modelo, test) {
#  pred_testing <- predict(modelo, test, type = "prob")
#  sum(
#    (pred_testing[, "evento"] >= 0.025) * ifelse(test$clase_binaria == "evento",
#                                                 78000, -2000) / 0.3
#  )
#}

####################################### Optimización bayesiana ###########################################################

set.seed(semillas[1])

ganancia <- function(probabilidades, clase, threshold = 0.025) {
  return(sum(
    (probabilidades >= threshold) * ifelse(clase == "evento", 78000, -2000))
  )
}

modelo_rpart_ganancia <- function(train, test, cp =  0, ms = 20, mb = 1, md = 10, threshold = 0.025) {
  modelo <- rpart(clase_binaria ~ ., data = train,
                  xval = 0,
                  cp = cp,
                  minsplit = ms,
                  minbucket = mb,
                  maxdepth = md)
  
  test_prediccion <- predict(modelo, test, type = "prob")
  ganancia(test_prediccion[, "evento"], test$clase_binaria, threshold) / 0.3
  
}

experimento_rpart_completo <- function(ds, semillas, cp = -1, ms = 20, mb = 1, md = 10, threshold = 0.025) {
  gan <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70, list = FALSE)
    train  <-  ds[in_training, ]
    test   <-  ds[-in_training, ]
    #train_sample <- tomar_muestra(train)
    r <- modelo_rpart_ganancia(train, test, cp = cp, ms = ms, mb = mb, md = md, threshold)
    gan <- c(gan, r)
  }
  mean(gan)
}


obj_fun_md_ms_mb <- function(x) {
  experimento_rpart_completo(dtrain, semillas
                             , md = x$maxdepth
                             , ms = x$minsplit
                             , mb = floor(x$minsplit*x$minbucket)
                             , threshold = x$threshold)
}

obj_fun <- makeSingleObjectiveFunction(
  minimize = FALSE,
  fn = obj_fun_md_ms_mb,
  par.set = makeParamSet(
    makeIntegerParam("maxdepth",  lower = 3L, upper = 20L),
    makeIntegerParam("minsplit",  lower = 200L, upper = 8000L),
    makeNumericParam("minbucket",  lower = 0L, upper = 1L),
    makeNumericParam("threshold",  lower = 0.02, upper = 0.075),
  ),
  noisy = TRUE,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 100L)
ctrl <- setMBOControlInfill(
  ctrl,
  crit = makeMBOInfillCritEI(),
  opt = "focussearch",
  # sacar parámetro opt.focussearch.points en próximas ejecuciones
  #opt.focussearch.points = 20
)

lrn <- makeMBOLearner(ctrl, obj_fun)

surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )
print(run_md_ms)


############################################# predigo marzo ##################################################################

modelo  <- rpart(formula=   "clase_binaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -0.315,   #esto significa no limitar la complejidad de los splits
                 minsplit=  3437,     #minima cantidad de registros para que se haga el split
                 minbucket= 807,     #tamaño minimo de una hoja
                 maxdepth=  8 )    #profundidad maxima del arbol

prediccion  <- predict(object= modelo, newdata= dapply, type = "prob")

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "evento"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/COMP1" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/COMP1/K101_002.csv",
        sep=  "," )


## cambiar punto de corte: podría ser también parte del proceso de optimización.