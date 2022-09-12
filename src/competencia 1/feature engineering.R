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


###################### pequeña exploración de los atributos #################################################

ggplot(dataset, aes(x = mcomisiones)) +
  facet_grid(clase_ternaria ~ .) +
  geom_density()


#tapply(ifelse(dataset$clase_binaria == 'evento', 1, 0), cut(dataset$comisiones_x_trx, breaks = 10), mean, na.rm = TRUE)
tapply(dataset$mcuenta_corriente, dataset$clase_ternaria, summary)
prop.table(table(dataset$cproductos, dataset$clase_ternaria), margin = 2)
View(dataset[sample(nrow(dataset), 15), ])


# binarizar:
     # edad, cliente_antiguedad, mrentabilidad, mrentabilidad_annual, mactivos_margen, mpasivos_margen, mcuentas_saldo
     # ctrx_quarter
     # floor(dataset$Master_fechaalta/365)

######################  Realizo feature engineering sobre el dataset (operaciones entre columnas) ##################################

## Tiempo de vida en el banco 
dataset[, vida_banco := (cliente_antiguedad/12) / (cliente_edad)]

## Ganancias banco
dataset[, mmargen := rowSums(.SD, na.rm = TRUE), .SDcols = c("mactivos_margen", "mpasivos_margen")]
dataset[, mmargen_x_producto := mmargen / cproductos]

## Total Pasivos
dataset[, total_deuda := rowSums(.SD, na.rm = TRUE), .SDcols = c("mprestamos_personales", "mprestamos_prendarios", "mprestamos_hipotecarios", "Visa_msaldototal", "Master_msaldototal")]

## Total Activos
dataset[, total_activos := rowSums(.SD, na.rm = TRUE), .SDcols = c("mplazo_fijo_dolares", "mplazo_fijo_pesos", "minversion1_pesos", "minversion1_dolares", "minversion2", "mcuentas_saldo")]

## Balance en el banco
dataset[, balance := total_activos - total_deuda]
dataset[, ratio_deuda := total_deuda / (total_activos + 1)]

## saldos
dataset[, has_cuentacorriente_saldo_pos := ifelse(mcuenta_corriente > 0, 1, 0) ]
dataset[, has_cajaahorro_saldo_pos := ifelse(mcaja_ahorro > 0, 1, 0) ]
dataset[, has_saldo_pos := ifelse(mcaja_ahorro + mcuenta_corriente > 0, 1, 0) ]

## Tiene cuenta homebanking
dataset[, has_internet := ifelse(dataset$internet > 0, 1, 0) ]

## Tiene movimientos/tarjetas
dataset[, has_debito_transacciones := ifelse(dataset$ctarjeta_debito_transacciones > 0, 1, 0) ]
dataset[, has_visa := ifelse(dataset$ctarjeta_visa > 0, 1, 0) ]
dataset[, has_visa_transacciones := ifelse(dataset$ctarjeta_visa_transacciones > 0, 1, 0) ]
dataset[, has_master := ifelse(dataset$ctarjeta_master > 0, 1, 0) ]
dataset[, has_master_transacciones := ifelse(dataset$ctarjeta_master_transacciones > 0, 1, 0) ]

## Cantidad de tarjetas total
dataset[, ctarjetas := rowSums(.SD, na.rm = TRUE), .SDcols = c("ctarjeta_visa", "ctarjeta_master")]

## Total seguros
dataset[, cseguros := cseguro_vida + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales]

## Recibo pago de sueldo?
dataset[, has_payroll := ifelse(dataset$cpayroll_trx + dataset$cpayroll2_trx  > 0, 1, 0) ]

## Total payroll
dataset[, mpayroll_total := mpayroll + mpayroll2]

## Tiene débitos automáticos?
dataset[, has_da := ifelse(dataset$ccuenta_debitos_automaticos + dataset$ctarjeta_visa_debitos_automaticos + dataset$ctarjeta_master_debitos_automaticos  > 0, 1, 0) ]

## Utiliza pago mis cuentas?
dataset[, has_pmc := ifelse(dataset$cpagomiscuentas  > 0, 1, 0) ]

## Total débitos automáticos
dataset[, debitos_automaticos := mcuenta_debitos_automaticos + mttarjeta_visa_debitos_automaticos + mttarjeta_master_debitos_automaticos]

## Total Consumos y gastos
dataset[, total_consumos := rowSums(.SD, na.rm = TRUE), .SDcols = c("mautoservicio", "mtarjeta_visa_consumo", "mtarjeta_master_consumo", "mpagodeservicios", "debitos_automaticos")]

## Total descuentos (sobre total de consumos?)
dataset[, total_descuentos := rowSums(.SD, na.rm = TRUE), .SDcols = c("mtarjeta_visa_descuentos", "mtarjeta_master_descuentos", "mcajeros_propios_descuentos")]

## Descuentos sobre consumos
dataset[, total_descuentos_sobre_consumos := ifelse(dataset$total_consumos == 0, 0, total_descuentos / total_consumos)]

## Total comisiones
dataset[, has_comisiones := ifelse(dataset$ccomisiones_mantenimiento + dataset$ccomisiones_otras > 0, 1, 0) ]
dataset[, total_comisiones := mcomisiones_mantenimiento + mcomisiones_otras]

## Balance transferencias
dataset[, balance_transferencias := mtransferencias_recibidas - mtransferencias_emitidas]

## ¿hace más transacciones en cajeros de otros bancos?
dataset[, cajeros_ajenos := ifelse(dataset$matm < dataset$matm_other, 1, 0)]

## ctrx quarter / cantidad de productos?
dataset[, ctrx_x_producto := ctrx_quarter / cproductos]

## comisiones / ctrx_quarter?
dataset[, comisiones_x_trx := total_comisiones / (ctrx_quarter + 1) ]

## estado master:
dataset[, Master_status := ifelse(dataset$Master_status  > 0, 1, 0) ]

# fechas tarjetas: llevo a años:
dataset[, master_vencimiento := floor(dataset$Master_Fvencimiento/365)]
dataset[, master_alta := floor(dataset$Master_fechaalta/365)]
dataset[, visa_vencimiento := floor(dataset$Visa_Fvencimiento/365)]
dataset[, visa_alta := floor(dataset$Visa_fechaalta/365)]

# consolido variables de las tarjetas
dataset[, saldo_total_tarjetas := rowSums(.SD, na.rm = TRUE), .SDcols = c("Master_msaldototal", "Visa_msaldototal")]
dataset[, pagado_total_tarjetas := rowSums(.SD, na.rm = TRUE), .SDcols = c("Master_mpagado", "Visa_mpagado")]
dataset[, mconsumos_total_tarjetas := rowSums(.SD, na.rm = TRUE), .SDcols = c("Master_mconsumototal", "Visa_mconsumototal")]
dataset[, cconsumos_total_tarjetas := rowSums(.SD, na.rm = TRUE), .SDcols = c("Master_cconsumos", "Visa_cconsumos")]

## total limite de compra
dataset[, limite_compra_total := rowSums(.SD, na.rm = TRUE), .SDcols = c("Visa_mlimitecompra", "Master_mlimitecompra")]
dataset[, limite_compra_promedio := ifelse(dataset$ctarjetas == 0, 0, limite_compra_total / ctarjetas) ]

## consumos sobre limites
dataset[, saldo_compra_sobre_limite := ifelse(dataset$limite_compra_total == 0, 0, saldo_total_tarjetas / limite_compra_total) ]
dataset[, consumos_compra_sobre_limite := ifelse(dataset$limite_compra_total == 0, 0, mconsumos_total_tarjetas / limite_compra_total) ]

# pagado sobre saldo
dataset[, pagado_sobre_saldo := ifelse(dataset$saldo_total_tarjetas == 0, 0, pagado_total_tarjetas / saldo_total_tarjetas) ]

# consumo promedio
dataset[, consumo_promedio := ifelse(dataset$cconsumos_total_tarjetas == 0, 0, mconsumos_total_tarjetas /  cconsumos_total_tarjetas) ]

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
#dtrain[, clase_ternaria := NULL]
#dapply[, clase_ternaria := NULL]


############################################### binarizo algunas variables ###########################################################

mis_variables <- c("cliente_edad",
                   "cliente_antiguedad",
                   "mrentabilidad",
                   "mrentabilidad_annual",
                   "mactivos_margen",
                   "mpasivos_margen",
                   "mcaja_ahorro",
                   "mcuentas_saldo",
                   "mtarjeta_visa_consumo",
                   "mtarjeta_master_consumo",
                   "mpayroll",
                   "mtransferencias_recibidas",
                   "mtransferencias_emitidas",
                   "mtarjeta_visa_consumo",
                   "chomebanking_transacciones",
                   "ctrx_quarter",
                   "Master_mfinanciacion_limite",
                   "Master_Fvencimiento",
                   "Master_mlimitecompra",
                   "Master_msaldototal",
                   "Master_fechaalta",
                   "Visa_mfinanciacion_limite",
                   "Visa_Fvencimiento",
                   "Visa_mlimitecompra",
                   "Visa_msaldototal",
                   "Visa_fechaalta",
                   "vida_banco",
                   "mmargen",
                   "mmargen_x_producto",
                   "total_deuda",
                   "total_activos",
                   "balance",
                   "ratio_deuda",
                   "total_consumos",
                   "total_comisiones",
                   "balance_transferencias",
                   "ctrx_x_producto",
                   "comisiones_x_trx",
                   "saldo_total_tarjetas",
                   "pagado_total_tarjetas",
                   "mconsumos_total_tarjetas",
                   "limite_compra_total",
                   "limite_compra_promedio",
                   "consumo_promedio",
                   "limite_compra_real_sobre_esperado"
                   )

# A todas las vamos a rankear

prefix <- "r_"
for (var in mis_variables) {
  dtrain[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
  dapply[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
}

#dtrain = dtrain[ , !names(dtrain) %in% mis_variables, with=FALSE]
#dapply = dapply[ , !names(dapply) %in% mis_variables, with=FALSE]

############################################ corrijo data drifting ###################################################################

# corrijo manualmente el drifting de  Visa_fultimo_cierre
dapply[ Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
dapply[ Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
dapply[ Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
dapply[ Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
dapply[ Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
dapply[ Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
dapply[ Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]

# corrijo manualmente el drifting de  Visa_fultimo_cierre
dapply[ Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
dapply[ Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
dapply[ Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
dapply[ Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
dapply[ Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
dapply[ Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
dapply[ Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]

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
  return(sum((probabilidades >= threshold) * ifelse(clase == "BAJA+2", 78000, -2000))
  )
}

modelo_rpart_ganancia <- function(train, test, cp = -1, ms = 20, mb = 1, md = 10, threshold = 0.025) {
  modelo <- rpart(clase_binaria ~ . -clase_ternaria, data = train,
                  xval = 0,
                  cp = cp,
                  minsplit = ms,
                  minbucket = mb,
                  maxdepth = md)
  
  test_prediccion <- predict(modelo, test, type = "prob")
  
  ganancia(test_prediccion[, "evento"], test$clase_ternaria, threshold = threshold) / 0.3
  
}

experimento_rpart_completo <- function(ds, semillas, cp = -1, ms = 20, mb = 1, md = 10, threshold = 0.025) {
  gan <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70, list = FALSE)
    train  <-  ds[in_training, ]
    test   <-  ds[-in_training, ]
    #train_sample <- tomar_muestra(train)
    r <- modelo_rpart_ganancia(train, test, cp = cp, ms = ms, mb = mb, md = md, threshold = threshold)
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
    makeIntegerParam("minsplit",  lower = 1L, upper = 5000L),
    makeNumericParam("minbucket",  lower = 0L, upper = 0.5),
    makeNumericParam("threshold",  lower = 0.03, upper = 0.08)
  ),
  noisy = TRUE,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 100L)
ctrl <- setMBOControlInfill(
  ctrl,
  crit = makeMBOInfillCritEI(),
  opt = "focussearch"
  # sacar parámetro opt.focussearch.points en próximas ejecuciones
  #opt.focussearch.points = 20
)

lrn <- makeMBOLearner(ctrl, obj_fun)

surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl)
print(run_md_ms)

df_parameters <- as.data.frame(run_md_ms$opt.path)

df_parameters$y

############################################# predigo marzo ##################################################################

modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -1,   #esto significa no limitar la complejidad de los splits
                 minsplit=  823,     #minima cantidad de registros para que se haga el split
                 minbucket= 148,     #tamaño minimo de una hoja
                 maxdepth=  7 )    #profundidad maxima del arbol

threshold = 0.0518

prediccion  <- predict(object= modelo, newdata= dapply, type = "prob")

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "evento"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > threshold ) ]

prop.table(table(dapply$Predicted))

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/COMP1" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/COMP1/K101_006.csv",
        sep=  "," )

write.csv(df_parameters, "./exp/COMP1/K101_006_parameters.csv", row.names = FALSE)


########################################### pruebo con múltiples cortes ################################################################

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dfinal  <- copy( dapply[ , list(numero_de_cliente) ] )
dfinal[ , prob_SI := prediccion[ , "evento"] ]

# por favor cambiar por una semilla propia
# que sino el Fiscal General va a impugnar la prediccion
set.seed(700423)  
dfinal[ , azar := runif( nrow(dapply) ) ]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder( dfinal, -prob_SI, azar )


for( corte  in  c( 7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000 ) )
{
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]
  
  
  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
          file= paste0( "./exp/COMP1/KA101_007_",  corte, ".csv"),
          sep=  "," )
}
