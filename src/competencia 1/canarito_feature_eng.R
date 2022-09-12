#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")
require("dplyr")

setwd("/home/lucas/Maestria/DMEyF") #establezco la carpeta donde voy a trabajar

#cargo el dataset
dataset  <- fread( "./datasets/competencia1_2022.csv")

######################################## feature engineering ##############################################################################

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

##################################################################################################################################

#uso esta semilla para los canaritos
set.seed(700423)

#agrego 30 canaritos
for( i in 1:30 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]

dtrain <- dataset[ foto_mes==202101 ]
dapply <- dataset[ foto_mes==202103 ]

dtrain[, clase_binaria := ifelse(
  clase_ternaria == "CONTINUA",
  "noevento",
  "evento"
)]

####################################################################################################################################

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


dtrain = dtrain[ , !names(dtrain) %in% mis_variables, with=FALSE]
dapply = dapply[ , !names(dapply) %in% mis_variables, with=FALSE]

#######################################################################################################################################

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

#######################################################################################################################################


#Primero  veo como quedan mis arboles
modelo_original <- rpart(
    formula= "clase_binaria ~ . -mcomisiones_mantenimiento -Visa_mpagado -clase_ternaria",
    data= dtrain,
    model= TRUE,
    xval= 0,
    cp= -1,
    minsplit= 2, # dejo que crezca y corte todo lo que quiera
    minbucket= 1,
    maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart

modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )

prediccion  <- predict( modelo_pruned, dapply, type = "prob")[,"evento"]

entrega  <-  as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente,
                                  "Predicted"= as.integer(  prediccion > 0.045 ) ) )

modelo_original$variable.importance

dir.create( "./exp/" )
dir.create( "./exp/COMP1" )
dir.create( "./exp/COMP1/canaritos" )

fwrite( entrega, file= "./exp/COMP1/canaritos/canaritos_012.csv", sep="," )

pdf(file = "./exp/COMP1/canaritos/stopping_at_canaritos.pdf", width=28, height=4)
prp(modelo_pruned, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

