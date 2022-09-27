rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("lightgbm")
require("xgboost")
require("DiagrammeR")

# Poner la carpeta de la materia de SU computadora local
setwd("/home/lucas/Maestria/DMEyF")

# Poner sus semillas
semillas <- c(700423, 700429, 700433, 700459, 700471)

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/dataset_7110.csv")
marzo <- dataset[foto_mes == 202103]

# Clase BAJA+1 y BAJA+2 juntas
clase_binaria <- ifelse(marzo$clase_ternaria == "CONTINUA", 0, 1)
clase_real <- dataset$clase_ternaria
marzo$clase_ternaria <- NULL
dataset$clase_ternaria <- NULL

## voy a buscar algunas features nuevas con xgboost

set.seed(semillas[1])

dtrain <- xgb.DMatrix(
  data = data.matrix(marzo),
  label = clase_binaria, missing = NA)

param_fe <- list(
  colsample_bynode = 0.8,
  learning_rate = 1,
  max_depth = 3, # <--- IMPORTANTE CAMBIAR
  num_parallel_tree = 10, # <--- IMPORTANTE CAMBIAR
  subsample = 0.8,
  objective = "binary:logistic"
)

xgb_model <- xgb.train(params = param_fe, data = dtrain, nrounds = 1)

# agrego algunos canaritos para ver qué variables de las creadas pueden ser relevantes
set.seed(semillas[1])

for (i in 1:20)  {
  marzo[, paste0("canarito", i) := runif(nrow(marzo))]
}

new_features <- xgb.create.features(model = xgb_model, data.matrix(marzo))

# Veo la importancia de las variables creadas en el lightGBM

dtrain_lgb  <- lgb.Dataset(
  data = data.matrix(new_features),
  label = clase_binaria)

mlgb <- lgb.train(
  dtrain_lgb,
  params = list(
    objective = "binary",
    max_bin = 15,
    min_data_in_leaf = 4000,
    learning_rate = 0.05,
    num_iterations = 500 ## <-- aumento las iteraciones
  ),
  verbose = -1)

var_importance <- lgb.importance(mlgb)$Feature

# ultimo canarito
list_canaritos <- grepl("canarito", var_importance)
idx <- seq(length(list_canaritos))
mejor_canarito = min(idx[list_canaritos])

# variables con v por encima del canarito
id_var_utiles <- grepl("V\\d+", var_importance[1:mejor_canarito])
var_utiles = var_importance[1:mejor_canarito][id_var_utiles]

## agrego nuevas features al dataset original.
rm(marzo)
rm(new_features)

new_features_full_dataset <- xgb.create.features(model = xgb_model, data.matrix(dataset))
dataset = as.data.frame(as.matrix(new_features_full_dataset))

rm(new_features_full_dataset)
dataset$clase_ternaria = clase_real

  
## elimino los canaritos y las variables del RF que no aportan nada

## vuelvo a agregar la clase ternaria

## guardo los cambios al dataset





#grabo el dataset
fwrite( dataset,
        "./datasets/dataset_7110.csv",
        logical01= TRUE,
        sep= "," )