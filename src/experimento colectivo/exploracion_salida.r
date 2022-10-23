#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("dplyr")
require("ggplot2")
require("lubridate")

#Parametros del script
PARAM  <- list()

PARAM$exp_input_cv  <- "HT9412a"
PARAM$exp_input_wfv  <- "HT9412b"

PARAM$experimento_cv  <- "ZZ9412a"
PARAM$experimento_wfv  <- "ZZ9412b"

base_dir <- "~/Documents/Maestria/"  # cloud:  "~/buckets/b1/"   #maestria

#leo la salida de la optimizaciob bayesiana (cross validation)
arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input_cv, "/BO_log.txt" )
tb_log_cv  <- fread( arch_log )
tb_log_cv$ganancia_max_acum = cummax(tb_log_cv$ganancia)

#leo la salida de la optimizaciob bayesiana (walk forward validation)
arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input_wfv, "/BO_log.txt" )
tb_log_wfv  <- fread( arch_log )
tb_log_wfv$ganancia_max_acum = cummax(tb_log_wfv$ganancia)

# leo las ganancias en test (cross validaition)
arch_log  <- paste0( base_dir, "exp/", PARAM$experimento_cv, "/ganancias_iteracion.csv" )
ganancias_cv  <- fread( arch_log )
colnames(ganancias_cv) <- c("iteracion_bayesiana", "ganancia_test")

# leo las ganancias en test (walk forward validation)
arch_log  <- paste0( base_dir, "exp/", PARAM$experimento_wfv, "/ganancias_iteracion.csv" )
ganancias_wfv  <- fread( arch_log )
colnames(ganancias_wfv) <- c("iteracion_bayesiana", "ganancia_test")

## corrijo la fecha de la iteración para contemplar el rato en el que se había caído la virtual machine, y parseo las fechas.
tb_log_cv = tb_log_cv %>%
  mutate(fecha_parsed = case_when(ymd_hms(fecha) > as.POSIXct("2022-10-21 16:00:00") ~ ymd_hms(fecha)- minutes(45),
                                    TRUE ~ ymd_hms(fecha)),
         cumulative_time = difftime(fecha_parsed, min(fecha_parsed), units='mins'),
         iteration_duration = as.numeric(difftime(fecha_parsed, lag(fecha_parsed), units='mins'))) %>%
  select(iteracion_bayesiana, fecha_parsed, num_iterations, learning_rate, feature_fraction, min_data_in_leaf, num_leaves, prob_corte, ganancia, ganancia_max_acum, cumulative_time, iteration_duration)


tb_log_wfv = tb_log_wfv %>%
  mutate(fecha_parsed = ymd_hms(fecha),
         cumulative_time = difftime(fecha_parsed, min(fecha_parsed), units='mins'),
         iteration_duration = as.numeric(difftime(fecha_parsed, lag(fecha_parsed), units='mins'))) %>%
  select(iteracion_bayesiana, fecha_parsed, num_iterations, learning_rate, feature_fraction, min_data_in_leaf, num_leaves, prob_corte, ganancia, ganancia_max_acum, cumulative_time, iteration_duration)

tb_log_cv = left_join(tb_log_cv, ganancias_cv, by = "iteracion_bayesiana")
tb_log_wfv = left_join(tb_log_wfv, ganancias_wfv, by = "iteracion_bayesiana")


rm(ganancias_cv)
rm(ganancias_wfv)
