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
PARAM$exp_input  <- "HT9412b"
PARAM$exp_input_cv  <- "HT9412a"

base_dir <- "~/Documents/Maestria/"  # cloud:  "~/buckets/b1/"   #maestria


#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
tb_log$ganancia_max_acum = cummax(tb_log$ganancia)

#leo la salida de la optimizaciob bayesiana (cross validation)
arch_log_cv  <- paste0( base_dir, "exp/", PARAM$exp_input_cv, "/BO_log.txt" )
tb_log_cv  <- fread( arch_log_cv )
tb_log_cv$ganancia_max_acum = cummax(tb_log_cv$ganancia)

tb_log = tb_log %>%
  mutate(fecha_parsed = ymd_hms(fecha),
         cumulative_time = difftime(fecha_parsed, min(fecha_parsed), units='mins'),
         iteration_duration = as.numeric(difftime(fecha_parsed, lag(fecha_parsed), units='mins')))

tb_log_cv = tb_log_cv %>%
  mutate(fecha_parsed = ymd_hms(fecha),
         
         cumulative_time = difftime(fecha_parsed, min(fecha_parsed), units='mins'),
         iteration_duration = as.numeric(difftime(fecha_parsed, lag(fecha_parsed), units='mins')))
         


tb_log_cv %>% filter(fecha_parsed > as.POSIXct("2022-10-21 16:00:00")) 
