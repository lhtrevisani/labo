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
colnames(ganancias_cv) <- c("iteracion_bayesiana", "ganancia_test", "ganancia_test_corte")

# leo las ganancias en test (walk forward validation)
arch_log  <- paste0( base_dir, "exp/", PARAM$experimento_wfv, "/ganancias_iteracion.csv" )
ganancias_wfv  <- fread( arch_log )
colnames(ganancias_wfv) <- c("iteracion_bayesiana", "ganancia_test", "ganancia_test_corte")

## corrijo la fecha de la iteración para contemplar el rato en el que se había caído la virtual machine, y parseo las fechas.
tb_log_cv = tb_log_cv %>%
  mutate(#fecha_parsed = ymd_hms(fecha),
         fecha_parsed = case_when(ymd_hms(fecha) > as.POSIXct("2022-10-21 16:00:00") ~ ymd_hms(fecha)- minutes(45), TRUE ~ ymd_hms(fecha)),
         cumulative_time = difftime(fecha_parsed, min(fecha_parsed), units='mins'),
         iteration_duration = as.numeric(difftime(fecha_parsed, lag(fecha_parsed), units='mins'))) %>%
  select(iteracion_bayesiana, fecha_parsed, num_iterations, learning_rate, feature_fraction, min_data_in_leaf, num_leaves, prob_corte, ganancia, ganancia_max_acum, cumulative_time, iteration_duration)


tb_log_wfv = tb_log_wfv %>%
  mutate(fecha_parsed = ymd_hms(fecha),
         cumulative_time = difftime(fecha_parsed, min(fecha_parsed), units='mins'),
         iteration_duration = as.numeric(difftime(fecha_parsed, lag(fecha_parsed), units='mins'))) %>%
  select(iteracion_bayesiana, fecha_parsed, num_iterations, learning_rate, feature_fraction, min_data_in_leaf, num_leaves, prob_corte, ganancia, ganancia_max_acum, cumulative_time, iteration_duration) %>%
  filter(iteracion_bayesiana < 148)

tb_log_cv = left_join(tb_log_cv, ganancias_cv, by = "iteracion_bayesiana")
tb_log_wfv = left_join(tb_log_wfv, ganancias_wfv, by = "iteracion_bayesiana")


rm(ganancias_cv)
rm(ganancias_wfv)


## cuál es la ganancia máxima a la que llegó cada uno de los modelos:

# cross validation:
tb_log_cv %>% tail(1) %>% select(ganancia_test_corte)

# walk forward validation:
tb_log_wfv %>% tail(1) %>% select(ganancia_test_corte)


## cuáles son los hiperparámetros de la mejor optimización bayesiana:

# cross validation:
tb_log_cv %>% tail(1) %>% select(num_iterations, learning_rate, feature_fraction, min_data_in_leaf, num_leaves)

# walk forward validation:
tb_log_wfv %>% tail(1) %>% select(num_iterations, learning_rate, feature_fraction, min_data_in_leaf, num_leaves)


## uno los dos datasets:
log_completo = rbind(tb_log_cv %>% mutate(modelo = 'cv'), tb_log_wfv %>% mutate(modelo = 'wfv'))


## tiempo promedio de la iteración:
log_completo %>%
  group_by(modelo) %>%
  summarise(iteracion_promedio = mean(iteration_duration, na.rm = TRUE))

log_ganancias = log_completo %>% filter(!is.na(ganancia_test_corte))
  

library(ggplot2)
library(hrbrthemes)

ggplot(log_ganancias,                            # Draw ggplot2 time series plot
       aes(x = cumulative_time,
           y = ganancia_test_corte,
           col = modelo)) +
  geom_line() + 
  geom_point() +
  geom_text(aes(label = iteracion_bayesiana), check_overlap = T, size = 3, nudge_y = -50000) +
  geom_text(aes(label = paste0("$",round(ganancia_test_corte/1000000, 2), "M")), check_overlap = T, nudge_y = 50000, size = 3) + 
  theme_ipsum() + 
  labs(title="Ganancia en test y cantidad de iteraciones",
       x ="Minutos desde la primera iteración", 
       y = "Ganancia en test",
       colour = "Validación"
       ) +
  scale_x_continuous(breaks = seq(0, 320, 60))

  