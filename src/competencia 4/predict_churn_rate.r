#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("dplyr")
require("prophet")
require("lubridate")
require("ggplot2")

PARAM  <- list()
PARAM$dataset  <- "./datasets/competenciaFINAL_2022.csv.gz"

setwd( "~/Documents/Maestria" )

#cargo el dataset
dataset  <- fread( PARAM$dataset )

dataset = dataset[,c("foto_mes", "clase_ternaria")]

dataset = dataset %>%
  group_by(foto_mes) %>%
  summarise(total = n(),
            bajas = sum(clase_ternaria == 'BAJA+2')) %>%
  mutate(foto_mes = ym(foto_mes),
         ratio_bajas = bajas/total)


dataset %>%
  ggplot(aes(x=foto_mes, y=ratio_bajas)) + geom_line() + geom_point()

prophet_df = dataset %>%
  select(foto_mes, ratio_bajas) %>%
  mutate(ds = foto_mes, y = ratio_bajas) %>%
  select(-c(foto_mes, ratio_bajas)) %>%
  filter(ds < '2021-08-01')


pandemia  = data.frame(holiday= 'pandemia',
                       ds=ym(c('2020-03','2020-04','2020-05','2020-06', '2020-07','2020-08')),
                       lower_window= 0,
                       upper_window= 0)

prophet_mensual = prophet(holidays=pandemia, seasonality.mode = 'multiplicative')
#prophet_mensual = add_seasonality(m=prophet_mensual, name='monthly', period=365/12, fourier.order = 4)

prophet_mensual = fit.prophet(m = prophet_mensual, prophet_df) 

plot(prophet_mensual,fcst=predict(prophet_mensual, prophet_df)) +theme_bw()
prophet_plot_components(prophet_mensual, fcst=predict(prophet_mensual, prophet_df))


future <- make_future_dataframe(prophet_mensual, periods=2, freq= 60 * 60 * 24 *31)

forecast <- predict(prophet_mensual, future)

plot(prophet_mensual,fcst=forecast) +theme_bw()


forecast %>%
  select(ds, yhat)
